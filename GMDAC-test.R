#################
# IOM GMDAC - Data Analytics Consultant test
#################


############ Packages
# install.packages("tidyverse")
# install.packages("knitr")
# install.packages("lme4")
# install.packages("leaflet")

#Let's load the required packages into the R session.
library(tidyverse)
library(knitr)  #used to print nice tables in R Markdown
library(lme4)  #mixed models
library(leaflet)  #maps

############ Data
students <- readxl::read_excel("data/data_students.xlsx")
faculty <- readxl::read_excel("data/data_faculty.xlsx")

#`read_xl` incorrectly reads some columns as characters instead of numbers. Let's change that:
students <- students %>% mutate(
  age = as.integer(age),
  gpa_2010 = as.numeric(gpa_2010),
  gpa_2011 = as.numeric(gpa_2011),
  gpa_2012 = as.numeric(gpa_2012),
  gpa_2013 = as.numeric(gpa_2013),
  gpa_2014 = as.numeric(gpa_2014),
  gpa_2015 = as.numeric(gpa_2015),
  gpa_2016 = as.numeric(gpa_2016),
  gpa_2017 = as.numeric(gpa_2017),
  gpa_2018 = as.numeric(gpa_2018),
  gpa_2019 = as.numeric(gpa_2019),
  gpa_2020 = as.numeric(gpa_2020),
  lifesat = as.integer(lifesat),
  like = as.integer(like),
  term = as.integer(term),
)

#I noticed several rows with no data. Let's filter out those rows. Let's also add in ID of the students
students <- students %>% filter(complete.cases(.)) %>% rowid_to_column("ID")

############
# DATA MANIPULATION & DESCRIPTIVE STATS
############

############ Summary table 
students %>%
  group_by(faculty) %>%
  summarise(n = length(cob),
            perc_notGerm = round(100*sum(cob != "Germany", na.rm=TRUE)/n),
            avg_lifesat = round(mean(lifesat, na.rm=TRUE), digits=2),
            perc_relationship = round(100*sum(relationship == "In a relationship", na.rm=TRUE)/n),
            perc_female= round(100*sum(sex == "Female", na.rm=TRUE)/n),
            perc_ageover30 = round(100*sum(age > 30, na.rm=TRUE)/n),
            avg_gpa_2010 = round(mean(gpa_2010, na.rm=TRUE), digits=2),
            avg_term = round(mean(term, na.rm=TRUE), digits=2)
  ) %>% t()   #I'll transpose for the sake of space in the pdf document

############ Differences in average life satisfaction 
#calculating average lifesat by faculty and relationship
students_lifesat <- students %>% 
  group_by(faculty, relationship) %>% 
  summarise(lifesat= mean(lifesat))

#let's sort the faculty based on mean lifesat so the graph is nicer
faculty_lifesat <- students %>% group_by(faculty) %>% summarise(lifesat = mean(lifesat)) %>% arrange(desc(lifesat)) 

#reordering the faculty in the table used for plotting
students_lifesat$faculty <- factor(students_lifesat$faculty, levels=faculty_lifesat$faculty)

ggplot(students_lifesat, aes(x=lifesat, y=faculty)) +
  geom_point(aes(color=relationship), size=3) +
  theme_minimal() + 
  theme(axis.title.y=element_blank(),
        legend.title=element_blank(),
        legend.position = "top") +
  xlab("Average Life Satisfaction") +
  xlim(0,100)


############Average cost of the career by faculty and job status

#joining student data with faculty data
students <- students %>% left_join(faculty, by="faculty")


############ Relationship between life satisfaction and age
ggplot(students, aes(lifesat, age)) +geom_hex(bins=8) +
  scale_fill_viridis_c()


############
# MODELLING
############

############ Relationship status effect on life satisfaction
lm1 <- lm(students$lifesat~ students$relationship)

ggplot(students, aes(x=relationship, y=lifesat, color=relationship)) + 
  geom_point(alpha = 0.5)+
  geom_label(x = 1.5, y = 80, label = paste0("R2 = ", round(summary(lm1)$r.squared, digits=3)))+
  geom_abline(slope = coef(lm1)[2], intercept = coef(lm1)[1]) + 
  geom_label(x = 1.5, y = 100, label = paste0("model: lifesat = ", round(coef(lm1)[1], digits=3), " + ", round(coef(lm1)[2], digits=3), "relationship"))+
  theme(legend.position = "none")


############ Job effect on mean GPA
#let's estimate average GPA
students <- students %>% mutate(avg_gpa = rowMeans(.[,6:16]))

lm1 <- lm(students$avg_gpa~ students$job)

ggplot(students, aes(x=job, y=avg_gpa, color=job)) + 
  geom_point(alpha = 0.5) +
  geom_label(x = 1.5, y = 3.5, label = paste0("model: avg_gpa = ", round(coef(lm1)[1], digits=3), " + ", round(coef(lm1)[2], digits=3), "job_status"))+
  geom_label(x = 1.5, y = 3, label = paste0("R2 = ", round(summary(lm1)$r.squared, digits=3)))+
  geom_abline(slope = coef(lm1)[2], intercept = coef(lm1)[1]) + 
  theme(legend.position = "none")  
                                

############ GPA forecast
#MIXED MODEL
students_long <- students %>%
  pivot_longer(cols = gpa_2010:gpa_2020, names_to="gpa_year", values_to = "gpa") %>% 
  mutate(year = as.integer(str_sub(gpa_year, -4, -1), "0101"),  #creating year number column
         gpa0 = lag(gpa, 1),   #adding the GPA of the previous year as a column to use as predictor
         delta_gpa  = gpa - gpa0)
         
#remove values for 2010 which are incorrect (there is no previous year from 2010)
students_long$gpa0[students_long$gpa_year == "gpa_2010"] <- NA

#mixed effects model to predict gpa 
lm1 <- lm(gpa ~ gpa0 + year + age + cob + faculty + job + relationship + sex + term, data = students_long)

#create new database to use for prediction and predict changing the initial values of GPA and year
students_2021 <- students %>% mutate(gpa0 = gpa_2020, year = 2021)
students$gpa_2021 <- predict(lm1, newdata=students_2021)
students_2022 <- students %>% mutate(gpa0 = gpa_2021, year = 2022)
students$gpa_2022 <- predict(lm1, newdata=students_2022)

#convert to long for easy plotting
students_long_lmer <- students %>%
  pivot_longer(cols = c(gpa_2010:gpa_2020,gpa_2021, gpa_2022), names_to="gpa_year", values_to = "gpa") %>% 
  mutate(year = as.integer(str_sub(gpa_year, -4, -1), "0101"))    #creating year number column


#SIMPLE MODEL WITH YEAR OF DELTA GPA
lm2 <- lm(delta_gpa~year, data=students_long)

students$gpa_2021 <- students$gpa_2020 + coef(lm2)[2]
students$gpa_2022 <- students$gpa_2021 + coef(lm2)[2]

#convert to long for easy plotting
students_long_lm <- students %>%
  pivot_longer(cols = c(gpa_2010:gpa_2020,gpa_2021, gpa_2022), names_to="gpa_year", values_to = "gpa") %>% 
  mutate(year = as.integer(str_sub(gpa_year, -4, -1), "0101"))    #creating year number column

#PLOTTING BOTH MODELS
ggplot(students_long_lmer,  aes(x=year, y=gpa))+
  geom_line(alpha=0.1, aes(group=ID)) +
  geom_line(data=students_long_lmer %>% filter(year %in% c(2020, 2021,2022)),alpha=0.1, aes(group=ID), color = "green") +
  ggtitle("lmer model: Converges because there is no trend with covariates")


ggplot(students_long_lm,  aes(x=year, y=gpa))+
  geom_line(alpha=0.1, aes(group=ID)) +
  geom_line(data=students_long_lm %>% filter(year %in% c(2020, 2021,2022)),alpha=0.1, aes(group=ID), color = "blue") +
  ggtitle("simple mean model: All change the same as the historical mean")



############
# VISUALIZATION
############

############
# MAPS
############

countries <- geojsonio::geojson_read("data/custom.geo.json", what = "sp")
#class(states)
#names(states)

#bind number of students to the sp map
n_students_cob <- table(students$cob) %>% data.frame()
n_students_cob$sovereignt <- as.character(n_students_cob$Var1)  #changing the name of the variable for join
n_students_cob$sovereignt[7] <- "United Kingdom" #changing the name to match in geojson

n_students_cob$sovereignt %in% countries$sovereignt  #check if the country names are in the names in the geojson

countries$n_students <- 0  #creating column
countries$n_students[match(n_students_cob$sovereignt, data.frame(countries)$sovereignt)] <- n_students_cob$Freq  #adding values to table
geojsonio::geojson_write(countries, file="data/FU.geo.json", overwrite=TRUE)


labels <- sprintf(
  "<strong>%s</strong><br/>%g students</sup>",
  countries$sovereignt, countries$n_students
) %>% lapply(htmltools::HTML)

#colors for the map
bins <- c(0, 100,150,200,250, Inf)
pal <- colorBin("YlOrRd", domain = countries$n_students, bins = bins)

#initialize map
m <- leaflet(countries) %>%
  setView(5, 48, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))


m %>% 
  addTiles() %>% 
  addPolygons(  fillColor = ~pal(n_students),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>% 
  addMarkers(lng=13.29, lat=52.45, popup="University of Berlin")
