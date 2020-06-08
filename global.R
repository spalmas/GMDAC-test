# OBJECTS AVAILABLE TO ui.r and server.r

# tables in use  ###########################################
students <- readxl::read_excel("data/data_students.xlsx")


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
