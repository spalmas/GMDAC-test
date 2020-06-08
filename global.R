# OBJECTS AVAILABLE TO ui.r and server.r

# tables in use  ###########################################
students <- readxl::read_excel("data/data_students.xlsx")


#colors for the map
bins <- c(0, 100,150,200,250, Inf)
pal <- colorBin("YlOrRd", domain = countries$n_students, bins = bins)

