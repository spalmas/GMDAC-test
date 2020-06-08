library(shiny)
library(RColorBrewer)
library(scales)
library(tidyverse)
library(leaflet)
library(DT)
library(ggplot2)

countries <- geojsonio::geojson_read("data/FU.geojson", what = "sp")


#colors for the map
bins <- c(0, 100,150,200,250, Inf)
pal <- colorBin("YlOrRd", domain = countries$n_students, bins = bins)



labels <- sprintf(
  "<strong>%s</strong><br/>%g students</sup>",
  countries$sovereignt, countries$n_students
) %>% lapply(htmltools::HTML)


shinyServer(function(input, output, session) {
  
  # create a color paletter for category type in the data file
  output$studentmap <- renderLeaflet({
    leaflet(countries) %>%
      setView(5, 48, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
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
      addMarkers(lng=13.29, lat=52.45, popup="Free University of Berlin")
  })
  

  output$student_plot <- renderPlot({
    ggplot(students, aes(x = cob, color = cob)) +
      geom_histogram(stat="count")
      
  })
  
  output$students_table <- renderDT(students,
                                    filter="top",
                                    options=list(pageLength=10))
 
  ## Download data handler ###########################################
  output$downloadData <- downloadHandler(filename = "students.csv",
                                         content = function(file) {
                                           write.csv(students, file, row.names = FALSE)
                                         })
})