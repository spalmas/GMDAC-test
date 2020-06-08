library(shiny)
library(shinyWidgets)
library(leaflet)
library(DT)


# MAIN PAGE 
navbarPage("Student population at FU", id="main",
           tabPanel("Map",
                    leafletOutput("studentmap", height=850),
                    # Shiny versions prior to 0.11 should use class = "modal" instead.
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  top = 100, left = "auto", right = 10, bottom = 100,  #padding from windows
                                  width = 450, height = "auto",
                                  h3("Student population at FU"),
                                  plotOutput("student_plot", height = 200),
                                  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),  #stylesheet
                                  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),   #for the map
                                  includeHTML("notes.html")  #notes at the bottom of the panel
                    )),

           tabPanel("Data",
                    DTOutput("students_table", width = "60%"),
                    downloadButton("downloadData", "Download all data")
           ),
           tabPanel("Read Me",includeMarkdown("README.md"))
)
