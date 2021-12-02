library(shiny)
library(leaflet)
library(tidyverse)
library(accidenttracker)

##########################################################################################
# Shiny app that plots US car accidents from 2019 on a map. 
# User is able to color and filter the data
# Also contains an option to plot the accident data as a histogram
# Data obtained from Kaggle, https://www.kaggle.com/sobhanmoosavi/us-accidents
##########################################################################################

# Load the data frame
us_accidents <- accidents

##########################################################################################
# Functions that Need to Be included in Package and Removed From App

color_pal <- function(a){
  switch(a,
         "None" =  colorBin("black", domain = NULL),
         "Severity" = colorFactor("YlOrRd", domain = us_accidents$severity),
         "Temperature (F)" = colorBin("RdBu", reverse = TRUE, domain = us_accidents$temp, bins = c(-50, 0, 32, 50, 80, 100, 175)),
         "Precipitation" = colorBin("BrBG", domain = us_accidents$precip, bins = c(0, 0.001, 0.05, 0.1, 0.3, 1, 2.0)),
         "Day/Night" = colorFactor("Dark2", domain = us_accidents$day.night),
         "Visibility" = colorBin("YlGnBu", domain = us_accidents$vis, bins = c(0, 0.5, 2, 150))
  )}

legend_title <- function(c){
  switch(c,
         "None" = "None",
         "Severity" = "Accident Severity",
         "Temperature (F)" = "Temperature (F)",
         "Precipitation" = "Precipitation",
         "Day/Night" = "Day/Night",
         "Visibility" = "Visibility",
  )}

filter_if <- function(condition, success) {
  if (condition) {
    success
  } else {
    TRUE
  }
}
##########################################################################################

ui <- fluidPage(titlePanel("US Car Accidents in 2019"),
                tabsetPanel(
                  # Tab containing a map showing where accidents occurred
                  tabPanel("Map", fluid = TRUE,
                           mainPanel(leafletOutput(outputId = "mymap"),
                                     # Allow the user to select a variable to color the data by
                                     column(4, select_input(inputId = "color", label = "Color the Data by:",
                                                            choices = c("None", 
                                                                        "Severity", 
                                                                        "Temperature (F)", 
                                                                        "Precipitation", 
                                                                        "Visibility", 
                                                                        "Day/Night"),
                                                            type = "select")
                                     ),
                                     # Allow the user to filter the data
                                     column(4, select_input(inputId = "sunrise", label = "Day/Night:",
                                                            choices = select_options(accidents, 
                                                                                     day.night,
                                                                                     all = TRUE),
                                                            type = "select"),
                                            select_input(inputId = "weather", label = "Weather:",
                                                         choices = select_options(accidents, 
                                                                                  wthr.cat),
                                                         type = "multiple")
                                     ),
                                     column(4,
                                            select_input(inputId = "month", label = "Month:",
                                                         choices = select_options(accidents,
                                                                                  month),
                                                         type = "multiple"
                                            ),
                                            select_input(inputId = "hour", label = "Hour (24-hour system):",
                                                         choices = select_options(
                                                           accidents,
                                                           hour),
                                                         type = "multiple")
                                     ),
                                     submitButton("Apply Changes"))
                  ),

##########################################################################################                  
                  # Tab containing a plot of the data 
                  tabPanel("Plot", fluid = TRUE,
                           mainPanel(
                             plotOutput(outputId = "plot"),
                             # Allow the user to choose which data is plotted
                             selectInput("plotby", label = "Plot By:",
                                         c("Temperature" = "temp",
                                           "Pressure" = "pressure",
                                           "Visibility" = "vis",
                                           "Wind speed" = "wind.spd",
                                           "State" = "state",
                                           "Day/Night" = "day.night",
                                           "Wind Direction" = "wind.dir",
                                           "Side of Road" = "side",
                                           "Month" = "month",
                                           "Time" = "hour",
                                           "Precipitation" = "precip")
                             ),
                             # Allow the user to change the number of bins
                             sliderInput("bins",
                                         label = "Number of Bins (only for continuous variables):",
                                         min = 5, 
                                         max = 50, 
                                         value = 20
                             ),
                             submitButton("Apply Changes")
))))

##########################################################################################                  
server <- function(input, output, session) {
  
  isolate({
    if ("mymap_center" %in% names(input)) {
      mapparams <- list(center = input$mymap_center, zoom = input$mymap_zoom)
    } else {
      mapparams <- list(center = list(lng=-86.5804, lat=35.5175), zoom = 7)
    }
    })
  
  #Filter the dataframe according to UI input
  df <- reactive ({
    us_accidents %>%
      filter(filter_if(is.null(input$weather) == FALSE, wthr.cat %in% input$weather),
             filter_if(input$sunrise != "All", day.night == input$sunrise),
             filter_if(is.null(input$month) == FALSE, month %in% input$month),
             filter_if(is.null(input$hour) == FALSE, hour %in% input$hour))
    })
  
  # Change how the map markers are colored according to UI input
  color_data <- reactive({
    switch(input$color,
           "None" = df()$severity,
           "Severity" = df()$severity,
           "Temperature (F)" = df()$temp,
           "Precipitation" = df()$precip,
           "Day/Night" = df()$day.night,
           "Visibility" = df()$vis,)
    })
  
##########################################################################################                  
  #Plot the map
  output$mymap <- renderLeaflet({
    leaflet(data = df(),
            options = leafletOptions(minZoom = 4, maxZoom = 20)) %>%
      setView(lng = mapparams$center$lng, lat = mapparams$center$lat, zoom = mapparams$zoom) %>%
      addTiles()
  })
  
  #Add markers to the map
  observe({
    leafletProxy("mymap", data = df()) %>%
      clearMarkerClusters() %>%
      addCircleMarkers(lng = ~ lng, lat = ~ lat, radius = 3,
                       color = color_pal(input$color)(color_data()),
                       fillOpacity = 0.7,
                       # Cluster datapoints to reduce load time
                       # shows all individual data points at zoom level 14
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 14,
                                                             spiderfyOnMaxZoom = FALSE))
    })
  
  observe({
    leafletProxy("mymap", data = df()) %>%
      clearControls() %>%
      addLegend("bottomright",
                pal = color_pal(input$color),
                values = color_data(),
                title = legend_title(input$color),
                opacity = 1)
  })
  
  output$plot <- renderPlot({
    histogram_plot(input$plotby, input$bins)
  })
}

# Run the app
shinyApp(ui, server)