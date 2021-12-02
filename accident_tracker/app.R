library(shiny)
library(leaflet)
library(tidyverse)
library(accidenttracker)

us_accidents <- accidents

ui <- fluidPage(titlePanel("US Car Accidents in 2019"),
                tabsetPanel(
                  tabPanel("Map", fluid = TRUE,
                           mainPanel(#this will create a space for us to display our map
                             leafletOutput(outputId = "mymap"),
                             column(4, # Options to color data by different variables
                                    select_input(inputId = "color", 
                                                 label = "Color the Data by:",
                                                 choices = c("None", 
                                                             "Severity", 
                                                             "Temperature (F)", 
                                                             "Precipitation", 
                                                             "Visibility", 
                                                             "Day/Night"),
                                                 type = "select")
                             ),
                             #Selecting filtering options
                             column(4,
                                    select_input(inputId = "sunrise",
                                                 label = "Day/Night:",
                                                 choices = select_options(
                                                   accidents,
                                                   day.night,
                                                   all = TRUE),
                                                 type = "select"),
                                    select_input(inputId = "weather",
                                                 label = "Weather:",
                                                 choices = select_options(
                                                   accidents,
                                                   wthr.cat),
                                                 type = "multiple")
                             ),
                             column(4,
                                    select_input(inputId = "month",
                                                 label = "Month:",
                                                 choices = select_options(
                                                   accidents,
                                                   month),
                                                 type = "multiple"),
                                    select_input(inputId = "hour",
                                                 label = "Hour (24-hour system):",
                                                 choices = select_options(
                                                   accidents,
                                                   hour),
                                                 type = "multiple")
                             ),
                             submitButton("Apply Changes")
                           )
                  ),
                  tabPanel("Plot", fluid = TRUE,
                    mainPanel(
                      plotOutput(outputId = "plot"),
                      selectInput("plotby",
                        label = "Plot By:",
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
                          "Precipitation" = "precip"
                          )
                      ),
                      sliderInput("bins",
                                  label = "Number of Bins (only for continuous variables):",
                                  min = 5, 
                                  max = 50, 
                                  value = 20
                                  ),
                      submitButton("Apply Changes")
                    )
                  )
                )
)

color_pal <- function(a){
  switch(a,
         "None" =  colorBin("black", domain = NULL),
         "Severity" = colorFactor("YlOrRd", domain = us_accidents$severity),
         "Temperature (F)" = colorBin("RdBu", reverse = TRUE, domain = us_accidents$temp, bins = c(-50, 0, 32, 50, 80, 100, 175)),
         "Precipitation" = colorBin("BrBG", domain = us_accidents$precip, bins = c(0, 0.001, 0.05, 0.1, 0.3, 1, 2.0)),
         "Day/Night" = colorFactor("Dark2", domain = us_accidents$day.night),
         "Visibility" = colorBin("YlGnBu", domain = us_accidents$vis, bins = c(0, 0.5, 2, 150))
  )
}

color_data <- function(b){
  switch(b,
         "None" = NULL,
         "Severity" = us_accidents$severity,
         "Temperature (F)" = us_accidents$temp,
         "Precipitation" = us_accidents$precip,
         "Day/Night" = us_accidents$day.night,
         "Visibility" = us_accidents$vis,
  )
}

legend_title <- function(c){
  switch(c,
         "None" = "None",
         "Severity" = "Accident Severity",
         "Temperature (F)" = "Temperature (F)",
         "Precipitation" = "Precipitation",
         "Day/Night" = "Day/Night",
         "Visibility" = "Visibility",
  )}

server <- function(input, output, session) {
  
  
  isolate({
    if ("mymap_center" %in% names(input)) {
      mapparams <- list(center = input$mymap_center,
                        zoom = input$mymap_zoom)
    } else {
      mapparams <- list(center = list(lng=-86.5804, lat=35.5175), zoom = 7)
    }
  })
  
  # pal <- reactive ({
  #   color_pal(input$color)
  #   colorData <- color_data(input$color)
  # })
  # 
  # c_data <- reactive ({
  #   s <-color_data(input$color)
  #   s
  # })
  
  output$mymap <- renderLeaflet({
    if (input$sunrise != "All") {
      us_accidents <- filter(us_accidents, day.night == input$sunrise)
    }
    if (is.null(input$weather) == FALSE) {
      us_accidents <- filter(us_accidents, wthr.cat %in% input$weather)
    }
    if (is.null(input$month) == FALSE) {
      us_accidents <- filter(us_accidents, month %in% input$month)
    }
    if (is.null(input$hour) == FALSE) {
      us_accidents <- filter(us_accidents, hour %in% input$hour)
    }
    
    leaflet(data = us_accidents,
            options = leafletOptions(minZoom = 4, maxZoom = 20)) %>%
      setView(lng = mapparams$center$lng, lat = mapparams$center$lat, zoom = mapparams$zoom) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~ lng, lat = ~ lat, radius = 3,
                       color = color_pal(input$color)(color_data(input$color)),
                       fillOpacity = 0.7,
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 14,
                                                             # shows all individual data points
                                                             # at zoom level 14
                                                             spiderfyOnMaxZoom = FALSE)
      ) %>%
      addLegend("bottomright",
                pal = color_pal(input$color),
                values = color_data(input$color),
                title = legend_title(input$color),
                opacity = 1)
  })
  
  # labels <- sprintf(
  #   "<strong>%s</strong>%s%s%s%s%s<br/><strong>%s</strong>%s<br/><strong>%s</strong>%s",
  #   "Place of accident: ", us_accidents$city," ", us_accidents$state,", ", us_accidents$zip,
  #   "Time of accident: ", us_accidents$time,
  #   "Weather: ", us_accidents$wthr.cond
  # ) %>% lapply(htmltools::HTML)
  
#  observe({
#    leafletProxy("mymap", data = us_accidents) %>%
#      clearShapes() %>%
#      addCircleMarkers(lng = ~ lng, lat = ~ lat, radius = 3,
#                       color = color_pal(input$color)(color_data(input$color)),
#                       fillOpacity = 0.7,
#                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 14,
                                                             # shows all individual data points
                                                             # at zoom level 14
#                                                             spiderfyOnMaxZoom = FALSE),
                       # label =labels,
                       # labelOptions = labelOptions(style = list("font-weight" = "normal",
                       #                                          padding = "3px 8px"),
                       #                             textsize = "11px",
                       #                             direction = "auto")
#      )
#  })
  
#  observe({
#    leafletProxy("mymap", data = us_accidents) %>%
#      clearControls() %>%
#      addLegend("bottomright",
#                pal = color_pal(input$color),
#                values = color_data(input$color),
#                title = legend_title(input$color),
#                opacity = 1)
#  })
  
  ##Adding this function to package
  histogram_plot <- function(x, y){
    if(x == "state" | x == "day.night" | x == "wind.dir" | x == "side" | x == "month" | x == "hour"){
      ggplot(us_accidents, aes_string(x)) +
        geom_bar(width=1)
    }
    else{
      ggplot(us_accidents, aes_string(x)) +
        geom_histogram(bins = y)
    }
  }
  
  output$plot <- renderPlot({
    histogram_plot(input$plotby, input$bins)
  })
}

shinyApp(ui, server)