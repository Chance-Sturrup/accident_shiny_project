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

# Limit max visibility to 10 miles to make distribution easier to visualize
accidents_vis <- accidents %>%
  mutate(vis = replace(vis, vis > 10, 10))

##########################################################################################

ui <- fluidPage(titlePanel("US Car Accidents in 2019"),
                tabsetPanel(
                  # Tab containing a map showing where accidents occurred
                  tabPanel("Map", fluid = TRUE,
                           mainPanel(leafletOutput(outputId = "mymap"),
                            # Allow the user to select a variable to color the data by
                             column(4,
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
                            # Allow the user to filter the data
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
                             actionButton("update", "Plot Map")
                            # Limits updating map to only after button is clicked,
                            # reducing load time while selecting options
                           )
                  ),
                  
##########################################################################################                  
                  # Tab containing a plot of the data  

                  tabPanel("Plot", fluid = TRUE,
                    mainPanel(
                      plotOutput(outputId = "plot"),
                      # Allow the user to select type of plot
                      radioButtons("plottype",
                                   label = "Explore:",
                                   c("Overall Distribution" = "dist",
                                     "Effect on Severity" = "severity")),
                      # A panel that appears when user chooses to plot distribution
                      conditionalPanel(
                        condition = "input.plottype == 'dist'",
                        # Allow the user to choose which data is plotted
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
                        # A bin selector that only appears for continuous data
                        conditionalPanel(
                          condition = "input.plotby == 'temp' || input.plotby == 'pressure' || input.plotby == 'vis' || input.plotby == 'wind.spd' || input.plotby == 'precip'",
                          # Allow the user to change the number of bins
                          sliderInput("bins",
                                  label = "Number of Bins:",
                                  min = 5, 
                                  max = 50, 
                                  value = 20
                                  )
                        )
                      ),
                      # A panel that appears when user chooses to plot by severity
                      conditionalPanel(
                        condition = "input.plottype == 'severity'",
                        # Allow the user to choose which data is plotted
                        selectInput("x",
                                    label = "Plot Severity vs.:",
                                    c("Precipitation" = "precip",
                                      "Visibility" = "vis",
                                      "Temperature" = "temp",
                                      "Day/Night" = "day.night",
                                      "Month" = "month",
                                      "Time" = "hour",
                                      "State" = "state",
                                      "Weather" = "wthr.cat",
                                      "Pressure" = "pressure")
                                    ),
                       # A bin selector that only appears for continuous data
                       conditionalPanel(
                         condition = "input.x == 'temp' || input.x == 'pressure' || input.x == 'vis' || input.x == 'precip'",
                         # Allow the user to change the number of bins
                         sliderInput("y",
                                     label = "Number of Bins:",
                                     min = 5, 
                                     max = 100, 
                                     value = 30
                         )
                       )
                      )
                    )
                  )
                )
)

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
    accidents %>%
      filter(conditional_filter(is.null(input$weather) == FALSE, wthr.cat %in% input$weather),
             conditional_filter(input$sunrise != "All", day.night == input$sunrise),
             conditional_filter(is.null(input$month) == FALSE, month %in% input$month),
             conditional_filter(is.null(input$hour) == FALSE, hour %in% input$hour)
      )
  }) %>%
    bindEvent(input$update)
  
  # Change how the map markers are colored according to UI input
  color_data <- reactive({
    switch(input$color,
           "None" = df()$severity,
           "Severity" = df()$severity,
           "Temperature (F)" = df()$temp,
           "Precipitation" = df()$precip,
           "Day/Night" = df()$day.night,
           "Visibility" = df()$vis,
    )
  }) %>%
    bindEvent(input$update)
  
##########################################################################################                  
  #Plot the map
  output$mymap <- renderLeaflet({
    leaflet(data = df(),
            options = leafletOptions(minZoom = 4, maxZoom = 20)) %>%
      setView(lng = mapparams$center$lng, lat = mapparams$center$lat, zoom = mapparams$zoom) %>%
      addTiles()
  })
  
  # Add markers to the map
  observe({
    leafletProxy("mymap", data = df()) %>%
      clearMarkerClusters() %>%
      addCircleMarkers(lng = ~ lng, lat = ~ lat, radius = 3,
                       color = color_pal(accidents, input$color)(color_data()),
                       fillOpacity = 0.7,
                       # Cluster datapoints to reduce load time
                       # shows all individual data points at zoom level 14
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 14,
                                                             spiderfyOnMaxZoom = FALSE))
    }) %>%
    bindEvent(input$update)
  
  # Add legend to the map
  observe({
    leafletProxy("mymap", data = df()) %>%
      clearControls() %>%
      addLegend("bottomright",
                pal = color_pal(accidents, input$color),
                values = color_data(),
                title = legend_title(input$color),
                opacity = 1)
  }) %>%
    bindEvent(input$update)
  
 #################################################################################
  # Summarise data based on UI input
  accidents_by_sev <- reactive({
    accidents %>%
      group_by(.data[[input$x]]) %>%
      summarise(avg.sev = mean(severity))
  })
  
  # Choose geom and details of plot based on UI input
  plot_by_sev <- reactive({
    if (input$x == "day.night" | input$x == "month" | input$x == "state" | input$x == "wthr.cat" | input$x == "hour") {
      ggplot(accidents_by_sev(), aes(.data[[input$x]], avg.sev)) +
        geom_col() 
    } else {
      ggplot(accidents_vis, aes(.data[[input$x]], y = ..density..)) +
        geom_freqpoly(aes(color = as.factor(severity)), bins = input$y)
    }
  })
  
##################################################################################
  # Plot the graph
  output$plot <- renderPlot({
    switch(input$plottype,
           dist = histogram_plot(accidents, input$plotby, input$bins),
           severity = plot_by_sev()
    )
  })
}

# Run the app
shinyApp(ui, server)