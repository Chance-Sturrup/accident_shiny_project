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
                             actionButton("update", "Apply Changes")
                           )
                  ),
                  tabPanel("Plot", fluid = TRUE,
                    mainPanel(
                      plotOutput(outputId = "plot"),
                      radioButtons("plottype",
                                   label = "Explore:",
                                   c("Overall Distribution" = "dist",
                                     "Effect on Severity" = "severity")),
                      conditionalPanel(
                        condition = "input.plottype == 'dist'",
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
                        conditionalPanel(
                          condition = "input.plotby == 'temp' || input.plotby == 'pressure' || input.plotby == 'vis' || input.plotby == 'wind.spd' || input.plotby == 'precip'",
                          sliderInput("bins",
                                  label = "Number of Bins:",
                                  min = 5, 
                                  max = 50, 
                                  value = 20
                                  )
                        )
                      ),
                      conditionalPanel(
                        condition = "input.plottype == 'severity'",
                       # radioButtons("analysis",
                       #              label = "Plot Severity As:",
                       #              c("Individual Points" = "all",
                        #               "Average" = "avg")
                       # ),
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
                       conditionalPanel(
                         condition = "input.x == 'temp' || input.x == 'pressure' || input.x == 'vis' || input.x == 'precip'",
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

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    input$update
    
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
    if (input$color == "None") {
      color_pal <- colorBin("black", domain = NULL)
      color_data = NULL
      legend = NULL
    }
    if (input$color == "Severity") {
      colorPal <- colorFactor("YlOrRd", domain = us_accidents$severity)
      colorData <- us_accidents$severity
      legend = "Accident Severity"
    }
    if (input$color == "Temperature (F)") {
      colorPal <- colorBin("RdBu", reverse = TRUE, domain = us_accidents$temp, 
                           bins = c(-50, 0, 32, 50, 80, 100, 175))
      colorData <- us_accidents$temp
      legend = "Temperature (F)"
    }
    if (input$color == "Precipitation") {
      colorPal <- colorBin("BrBG", domain = us_accidents$precip,
                           bins = c(0, 0.001, 0.05, 0.1, 0.3, 1, 2))
      colorData <- us_accidents$precip
      legend = "Precipitation"
    }
    if (input$color == "Day/Night") {
      color_pal <- colorFactor("Dark2", domain = us_accidents$day.night)
      color_data <- us_accidents$day.night
      legend = "Day/Night"
    }
    if (input$color == "Visibility") {
      colorPal <- colorBin("YlGnBu", domain = us_accidents$vis, 
                           bins = c(0, 0.5, 2, 150))
      colorData <- us_accidents$vis
      legend = "Visibility"
    }
    isolate({
      if ("mymap_center" %in% names(input)) {
        mapparams <- list(center = input$mymap_center,
                          zoom = input$mymap_zoom)
      } else {
        mapparams <- list(center = list(lng=-86.5804, lat=35.5175), zoom = 7)
      }
    })
    
#    labels <- sprintf(
#      "<strong>%s</strong>%s%s%s%s%s<br/><strong>%s</strong>%s<br/><strong>%s</strong>%s",
#      "Place of accident: ", us_accidents$city," ", us_accidents$state,", ", us_accidents$zip,
#      "Time of accident: ", us_accidents$time,
#      "Weather: ", us_accidents$wthr.cond
#    ) %>% lapply(htmltools::HTML)
    
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 20)) %>%
      setView(lng = mapparams$center$lng, lat = mapparams$center$lat, zoom = mapparams$zoom) %>%
      addTiles() %>%
      addCircleMarkers(data = us_accidents, lng = ~ lng, lat = ~ lat, radius = 5,
                       color = ~ color_pal(color_data),
                       fillOpacity = 0.7,
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 14,
                                                             # shows all individual data points
                                                             # at zoom level 14
                                                             spiderfyOnMaxZoom = FALSE)#,
                       #label =labels,
                       #labelOptions = labelOptions(style = list("font-weight" = "normal", 
                       #                                         padding = "3px 8px"),
                       #                            textsize = "11px",
                       #                            direction = "auto")
      ) %>%
      addLegend("bottomright", 
                pal = color_pal, 
                values = color_data,
                title = legend,
                opacity = 1)
  })
  
  ##Adding this function to package
  histogram_plot <- function(x, y){
    if(x == "state" | x == "day.night" | x == "wind.dir" | x == "side" | x == "month" | x == "hour"){
      ggplot(accidents, aes_string(x)) +
        geom_bar(width=1)
    }
    else{
      ggplot(accidents, aes_string(x)) +
        geom_histogram(bins = y)
    }
  }
  
  ##Add to package
  plot_by_sev <- function(var) {
      if (var == "day.night" | var == "month" | var == "state" | var == "wthr.cat") {
        ggplot(accidents_by_sev(), aes(.data[[var]], avg.sev)) +
          plot_geom() 
      } else {
        ggplot(accidents_vis, aes(.data[[var]], y = ..density..)) +
          plot_geom()
      }
  }
  
  accidents_vis <- accidents %>%
    mutate(vis = replace(vis, vis > 10, 10))
  
  display_plot <- reactive({
    switch(input$plottype,
           dist = histogram_plot(input$plotby, input$bins),
           severity = plot_by_sev(input$x)
           )
  })
  
  plot_geom <- reactive({
      switch(input$x,
             day.night = geom_col(),
             month = geom_col(),
             state = geom_col(),
             wthr.cat = geom_col(),
             geom_freqpoly(aes(color = as.factor(severity)), bins = input$y)
      )
    # if (input$analysis == "all") {
    #   plot_geom <- switch(input$x,
    #                      day.night = geom_count(),
    #                     month = geom_count(),
    #                     state = geom_count(),
    #                     wthr.cat = geom_count(),
    #                    geom_dotplot()
    # )
    # }
  })
  
  accidents_by_sev <- reactive({
    accidents %>%
      group_by(.data[[input$x]]) %>%
      summarise(avg.sev = mean(severity))
  })
  
  output$plot <- renderPlot({
    display_plot()
  })
}

shinyApp(ui, server)