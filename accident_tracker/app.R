library(shiny)
library(leaflet)
library(tidyverse)
library(accidenttracker)
source("DataOrganize.R")
#colorSelect Function
#Creates UI selecter to choos how the data points on the map are colored 
colorSelect <- function() {
  selectInput("color", label = "Color the Data by:",
              c("None", "Severity", "Temperature (F)", "Precipitation", "Visibility"))
}
#Filtering function
#Filter and reorganize the input options
us_accidents <- DataOrganize(accidents)

#Creates UI selecter to choose filtering options
filtersunrise <- function() {
  selectInput(inputId = "sunrise", label = "Sunrise/Sunset:",
              c("All",
                sort(unique(as.character(us_accidents$day.night)))
              )
  )}
filterweather <- function() {     
  selectInput(inputId = "weather", label = "Weather:",
              c(
                sort(unique(as.character(us_accidents$Weather)))),
              multiple = TRUE
  )}
filtermonth <- function() {                     
  selectInput(inputId = "Month", label = "Month:",
                     choices =c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     multiple = TRUE
  )}

filtertime <- function() {     
  selectInput(inputId = "actualtime", label = "Actual time (24 hours systems):",
              choices = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
              multiple = TRUE
  )}

ui <- fluidPage(
  titlePanel("US Car Accidents in 2019"),
  mainPanel(#this will create a space for us to display our map
    leafletOutput(outputId = "mymap"),
    #Execute colorSelect function to make UI selector
    column(4,
           colorSelect()
    ),
    #Execute filtering functions
    column(4,
           filtersunrise(),
           filterweather()
    ),
    column(4,
           filtermonth(),
           filtertime()
    ),
    submitButton("Apply Changes")
  )
)

server <- function(input, output, session) {
 output$mymap <- renderLeaflet({
   if (input$sunrise != "All") {
     us_accidents <- filter(us_accidents, us_accidents$day.night ==input$sunrise)
   }
   if (is.null(input$weather) == FALSE) {
     us_accidents <- filter(us_accidents, us_accidents$Weather %in% input$weather)
   }
   if (is.null(input$Month) == FALSE) {
     us_accidents <- filter(us_accidents, us_accidents$month %in% input$Month)
   }
   if (is.null(input$actualtime) == FALSE) {
     us_accidents <- filter(us_accidents, us_accidents$hour %in% input$actualtime)
   }
    if (input$color == "None") {
      selectedColor <- "Black"
      colorPal <- colorBin(selectedColor, domain = NULL)
      colorData = NULL
      legend = NULL
    }
    if (input$color == "Severity") {
      colorPal <- colorBin("YlOrRd", domain = us_accidents$severity)
      colorData = us_accidents$severity
      legend = "Accident Severity"
    }
    if (input$color == "Temperature (F)") {
      colorPal <- colorBin("RdBu", reverse = TRUE, domain = us_accidents$temp)
      colorData = us_accidents$temp
      legend = "Temperature (F)"
    }
   if (input$color == "Precipitation") {
     colorPal <- colorBin("BrBG", domain = us_accidents$precip)
     colorData <- us_accidents%>%precip
     legend = "Precipitation"
   }
#   if (input$color == "Day/Night") {
#     colorPal <- colorBin("OrRd", domain = us_accidents$day.night)
#     colorData <- us_accidents %>%day.night
#     legend = "Day/Night"
#   }
   if (input$color == "Visibility") {
     colorPal <- colorBin("YlGnBu", domain = us_accidents$vis)
     colorData = us_accidents$vis
     legend = "Visibility"
   }
   isolate({
     if ("mymap_center" %in% names(input)) {
       mapparams <- list(center = input$mymap_center,
                         zoom = input$mymap_zoom)
     } else {
       mapparams <- list(center = list(lng=-86.5804, lat=35.5175),
                         zoom = 7)
     }
   })
   
    #colorPal <- colorBin(selectedColor, domain = data)
   labels <- sprintf(
     "<strong>%s</strong>%s%s%s%s%s<br/><strong>%s</strong>%s<br/><strong>%s</strong>%s",
     "Place of accident: ", us_accidents$city," ", us_accidents$state,", ", us_accidents$zip,
     "Time of accident: ",us_accidents$time,
     "Weather: ",us_accidents$wthr.cond
   ) %>% lapply(htmltools::HTML)
   
    leaflet(data = us_accidents,
            options = leafletOptions(minZoom = 4, maxZoom = 20)) %>%
      setView(lng = mapparams$center$lng, lat = mapparams$center$lat, zoom = mapparams$zoom) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~ lng, lat = ~ lat, radius = 5,
                       color = ~ colorPal(colorData),
                       fillOpacity = 0.7,
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 14,
                                                             # shows all individual data points
                                                             # at zoom level 14
                                                             spiderfyOnMaxZoom = FALSE),
                       label =labels,
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "11px",
                         direction = "auto")
      ) %>%
      addLegend("bottomright", pal = colorPal, values = data,
                title = legend,
                opacity = 1)
  })
  
}

shinyApp(ui, server)