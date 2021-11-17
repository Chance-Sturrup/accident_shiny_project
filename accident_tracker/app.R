library(shiny)
library(leaflet)
library(tidyverse)
library(accidenttracker)

#colorSelect Function
#Creates UI selecter to choos how the data points on the map are colored 
colorSelect <- function() {
  selectInput("color", label = "Color the Data by:",
              c("None", "Severity", "Temperature (F)"))
}
#Filtering function
#Filter and reorganize the input options
rmv_mssng_accidents <- na.omit(accidents)
us_accidents <- rmv_mssng_accidents %>% 
  separate(time, c("year", "month","date","actual.time"))

#Creates UI selecter to choose filtering options
filtersunrise <- function() {
  selectInput(inputId = "sunrise", label = "Sunrise/Sunset:",
              c("All",
                sort(unique(as.character(us_accidents$day.night)))
              )
  )}
filterweather <- function() {     
  selectInput(inputId = "weather", label = "Weather:",
              c("All",
                sort(unique(as.character(us_accidents$wthr.cond)))
              )
  )}
filtermonth <- function() {                     
  selectInput(inputId = "month", label = "Month:",
              c("All",
                sort(unique(as.character(us_accidents$month)))
              )
  )}
filterdate <- function() {     
  selectInput(inputId = "date", label = "Date:",
              c("All",
                sort(unique(as.character(us_accidents$date)))
              )
  )}
filtertime <- function() {     
  selectInput(inputId = "actualtime", label = "Actual time (24 hours systems):",
              c("All",
                sort(unique(as.character(us_accidents$actual.time)))
              )
  )}

ui <- fluidPage(
  titlePanel("US Car Accidents in 2019"),
  mainPanel(#this will create a space for us to display our map
    leafletOutput(outputId = "mymap"),
    #Execute colorSelect function to make UI selector
    column(4,
           colorSelect()),
    #Execute filtering functions
    column(4,
           filtersunrise(),
           filterweather()
    ),
    column(4,
           filtermonth(),
           filterdate(),
           filtertime()
    )
  )
)

server <- function(input, output, session) {
 output$mymap <- renderLeaflet({
    if (input$sunrise != "All") {
      us_accidents <- filter(us_accidents, us_accidents$day.night == input$sunrise)
    }
    if (input$weather != "All") {
      us_accidents <- filter(us_accidents, us_accidents$wthr.cond == input$weather)
    }
    if (input$month != "All") {
      us_accidents <- filter(us_accidents, us_accidents$month == input$month)
    }
    if (input$date != "All") {
      us_accidents <- filter(us_accidents, us_accidents$date == input$date)
    }
    if (input$actualtime != "All") {
      us_accidents <- filter(us_accidents, us_accidents$actual.time == input$actualtime)
    }
    if (input$color == "None") {
      selectedColor <- "Black"
      colorPal <- colorBin(selectedColor, domain = NULL)
      colorData = NULL
      legend = NULL
    }
    if (input$color == "Severity") {
      colorPal <- colorBin("OrRd", domain = us_accidents$severity)
      colorData = us_accidents$severity
      legend = "Accident Severity"
    }
    if (input$color == "Temperature (F)") {
      colorPal <- colorBin("RdBu", reverse = TRUE, domain = us_accidents$temp)
      colorData = us_accidents$temp
      legend = "Temperature (F)"
    }
    #colorPal <- colorBin(selectedColor, domain = data)
    
    leaflet(data = us_accidents,
            options = leafletOptions(minZoom = 4, maxZoom = 20)) %>%
      setView(-86.5804, 35.5175, 7) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~ lng, lat = ~ lat, radius = 5,
                       color = ~ colorPal(colorData),
                       fillOpacity = 0.7,
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 14,
                                                             # shows all individual data points
                                                             # at zoom level 14
                                                             spiderfyOnMaxZoom = FALSE) 
      ) %>%
      addLegend("bottomright", pal = colorPal, values = data,
                title = legend,
                bins = 7,
                opacity = 1)
  })
  
}

shinyApp(ui, server)