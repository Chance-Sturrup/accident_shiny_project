server <- function(input, output, session) {
  us_accidents <- read_csv("../data/US_Accidents2019.csv")
  us_accidents <- na.omit(us_accidents)
  us_accidents <- us_accidents %>% separate(Start_Time, c("Year", "Month","Date","Actualtime"))
  # server not seeing us_accidents dataframe unless included in server.R script
  # consider including dataset in package to avoid
  output$mymap <- renderLeaflet({
    if (input$sunrise != "All") {
      us_accidents <- filter(us_accidents, us_accidents$Sunrise_Sunset ==input$sunrise)
    }
    if (input$weather != "All") {
      us_accidents <- filter(us_accidents, us_accidents$Weather_Condition == input$weather)
    }
    if (input$month != "All") {
      us_accidents <- filter(us_accidents, us_accidents$Month == input$month)
    }
    if (input$date != "All") {
      us_accidents <- filter(us_accidents, us_accidents$Date == input$date)
    }
    if (input$actualtime != "All") {
      us_accidents <- filter(us_accidents, us_accidents$Actualtime == input$actualtime)
    }
    if (input$color == "None") {
      selectedColor <- "Black"
      colorPal <- colorBin(selectedColor, domain = NULL)
      data = NULL
      legend = NULL
    }
    if (input$color == "Severity") {
      colorPal <- colorBin("OrRd", domain = us_accidents$Severity)
      data = us_accidents$Severity
      legend = "Accident Severity"
    }
    if (input$color == "Temperature (F)") {
      colorPal <- colorBin("RdBu", reverse = TRUE, domain = us_accidents$`Temperature(F)`)
      data = us_accidents$`Temperature(F)`
      legend = "Temperature (F)"
    }
    #colorPal <- colorBin(selectedColor, domain = data)
    
    leaflet(data = us_accidents, # change df to accidents if using data in package
            options = leafletOptions(minZoom = 4, maxZoom = 20)) %>%
      setView(-86.5804, 35.5175, 7) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~ Start_Lng, lat = ~ Start_Lat, radius = 5,
                       color = ~ colorPal(data),
                       fillOpacity = 0.7,
                       # change variable names if using data from package
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
