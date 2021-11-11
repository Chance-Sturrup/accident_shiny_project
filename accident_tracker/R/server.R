server <- function(input, output, session) {
  us_accidents <- read_csv("../data/US_Accidents2019.csv")
  # server not seeing us_accidents dataframe unless included in server.R script
  # consider including dataset in package to avoid
  
  output$mymap <- renderLeaflet({
    leaflet(data = us_accidents, options = leafletOptions(minZoom = 4, maxZoom = 20)) %>%
      setView(-86.5804, 35.5175, 7) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~ Start_Lng, lat = ~ Start_Lat,
        clusterOptions = markerClusterOptions(disableClusteringAtZoom = 14,
                                              # shows individual data points
                                              spiderfyOnMaxZoom = FALSE)
      )
  })
  
  
  
  
}