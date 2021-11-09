server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 20)) %>%
      setView(-86.5804, 35.5175, 7) %>%
      addTiles()
  })
  
  
  
  
}