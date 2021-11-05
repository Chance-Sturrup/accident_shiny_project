library(shiny)
library(leaflet)
library(tidyverse)

US_Accidents <- read_csv("../data/US_Accidents_Dec20_updated.csv")

ui <- fluidPage(
  titlePanel("US Car Accidents"),
  mainPanel( 
    #this will create a space for us to display our map
    leafletOutput(outputId = "mymap"), 
    
    checkboxInput("color", "Color by Severity", FALSE),
    sliderInput(inputId = "severity", label = "SEVERITY", min = 1, max = 4, value = c(1, 4), step = 1)
  )
)


server <- function(input, output, session) {
  
  US_Accidents <- US_Accidents %>%
    select(c(Severity, Start_Lat, Start_Lng, State, `Temperature(F)`)) %>%
    filter(State == "TN")
  
  sevPal <- colorNumeric("OrRd", domain = US_Accidents$Severity)
  
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 20)) %>%
      setView(-86.5804, 35.5175, 7) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron)
  })

  #plot data and make UI responsive
  observe({
    df <- US_Accidents[US_Accidents$Severity>=input$severity[1] & US_Accidents$Severity<=input$severity[2],]
    proxy <- leafletProxy("mymap", data = US_Accidents)
    proxy %>% clearMarkers() %>% clearControls()
    if (input$color) {
      proxy %>% addCircleMarkers(data = df, 
                                 lat = ~ Start_Lat, 
                                 lng = ~ Start_Lng,
                                 radius = 3,
                                 color = ~ sevPal(Severity), 
                                 stroke = FALSE, 
                                 fillOpacity = 0.5) %>%
        addLegend("bottomright", pal = sevPal, values = df$Severity,
                  title = "Accident Severity",
                  bins = 4,
                  opacity = 1)}
    else{
      proxy %>% 
        addCircleMarkers(data = df, 
                         lat = ~ Start_Lat, 
                         lng = ~ Start_Lng,
                         radius = 3,
                         stroke = FALSE, 
                         color = ~ sevPal,
                         fillOpacity = 0.5) %>%
        clearControls()
        
    }
    
  })
  
  
}

shinyApp(ui, server)