ui <- function() {
  fluidPage(titlePanel("US Car Accidents in 2019"),
            mainPanel(#this will create a space for us to display our map
              leafletOutput(outputId = "mymap"),
            ))
  
}