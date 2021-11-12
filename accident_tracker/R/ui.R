
#colorSelect Function
#Creates UI selecter to choos how the data points on the map are colored 
colorSelect <- function() {
  selectInput("color", label = "Color the Data by:",
              c("None", "Severity", "Temperature (F)"))
}
  
ui <- function() {
  fluidPage(titlePanel("US Car Accidents in 2019"),
            mainPanel(#this will create a space for us to display our map
              leafletOutput(outputId = "mymap"),
              #Execute colorSelect function to make UI selector
              colorSelect()
            ))
  
}