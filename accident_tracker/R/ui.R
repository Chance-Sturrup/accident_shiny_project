
#colorSelect Function
#Creates UI selecter to choos how the data points on the map are colored 
colorSelect <- function() {
  selectInput("color", label = "Color the Data by:",
              c("None", "Severity", "Temperature (F)"))
}
#Filtering function
#Filter and reorganize the input options
us_accidents <- read_csv("../data/US_Accidents2019.csv")
us_accidents <- na.omit(us_accidents)
us_accidents <- us_accidents %>% separate(Start_Time, c("Year", "Month","Date","Actualtime"))
#Creates UI selecter to choose filtering options
filtersunrise <- function() {
  selectInput(inputId = "sunrise", label = "Sunrise/Sunset:",
              c("All",
                sort(unique(as.character(us_accidents$Sunrise_Sunset)))
              )
  )}
filterweather <- function() {     
  selectInput(inputId = "weather", label = "Weather:",
              c("All",
                sort(unique(as.character(us_accidents$Weather_Condition)))
              )
  )}
filtermonth <- function() {                     
  selectInput(inputId = "month", label = "Month:",
              c("All",
                sort(unique(as.character(us_accidents$Month)))
              )
  )}
filterdate <- function() {     
  selectInput(inputId = "date", label = "Date:",
              c("All",
                sort(unique(as.character(us_accidents$Date)))
              )
  )}
filtertime <- function() {     
  selectInput(inputId = "actualtime", label = "Actual time (24 hours systems):",
              c("All",
                sort(unique(as.character(us_accidents$Actualtime)))
              )
  )}

ui <- function() {
  fluidPage(titlePanel("US Car Accidents in 2019"),
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
            ))
}
