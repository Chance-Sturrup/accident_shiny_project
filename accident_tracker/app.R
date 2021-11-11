library(shiny)
library(leaflet)
library(tidyverse)

# us_accidents <- read_csv("../data/US_Accidents2019.csv")

source("R/ui.R")

source("R/server.R")

shinyApp(ui, server)