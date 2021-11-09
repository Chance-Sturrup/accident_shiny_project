library(shiny)
library(leaflet)
library(tidyverse)

US_Accidents <- read_csv("../data/US_Accidents2019.csv")

source("R/ui.R")

source("R/server.R")

shinyApp(ui, server)