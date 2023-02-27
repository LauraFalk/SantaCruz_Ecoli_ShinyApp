#Load the relevant libraries

if(!require(climateAnalyzeR)){
  remotes::install_github("scoyoc/climateAnalyzeR")
}

library(dataRetrieval)
library(DT)
library(ggplot2)
library(janitor)
library(knitr)
library(leaflet)
library(leaflegend)
library(lubridate)
library(mapview)
library(readxl)
library(shiny)
library(shinyTime)
library(sp)
library(tidyverse)
library(xgboost)
library(climateAnalyzeR)


#Load components
source('ui.R')
source('server.R')
source(list.files('data/functions', full.names = TRUE))
source('data/calculations.R')

#Run the app
shinyApp(ui, server)