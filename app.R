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
source('App/ui.R')
source('App/server.R')
source(list.files('App/functions', full.names = TRUE))
source('App/calculations.R')

#Run the app
shinyApp(ui, server)