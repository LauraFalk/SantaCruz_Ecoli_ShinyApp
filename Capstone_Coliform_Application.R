#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dataRetrieval)
library(DT)
library(ggplot2)
library(janitor)
library(knitr)
library(leaflet)
library(lubridate)
library(mapview)
library(readxl)
library(shiny)
library(shinyTime)
library(sp)
library(tidyverse)
library(xgboost)
# Not currently on CRAN - devtools::install_github("scoyoc/climateAnalyzeR")
library(climateAnalyzeR)

#### Define Functions ##########################################################

# Use the lubridate package to retrieve UTC rather than sysTime
sysDate1 <- now(tz = 'America/Phoenix')

# T Min
get.Tmin <- function(sysDate) {
  formattedEndYear <- as.numeric(format(sysDate, "%Y"))
  TMin <- climateAnalyzeR::import_data("daily_wx"
                                       , station_id = 'KA7WSB-1'
                                       , start_year = formattedEndYear-1
                                       , end_year = formattedEndYear
                                       , station_type = 'RAWS')
  
  Var_TMin <- as.numeric(unlist(TMin %>%
                                  mutate(DateasDate = as.POSIXct(TMin$date, format = "%m/%d/%Y")) %>%
                                  subset(DateasDate == as.Date(sysDate) - 2) %>%
                                  select(tmin_f)))
}

Var_TMin <- get.Tmin(sysDate1)

# Discharge
get.DischargeCFS <- function(sysDate) {
  startDate <- as.Date(format(sysDate1,'%Y-%m-%d')) - 31
  endDate <- as.Date(format(sysDate1,'%Y-%m-%d')) - 1
  USGSRaw <- readNWISuv(siteNumbers = '09481740', c('00060','00065'), startDate,endDate, tz = 'America/Phoenix')
  
  tail(USGSRaw$X_00060_00000, n=1)
}

Var_Discharge_CFS <- get.DischargeCFS(sysDate1)

# Stage
get.stage <- function(sysDate) {
  startDate <- as.Date(format(sysDate1,'%Y-%m-%d')) - 31
  endDate <- as.Date(format(sysDate1,'%Y-%m-%d')) - 1
  USGSRaw <- readNWISuv(siteNumbers = '09481740', c('00060','00065'), startDate,endDate, tz = 'America/Phoenix')
  
  # Create quantiles for categorization
  CFS_Quantiles<- quantile(USGSRaw$X_00060_00000, na.rm = TRUE)
  
  # Determine the difference between prior reading and current.
  USGSRaw <- USGSRaw %>% 
    mutate(DisDif = X_00060_00000 - lag(X_00060_00000))
  
  # This will create a binary variable or either rise of fall. Rise = 1, fall = 0. It will allow me to more easily create summary statistics.
  USGSRaw$DisDif2 <- ifelse(USGSRaw$DisDif>0,1,0)
  
  # Create a numeric classifier. 
  # 1 = Low Flow, 2 = Base flow, 3 = High and Rising Flow 4 = High and Falling Flow
  USGSRaw$Stage <- ifelse(USGSRaw$X_00060_00000 <=CFS_Quantiles[2], 1, 
                          ifelse(USGSRaw$X_00060_00000 > CFS_Quantiles[2] & USGSRaw$X_00060_00000 <= CFS_Quantiles[4],2,
                                 ifelse(USGSRaw$X_00060_00000 > CFS_Quantiles[4] & USGSRaw$DisDif2 == 1,3,
                                        ifelse(USGSRaw$X_00060_00000 > CFS_Quantiles[4] & USGSRaw$DisDif2 == 0,4, NA))))
  
  
  # Create the stage variable.
  tail(USGSRaw$Stage, n=1)
}

Var_Stage <- get.stage(sysDate1)

# El Nino
get.NinXTS <- function(sysDate) {
  formattedEndYear <- as.numeric(format(sysDate, "%Y"))
  formattedMonth <- as.numeric(format(sysDate,"%m"))
  
  # Bring in the website data
  url <- "https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php"
  
  NinXTS <- url %>%
    rvest::read_html()
  
  # Grab the data table
  NinText <- rvest::html_table(rvest::html_nodes(NinXTS, xpath = './/table[4]//table[2]'))
  
  # Convert ONI index to dataframe
  NinTable <- as.data.frame(NinText[1]) %>%
    row_to_names(row_number = 1) %>%
    mutate(Year = as.numeric(Year)) %>%
    drop_na(Year)
  
  # I need to do either month-of or last non-na value to account for delays. 
  formattedMonth <-12
  
  NinVal <- NinTable %>%
    subset(2022 == Year) %>%
    select(case_when(formattedMonth == 1 ~ "NDJ",
                     formattedMonth == 2 ~ "DJF",
                     formattedMonth == 3 ~ "JFM",
                     formattedMonth == 4 ~ "FMA",
                     formattedMonth == 5 ~ "MAM",
                     formattedMonth == 6 ~ "AMJ",
                     formattedMonth == 7 ~ "MJJ",
                     formattedMonth == 8 ~ "JJA",
                     formattedMonth == 9 ~ "JAS",
                     formattedMonth == 10 ~ "ASO",
                     formattedMonth == 11 ~ "SON",
                     formattedMonth == 12 ~ "OND")) %>%
    unlist()
  
  PrevVal <- NinTable %>%
    subset(2022 == Year) %>%
    select(case_when(formattedMonth == 2 ~ "NDJ",
                     formattedMonth == 3 ~ "DJF",
                     formattedMonth == 4 ~ "JFM",
                     formattedMonth == 5 ~ "FMA",
                     formattedMonth == 6 ~ "MAM",
                     formattedMonth == 7 ~ "AMJ",
                     formattedMonth == 8 ~ "MJJ",
                     formattedMonth == 9 ~ "JJA",
                     formattedMonth == 10 ~ "JAS",
                     formattedMonth == 11 ~ "ASO",
                     formattedMonth == 12 ~ "SON",
                     formattedMonth == 1 ~ "OND")) %>%
    unlist()
  
  if_else(!is.na(NinVal), NinVal, PrevVal)                 
}

Var_NinXTS <- get.NinXTS(sysDate1)


# Time of Day
#modified from https://stackoverflow.com/questions/49370387/convert-time-object-to-categorical-morning-afternoon-evening-night-variable

get.TOD <- function(sysTime) {
  
  # Create categorical variables
  currenttime <- as.POSIXct(sysTime, format = "%H:%M") %>% format("%H:%M:%S")
  
  currenttime <- cut(chron::times(currenttime) , breaks = (1/24) * c(0,5,11,16,19,24))
  Var_TOD <- c(4, 1, 2, 3, 4)[as.numeric(currenttime)]
}

Var_TOD <- get.TOD(sysDate1)

# Dist from Sonoita is within the mapping layer

spatiallocs <-read_xls("Data/SantaCruzLocs.xls")
spatiallocs <- spatiallocs %>%
  arrange(DistCatego)

# Retrieve all variables using the functions
predictionDF <- as.data.frame(spatiallocs)
predictionDF$PreviousTmin <- c(Var_TMin)
predictionDF$Discharge_CFS	<- c(Var_Discharge_CFS)
predictionDF$Stage	<- c(Var_Stage)
predictionDF$NinXTS	<- c(Var_NinXTS)
predictionDF$TOD <- c(Var_TOD)

predictionDF <- predictionDF %>%
  rename(DistFromSonoita = DistCatego) %>%
  select(PreviousTmin, Discharge_CFS, Stage, NinXTS, TOD, DistFromSonoita)

DisplayDF <- predictionDF %>%
  select(-DistFromSonoita)%>%
  distinct()

# Run the model for 235
XGBModel <- xgb.load('Data/XGBmodel235')
predictionDM <- data.matrix(predictionDF)
pred <- predict(XGBModel,predictionDM)
pred <-  as.numeric(pred > 0.4)
spatiallocs$pred235 <- c(pred)
#spatiallocs$pred235 <- ifelse(spatiallocs$pred235 > 0, "Bacteria Level >235  Likely", "High Bacteria levels > 235 not predicted")


# Run the model for 575
XGBModel <- xgb.load('Data/XGBmodel575')
pred <- predict(XGBModel,predictionDM)
pred <-  as.numeric(pred > 0.4)
spatiallocs$pred575 <- c(pred)
#spatiallocs$pred575 <- ifelse(spatiallocs$pred575 > 0, "Bacteria Level >575 Likely", "High Bacteria levels > 575 not predicted")

# For map


points <- spatiallocs %>%
  select(Lat,Long,DistCatego,pred235, pred575) %>%
  mutate(pointcolor = (pred235+pred575*3)) %>%
  mutate(pointlegend = ifelse(pointcolor > 1 ,">575 MPN likely",(ifelse(pointcolor=1,">235 MPN likely","<235 MPN likely"))))

factpal <- colorFactor(topo.colors(3), points$pointlegend)
### Shiny UI ##################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("E. coli Prediction: Upper Santa Cruz River"),
  
  sidebarPanel(" LAURA FIGURE OUT HOW TO FORMAT THIS
  
  This tool is intended to predict Escherichia coli (E. coli) levels
               in the Upper Santa Cruz River using a model trained on sampling data
               collected after the 2009 upgrade of the Nogales International Wastewater Treatment Plant.
               
               Variables were as follows:
               PreviousTMin - Minimum temperature from 2 days prior from KA7WSB-1 weather station upload on ClimateAnalyzer.org
               Discharge_CFS - Current (within last 5 minute) discharge reading from the USGS gage 09481740,
               Stage - Categorical classification of current trend from USGS gage 09481740 (Low Flow, Base flow, High and Rising Flow, High and Falling Flow)),
               NinXTS - Current (or most recent) Oceanic Nino Index from NOAA
               TOD - Categorical quartile classification of time of day,
               DistFromSonoita - Categorical distance classification from the Rio Sonoita Inputs"
               ),
  mainPanel(
    fluidRow(
      column(12,
             tableOutput('table')
      )),
    fluidPage(
      leafletOutput("mymap"))
    )
      )



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$table <- renderTable(DisplayDF)
  
  #map
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircleMarkers(data = points,fillColor = ~factpal(pointlegend))%>%
      addLegend("bottomright", pal = factpal, values = points$pointlegend,
                title = "Legend Not Working and I Need a Break")
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
