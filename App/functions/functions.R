###################
# functions.R
# 
# Functions used in the app. 
# Used to define the back-end aspects of the app.
###################

# Use the lubridate package to retrieve UTC rather than sysTime
sysDate1 <- lubridate::now(tz = 'America/Phoenix')
library(dplyr)

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
                                  subset(DateasDate == as.Date(sysDate, tz = 'America/Phoenix') - 2) %>%
                                  select(tmin_f)))
  
  Var_TMin <- ifelse(is_empty(Var_TMin) == TRUE | is.null(Var_TMin) == TRUE, 
                     TMin %>% pull(tmin_f) %>% na.omit() %>% tail(1),
                     Var_TMin)
}

Var_TMin <- get.Tmin(sysDate1)


# T Min Flag
get.Tmin.Flag <- function(sysDate) {
  formattedEndYear <- as.numeric(format(sysDate, "%Y"))
  TMin <- climateAnalyzeR::import_data("daily_wx"
                                       , station_id = 'KA7WSB-1'
                                       , start_year = formattedEndYear-1
                                       , end_year = formattedEndYear
                                       , station_type = 'RAWS')
  
  Var_TMin <- as.numeric(unlist(TMin %>%
                                  mutate(DateasDate = as.POSIXct(TMin$date, format = "%m/%d/%Y")) %>%
                                  subset(DateasDate == as.Date(sysDate, tz = 'America/Phoenix') - 2) %>%
                                  select(tmin_f)))
  
  Var_TMin <- ifelse(is_empty(Var_TMin) == TRUE | is.null(Var_TMin) == TRUE, 
                     "Temperature data missing, used last previous value",
                     Var_TMin)
}

Var_TMin_Flag <- get.Tmin.Flag(sysDate1)

# Discharge
get.DischargeCFS <- function(sysDate) {
  startDate <- as.Date(format(as.Date(sysDate, tz = 'America/Phoenix'),'%Y-%m-%d')) - 1
  endDate <- as.Date(format(as.Date(sysDate, tz = 'America/Phoenix'),'%Y-%m-%d'))
  USGSRaw <- readNWISuv(siteNumbers = '09481740', c('00060','00065'), startDate,endDate, tz = 'America/Phoenix')

  tail(USGSRaw$X_00060_00000, n=1)
}

Var_Discharge_CFS <- get.DischargeCFS(sysDate1)

# Stage
get.stage <- function(sysDate) {
  startDate <- as.Date(format(as.Date(sysDate, tz = 'America/Phoenix'),'%Y-%m-%d')) - 1
  endDate <- as.Date(format(as.Date(sysDate, tz = 'America/Phoenix'),'%Y-%m-%d'))
  USGSRaw <- readNWISuv(siteNumbers = '09481740', c('00060','00065'), startDate,endDate, tz = 'America/Phoenix')
  
  # Determine the difference between prior reading and current.
  USGSRaw <- USGSRaw %>% 
    mutate(DisDif = X_00060_00000 - lag(X_00060_00000))
  
  # This will create a binary variable or either rise of fall. Rise = 1, fall = 0. It will allow me to more easily create summary statistics.
  USGSRaw$DisDif2 <- ifelse(USGSRaw$DisDif>0,1,0)
  
  # Create a numeric classifier. 
  # 1 = Low Flow (<=25%), 2 = Base flow (<=75%), 3 = High and Rising Flow 4 = High and Falling Flow
  USGSRaw$Stage <- ifelse(USGSRaw$X_00060_00000 <= 2.12, 1, 
                          ifelse(USGSRaw$X_00060_00000 > 2.12 & USGSRaw$X_00060_00000 < 14.50 ,2,
                                 ifelse(USGSRaw$X_00060_00000 > 14.50 & USGSRaw$DisDif2 == 1,3,
                                        ifelse(USGSRaw$X_00060_00000 > 14.50 & USGSRaw$DisDif2 == 0,4, NA))))
  

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


#modified from https://stackoverflow.com/questions/49370387/convert-time-object-to-categorical-morning-afternoon-evening-night-variable

get.TOD <- function(sysTime) {
  
  # Create categorical variables
  currenttime <- as.POSIXct(sysTime, format = "%H:%M") %>% format("%H:%M:%S")
  
  currenttime <- cut(chron::times(currenttime) , breaks = (1/24) * c(0,5,11,16,19,24))
  Var_TOD <- c(4, 1, 2, 3, 4)[as.numeric(currenttime)]
}


Var_TOD <- get.TOD(sysDate1)
