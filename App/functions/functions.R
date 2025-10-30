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
  year <- as.numeric(format(sysDate, "%Y"))
  
  latest <- climateAnalyzeR::import_data(
    data_type = "daily_wx",
    station_id = "KA7WSB-1",
    start_year = year - 1,
    end_year = year,
    station_type = "RAWS"
  ) %>%
    tidyr::drop_na(tmin_f) %>%
    dplyr::mutate(date_parsed = as.POSIXct(date, format = "%m/%d/%Y")) %>%
    dplyr::filter(date_parsed == max(date_parsed)) %>%
    dplyr::slice(1)
  
  list(
    Tmin = as.numeric(latest$tmin_f),
    Flag = if (latest$date_parsed == sysDate) {
      "None"
    } else {
      paste0("Temperature data missing, used last known value from: ", format(latest$date_parsed, '%m-%Y'))
    }
  )
}

Tminresult <- get.Tmin.Flag(sysDate1)
Var_TMin <- Tminresult$Tmin
Var_TMin_Flag  <- Tminresult$Flag


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
  formattedMonth <- as.numeric(format(sysDate, "%m"))
  
  url <- "https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php"
  NinXTS <- url %>% rvest::read_html()
  
  NinText <- rvest::html_table(rvest::html_nodes(NinXTS, xpath = './/table[4]//table[2]'))
  NinTable <- as.data.frame(NinText[[1]]) %>%
    janitor::row_to_names(row_number = 1) %>%
    filter(Year != "Year") %>%
    mutate(Year = as.numeric(Year)) %>%
    tidyr::drop_na(Year)
  
  NinVal <- NinTable %>%
    filter(Year == formattedEndYear) %>%
    select(case_when(
      formattedMonth == 1 ~ "NDJ",
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
      formattedMonth == 12 ~ "OND"
    )) %>%
    unlist(use.names = FALSE) %>%
    as.numeric()
  
  PrevVal <- NinTable %>%
    filter(Year == max(Year)) %>%
    tidyr::pivot_longer(-Year) %>%
    mutate(month = case_when(
      name == "NDJ" ~ 1, name == "DJF" ~ 2, name == "JFM" ~ 3, name == "FMA" ~ 4,
      name == "MAM" ~ 5, name == "AMJ" ~ 6, name == "MJJ" ~ 7, name == "JJA" ~ 8,
      name == "JAS" ~ 9, name == "ASO" ~ 10, name == "SON" ~ 11, name == "OND" ~ 12,
      TRUE ~ NA_real_
    )) %>%
    tidyr::drop_na(value) %>%
    filter(month == max(month))
  
  PrevVal_value <- PrevVal$value
  PrevVal_date <- paste0(PrevVal$month,"-",PrevVal$Year)
  
  fallback_value <- -0.3
  fallback_flag <- "Nino data missing, used last known value from: 9-2025"
  
  result <- tryCatch({
    if (!is.na(NinVal)) {
      list(
        oni = as.numeric(NinVal),
        flag = NA_character_
      )
    } else if (!is.na(PrevVal_value)) {
      list(
        oni = as.numeric(PrevVal_value),
        flag = paste0(
          "Nino data missing, used last known value from: ",
          PrevVal_date
        )
      )
    }
  },
  error = function(e) {
    list(
      oni = fallback_value,
      flag = fallback_flag
    )
  })
  
  return(result)
}

# Example usage:
oni_res <- get.NinXTS(sysDate1)
Var_NinXTS <- oni_res$oni
Var_NinXTSFlag <- oni_res$flag

#modified from https://stackoverflow.com/questions/49370387/convert-time-object-to-categorical-morning-afternoon-evening-night-variable

get.TOD <- function(sysTime) {
  # Create categorical variables
  currenttime <- as.POSIXct(sysTime, format = "%H:%M") %>% format("%H:%M:%S")
  
  currenttime <- cut(chron::times(currenttime) , breaks = (1/24) * c(0,5,11,16,19,24))
  Var_TOD <- c(4, 1, 2, 3, 4)[as.numeric(currenttime)]
}


Var_TOD <- get.TOD(sysDate1)
