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
      paste0("Temperature data missing, used last known value on: ", latest$date_parsed)
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
library(httr)

get_latest_ONI_safe <- function(url = "https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/oni.ascii.txt") {
  fallback_value <- -0.17
  fallback_message <- "Nino data missing, used last known value on: July 2025"
  
  result <- tryCatch({
    # Make GET request with 10 second timeout
    resp <- httr::GET(url, httr::timeout(5))
    
    # Check for HTTP errors
    httr::stop_for_status(resp)
    
    # Extract content as text
    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    
    # Split text into lines
    oni_data <- unlist(strsplit(content_text, "\n"))
    
    # Clean and filter data lines
    oni_data <- oni_data[!grepl("^#", oni_data)]
    oni_data <- oni_data[nzchar(oni_data)]
    
    # Parse table
    oni_table <- read.table(text = oni_data, header = TRUE, fill = TRUE)
    
    # Get last row (most recent year)
    last_row <- tail(oni_table, 1)
    
    # Extract ONI values (excluding year column)
    oni_values <- as.numeric(last_row[ , -1])
    
    # Latest ONI = last non-NA value
    latest_oni <- tail(oni_values[!is.na(oni_values)], 1)
    
    list(
      oni = latest_oni,
      flag = FALSE,
      message = paste0("Latest ONI value successfully fetched: ", latest_oni)
    )
  }, error = function(e) {
    # On error, return fallback value and flag
    list(
      oni = fallback_value,
      flag = TRUE,
      message = fallback_message
    )
  })
  
  return(result)
}

# Example usage:
oni_res <- get_latest_ONI_safe()
Var_NinXTS <- oni_res$oni
Var_NinXTSFlag <- oni_res$message

#modified from https://stackoverflow.com/questions/49370387/convert-time-object-to-categorical-morning-afternoon-evening-night-variable

get.TOD <- function(sysTime) {
  # Create categorical variables
  currenttime <- as.POSIXct(sysTime, format = "%H:%M") %>% format("%H:%M:%S")
  
  currenttime <- cut(chron::times(currenttime) , breaks = (1/24) * c(0,5,11,16,19,24))
  Var_TOD <- c(4, 1, 2, 3, 4)[as.numeric(currenttime)]
}


Var_TOD <- get.TOD(sysDate1)
