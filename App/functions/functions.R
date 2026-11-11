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
  
  TMin <- climateAnalyzeR::import_daily(
    "daily_wx",
    station_id = 'KA7WSB-1',
    start_year = formattedEndYear - 1,
    end_year = formattedEndYear,
    station_type = 'RAWS'
  )
  
  Tmin_Flag <- NULL  # default
  
  if (ncol(TMin) > 1) {
    Var_TMin <- as.numeric(unlist(
      TMin %>%
        mutate(DateasDate = as.POSIXct(date, format = "%m/%d/%Y")) %>%
        subset(DateasDate == as.Date(sysDate, tz = 'America/Phoenix') - 2) %>%
        select(tmin_f)
    ))
    
  } else {
    
    TMin <- climateAnalyzeR::import_daily(
      "daily_wx",
      station_id = 'KA7WSB-1',
      start_year = 2014,
      end_year = as.numeric(format(sysDate, "%Y")),
      station_type = 'RAWS'
    )
    
    Var_TMin <- TMin %>%
      select(date, tmin_f) %>%
      drop_na() %>%
      mutate(date = as.POSIXct(date, format = "%m/%d/%Y")) %>%
      filter(month(date) == month(sysDate),
             day(date) == day(sysDate)) %>%
      summarise(mean_tmin = mean(tmin_f, na.rm = TRUE)) %>%
      pull(mean_tmin)
    
    Tmin_Flag <- "Current data unavailable, used average temperature on this date for extent of record."
  }
  
  return(list(
    Var_TMin = Var_TMin,
    Tmin_Flag = Tmin_Flag
  ))
}
Var_TMin <- get.Tmin(sysDate1)$Var_TMin
Var_TMin_Flag <- get.Tmin(sysDate1)$Tmin_Flag

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
get.NinXTS <- function() {
  fallback_value <- 2.46
  fallback_flag  <- "Nino data missing, used last known value from: 11-2026"
  
  result <- tryCatch({
    txt <- read_html("https://elninoguide.com/dashboard") %>% html_text2()
    # Headline value, e.g. "Niño 3.4: +2.46°C" ("Ni.o" avoids ñ encoding issues)
    value <- regmatches(txt, regexpr("Ni.o 3\\.4:\\s*[+-]?[0-9.]+", txt))
    value <- as.numeric(sub(".*:\\s*", "", value))
    
    if (length(value) != 1 || is.na(value)) stop("Value not found on page")
    
    list(oni = value, flag = NA_character_)
    
  }, error = function(e) {
    list(oni = fallback_value, flag = fallback_flag)
  })
  
  return(result)
}

# Example usage:
oni_res        <- get.NinXTS()
Var_NinXTS     <- oni_res$oni
Var_NinXTSFlag <- oni_res$flag

#modified from https://stackoverflow.com/questions/49370387/convert-time-object-to-categorical-morning-afternoon-evening-night-variable

get.TOD <- function(sysTime) {
  # Create categorical variables
  currenttime <- as.POSIXct(sysTime, format = "%H:%M") %>% format("%H:%M:%S")
  
  currenttime <- cut(chron::times(currenttime) , breaks = (1/24) * c(0,5,11,16,19,24))
  Var_TOD <- c(4, 1, 2, 3, 4)[as.numeric(currenttime)]
}


Var_TOD <- get.TOD(sysDate1)
