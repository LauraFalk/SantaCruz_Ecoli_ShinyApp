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
get.NinXTS <- function(sysDate = Sys.Date(), max_lag_months = 3) {
  
  # Last resort only, if the NOAA file itself can't be read
  fallback_value <- 2.46
  fallback_flag  <- "Nino data unavailable, used last known value from: 11-2026"
  
  # Position in this vector = the season's center month (DJF = 1, ... NDJ = 12)
  seasons <- c("DJF", "JFM", "FMA", "MAM", "AMJ", "MJJ",
               "JJA", "JAS", "ASO", "SON", "OND", "NDJ")
  
  tryCatch({
    
    oni <- read.table(
      "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt",
      header = TRUE, stringsAsFactors = FALSE
    )
    
    # Latest row with a valid value
    oni <- oni[!is.na(oni$ANOM) & oni$SEAS %in% seasons, ]
    if (nrow(oni) == 0) stop("No valid rows in NOAA file")
    last <- oni[nrow(oni), ]
    
    # Attribute a date: the last month of the 3-month season
    # (e.g. ASO 2026 -> 10-2026; NDJ 2026 -> 01-2027)
    center_date <- as.Date(sprintf("%d-%02d-01", last$YR, match(last$SEAS, seasons)))
    end_date    <- seq(center_date, by = "month", length.out = 2)[2]
    end_label   <- format(end_date, "%m-%Y")
    
    # Compare to system date (whole months)
    ym  <- function(d) as.numeric(format(d, "%Y")) * 12 + as.numeric(format(d, "%m"))
    lag <- ym(sysDate) - ym(end_date)
    
    if (lag <= max_lag_months) {
      flag <- NA_character_              # usable, no flag
    } else {
      flag <- paste0("Nino data stale (", lag, " months old), ",
                     "used last known value from: ", end_label,
                     " (", last$SEAS, " ", last$YR, ")")
    }
    
    list(oni = as.numeric(last$ANOM), flag = flag,
         period = paste(last$SEAS, last$YR), date = end_date)
    
  }, error = function(e) {
    message("get.NinXTS failed: ", conditionMessage(e))
    list(oni = fallback_value, flag = fallback_flag,
         period = NA_character_, date = NA)
  })
}

# Example usage:
oni_res        <- get.NinXTS(sysDate1)
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
