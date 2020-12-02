weighted.edgar.sector <- function(citylon = NULL, citylat = NULL,
                                  sector.name = NULL, edgar.dir = NULL,
                                  temporal.downscaling.files = NULL,
                                  time = NULL, nc.extent = NULL) {
  
  # First, read in the butt load of temporal downscaling csv files
  scaling.files <- list.files(temporal.downscaling.files, full.names = TRUE,
                              pattern = '\\.csv$')
  
  if(length(scaling.files) != 7)
    stop('Incorrect number of temporal downscaling files.')
  
  # Read in each csv file and name give it a variable name based on
  # the csv base name. There should be 7 csv files for EDGARv5.
  for(i in 1:7) {
    scaling.file.name <- substr(basename(scaling.files[i]), 1,
                                nchar(basename(scaling.files[i]))-4)
    scaling.file.path <- scaling.files[i]
    eval(parse(text = paste0(scaling.file.name,
                             " <- read.csv('", scaling.file.path, "', sep = ';')")))
  }
  
  # Before running through the temporal downscaling files,
  # grab the activity code.
  activity_code <- sector.name
  
  #' First, determine the time zone (tz) code
  #' Data from `lat_lon_TZ_id.csv`
  tz.idx <- which.min(sqrt((lat_lon_TZ_id$lon - citylon)^2 +
                             (lat_lon_TZ_id$lat - citylat)^2))
  TZ_ID <- lat_lon_TZ_id$TZ_ID[tz.idx]
  
  #' Next, identify the country code and UTC_reference.
  #' Data from `timezones_definition.csv`
  Country_code_A3 <- subset(timezones_definition, TZ_id == TZ_ID)$Country_code_A3
  UTC_reference <- subset(timezones_definition, TZ_id == TZ_ID)$UTC_reference
  
  #' The weekend type is needed. This changes depending on the country
  #' Data from `weekenddays.csv`
  cntry <- Country_code_A3 #rename for subset
  Weekend_type_id <- subset(weekenddays, Country_code_A3 == cntry)$Weekend_type_id
  
  #' Identify the Daytype_id of each day of the assigned week
  #' Data from `weekdays.csv`
  wk_typ_id <- Weekend_type_id #rename for subset
  sub.weekdays <- subset(weekdays, Weekend_type_id == wk_typ_id)
  
  #' Identify the daily_factor of each day of the assigned week
  #' Data from `weekly_profiles.csv`
  
  # as.POSIXlt iterates Sun thru Sat as 0 thrus 6. Sun must become 7 for EDGAR
  POSIX.format <- as.POSIXlt(time, format = '%Y.%m.%d.%H.%M.%S', tz = 'UTC')
  
  # Insert UTC offset here
  
  Weekday_id <- POSIX.format$wday; if(Weekday_id == 0) {Weekday_id <- 7}

  # Monthly profiles are not currently provided for EDGAR v5.
  # A simple division by 12 is used for now.
  MONTHLY_FACTOR <- 1/12

  act.cde <- activity_code; Wkdy_id <- Weekday_id #rename for subset
  DAILY_FACTOR <- subset(weekly_profiles,
                         Country_code_A3 == cntry &
                           activity_code == act.cde &
                           Weekday_id == Wkdy_id)$daily_factor
  if(length(DAILY_FACTOR) == 0) DAILY_FACTOR <- 0
  
  #' Determine the Daytype_id by using the Weekend_id (`Wkdy_id`)
  Dy_typ <- subset(sub.weekdays, Weekday_id == Wkdy_id)$Daytype_id
  
  #' Identify the hourly_factor of each day of the assigned week
  #' Data from `hourly_profiles.csv`
  #' For now, the factor from the nearest hour is applied.
  #' This needs to be linearly interpolated later, with particular
  #' attention given to the end times of each day.
  hr <- as.numeric(POSIX.format$hour); min <- as.numeric(POSIX.format$min)
  hr.round <- round(hr + min/60, 0)
  HOURLY_FACTOR <- subset(hourly_profiles,
                          Country_code_A3 == cntry &
                            activity_code == act.cde &
                            month_id == month(POSIX.format) &
                            Daytype_id == Dy_typ)[,(4+hr.round)]
  if(length(HOURLY_FACTOR) == 0) HOURLY_FACTOR <- 0
  
  #' Apply the temporal downscaling to the EDGAR sector here
  #' Determine days in month for part of the downscaling process
  #' Resample the sector based on the supplied footprint (bilinear interp.)
  #' After the bilinear interpolation, replace negative values with zero.
  #' Weighted as described in `Crippa et al. (2020)`.
  n_days <- days_in_month(POSIX.format)
  weighting <- (MONTHLY_FACTOR*(7/n_days)*DAILY_FACTOR*HOURLY_FACTOR)
  names(weighting) <- 'weight.value'
  
  # Finally, get the EDGAR sector raster and weight it accordingly!
  nc.path <- list.files(edgar.dir, pattern = sector.name, full.names = TRUE)
  edgar.sector <- get.edgar.sector(nc.path, nc.extent)

  return(weighting*edgar.sector)
  
}