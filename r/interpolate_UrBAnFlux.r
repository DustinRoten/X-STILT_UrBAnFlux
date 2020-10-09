interpolate_UrBAnFlux <- function(input.variables = NULL) {
  
  library(stringr)
  
  timestamp <- input.variables$timestamp
  store.path <- input.variables$store.path
  met <- input.variables$met
  oco.sensor <- input.variables$oco.sensor
  
  timestr<- strftime(as.POSIXct(timestamp, format = '%Y%m%d-%H%M%S', tz = 'UTC'),
                     format = '%Y%m%d%H%M', tz = 'UTC')
  outdir <- file.path(store.path,
                      paste('out', gsub(' ', '', site),
                            timestr, met, oco.sensor, sep = '_'))
  if(!dir.exists(outdir)) {
    message('Footprint directory not found.'); return(NULL)
  }
  
  # Grab the grid data
  grid.data <- read.csv(file.path(outdir, 'receptor_list.csv'), header = TRUE)
  
  # Grab the available footprints and make a dataframe.
  footprint.files <- list.files(file.path(outdir, 'footprints'), full.names = TRUE)
  footprints_df <- data.frame(
    str_split_fixed(string = basename(footprint.files),
                    pattern = '_', n = 4)[,1:3], footprint.files
  ); names(footprints_df) <- c('timestr', 'lon', 'lat', 'file.path')
  
  # Break apart the timestr column into year, month, day, hour, sec
  footprints_df <- data.frame(
    year = as.numeric(substr(footprints_df$timestr, 1, 4)),
    month = as.numeric(substr(footprints_df$timestr, 5, 6)),
    day = as.numeric(substr(footprints_df$timestr, 7, 8)),
    hour = as.numeric(substr(footprints_df$timestr, 9, 10)),
    sec = as.numeric(substr(footprints_df$timestr, 11, 12)),
    lon = as.numeric(footprints_df$lon),
    lat = as.numeric(footprints_df$lat),
    footprint.files
  )
  
  # Quick QA/QC check here
  QAQC <- nrow(grid.data[grid.data$type == 'interpolate',]) == nrow(footprints_df)
  if(QAQC == FALSE) {
    message('Missing some or all footprint files.'); return(NULL)
  }
} # Close function