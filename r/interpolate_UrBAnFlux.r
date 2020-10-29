interpolate_UrBAnFlux <- function(homedir = NULL,
                                  site = NULL,
                                  timestamp = NULL,
                                  store.path = NULL,
                                  met = NULL,
                                  oco.sensor = NULL,
                                  time_integrate = NULL,
                                  receptor.resolution = NULL) {
  
  # load the required libraries
  library(stringr); library(dplyr)
  library(raster); library(geosphere)
  
  setwd(file.path(homedir, 'X-STILT_UrBAnFlux'))
  source('r/dependencies.r')
  
  # convert the timestamp into a "traditional" timestr variable.
  timestr<- strftime(as.POSIXct(timestamp, format = '%Y%m%d-%H%M%S', tz = 'UTC'),
                     format = '%Y%m%d%H%M', tz = 'UTC')
  
  # construct the output directory and check that it exists.
  outdir <- file.path(store.path,
                      paste('out', gsub(' ', '', site),
                            timestr, met, 'Modeled', sep = '_'))
  
  if(!dir.exists(outdir)) {message('Footprint directory not found.'); return(NULL)}
  
  # Grab the grid data generated from by the initial x-stilt run.
  # These data indicate which receptors were generated and which
  # need to be interpolated.
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
  
  # Determine the unique indices in which xstilt footprints are located.
  xstilt.lons.idx <- unique(subset(grid.data, type == 'xstilt')$x.idx)
  xstilt.lats.idx <- unique(subset(grid.data, type == 'xstilt')$y.idx)
  
  # Create a dataframe to save a list of completed interpolations.
  completed.interpolations <- data.frame(matrix(NA, nrow = 0, ncol = 6))
  names(completed.interpolations) <- names(grid.data)
  
  # Begin the interpolation algorithm here. Subset each subdomain by indices.
  for(i in 1:(length(xstilt.lons.idx)-1)) {
    for(j in 1:(length(xstilt.lats.idx)-1)) {
    
      xstilt.receptors <-
        subset(grid.data,
               type == 'xstilt' &
                 (x.idx >= xstilt.lons.idx[i] & x.idx <= xstilt.lons.idx[i+1]) &
                 (y.idx >= xstilt.lats.idx[j] & y.idx <= xstilt.lats.idx[j+1]))
      
      interpolated.receptors <-
        subset(grid.data,
               type == 'interpolate' &
                 (x.idx >= xstilt.lons.idx[i] & x.idx <= xstilt.lons.idx[i+1]) &
                 (y.idx >= xstilt.lats.idx[j] & y.idx <= xstilt.lats.idx[j+1]))
      interpolated.receptors$file.path <- NA
      
      #' construct subdomains of the receptor grid here. Each subdomain will have
      #' its own number and duplicate rows will be removed for efficiency
      for(k in 1:nrow(xstilt.receptors)) {
        foot.idx <- which(abs(footprints_df$lon - xstilt.receptors$long[k]) < 1e-4 &
                            abs(footprints_df$lat - xstilt.receptors$lati[k]) < 1e-4)
        if(length(foot.idx) != 1) stop('Too many or no footprints found!')
        xstilt.receptors$file.path[k] <- footprints_df$footprint.files[foot.idx]
      }
      
      for(k in 1:nrow(interpolated.receptors)) {
        
        # check the list of completed interpolations so a footprint isn't duplicated.
        check.for.completion <- subset(completed.interpolations,
                                       (lati == interpolated.receptors$lati[k] &
                                          long == interpolated.receptors$long[k]))
        if(nrow(check.for.completion) == 0) {
          new.directory <- file.path(outdir, 'by-id',
                                     paste(timestr,
                                           interpolated.receptors$long[k],
                                           interpolated.receptors$lati[k],
                                           'X', sep = '_'))
          
          if(time_integrate == TRUE) {
            # interpolate the TIME-INTEGRATED missing footprint
            # save in the appropriate directory
            interpolate.footprint_1(xstilt.receptors,
                                    time_integrate,
                                    interpolated.receptor = interpolated.receptors[k,],
                                    receptor.resolution, output.directory = outdir,
                                    footprint.directory = new.directory)
          } else if(time_integrate == FALSE) {
            # interpolate the NON-TIME-INTEGRATED missing footprint
            # save in the appropriate directoy
            interpolate.footprint_2(xstilt.receptors,
                                    time_integrate,
                                    interpolated.receptor = interpolated.receptors[k,],
                                    receptor.resolution, output.directory = outdir,
                                    footprint.directory = new.directory)
          }
          
          #' Add this column-receptor to the list of interpolated receptors.
          #' Receptors on this list will not be interpolated again.
          completed.interpolations <-
            rbind(completed.interpolations, interpolated.receptors[k,])
        } else {} # close the conditional interpolation statement here
      } # close the interpolation method for a subdomain
      
    } #close j for loop
  } #close i for loop
  
    
} # Close function