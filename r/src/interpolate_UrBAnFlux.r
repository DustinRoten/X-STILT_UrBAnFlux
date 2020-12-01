interpolate_UrBAnFlux <- function(input.variables = NULL) {

  options(stringsAsFactors = FALSE)
  
  # load the required libraries
  library(stringr); library(dplyr)
  library(raster); library(geosphere); library(ncdf4)
  
  setwd(file.path(input.variables$homedir, 'X-STILT_UrBAnFlux'))
  source('r/dependencies.r')

  # convert the timestamp into a "traditional" timestr variable.
  timestr<- strftime(as.POSIXct(input.variables$timestamp,
                                format = '%Y%m%d-%H%M%S', tz = 'UTC'),
                     format = '%Y%m%d%H%M', tz = 'UTC')

  # construct the output directory and check that it exists.
  outdir <- file.path(input.variables$store.path,
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
    lat = as.numeric(footprints_df$lat)
  ); footprints_df$footprint.files <- footprint.files
  
  # Determine the unique indices in which xstilt footprints are located.
  xstilt.lons.idx <- unique(subset(grid.data, type == 'xstilt')$x.idx)
  xstilt.lats.idx <- unique(subset(grid.data, type == 'xstilt')$y.idx)

  # Create a dataframe to save a list of completed interpolations.
  completed.interpolations <- data.frame(matrix(NA, nrow = 0, ncol = 6))
  names(completed.interpolations) <- names(grid.data)

  slurm.submit <- data.frame(matrix(NA, nrow = 0, ncol = 27))
  names(slurm.submit)  <- c('receptor.resolution', 'workdir',
                            'x.idx', 'y.idx', 'interp.lon', 'interp.lat', 'interp.filepath',
                            'x.idx1', 'y.idx1', 'vertex1.lon', 'vertex1.lat', 'vertex1.file.name',
                            'x.idx2', 'y.idx2', 'vertex2.lon', 'vertex2.lat', 'vertex2.file.name',
                            'x.idx3', 'y.idx3', 'vertex3.lon', 'vertex3.lat', 'vertex3.file.name',
                            'x.idx4', 'y.idx4', 'vertex4.lon', 'vertex4.lat', 'vertex4.file.name')
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
      
      if(nrow(xstilt.receptors) == 4) {
        
        for(k in 1:4) {
        
          new.directory <- file.path(outdir, 'by-id',
                                     paste(timestr,
                                           interpolated.receptors$long[k],
                                           interpolated.receptors$lati[k],
                                           'X', sep = '_'))
          
          # Construct the dataframe for SLURM jobs
          add.line <- data.frame(receptor.resolution,
                                 workdir = file.path(input.variables$homedir, 'X-STILT_UrBAnFlux'),
                                 x.idx = as.numeric(interpolated.receptors$x.idx[k]),
                                 y.idx = as.numeric(interpolated.receptors$y.idx[k]),
                                 interp.lon = as.numeric(interpolated.receptors$long[k]),
                                 interp.lat = as.numeric(interpolated.receptors$lati[k]),
                                 interp.filepath = new.directory,
                                 
                                 #First vertex of the subdomain
                                 x.idx1 = as.numeric(xstilt.receptors$x.idx[1]),
                                 y.idx1 = as.numeric(xstilt.receptors$y.idx[1]),
                                 vertex1.lon = as.numeric(xstilt.receptors$long[1]),
                                 vertex1.lat = as.numeric(xstilt.receptors$lati[1]),
                                 vertex1.file.name = xstilt.receptors$file.path[1],
                                 
                                 #Second vertex of the subdomain
                                 x.idx2 = as.numeric(xstilt.receptors$x.idx[2]),
                                 y.idx2 = as.numeric(xstilt.receptors$y.idx[2]),
                                 vertex2.lon = as.numeric(xstilt.receptors$long[2]),
                                 vertex2.lat = as.numeric(xstilt.receptors$lati[2]),
                                 vertex2.file.name = xstilt.receptors$file.path[2],
                                 
                                 #Third vertex of the subdomain
                                 x.idx3 = as.numeric(xstilt.receptors$x.idx[3]),
                                 y.idx3 = as.numeric(xstilt.receptors$y.idx[3]),
                                 vertex3.lon = as.numeric(xstilt.receptors$long[3]),
                                 vertex3.lat = as.numeric(xstilt.receptors$lati[3]),
                                 vertex3.file.name = xstilt.receptors$file.path[3],
                                 
                                 #Fourth vertex of the subdomain
                                 x.idx4 = as.numeric(xstilt.receptors$x.idx[4]),
                                 y.idx4 = as.numeric(xstilt.receptors$y.idx[4]),
                                 vertex4.lon = as.numeric(xstilt.receptors$long[4]),
                                 vertex4.lat = as.numeric(xstilt.receptors$lati[4]),
                                 vertex4.file.name = xstilt.receptors$file.path[4])
          
          slurm.job.idx <- subset(slurm.submit,
                                  interp.lon == as.numeric(interpolated.receptors$long[k]) &
                                    interp.lat == as.numeric(interpolated.receptors$lati[k]) &
                                    interp.filepath == new.directory)
          
          if(nrow(slurm.job.idx) == 0) {slurm.submit <- rbind(slurm.submit, add.line)}
            
        }
      } # Closes the conditional statement checking for a complete set of vertices
    }
  }
  
  interpolate_apply(input.variables, slurm.submit)
  
} # Close function