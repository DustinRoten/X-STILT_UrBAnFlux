#' `write_interp.footprint` is a small function that formats interpolated rasters
#' into something that can be saved as a *.nc file with attributes similar to those
#' of X-STILT generated footprints.
#' 
#' param 1: `footprint.raster` requires any raster that needs to be saved as an nc file
#' param 2: `output.filename` contains the output directory and file name of the nc file
write_interp.footprint <- function(footprint.raster = NULL, output.filename = NULL) {
  
  # First, the raster must be saved as a footprint
  footprint.array <- as.array(footprint.raster)
  
  # Determine the extent and resolution of the raster
  xmn <- extent(footprint.raster)[1]; xmx <- extent(footprint.raster)[2]
  ymn <- extent(footprint.raster)[3]; ymx <- extent(footprint.raster)[4]
  xres <- res(footprint.raster)[1]; yres <- res(footprint.raster)[2]
  
  # Build the long and lat values
  glong <- head(seq(xmn, xmx, by = xres), -1)
  glati <- head(seq(ymn, ymx, by = yres), -1)
  
  # Determine the projection used in the previous raster
  projection <- raster::projection(footprint.raster)
  
  # Save the previous raster's layer names as numeric time intervals
  time_out <- as.POSIXct(names(footprint.raster),
                         format = 'X%Y.%m.%d.%H.%M.%S',
                         tz = 'UTC')
  
  #' Pass these values along to B. Fasoli's `write_footprint` script
  #' More information can be found at:
  #' https://github.com/uataq/stilt/blob/main/r/src/write_footprint.r
  write_footprint(foot = footprint.array, output.filename, glong, glati,
                  projection, time_out, xres, yres)
}