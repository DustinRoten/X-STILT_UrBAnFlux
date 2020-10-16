#' `intepolate.footprint_1` is used to interpolate footprints that are already
#' time-integrated by the X-STILT model. To interpolate footprints that are NOT
#' time-integrated, use `interpolate.footprint_2`.
interpolate.footprint_2 <- function(xstilt.receptors = NULL, interpolated.receptor = NULL,
                                  receptor.resolution = NULL, output.directory = NULL,
                                  footprint.directory = NULL, time_integrate = NULL) {
  
  # Add in the weighted footprints from xstilt-generated footprints.
  # First, determine the number of footprints required.
  inline.receptors <- subset(xstilt.receptors,
         x.idx == interpolated.receptor$x.idx | y.idx == interpolated.receptor$y.idx)
  
  #' If the receptor location to be interpolated is in-line with xstilt receptors,
  #' use only the two closest column-receptors and their associated footprints to
  #' perform the interpolation. These tend to be more accurate than the interior
  #' interpolations of each subdomain.
  if(nrow(inline.receptors) != 0) {
    distance.list <- NULL
    for(i in 1:nrow(inline.receptors)) {
      # first, calculate the distance from the xstilt receptor to the interpolated location.
      # Add this distance to the "total.distance" to use for normalization later.
      distance <- distHaversine(p1 = c(inline.receptors$long[i], inline.receptors$lati[i]),
                                p2 = c(interpolated.receptor$long, interpolated.receptor$lati))
      distance.list[i] <- abs(distance)
      
      # read in the footprint and shift if accordingly.
      footprint <- stack(inline.receptors$file.path[i])
      
      # determine increments to shift by.
      delta.x <- receptor.resolution*(interpolated.receptor$x.idx - inline.receptors$x.idx[i])
      delta.y <- -1*receptor.resolution*(interpolated.receptor$y.idx - inline.receptors$y.idx[i])
      
      extent(footprint) <-
        c(extent(footprint)[1] + delta.x,
          extent(footprint)[2] + delta.x,
          extent(footprint)[3] + delta.y,
          extent(footprint)[4] + delta.y)
      
      eval(parse(text = paste0('foot.', i, ' <- footprint')))
    }
    
    #Find the lowest number of layers among the xstilt footprints
    nlayers <- min(c(nlayers(foot.1), nlayers(foot.2)))
    interpolated.footprint <- raster() #Create an empty raster to store layers
    for(layer in 1:nlayers) {
      # Add the two footprints together. Suppress warnings about the intersection of the footprints.
      interpolated.layer <-
        suppressWarnings((1/sum(distance.list))*
                           (distance.list[1]*foot.1[[layer]] + distance.list[2]*foot.2[[layer]]))
      
      interpolated.footprint <- addLayer(interpolated.footprint, interpolated.layer)
    }; names(interpolated.footprint) <- names(foot.1)[1:nlayers]
    
    # Snap the interpolated footprint to the common grid.
    interpolated.footprint <- resample(interpolated.footprint, foot.1, method = 'ngb')
    
    # create the directory and save the footprint file (*.nc) in it
    suppressWarnings(dir.create(footprint.directory, recursive = TRUE))
    output.filename <- file.path(footprint.directory,
                                 paste0(basename(footprint.directory), '_foot.nc'))
    writeRaster(interpolated.footprint, filename = output.filename,
                format = 'CDF', overwrite = TRUE)
    
    # add a symbolic link in the "footprints" folder for easy access
    # Symlink footprint to out/footprints
    link <- file.path(output.directory, 'footprints', basename(output.filename))
    suppressWarnings(file.symlink(output.filename, link))
  }
  
  #' If the receptor location to be interpolated is NOT in-line with xstilt receptors,
  #' use all four of the column-receptors and their associated footprints to
  #' perform the interpolation.
  if(nrow(inline.receptors) == 0) {
    distance.list <- NULL
    for(i in 1:nrow(xstilt.receptors)) {
      # first, calculate the distance from the xstilt receptor to the interpolated location.
      # Add this distance to the "total.distance" to use for normalization later.
      distance <- distHaversine(p1 = c(xstilt.receptors$long[i], xstilt.receptors$lati[i]),
                                p2 = c(interpolated.receptor$long, interpolated.receptor$lati))
      distance.list[i] <- abs(distance)
      
      # read in the footprint and shift if accordingly.
      footprint <- stack(xstilt.receptors$file.path[i])
      
      # determine increments to shift by.
      delta.x <- receptor.resolution*(interpolated.receptor$x.idx - xstilt.receptors$x.idx[i])
      delta.y <- -1*receptor.resolution*(interpolated.receptor$y.idx - xstilt.receptors$y.idx[i])
      
      extent(footprint) <-
        c(extent(footprint)[1] + delta.x,
          extent(footprint)[2] + delta.x,
          extent(footprint)[3] + delta.y,
          extent(footprint)[4] + delta.y)
      
      eval(parse(text = paste0('foot.', i, ' <- footprint')))
    }
    
    #Find the lowest number of layers among the xstilt footprints
    nlayers <- min(c(nlayers(foot.1), nlayers(foot.2), nlayers(foot.3), nlayers(foot.4)))
    interpolated.footprint <- raster() #Create an empty raster to store layers
    for(layer in 1:nlayers) {
      # Add the four footprints together. Suppress warnings about the intersection of the footprints.
      interpolated.layer <-
        suppressWarnings((1/sum(distance.list))*
                           (distance.list[1]*foot.1[[layer]] + distance.list[2]*foot.2[[layer]] +
                              distance.list[3]*foot.3[[layer]] + distance.list[4]*foot.4[[layer]]))
      
      interpolated.footprint <- addLayer(interpolated.footprint, interpolated.layer)
      
    }; names(interpolated.footprint) <- names(foot.1)[1:nlayers]
    
    # Snap the interpolated footprint to the common grid.
    interpolated.footprint <- resample(interpolated.footprint, foot.1, method = 'ngb')
    
    # create the directory and save the footprint file (*.nc) in it
    dir.create(footprint.directory, recursive = TRUE)
    output.filename <- file.path(footprint.directory,
                                 paste0(basename(footprint.directory), '_foot.nc'))
    writeRaster(interpolated.footprint, filename = output.filename, format = 'CDF')
    
    # add a symbolic link in the "footprints" folder for easy access
    # Symlink footprint to out/footprints
    link <- file.path(output.directory, 'footprints', basename(output.filename))
    suppressWarnings(file.symlink(output.filename, link))
  }
  
}