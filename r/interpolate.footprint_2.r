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
      
      # shift each footprint by the appropriate amount.
      extent(footprint) <-
        c(extent(footprint)[1] + delta.x,
          extent(footprint)[2] + delta.x,
          extent(footprint)[3] + delta.y,
          extent(footprint)[4] + delta.y)
      
      # save shifted footprint to a generic variable
      eval(parse(text = paste0('foot.', i, ' <- footprint')))
    }
    
    ##### Interpolate each layer here (2 footprints used) #####
    #' Collect all of the raster layer names here.
    #' Next, begin interpolating each layer.
    layer.names <- unique(c(names(foot.1), names(foot.2)))
    for(i in 1:length(layer.names)) {
      
      #' Grab each layer of footprint 1
      #' If a layer is missing, make one with all zeros
      foot.1.layer <- tryCatch({
        eval(parse(text = paste0('foot.1.layer <- foot.1$', layer.names[i])))
      }, error=function(cond){
        r <- raster(); extent(r) <- extent(foot.1); values(r) <- 0
        return(r)
      }, finally = {})
      
      #' Grab each layer of footprint 2
      #' If a layer is missing, make one with all zeros
      foot.2.layer <- tryCatch({
        eval(parse(text = paste0('foot.2.layer <- foot.2$', layer.names[i])))
      }, error=function(cond){
        r <- raster(); extent(r) <- extent(foot.2); values(r) <- 0
        return(r)
      }, finally = {})
      
      #' Interpolate the missing footprint here. Assign the layer names
      #' from a complete x-stilt generated footprint.
      interpolated.layer <-
        suppressWarnings((sum(1/(1+distance.list^2))^-1)*
                           ((1/(1+distance.list[1]^2))*foot.1.layer +
                              (1/(1+distance.list[2]^2))*foot.2.layer))
      eval(parse(text = paste0("names(interpolated.layer) <- '", layer.names[i], "'")))
      
      # Use the first layer to generate the interpolated.footprint file
      if(i == 1) interpolated.footprint <- interpolated.layer
      if(i > 1) interpolated.footprint <- addLayer(interpolated.footprint,
                                                   interpolated.layer)
    }
    ##### End of layer interpolation (2) #####
    
    # create the interpolated footprint's directory and create its filename
    suppressWarnings(dir.create(footprint.directory, recursive = TRUE))
    output.filename <- file.path(footprint.directory,
                                 paste0(basename(footprint.directory), '_foot.nc'))
    if(file.exists(output.filename)) file.remove(output.filename)
    
    # Snap the interpolated footprint to the common grid.
    resampled.footprint <- resample(interpolated.footprint, foot.1,
                                    method = 'ngb', filename = output.filename)
    
    #' The current process needs to be replaced with something more clever.
    #' The previous line saves the appropriate file in the appropriate location.
    #' This keeps the resample function from storing tmp files and taking up too
    #' much memory; however, the layer names (timestamps) are not saved *rolls eyes*
    #' Therefore, the raster is read back into memory and then removed.
    interpolated.footprint <- raster::readAll(output.filename); file.remove(output.filename)
    
    # Now, the file is saved properly in a format similar to the other footprints.
    write_interp.footprint(footprint.raster = interpolated.footprint, output.filename)
    
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
    
    ##### Interpolate each layer here (4 footprints used) #####
    #' Collect all of the raster layer names here.
    #' Next, begin interpolating each layer.
    layer.names <- unique(c(names(foot.1), names(foot.2), names(foot.3), names(foot.4)))
    for(i in 1:length(layer.names)) {
      
      #' Grab each layer of footprint 1
      #' If a layer is missing, make one with all zeros
      foot.1.layer <- tryCatch({
        eval(parse(text = paste0('foot.1.layer <- foot.1$', layer.names[i])))
      }, error=function(cond){
        r <- raster(); extent(r) <- extent(foot.1); values(r) <- 0
        return(r)
      }, finally = {})
      
      #' Grab each layer of footprint 2
      #' If a layer is missing, make one with all zeros
      foot.2.layer <- tryCatch({
        eval(parse(text = paste0('foot.2.layer <- foot.2$', layer.names[i])))
      }, error=function(cond){
        r <- raster(); extent(r) <- extent(foot.2); values(r) <- 0
        return(r)
      }, finally = {})
      
      #' Grab each layer of footprint 3
      #' If a layer is missing, make one with all zeros
      foot.3.layer <- tryCatch({
        eval(parse(text = paste0('foot.3.layer <- foot.3$', layer.names[i])))
      }, error=function(cond){
        r <- raster(); extent(r) <- extent(foot.3); values(r) <- 0
        return(r)
      }, finally = {})
      
      #' Grab each layer of footprint 4
      #' If a layer is missing, make one with all zeros
      foot.4.layer <- tryCatch({
        eval(parse(text = paste0('foot.4.layer <- foot.4$', layer.names[i])))
      }, error=function(cond){
        r <- raster(); extent(r) <- extent(foot.4); values(r) <- 0
        return(r)
      }, finally = {})
      
      #' Interpolate the missing footprint here. Assign the layer names
      #' from a complete x-stilt generated footprint.
      interpolated.layer <-
        suppressWarnings((sum(1/(1+distance.list^2))^-1)*
                           ((1/(1+distance.list[1]^2))*foot.1.layer +
                              (1/(1+distance.list[2]^2))*foot.2.layer +
                              (1/(1+distance.list[2]^2))*foot.3.layer +
                              (1/(1+distance.list[2]^2))*foot.4.layer))
      
      eval(parse(text = paste0("names(interpolated.layer) <- '", layer.names[i], "'")))
      
      # Use the first layer to generate the interpolated.footprint file
      if(i == 1) interpolated.footprint <- interpolated.layer
      if(i > 1) interpolated.footprint <- addLayer(interpolated.footprint,
                                                   interpolated.layer)
    }
    ##### End of layer interpolation (4) #####
    
    # create the interpolated footprint's directory and create its filename
    suppressWarnings(dir.create(footprint.directory, recursive = TRUE))
    output.filename <- file.path(footprint.directory,
                                 paste0(basename(footprint.directory), '_foot.nc'))
    if(file.exists(output.filename)) file.remove(output.filename)
    
    # Snap the interpolated footprint to the common grid.
    resampled.foot <- resample(interpolated.footprint, foot.1,
                               method = 'ngb', filename = output.filename)
    
    #' The current process needs to be replaced with something more clever.
    #' The previous line saves the appropriate file in the appropriate location.
    #' This keeps the resample function from storing tmp files and taking up too
    #' much memory; however, the layer names (timestamps) are not saved *rolls eyes*
    #' Therefore, the raster is read back into memory and then removed.
    interpolated.footprint <- raster::readAll(output.filename); file.remove(output.filename)
    
    # Now, the file is saved properly in a format similar to the other footprints.
    write_interp.footprint(footprint.raster = interpolated.footprint, output.filename)
    
    # add a symbolic link in the "footprints" folder for easy access
    # Symlink footprint to out/footprints
    link <- file.path(output.directory, 'footprints', basename(output.filename))
    suppressWarnings(file.symlink(output.filename, link))
  }
  
}