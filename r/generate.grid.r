# This function generates a uniform grid of X-STILT receptors along any orientation.
# This function is NOT equipped to straddle the Prime Meridian!
generate.grid <- function(top.left = NULL, top.right = NULL, width = NULL, receptor.resolution = NULL,
                          date.time = NULL, agl = NULL, interpolation.resolution = NULL,
                          output.path = outdir) {

  if(is.null(top.left) | is.null(top.right) | is.null(width) | is.null(receptor.resolution))
    stop('An input has been left as NULL.')
  
  # basic geometric values are calculated here
  top.left <- unlist(top.left); top.right <- unlist(top.right)
  distance <- sqrt((top.right[1]-top.left[1])^2 + ((top.right[2]-top.left[2]))^2)
  iterations <- ceiling(distance/receptor.resolution)
  angle <- atan2(y = top.right[2]-top.left[2], x = top.right[1]-top.left[1])
  
  delta.x <- receptor.resolution*cos(angle)
  delta.y <- receptor.resolution*sin(angle)
  
  x.vals <- NULL; y.vals <- NULL
  x.vals[1] <- top.left[1]; y.vals[1] <- top.left[2]
  for(i in 2:iterations) {
    x.vals[i] <- x.vals[i-1] + delta.x
    y.vals[i] <- y.vals[i-1] + delta.y
  }
  
  # iterate the initial row of receptors
  receptor.grid <- data.frame(x.vals, y.vals, 1:iterations, 1)
  names(receptor.grid) <- c('lon', 'lat', 'x.idx', 'y.idx')
  for(j in 1:ceiling(width/receptor.resolution)) {
    
    if(angle > 0) add.data <- data.frame(x.vals + delta.x*j, y.vals - delta.y*j, 1:iterations, j+1)
    if(angle <= 0) add.data <- data.frame(x.vals - delta.x*j, y.vals + delta.y*j, 1:iterations, j+1)
    
    names(add.data) <- names(receptor.grid)
    receptor.grid <- rbind(receptor.grid, add.data)
  }
  
  receptor.grid <- data.frame(as.POSIXct(date.time, '%Y%m%d-%H%M%S', tz = 'UTC'),
                              receptor.grid$lat, receptor.grid$lon,
                              receptor.grid$y.idx, receptor.grid$x.idx)
  names(receptor.grid) <- c('run_time', 'lati', 'long', 'y.idx', 'x.idx')
  receptor.grid$zagl <- list(agl)
  
  # Here, the identification of receptors for interplation occurs.
  xstilt.x.idx <- seq(min(receptor.grid$x.idx), max(receptor.grid$x.idx), 
                      as.integer(interpolation.resolution))
  xstilt.y.idx <- seq(min(receptor.grid$y.idx), max(receptor.grid$y.idx),
                      as.integer(interpolation.resolution))
  
  xstilt.idx <- data.frame(rep(xstilt.x.idx, each = length(xstilt.y.idx)),
                           xstilt.y.idx); names(xstilt.idx) <- c('x.idx', 'y.idx')
  
  for(k in 1:nrow(xstilt.idx)) {
    receptor.grid$type[(receptor.grid$x.idx == xstilt.idx$x.idx[k] &
                          receptor.grid$y.idx == xstilt.idx$y.idx[k])] <- 'xstilt'
  }; receptor.grid$type[is.na(receptor.grid$type)] <- 'interpolate'
  
  if(!dir.exists(output.path)) dir.create(output.path, recursive = TRUE)
  write.csv(receptor.grid[,c(1:5,7)], file = file.path(output.path, 'receptor_list.csv'),
            row.names = FALSE)
  
  return(receptor.grid)
}
