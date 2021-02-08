convolve.EDGAR.sector <- function(footprint = NULL, lon.lat = NULL,
                                  sector.name = NULL,
                                  edgar.inventory = NULL,
                                  temporal.downscaling = NULL,
                                  monthly = FALSE,
                                  convert.units = TRUE) {
  
  #grab the sector to be convolved
  sector.path <- list.files(edgar.inventory,
                            pattern = sector.name,
                            full.names = TRUE)
  
  layer.names <- names(footprint)
  for(layer in 1:length(layer.names)) {
    
    time <- gsub('X', '', layer.names[layer])
    
    #grab the footprint layer
    eval(parse(text = paste0('footprint.layer <- footprint$',
                             layer.names[layer])))
    
    #get edgar sector
    edgar.sector <-
      get.edgar.sector(nc.path = sector.path,
                       nc.extent = extent(footprint))
    
    #calculate and apply edgar weighting
    weight <- edgar.sector.weighting(citylon = lon.lat$citylon,
                                     citylat = lon.lat$citylat,
                                     local.tz = lon.lat$tz,
                                     sector.name,
                                     temporal.downscaling,
                                     time, monthly)
    weighted.edgar <- weight*edgar.sector
    remove('edgar.sector') #save RAM
    
    if(layer == 1)
      sector.XCO2 <- weighted.edgar*footprint.layer
    if(layer > 1)
      sector.XCO2 <- sector.XCO2 + (weighted.edgar*footprint.layer)
    
    remove('weighted.edgar') #save RAM
    
  }#closes layer loop
  
  names(sector.XCO2) <- sector.name
  return(sector.XCO2)
  
}