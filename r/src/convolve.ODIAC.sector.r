convolve.ODIAC.sector <- function(footprint = NULL, lon.lat = NULL,
                                  sector.name = NULL,
                                  categories = NULL,
                                  edgar.inventory = NULL,
                                  odiac.inventory = NULL,
                                  temporal.downscaling = NULL,
                                  monthly = FALSE,
                                  convert.units = TRUE) {
  
  #grab the EDGAR sector to be convoled
  sector.path <- list.files(edgar.inventory,
                            pattern = sector.name,
                            full.names = TRUE)
  
  layer.names <- names(footprint)
  for(layer in 1:length(layer.names)) {
    
    time <- gsub('X', '', layer.names[layer])
    
    #grab the footprint layer
    eval(parse(text = paste0('footprint.layer <- footprint$',
                             layer.names[layer])))
    
    #First, grab ODIAC and split it into LPS and non-LPS rasters
    YYYYMM <- gsub('\\.', '', substr(time, 1, 7))
    ODIAC <- get.odiac(tiff.path = odiac.inventory,
                       nc.extent = extent(footprint),
                       YYYYMM = YYYYMM,
                       convert.units = FALSE)
    
    #Find LPS and non-LPS locations in ODIAC
    ODIAC.lps <- get.odiac.lps(read.csv(carma.inventory), ODIAC)
    ODIAC.nlps <- get.odiac.nlps(read.csv(carma.inventory), ODIAC)
    remove('ODIAC') #save RAM
    
    # get the total EDGAR raster for normalizing
    total.edgar <- get.edgar.total(edgar.inventory,
                                   downscaling.extension,
                                   lon.lat, categories,
                                   time, monthly = TRUE,
                                   ODIAC = ODIAC.nlps,
                                   convert.units = TRUE)
    
    #if we are convolving Power Plants, use EDGAR 'ENE'
    if(sector.name == 'ENE') {
      
      #get edgar sector and resample it
      edgar.sector <-
        get.edgar.sector(nc.path = sector.path,
                         nc.extent = extent(footprint))
      edgar.sector <- resample(edgar.sector, ODIAC.lps)
      edgar.sector[edgar.sector < 0] <- 0
      
      #calculate and apply edgar weighting
      weight <- edgar.sector.weighting(citylon = lon.lat$citylon,
                                       citylat = lon.lat$citylat,
                                       local.tz = lon.lat$tz,
                                       sector.name,
                                       temporal.downscaling,
                                       time, monthly)
      weighted.edgar <- weight*edgar.sector
      remove('edgar.sector') #save RAM
      
      sector.footprint <- (weighted.edgar/total.edgar)*ODIAC.lps
      
      sector.XCO2 <-
        (weighted.edgar/total.edgar)*footprint.layer*ODIAC.lps
      
      
      
    }
    

    
    if(convert.units) {
      ### Convert the weighted layer to umol/m2/s ###
      # Compute area using area() function in raster package
      area.raster <- raster::area(weighted.edgar) * 1E6 # convert km2 to m2
      
      # Convert the unit of CO2 emiss from Tonne Carbon/cell/hour to umol/m2/s
      weighted.edgar <- weighted.edgar * 1E6 / 12 * 1E6 # convert tonne-C to uomol-C (= umole-CO2)
      weighted.edgar <- weighted.edgar / 60 / 60	# convert to per second
      weighted.edgar <- weighted.edgar / area.raster		# convert per cell to per m2
      # NOW sel.co2 has unit of umole-CO2/m2/s, can be used directly with footprint
      
      remove('area.raster') #save RAM
      
    }#close convert.units
    
    if(layer == 1) sector.XCO2 <- weighted.edgar
    if(layer > 1) sector.XCO2 <- sector.XCO2 + weighted.edgar
    
    remove('weighted.edgar') #save RAM
    
  }#closes layer loop
  
  names(sector.XCO2) <- sector.name
  return(sector.XCO2)
  
}