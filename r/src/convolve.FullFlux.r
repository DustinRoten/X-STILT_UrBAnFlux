convolve.FullFlux <- function(site = NULL, citylon = NULL, citylat = NULL,
                              local.tz = NULL, work.dir = NULL, temp.dir = NULL,
                              odiac.dir = NULL, carma.file = NULL, out.dir = NULL,
                              edgar.dir = NULL, footprint = NULL) {
  
  options(stringsAsFactors = FALSE)
  
  #source stuff here
  setwd(work.dir)
  source('r/dependencies.r')
  
  # Acquire date and time from the file path
  date.time <- str_split_fixed(basename(footprint), pattern = '_', n = 5)[1]

  # Identify the year, month, day, hour, and minute
  if(nchar(date.time) != 12) {print('Invalid date.time format'); return()}
  
  year <- substr(date.time, 1, 4)
  month <- substr(date.time, 5, 6)
  day <- substr(date.time, 7, 8)
  hour <- substr(date.time, 9, 10)
  minute <- substr(date.time, 11, 12)
  
  # Convert the time and date to POSIX format to obtain the days in month
  string.format <- paste0(year, '-', month, '-', day, ' ', hour, ':', minute)
  pos.format <- as.POSIXlt(string.format, format = '%Y-%m-%d %H:%M', tz = 'UTC')
  month.days <- days_in_month(pos.format)
  
  # Create the appropriate directory for results
  if(!dir.exists(out.dir))
    dir.create(out.dir, showWarnings = FALSE)
  
  ########################################################################
  ### First, read in the footprint raster to obtain the working domain ###
  ########################################################################
  footprint.raster <- stack(footprint)
  layer.names <- names(footprint.raster)
  ########################################################################
  ########################################################################
  ########################################################################

  
    
  #####################################################################
  ### Now, acquire the ODIAC data. Separate the Large Point Sources ###
  #####################################################################
  
  # Find the appropriate ODIAC file and crop it to the footprint size
  # get.odiac returns the ODIAC raster in kg/m2/s
  odiac.file <- list.files(odiac.dir, pattern = paste0(month, '.tif'),
                           full.names = TRUE)
  ODIAC <- get.odiac(tiff.path = odiac.file,
                     nc.extent = extent(footprint.raster),
                     convert.units = FALSE)
  extent(ODIAC) <- extent(footprint.raster)
  
  # Read in the CarMA file here.
  # Disaggregate the large point sources within ODIAC
  CarMA <- read.csv(carma.file)
  ODIAC.lps <- get.odiac.lps(CarMA, ODIAC)
  ODIAC.nlps <- get.odiac.nlps(CarMA, ODIAC)
  
  #####################################################################
  #####################################################################
  #####################################################################

  
  #####################################
  ### Next, read in the EDGAR files ###
  #####################################
  
  ### All EDGAR sectors are listed below
  ### Not all of them may be available
  
  # AGS - Agricultural soils
  # AWB - Agricultural waste burning
  # CHE - Chemical processes
  # ENE - Power industry
  # ENF - Enteric fermentation
  # FFF - Fossil Fuel Fires
  # IDE - Indirect emissions from NOx and NH3
  # IND - Combustion for manufacturing
  # IRO - Iron and steel production
  # MNM - Manure management
  # N2O - Indirect N2O emissions from agriculture
  # PRO_COAL - Fuel exploitation COAL
  # PRO_GAS - Fuel exploitation GAS
  # PRO_OIL - Fuel exploitation OIL
  # PRU_SOL - Solvents and products use
  # RCO - Energy for buildings
  # REF_TRF - Oil refineries and Transformation industry
  # SWD_INC - Solid waste incineration
  # SWD_LDF - Solid waste landfills
  # TNR_Aviation_CDS - Aviation climbing&descent
  # TNR_Aviation_CRS - Aviation cruise
  # TNR_Aviation_LTO - Aviation landing&takeoff
  # TNR_Other - Railways, pipelines, off-road transport
  # TNR_Ship - Shipping
  # TRO - Road transportation
  # WWT - Waste water handling
  
  #all sectors
  sectors <- c('AGS', 'AWB', 'CHE', 'ENE', 'ENF', 'NFE', 'FFF', 'IDE', 'IND', 'IRO',
               'MNM', 'N2O', 'PRO', 'PRO_COAL', 'PRO_GAS', 'PRO_OIL', 'PRU_SOL', 'RCO',
               'REF_TRF', 'SWD_INC', 'SWD_LDF', 'TNR_Aviation_CDS', 'TNR_Aviation_CRS',
               'TNR_Aviation_LTO', 'TNR_Other', 'TNR_Ship', 'TRO', 'WWT')
  
  # identify available sectors
  Available.Sector.List <- NULL; j <- 1
  for(i in 1:length(sectors)) {
    
    # Step through the list of reported sectors and search for each file
    eval(parse(text = paste0('tmp <- list.files(edgar.dir, pattern = "',
                             sectors[i], '", full.names = TRUE)')))
    
    # If the file exists, save the file path to a custom variable
    if(length(tmp) > 0) {
      if(extension(tmp) == '.nc') {
        Available.Sector.List[j] <- sectors[i]; j<-j+1      
      }
    }
  }; remove('j')
  #####################################
  #####################################
  #####################################
  
  ### Determine the total emissions from EDGAR ###
  for(i in 1:length(Available.Sector.List)) {
    
    # Generate a temporary file path variable for each sector
    temp.filepath <- list.files(edgar.dir,
                                pattern = Available.Sector.List[i],
                                full.names = TRUE)
    edgar.sector <- get.edgar.sector(nc.path = temp.filepath,
                                     nc.extent = extent(footprint.raster))
    
    # Resample each edgar sector
    resampled.sector <- resample(edgar.sector, ODIAC)
    resampled.sector[resampled.sector < 0] <- 0
    
    if(i == 1) total.EDGAR <- resampled.sector
    if(i > 1) total.EDGAR <- total.EDGAR + resampled.sector
    remove('resampled.sector')
    
  }
  
  time <- gsub('X', '', layer.names)
  for(i in 1:length(Available.Sector.List)) {
    
    ### Step through each time increment ###
    for(j in 1:length(time)) {
      
      # Generate a temporary file path variable for each sector
      temp.filepath <- list.files(edgar.dir,
                                  pattern = Available.Sector.List[i],
                                  full.names = TRUE)
      edgar.sector <- get.edgar.sector(nc.path = temp.filepath,
                                       nc.extent = extent(footprint.raster))
      
      # Resample each edgar sector
      resampled.sector <- resample(edgar.sector, ODIAC)
      resampled.sector[resampled.sector < 0] <- 0
      
      # Grab the weighting value for the sector here
      weight <- edgar.sector.weighting(citylon, citylat, local.tz,
                                       sector.name = Available.Sector.List[i],
                                       temporal.downscaling.files = temp.dir,
                                       time = time[j], monthly = TRUE)
      weighted.sector <- weight*resampled.sector
      names(weighted.sector) <- layer.names[j]
      
      # Grab the layer of interest
      eval(parse(text = paste0('footprint.layer <- footprint.raster$',
                               layer.names[j])))
      
      # Perform the weighting on the footprint layer (hour)
      if(Available.Sector.List[i] != 'ENE') {
        
        # Weight ODIAC to generate the sector
        # The weighted layer has NEW UNITS: Tonne Carbon/cell/hour
        weighted.layer <-
          (weighted.sector/total.EDGAR)*ODIAC.nlps
        
        ### Convert the weighted layer to umol/m2/s ###
        #' This next bit of code has been modified from `get.odiac()`.
        
        # Compute area using area() function in raster package
        area.raster <- raster::area(weighted.layer) * 1E6    # convert km2 to m2
        
        # Convert the unit of CO2 emiss from Tonne Carbon/cell/hour to umol/m2/s
        weighted.layer <- weighted.layer * 1E6 / 12 * 1E6 # convert tonne-C to uomol-C (= umole-CO2)
        weighted.layer <- weighted.layer / 60 / 60	# convert per month to per second
        weighted.layer <- weighted.layer / area.raster		# convert per cell to per m2
        # NOW sel.co2 has unit of umole-CO2/m2/s, can be used directly with footprint
        
        # Convolve with the footprint layer
        weighted.XCO2.layer <- weighted.layer*footprint.layer
        
      } else if(Available.Sector.List[i] == 'ENE') {
        # Weight ODIAC to generate the sector
        weighted.layer <-
          (weighted.sector/total.EDGAR)*ODIAC.lps
        
        ### Convert the weighted layer to umol/m2/s ###
        # Compute area using area() function in raster package
        area.raster <- raster::area(weighted.layer) * 1E6    # convert km2 to m2
        
        # Convert the unit of CO2 emiss from Tonne Carbon/cell/hour to umol/m2/s
        weighted.layer <- weighted.layer * 1E6 / 12 * 1E6 # convert tonne-C to uomol-C (= umole-CO2)
        weighted.layer <- weighted.layer / 60 / 60	# convert per month to per second
        weighted.layer <- weighted.layer / area.raster		# convert per cell to per m2
        # NOW sel.co2 has unit of umole-CO2/m2/s, can be used directly with footprint
        
        # Convolve with the footprint layer
        weighted.XCO2.layer <- weighted.layer*footprint.layer
      }
      
      if(j == 1) {
        receptor.XCO2 <- weighted.XCO2.layer
        remove('weighted.layer')
      }

      if(j > 1) {
        receptor.XCO2 <- receptor.XCO2 + weighted.XCO2.layer
        remove('weighted.layer')
      }
      
      message(paste0(i, '/', length(Available.Sector.List),
                     '; ', j, '/', length(time)))
      
    } # closes layer loop
    
    ######

    # Create a sub-directory for the sector and basename for the XCO2 raster
    receptor.XCO2.raster_name <-
      paste(Available.Sector.List[i],
             gsub('X_foot.nc', 'XCO2.nc', basename(footprint)), sep = '_')
    
    # Directory
    sector.out.dir <- file.path(out.dir, Available.Sector.List[i])
    if(!dir.exists(sector.out.dir)) dir.create(sector.out.dir)
    
    # Save the raster
    writeRaster(receptor.XCO2, overwrite = TRUE,
                filename = file.path(sector.out.dir,
                                     receptor.XCO2.raster_name))

  }
  
  # Converted ODIAC data
  ODIAC.converted <- get.odiac(tiff.path = odiac.file,
                               nc.extent = extent(footprint.raster),
                               convert.units = TRUE)
  
  # Create the summed footprint here for static emissions
  # Raster functions would be faster but they generate temp files
  # Thus, a low-tech for loop is used.
  for(i in 1:length(time)) {
    # Grab the layer of interest
    eval(parse(text = paste0('footprint.layer <- footprint.raster$',
                             layer.names[j])))
    if(i == 1) summed.footprint <- footprint.layer
    if(i > i) summed.footprint <- summed.footprint + footprint.layer
  }
  
  # Static XCO2 directory
  static.XCO2.dir <- file.path(out.dir, '_Static.XCO2')
  if(!dir.exists(static.XCO2.dir)) dir.create(static.XCO2.dir)
  
  static.XCO2.raster_name <-
    paste('static', gsub('X_foot.nc', 'XCO2.nc', basename(footprint)),
          sep = '_')
  
  # Save the static raster
  writeRaster(summed.footprint*ODIAC.converted, overwrite = TRUE,
              filename = file.path(static.XCO2.dir,
                                   static.XCO2.raster_name))
  
} # close function