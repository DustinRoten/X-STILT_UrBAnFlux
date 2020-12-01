convolve.FullFlux.receptor <- function(site = NULL, citylon = NULL, citylat = NULL,
                                       filepath = NULL, edgar.dir = NULL, temp.dir = NULL,
                                       odiac.dir = NULL, work.dir = NULL, out.dir = NULL) {
  
  options(stringsAsFactors = FALSE)
  
  #source stuff here
  setwd(work.dir)
  source('r/dependencies.r')
  
  # Acquire date and time from the file path
  date.time <- str_split_fixed(basename(filepath), pattern = '_', n = 5)[,1]
  
  # Identify the year, month, day, hour, and minute
  if(nchar(date.time) != 12) {print('Invalid date.time format'); return()}
  
  year <- substr(date.time, 2, 4)
  month <- substr(date.time, 5, 6)
  day <- substr(date.time, 7, 8)
  hour <- substr(date.time, 9, 10)
  minute <- substr(date.time, 11, 12)
  
  # First, read in the footprint raster to obtain the working domain
  footprint <- stack(filepath)
  
  # Second, find the appropriate ODIAC file and crop it to the footprint size
  # get.odiac returns the ODIAC raster in kg/m2/s
  odiac.file <- list.files(odiac.dir, pattern = paste0(month, '.tif'),
                           full.names = TRUE)
  ODIAC <- get.odiac(tiff.path = odiac.file, nc.extent = extent(footprint))
  
  ##### Temporary #####
  #' This code provides a temporary means of identifying large point sources
  #' within ODIAC. It must be replaced with Oda's custom raster file that 
  #' contains the ACTUAL locations later on.
  
  # Identify large point sources
  if(gsub(' ', '', site) == 'LosAngeles') threshold <- 50 #custom threshold
  lps.raster <- ODIAC; regional.raster <- ODIAC
  lps.raster[lps.raster <= threshold] <- 0 #large point sources
  regional.raster[regional.raster > threshold] <- 0 #everything else
  #####################
  
  ### Next, read in the EDGAR files
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
  }
  
  ##### Begin weighting each sector here #####
  times <- gsub('X', '', names(footprint)) #YYYY.MM.DD.HH.MM.SS
  for(i in 1:length(times)) {
    for(j in 1:length(Available.Sector.List)) {
      
      w.edgar.sector <-
        weighted.edgar.sector(citylon = citylon, citylat = citylat,
                              sector.name = Available.Sector.List[j],
                              edgar.dir = edgar.dir, time = times[i],
                              temporal.downscaling.files = temporal.directory,
                              nc.extent = extent(footprint))
      resampled.w.edgar.sector <- resample(w.edgar.sector, footprint)
      resampled.w.edgar.sector[resampled.w.edgar.sector < 0] <- 0
      
      if(j == 1) {
        hourly.compiled.edgar <- resampled.w.edgar.sector
        names(hourly.compiled.edgar) <- Available.Sector.List[j]
      } else if(j > 1) {
        hourly.current.names <- names(hourly.compiled.edgar)
        hourly.compiled.edgar <- addLayer(hourly.compiled.edgar,
                                          resampled.w.edgar.sector)
        names(hourly.compiled.edgar) <- c(hourly.current.names,
                                          Available.Sector.List[j])
      }
    }
    
    if(i == 1) {
      total.edgar <- calc(hourly.compiled.edgar, sum)
    }
  }
  
}