convolve.FullFlux <- function(site = NULL, citylon = NULL, citylat = NULL,
                              work.dir = NULL, dir.list = NULL, edgar.dir = NULL,
                              temp.dir = NULL, odiac.dir = NULL, carma.file = NULL,
                              out.dir = NULL) {
  
  options(stringsAsFactors = FALSE)
  
  #source stuff here
  setwd(work.dir)
  source('r/dependencies.r')
  
  # Acquire date and time from the file path
  date.time <- str_split_fixed(basename(dir.list), pattern = '_', n = 5)[,3]
  
  # Identify the year, month, day, hour, and minute
  if(nchar(date.time) != 12) {print('Invalid date.time format'); return()}
  
  year <- substr(date.time, 1, 4)
  month <- substr(date.time, 5, 6)
  day <- substr(date.time, 7, 8)
  hour <- substr(date.time, 9, 10)
  minute <- substr(date.time, 11, 12)
  
  # Create the appropriate directory for results
  if(!dir.exists(out.dir))
    dir.create(out.dir, showWarnings = FALSE)
  
  ########################################################################
  ### First, read in the footprint raster to obtain the working domain ###
  ########################################################################
  
  footprint.list <- list.files(file.path(dir.list, 'footprints'),
                               full.names = TRUE)
  
  # List all of the layers in the footprints
  layer.names <- NULL
  extents <- data.frame(matrix(NA, nrow = length(footprint.list), ncol = 4))
  names(extents) <- c('xmin', 'xmax', 'ymin', 'ymax')
  for(i in 1:length(footprint.list)) {
    footprint <- stack(footprint.list[i])
    layer.names <- c(layer.names, names(footprint))
    extents$xmin[i] <- extent(footprint)[1]
    extents$xmax[i] <- extent(footprint)[2]
    extents$ymin[i] <- extent(footprint)[3]
    extents$ymax[i] <- extent(footprint)[4]
  }
  
  # Get all of the possible layer names
  layer.names <- unique(layer.names)
  new.extent <- c(max(extents$xmin), min(extents$xmax),
                  max(extents$ymin), min(extents$ymax))
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
  ODIAC <- get.odiac(tiff.path = odiac.file, nc.extent = new.extent)
  
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
  }
  #####################################
  #####################################
  #####################################
  
  
    
  # Step through the list of times/layers and average them across all footprints.
  # The dreaded triple nested for loop... Buckle up!
  
  # First, we will begin with a particular EDGAR sector
  for(i in 1:length(Available.Sector.List)) {
    
    # Ultimately, we need to convolve each footprint with
    # a weighted ODIAC sector.
    for(j in 1:length(footprint.list)) {
      
      footprint <- stack(footprint.list[j])
      footprint <- crop(footprint, new.extent)
      
      # Each hourly layer of each footprint will change the
      # weighting of the EDGAR footprint.
      for(k in 1:length(layer.names)) {
        
        tryCatch({
          eval(parse(text = paste0('footprint.layer <- footprint$', layer.names[k])))
        }, error = function(err){
          r <- raster(); extent(r) <- extent(footprint); res(r) <- res(footprint)
          values(r) <- 0; footprint.layer <- r; names(footprint.layer) <- layer.names[k]
        }, finally = {})
        
        # Aquire the weighted EDGAR sector here
        # Resample the sector at ODIAC resolution
        edgar.sector <- 
          weighted.edgar.sector(citylon = citylon, citylat = citylat,
                                sector.name = Available.Sector.List[i],
                                edgar.dir = edgar.dir,
                                time = gsub('X', '', layer.names[k]),
                                temporal.downscaling.files = temp.dir,
                                nc.extent = extent(footprint))
        edgar.sector <- resample(edgar.sector, ODIAC)
        edgar.sector[edgar.sector < 0] <- 0
        
        if(j == 1) {summed.layers <- footprint.layer}
        if(j > 1) {summed.layers <- summed.layers + footprint.layer}
        
      } # close the layer loop
      
      avg.layer <- summed.layers/length(footprint.list)
      names(avg.layer) <- layer.names[i]
      
      if(i == 1) {regional.footprint <- avg.layer}
      if(i > 1) {regional.footprint <- addLayer(regional.footprint, avg.layer)}
      
    } # close the footprint list loop
    
  } # close the sector loop
  
} # close function