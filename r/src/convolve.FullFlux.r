convolve.FullFlux <- function(homedir, site, timestr, cityid, citylon,
                              citylat, tz, countryid, regid, iso3, minlon,
                              maxlon, minlat, maxlat, urban.domain,
                              EDGAR.directory, ODIAC.directory,
                              SMUrF.directory, sector.list,
                              edgar.inventory, odiac.inventory,
                              smurf.inventory, carma.inventory,
                              temporal.downscaling, output.path) {
  
  workdir <- file.path(homedir, 'X-STILT_UrBAnFlux')
  setwd(workdir); source('r/dependencies.r')
  
  lon.lat <- data.frame(cityid, citylon, citylat, tz, countryid,
                        regid, iso3, minlon, maxlon, minlat, maxlat)
  
  urban.polygon <- read.csv(urban.domain)
  
  #grab the EDGAR footprints and place the info into a dataframe
  if(!is.na(EDGAR.directory)) {
    temp.dir <- list.files(EDGAR.directory, pattern = timestr,
                           full.names = TRUE)
    EDGAR.footprints <- list.files(file.path(temp.dir, 'footprints'),
                                   full.names = TRUE)
    EDGAR.df <- str_split_fixed(basename(EDGAR.footprints),
                                pattern = '_', n = 5)[,1:3]
    EDGAR.df <- as.data.frame(EDGAR.df)
    names(EDGAR.df) <- c('date.time', 'lon', 'lat')
    EDGAR.df$lon <- as.numeric(EDGAR.df$lon)
    EDGAR.df$lat <- as.numeric(EDGAR.df$lat)
    EDGAR.df$filepath <- EDGAR.footprints
  }
  
  #grab the ODIAC footprints and place the info into a dataframe
  if(!is.na(ODIAC.directory)) {
    temp.dir <- list.files(ODIAC.directory, pattern = timestr,
                           full.names = TRUE)
    ODIAC.footprints <- list.files(file.path(temp.dir, 'footprints'),
                                   full.names = TRUE)
    ODIAC.df <- str_split_fixed(basename(ODIAC.footprints),
                                pattern = '_', n = 5)[,1:3]
    ODIAC.df <- as.data.frame(ODIAC.df)
    names(ODIAC.df) <- c('date.time', 'lon', 'lat')
    ODIAC.df$lon <- as.numeric(ODIAC.df$lon)
    ODIAC.df$lat <- as.numeric(ODIAC.df$lat)
    ODIAC.df$filepath <- ODIAC.footprints
  }
  
  #grab the SMUrF footprints and place the info into a dataframe
  if(!is.na(SMUrF.directory)) {
    temp.dir <- list.files(SMUrF.directory, pattern = timestr,
                           full.names = TRUE)
    SMUrF.footprints <- list.files(file.path(temp.dir, 'footprints'),
                                   full.names = TRUE)
    SMUrF.df <- str_split_fixed(basename(SMUrF.footprints),
                                pattern = '_', n = 5)[,1:3]
    SMUrF.df <- as.data.frame(SMUrF.df)
    names(SMUrF.df) <- c('date.time', 'lon', 'lat')
    SMUrF.df$lon <- as.numeric(SMUrF.df$lon)
    SMUrF.df$lat <- as.numeric(SMUrF.df$lat)
    SMUrF.df$filepath <- SMUrF.footprints
  }
  
  ### Check that the lons/lats of the above dataframes match up ###
  if(exists('EDGAR.df') & exists('ODIAC.df')) {
    if((nrow(EDGAR.df) != nrow(ODIAC.df)) |
       !all(EDGAR.df$date.time == ODIAC.df$date.time) |
       !all(EDGAR.df$lon == ODIAC.df$lon) |
       !all(EDGAR.df$lat == ODIAC.df$lat))
      stop('Footprint mismatch')
  }
  
  if(exists('SMUrF.df') & exists('ODIAC.df')) {
    if((nrow(SMUrF.df) != nrow(ODIAC.df)) |
       !all(SMUrF.df$date.time == ODIAC.df$date.time) |
       !all(SMUrF.df$lon == ODIAC.df$lon) |
       !all(SMUrF.df$lat == ODIAC.df$lat))
      stop('Footprint mismatch')
  }
  
  if(exists('SMUrF.df') & exists('EDGAR.df')) {
    if((nrow(SMUrF.df) != nrow(EDGAR.df)) |
       !all(SMUrF.df$date.time == EDGAR.df$date.time) |
       !all(SMUrF.df$lon == EDGAR.df$lon) |
       !all(SMUrF.df$lat == EDGAR.df$lat))
      stop('Footprint mismatch')
  }
  
  #if all of the dataframes match, combine them into 1 dataframe
  #since they all match, grab one of the dataframes to use for lon/lat
  df.options <- c('EDGAR.df', 'ODIAC.df', 'SMUrF.df')
  existing.dfs <-
    which(c(exists('EDGAR.df'), exists('ODIAC.df'), exists('SMUrF.df')))
  df.name <- df.options[existing.dfs[1]]
  
  eval(parse(text = paste0('footprint.df <- ', df.name, '[,1:3]')))
  if(exists('EDGAR.df')) footprint.df$EDGAR.filepath <- EDGAR.df$filepath
  if(exists('ODIAC.df')) footprint.df$ODIAC.filepath <- ODIAC.df$filepath
  if(exists('SMUrF.df')) footprint.df$SMUrF.filepath <- SMUrF.df$filepath
  
  #construct a list of the user-defined categories
  categories <- read.csv(sector.list)
  
  ##### Begin Convolutions and Calculations Here #####
  output.results <- data.frame(matrix(NA, nrow = 0, ncol = 7))
  names(output.results) <- c('time', 'lon', 'lat', 'bkg.influence',
                             'Inventory', 'Enhancement', 'Category')
  for(i in 1:nrow(footprint.df)) {
  
    #####################################################  
    ### Convolve EDGAR (if available) with footprints ###
    #####################################################
    if(exists('EDGAR.df')) {
      
      #grab the 0.1 x 0.1
      edgar.footprint <- stack(footprint.df$EDGAR.filepath[i])
      
      ### Calculate Background Influence ###
      #integrate the footprint
      #can't use raster package due to the generation of tmp files
      steps <- names(edgar.footprint)
      for(j in 1:length(steps)) {
        #grab each layer as a tmp.layer
        eval(parse(text = paste0('tmp.layer <- edgar.footprint$',
                                 steps[j])))
        #add the layers together
        if(j == 1)
          total.edgar.footprint <- tmp.layer
        if(j > 1)
          total.edgar.footprint <- total.edgar.footprint + tmp.layer
      }; remove('tmp.layer') #save RAM

      #save the footprint as a dataframe
      total.edgar.footprint_df <-
        raster::as.data.frame(total.edgar.footprint, xy = TRUE)
      total.edgar.footprint_df <- subset(total.edgar.footprint_df,
                                         layer > 0)
      remove('total.edgar.footprint') #save RAM
      
      #determine background influence
      is.polygon <- in.polygon(total.edgar.footprint_df[,1:2],
                               polygon = urban.polygon[,1:2])
      #record value
      bkg.influence <-
        nrow(subset(is.polygon, polygon == 'in'))/nrow(is.polygon)
      ######################################
      
      #now, determine category contributions
      for(j in 1:nrow(categories)) {
        # Grab a category and loop through the sectors
        sectors <- unlist(str_split(categories[j,2], pattern = ' '))
        # Loop through each sector and convolve
        for(k in 1:length(sectors)) {

          #convolved edgar sector
          sector.XCO2 <-
            convolve.EDGAR.sector(footprint = edgar.footprint,
                                  lon.lat = lon.lat,
                                  sector.name = sectors[k],
                                  edgar.inventory,
                                  temporal.downscaling)
          #add the sectoral emission to the category emission
          if(k == 1) category.XCO2 <- sector.XCO2
          if(k > 1) category.XCO2 <- category.XCO2 + sector.XCO2
          remove('sector.XCO2') #save RAM
        }#build category loop
        
        #rename and save the category convolution
        names(category.XCO2) <- categories[j,1]
        
        #put these results together
        EDGAR_add.line <- data.frame(footprint.df$date.time[i],
                                     footprint.df$lon[i],
                                     footprint.df$lat[i],
                                     bkg.influence,
                                     'EDGAR',
                                     cellStats(category.XCO2, sum),
                                     categories[j,1])
        names(EDGAR_add.line) <- names(output.results)
        
        #rbind these results to the cumulative dataframe
        output.results <- rbind(output.results, EDGAR_add.line)
        
        #X-STILT directory
        directory <- list.files(EDGAR.directory,
                                pattern = timestr,
                                full.names = TRUE)
        #category directory
        raster.filepath <-
          file.path(directory, 'XCO2', gsub(' ', '.', categories[j,1]))
        if(!dir.exists(raster.filepath))
          dir.create(raster.filepath, recursive = TRUE)

        #raster file name
        raster.filename <-
          paste(gsub(' ', '.',categories[j,1]),
                gsub('X_foot.nc', 'XCO2.nc',
                     basename(footprint.df$EDGAR.filepath[i])),
                sep = '_')
        
        #save the raster!
        writeRaster(category.XCO2,
                    filename = file.path(raster.filepath,
                                         raster.filename),
                    overwrite = TRUE)
        remove('category.XCO2')
      }#close the category loop
    } #closes EDGAR conditional statement
    remove('edgar.footprint'); remove('EDGAR.df')
    Sys.time()
    #####################################################
    #####################################################
    
    
    
    #####################################################  
    ### Convolve ODIAC (if available) with footprints ###
    #####################################################
    if(exists('ODIAC.df')) {
      
      #grab the 0.1 x 0.1
      odiac.footprint <- stack(footprint.df$ODIAC.filepath[i])
      
      #integrate the footprint
      #can't use raster package due to the generation of tmp files
      steps <- names(odiac.footprint)
      for(j in 1:length(steps)) {
        #grab each layer as a tmp.layer
        eval(parse(text = paste0('tmp.layer <- odiac.footprint$',
                                 steps[j])))
        #add the layers together
        if(j == 1)
          total.odiac.footprint <- tmp.layer
        if(j > 1)
          total.odiac.footprint <- total.odiac.footprint + tmp.layer
      }; remove('tmp.layer') #save RAM
      
      #determine XCO2 for a static inventory emissions
      ODIAC <- get.odiac(tiff.path = odiac.inventory,
                         nc.extent = extent(odiac.footprint),
                         YYYYMM = substr(timestr, 1, 6),
                         convert.units = TRUE)
      static.XCO2 <- total.odiac.footprint*ODIAC
      
      static.XCO2_sum <- cellStats(static.XCO2, sum)
      remove('static.XCO2'); remove('ODIAC') #save RAM
      
      #save the footprint as a dataframe
      total.odiac.footprint_df <-
        raster::as.data.frame(total.odiac.footprint, xy = TRUE)
      total.odiac.footprint_df <- subset(total.odiac.footprint_df,
                                         layer > 0)
      remove('total.odiac.footprint') #save RAM
      
      #determine background influence
      is.polygon <- in.polygon(total.odiac.footprint_df[,1:2],
                               polygon = urban.polygon[,1:2])
      #record value
      bkg.influence <-
        nrow(subset(is.polygon, polygon == 'in'))/nrow(is.polygon)
      
      ### Add Static Inventory Emissions Scenario to Results ###
      #put these results together
      ODIAC_add.line <- data.frame(footprint.df$date.time[i],
                                   footprint.df$lon[i],
                                   footprint.df$lat[i],
                                   bkg.influence,
                                   'ODIAC',
                                   static.XCO2_sum,
                                   'ODIAC (Static)')
      names(ODIAC_add.line) <- names(output.results)
      
      #rbind these results to the cumulative dataframe
      output.results <- rbind(output.results, ODIAC_add.line)
      
      #now, determine category values
      for(j in 1:nrow(categories)) {
        # Grab a category and loop through the sectors
        sectors <- unlist(str_split(categories[j,2], pattern = ' '))
        # Loop through each sector and convolve
        for(k in 1:length(sectors)) {

          #convolved edgar sector
          sector.XCO2 <-
            convolve.ODIAC.sector(footprint = odiac.footprint,
                                  lon.lat = lon.lat,
                                  sector.name = sectors[k],
                                  categories,
                                  edgar.inventory,
                                  odiac.inventory,
                                  temporal.downscaling)
          
          #add the sectoral emission to the category emission
          if(k == 1) category.XCO2 <- sector.XCO2
          if(k > 1) category.XCO2 <- category.XCO2 + sector.XCO2
          remove('sector.XCO2') #save RAM
        }#build category loop
        
        #rename and save the category convolution
        names(category.XCO2) <- categories[j,1]
        
        EDGAR_add.line <- data.frame(footprint.df$date.time[i],
                                     footprint.df$lon[i],
                                     footprint.df$lat[i],
                                     bkg.influence,
                                     'EDGAR',
                                     cellStats(category.XCO2, sum),
                                     categories[j,1])
        
        #X-STILT directory
        directory <- list.files(EDGAR.directory,
                                pattern = timestr,
                                full.names = TRUE)
        #category directory
        raster.filepath <-
          file.path(directory, 'XCO2', gsub(' ', '.', categories[j,1]))
        if(!dir.exists(raster.filepath))
          dir.create(raster.filepath, recursive = TRUE)
        
        #raster file name
        raster.filename <-
          paste(gsub(' ', '.',categories[j,1]),
                gsub('X_foot.nc', 'XCO2.nc',
                     basename(footprint.df$EDGAR.filepath[i])),
                sep = '_')
        
        #save the raster!
        writeRaster(category.XCO2,
                    filename = file.path(raster.filepath,
                                         raster.filename),
                    overwrite = TRUE)
        remove('category.XCO2')
      }#close the category loop
    } #closes EDGAR conditional statement
    #####################################################
    #####################################################
    
  
  } #closes footprint loop
  
  write.csv(row.names = FALSE)
  
} #closes function
