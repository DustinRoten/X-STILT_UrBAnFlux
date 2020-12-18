convolution_apply <- function(site = NULL, ftpt.dir = NULL, edgar.dir = NULL,
                              temp.dir = NULL, odiac.dir = NULL, carma.filepath = NULL,
                              work.dir = NULL, slurm_options = NULL, user.id = NULL) {
  
  output.directory <- list.files(ftpt.dir, pattern = gsub(' ', '', site),
                                 full.names = TRUE)
  
  # Create output XCO2 directories
  lapply(file.path(output.directory, 'XCO2'), 'dir.create',
         showWarnings = FALSE, recursive = TRUE)
  
  #' Use the location string to obtain geographic information.
  lon.lat <- geocode(site)
  time.zone <- tz_lookup_coords(lat = lon.lat$lat, lon = lon.lat$lon)
  
  for(i in 1:length(output.directory)) {
    
    footprint.list <- list.files(file.path(output.directory[i], 'footprints'),
                                 full.names = TRUE)
  
    job.list <- data.frame(
      site = site,
      local.tz = time.zone,
      citylon = lon.lat$lon,
      citylat = lon.lat$lat,
      work.dir = getwd(),
      temp.dir = temp.dir,
      odiac.dir = odiac.dir,
      edgar.dir = edgar.dir,
      carma.file = carma.filepath,
      out.dir = file.path(output.directory[i], 'XCO2'),
      footprint = footprint.list
    )
    
    SLURM.wait(selected.partition = slurm_options$partition,
               user.id = user.id)
    
    sjob <- rslurm::slurm_apply(f = convolve.FullFlux,
                                job.list,
                                jobname = 'Convolution',
                                pkgs = 'base',
                                nodes = 6,
                                cpus_per_node = 10,
                                slurm_options = slurm_options)
    
    message(paste0('Job ', i, ' of ', length(output.directory),
                   ' submitted.'))
    
    stop('Test Loop')
  }
  
}