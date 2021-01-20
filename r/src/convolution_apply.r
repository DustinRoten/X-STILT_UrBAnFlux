convolution_apply <- function(site = NULL, footprint.directory = NULL, edgar.directory = NULL,
                              temporal.directory = NULL,odiac.directory = NULL, carma.filepath = NULL,
                              work.dir = NULL, slurm_options = NULL, user.id = NULL) {
  
  output.directory <- list.files(footprint.directory, pattern = gsub(' ', '', site),
                                 full.names = TRUE)
  
  # Create output XCO2 directories
  lapply(file.path(output.directory, 'XCO2'), 'dir.create',
         showWarnings = FALSE, recursive = TRUE)
  
  #' Use the location string to obtain geographic information.
  lon.lat <- geocode(site)
  time.zone <-
    suppressWarnings(tz_lookup_coords(lat = lon.lat$lat, lon = lon.lat$lon))
  
  for(i in 1:length(output.directory)) {
    
    footprint.list <- list.files(file.path(output.directory[i], 'footprints'),
                                 full.names = TRUE)
  
    job.list <- data.frame(
      site = site,
      local.tz = time.zone,
      citylon = lon.lat$lon,
      citylat = lon.lat$lat,
      work.dir = getwd(),
      temp.dir = temporal.directory,
      odiac.dir = odiac.directory,
      edgar.dir = edgar.directory,
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
    
    notify(user.email = '3366202544@cwwsms.com',
           subject.text = 'CHCP - Kingspeak Jobs',
           body.text = paste0('Job ', i, ' of ', length(output.directory),
                              ' submitted.'))
    
  }
  
}