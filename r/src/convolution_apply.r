convolution_apply <- function(site = NULL, ftpt.dir = NULL, edgar.dir = NULL,
                              temp.dir = NULL, odiac.dir = NULL, carma.filepath = NULL,
                              work.dir = NULL, slurm_options = NULL,
                              edgar.monthly.profiles = T, receptors = FALSE) {
  
  output.directory <- list.files(ftpt.dir, pattern = gsub(' ', '', site),
                                 full.names = TRUE)
  
  #' Use the location string to obtain geographic information.
  #' This uses X-STILT's `get.lon.lat()` function.
  #' The values of `dlon` and `dlat` are irrelevant.
  lon.lat <- geocode(site)
  
  lapply(file.path(output.directory, 'XCO2'), 'dir.create',
         showWarnings = FALSE, recursive = TRUE)
  
  if(receptors == FALSE) {
    
    job.list <- data.frame(
      site = site,
      citylon = lon.lat$lon,
      citylat = lon.lat$lat,
      work.dir = getwd(),
      temp.dir = temp.dir,
      odiac.dir = odiac.dir,
      carma.file = carma.filepath,
      out.dir = file.path(output.directory, 'XCO2'),
      dir.list = output.directory
    )
    
    sjob <- rslurm::slurm_apply(f = convolve.FullFlux,
                                job.list,
                                jobname = 'Convolution',
                                pkgs = 'base',
                                nodes = 5,
                                cpus_per_node = 12,
                                slurm_options = slurm_options)
  }
  
  #site and file path
  footprint_df <- data.frame(
    site = site,
    citylon = lon.lat$lon,
    citylat = lon.lat$lat,
    filepath = footprint.filepaths,
    
    #add external file paths
    edgar.dir = edgar.dir,
    temp.dir = temp.dir,
    odiac.dir = odiac.dir,
    work.dir = work.dir,
    out.dir = file.path(output.directory, 'XCO2')
  )

}