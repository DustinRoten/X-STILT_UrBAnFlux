#convolution_apply
convolution_apply <- function(user.id, homedir, site, lon.lat,
                              urban.domain, available.directories,
                              slurm_options, sector.list, edgar.inventory,
                              odiac.inventory, smurf.inventory,
                              carma.inventory, temporal.downscaling,
                              output.path) {
  
  #check that the timestrs in each directory are consistent.
  for(i in 1:length(available.directories)) {
    timestr.list <- c(str_split_fixed(list.files(available.directories[i]),
                                      pattern = '_', n = 5)[,3])
    
    if(i > 1) {
      check <- all(timestr.list == timestr.list.previous)
    } else {check <- TRUE}
    if(!check) stop('Timestr mismatch in directories')
    timestr.list.previous <- timestr.list
  }
  
  timestr.list <- c(str_split_fixed(list.files(available.directories),
                                    pattern = '_', n = 5)[,3])
  timestr.list <- unique(timestr.list)
  
  #what directories are present?
  edgar.idx <-
    which(!is.na(str_match(available.directories, pattern = 'EDGAR')))
  if(length(edgar.idx) != 0) {
    EDGAR.directory <- available.directories[edgar.idx]
  } else {EDGAR.directory <- NA}
  
  odiac.idx <-
    which(!is.na(str_match(available.directories, pattern = 'ODIAC')))
  if(length(odiac.idx) != 0) {
    ODIAC.directory <- available.directories[odiac.idx]
  } else {ODIAC.directory <- NA}
  
  smurf.idx <-
    which(!is.na(str_match(available.directories, pattern = 'SMUrF')))
  if(length(smurf.idx) != 0) {
    SMUrF.directory <- available.directories[smurf.idx]
  } else {SMUrF.directory <- NA}
  
  
  for(i in 1:length(timestr))
  
  #construct the dataframe of SLURM jobs
  job.list <- data.frame(homedir, site, timestr = timestr.list,
                         lon.lat, urban.domain,
                         EDGAR.directory, ODIAC.directory,
                         SMUrF.directory, sector.list,
                         edgar.inventory, odiac.inventory,
                         smurf.inventory, carma.inventory,
                         temporal.downscaling, output.path)
  
  nodes <- ceiling(nrow(job.list)/8)
  
  sjob <- rslurm::slurm_apply(f = convolve.FullFlux,
                              job.list,
                              jobname = 'FullFlux',
                              pkgs = 'base',
                              nodes = nodes,
                              cpus_per_node = 12,
                              slurm_options = slurm_options)
  
}