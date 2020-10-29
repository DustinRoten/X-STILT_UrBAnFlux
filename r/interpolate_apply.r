interpolate_apply <- function(f, input.variables = NULL) {
  
  # Expand arguments to form a data frame where rows serve as iterations of FUN
  # using named columns as arguments to FUN. Base R's data.frame class expands
  # list arguments to multiple rows when constructing a new dataframe (not when
  # adding a new column) but tibble retains list classed objects
  
  slurm_options <- list(time = '48:00:00',
                        account = unique(input.variables$account),
                        partition = unique(input.variables$partition))
  
  if (nrow(input.variables) > 1) {
    # Confirm availability of sbatch executable and dispatch simulation
    # configurations to SLURM
    sbatch_avail <- system('which sbatch', intern = T)
    if (length(sbatch_avail) == 0 || nchar(sbatch_avail[1]) == 0)
      stop('Problem identifying sbatch executable for slurm...')
    
    message('Multi node parallelization using slurm. Dispatching jobs...')
    #load_libs('rslurm')
    library(rslurm)
    
    #Build a dataframe that only contains the relevant variables for interpolation.
    subset.input.variables <- data.frame(homedir = input.variables$homedir,
                                         site = input.variables$site,
                                         timestamp = input.variables$timestamp,
                                         store.path = input.variables$store.path,
                                         met = input.variables$met,
                                         oco.sensor = input.variables$oco.sensor,
                                         time_integrate = input.variables$time_integrate,
                                         receptor.resolution = input.variables$receptor.resolution)
    
    if(length(unique(input.variables$oco.sensor)) > 1) stop('Multiple modes submitted')
    sjob <- rslurm::slurm_apply(f,
                                subset.input.variables,
                                jobname = unique(input.variables$oco.sensor),
                                pkgs = 'base',
                                nodes = max(input.variables$n_nodes),
                                cpus_per_node = max(input.variables$n_cores),
                                slurm_options = slurm_options)
    return(invisible(sjob))
  } else {
    interpolate_UrBAnFlux(input.variables$homedir,
                          input.variables$site,
                          input.variables$timestamp,
                          input.variables$store.path,
                          input.variables$met,
                          input.variables$oco.sensor,
                          input.variables$time_integrate,
                          input.variables$receptor.resolution)
  }

}