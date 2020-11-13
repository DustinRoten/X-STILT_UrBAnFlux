interpolate_apply <- function(input.variables = NULL, slurm.submit = NULL) {
  
  # Expand arguments to form a data frame where rows serve as iterations of FUN
  # using named columns as arguments to FUN. Base R's data.frame class expands
  # list arguments to multiple rows when constructing a new dataframe (not when
  # adding a new column) but tibble retains list classed objects
  
  slurm_options <- list(time = '48:00:00',
                        account = unique(input.variables$account),
                        partition = unique(input.variables$partition))
  
  # Confirm availability of sbatch executable and dispatch simulation
  # configurations to SLURM
  sbatch_avail <- system('which sbatch', intern = T)
  if (length(sbatch_avail) == 0 || nchar(sbatch_avail[1]) == 0)
    stop('Problem identifying sbatch executable for slurm...')
  
  message('Multi node parallelization using slurm. Dispatching jobs...')
  #load_libs('rslurm')
  library(rslurm)
  
  if(input.variables$time_integrate == TRUE) {
    
    stop('Time-integrated interpolation not yet working.')
    
  } else if(input.variables$time_integrate == FALSE) {
    
    sjob <- rslurm::slurm_apply(f = interpolate.footprint_2,
                                slurm.submit,
                                jobname = 'Interpolating',
                                pkgs = 'base',
                                nodes = input.variables$n_nodes,
                                cpus_per_node = input.variables$n_cores,
                                slurm_options = slurm_options)
    return(invisible(sjob))
  }
}
