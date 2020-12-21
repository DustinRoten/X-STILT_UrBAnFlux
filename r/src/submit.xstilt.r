submit.xstilt <- function(input.variables = NULL) {
  
  mode <- unique(input.variables$oco.sensor)
  if(length(mode) != 1) stop('Multiple/zero modes submitted')
  
  # Input variables for OCO-2 and OCO-3 modeling
  if(mode != 'Modeled' & mode != 'Interpolate') {

    # Prepare to loop through each submitted job.
    for(i in 1:nrow(input.variables)) {
      
      # Wait for other jobs to complete
      SLURM.wait(selected.partition = input.variables$partition[i],
                 user.id = input.variables$user.id)
      
      # Submit the job
      run_xstilt_UrBAnFlux_OCO(input.variables = input.variables[i,])
      message(paste0('Job ', i, ' of ', nrow(input.variables), ' submitted.'))
    }
    
    # Input variables for modeled XCO2 over custom domains.
  } else if(mode == 'Modeled' | mode == 'Interpolate') {
    
    if(mode == 'Modeled') {
    
      for(i in 1:nrow(input.variables)) {
        
        # Wait for other jobs to complete
        SLURM.wait(selected.partition = input.variables$partition[i],
                   user.id = input.variables$user.id[i])

        # Submit the job (interpolation)
        run_xstilt_UrBAnFlux_modeled(input.variables = input.variables[i,])
        message(paste0('Custom domain job ', i, ' of ', nrow(input.variables), ' submitted.'))
        Sys.sleep(60)
        
        #' To save space, remove the unwanted files from the previous directory
        if(i > 1) {purge.directories(directory = input.variables$store.path,
                                     iteration = (i-1))}
      }
      
      ##### 
      # One final check after the runs are complete. Unused files are removed.
      # Wait for other jobs to complete
      SLURM.wait(selected.partition = input.variables$partition[i],
                 user.id = input.variables$user.id[i])
      purge.directories(directory = input.variables$store.path, iteration = 0)
      
    } else if(mode == 'Interpolate') {
    
      for(i in 1:nrow(input.variables)) {
        
        # Wait for other jobs to complete
        SLURM.wait(selected.partition = input.variables$partition[i],
                   user.id = input.variables$user.id[i])
        
        interpolate_UrBAnFlux(input.variables = input.variables[i,])
        message(paste0('Interpolation job ', i, ' of ', nrow(input.variables), ' submitted.'))
        
      }
    }
    
  }; message('Submissions complete!')
}