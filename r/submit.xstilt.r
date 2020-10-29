submit.xstilt <- function(input.variables = NULL) {
  
  mode <- unique(input.variables$oco.sensor)
  if(length(mode) != 1) stop('Multiple modes submitted')
  
  # Input variables for OCO-2 and OCO-3 modeling
  if(mode != 'Modeled' & mode != 'Interpolate') {

    # Prepare to loop through each submitted job.
    for(i in 1:nrow(input.variables)) {
      
      #' monitor parallel jobs. A new job will not be submitted until the
      #' previous one is completed.
      user.jobs <- subset(SLURM.jobs(partition = input.variables$partition[i]),
                          USER == input.variables$user.id[i])
      msg.flag <- 0 # msg.flag == 0 will display a message when jobs are running.
      
      # Hang out while previous jobs are running.
      while(nrow(user.jobs) > 1) {
        if(msg.flag == 0) message('User has jobs in progress. Please wait.')
        Sys.sleep(300); msg.flag <- 1
        user.jobs <- subset(SLURM.jobs(partition = input.variables$partition[i]),
                            USER == input.variables$user.id[i])
      }; msg.flag <- 0
      
      # Submit the job
      run_xstilt_UrBAnFlux_1(input.variables = input.variables[i,])
      message(paste0('Job ', i, ' of ', nrow(input.variables), ' submitted.'))
    }
    
    # Input variables for modeled XCO2 over custom domains.
  } else if(mode == 'Modeled' | mode == 'Interpolate') {
    
    if(mode == 'Modeled') {
    
      for(i in 1:nrow(input.variables)) {
        #' monitor parallel jobs. A new job will not be submitted until the
        #' previous one is completed.
        user.jobs <- subset(SLURM.jobs(partition = input.variables$partition[i]),
                            USER == input.variables$user.id[i])
        msg.flag <- 0 # msg.flag == 0 will display a message when jobs are running.
        
        # Hang out while previous jobs are running.
        while(nrow(user.jobs) > 1 | i == 2) {
          if(msg.flag == 0) message('User has jobs in progress. Please wait.')
          Sys.sleep(300); msg.flag <- 1
          user.jobs <- subset(SLURM.jobs(partition = input.variables$partition[i]),
                              USER == input.variables$user.id[i])
        }; msg.flag <- 0
        
        # Submit the job (interpolation)
        run_xstilt_UrBAnFlux_2(input.variables = input.variables[i,])
        message(paste0('Custom domain job ', i, ' of ', nrow(input.variables), ' submitted.'))
        
        #' To save space, remove the unwanted files from the previous directory
        if(i > 1) {purge.directories(directory = input.variables$store.path,
                                     iteration = (i-1))}
      }
      
      # One final check after the runs are complete. Unused files are removed.
      purge.directories(directory = input.variables$store.path, iteration = 0)
      
    } #closes if statement for interpolation
    
    interpolate_apply(f = interpolate_UrBAnFlux, input.variables)
    
  }
}