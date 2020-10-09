submit.xstilt <- function(input.variables = NULL, user.id = NULL) {
  
  # Input variables for OCO-2 and OCO-3 modeling
  if(input.variables$oco.sensor != 'Modeled') {

    # Prepare to loop through each submitted job.
    for(i in 1:nrow(input.variables)) {
      
      #' monitor parallel jobs. A new job will not be submitted until the
      #' previous one is completed.
      user.jobs <- subset(SLURM.jobs(partition = 'lin-kp'), USER == user.id)
      msg.flag <- 0 # msg.flag == 0 will display a message when jobs are running.
      
      # Hang out while previous jobs are running.
      while(nrow(user.jobs) > 1) {
        if(msg.flag == 0) message('User has jobs in progress. Please wait.')
        Sys.sleep(300); msg.flag <- 1
        user.jobs <- subset(SLURM.jobs(partition = 'lin-kp'), USER == user.id)
      }; msg.flag <- 0
      
      # Submit the job
      run_xstilt_UrBAnFlux_1(input.variables = input.variables[i,])
      message(paste0('Job ', i, ' of ', nrow(input.variables), ' submitted.'))
    }
    
    # Input variables for modeled XCO2 over custom domains.
  } else if(oco.sensor == 'Modeled') {
    
    for(i in 1:nrow(input.variables)) {
      
      #' monitor parallel jobs. A new job will not be submitted until the
      #' previous one is completed.
      user.jobs <- subset(SLURM.jobs(partition = 'lin-kp'), USER == user.id)
      msg.flag <- 0 # msg.flag == 0 will display a message when jobs are running.
      
      # Hang out while previous jobs are running.
      while(nrow(user.jobs) > 1) {
        if(msg.flag == 0) message('User has jobs in progress. Please wait.')
        Sys.sleep(300); msg.flag <- 1
        user.jobs <- subset(SLURM.jobs(partition = 'lin-kp'), USER == user.id)
      }; msg.flag <- 0
      
      # Submit the job (interpolation)
      run_xstilt_UrBAnFlux_2(input.variables = input.variables[i,])
      message(paste0('Custom domain job ', i, ' of ', nrow(input.variables), ' submitted.'))
    }
    
    # Start interpolation scheme here
    #for(i in 1:nrow(input.variables)) {
    # interpolate_UrBAnFlux()  
    #}
    
  }
}