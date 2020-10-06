submit.xstilt <- function(input.variables = NULL) {
  
  # Input variables for OCO-2 and OCO-3 modeling
  if(input.variables$oco.sensor != 'Modeled') {

    for(i in 1:nrow(input.variables)) {
      run_xstilt_UrBAnFlux_1(input.variables)
      
      # monitor parallel jobs
      SLURM.jobs(partition = 'lin-kp')
    }
    
    # Input variables for modeled XCO2 over custom domains.
  } else if(oco.sensor == 'Modeled') {
    
    
    for(i in 1:nrow(input.variables)) {
      run_xstilt_UrBAnFlux_2(input.variables)
      
      # monitor parallel jobs
      SLURM.jobs(partition = 'lin-kp')
    }
  }
}