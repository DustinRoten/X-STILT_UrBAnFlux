purge.directories <- function(directory = NULL, iteration = NULL) {
  
  if(is.null(iteration)) stop('Provide an iteration (or zero) to purge function.')
  
  # List all of the out_* directories
  output.locations<- unique(list.files(directory, full.names = TRUE,
                                       pattern = 'out_'))
  
  if(iteration != 0) {
    # Pick one from the list. This is so i-1 can be used while new
    # files are being generated.
    receptor.directories <- list.files(file.path(output.locations[iteration],
                                                 'by-id'), full.names = TRUE)
  } else if(iteration == 0) {
    # Purge all files
    receptor.directories <- list.files(file.path(output.locations, 'by-id'),
                                       full.names = TRUE)
  }
  
  # List all of the files that aren't X_foot.nc files.
  remove.these <- grep(list.files(receptor.directories, full.names = TRUE),
                       pattern = '\\_X_foot.nc$', invert = TRUE, value = TRUE)
  file.remove(remove.these)
  
}