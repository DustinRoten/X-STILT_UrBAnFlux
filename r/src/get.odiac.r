# subroutine to readin tiff format of 1kmx1km ODIAC emissions
# and return *.nc format with selected emissions given a lat/lon domain
# This is a shameless knockoff of D. Wu's function tif2nc.odiacv3.r
# DW, update with ODIACv2017, 11/01/2017

get.odiac <- function(tiff.path, nc.extent, YYYYMM = NULL){

  # Some QA/QC
  if(!is.character(YYYYMM) & !is.null(YYYYMM))
    stop('YYYYMM must be a STRING!')
  
  # Load required libraries
  library(raster)
  
  ext <- extension(tiff.path)
  if(ext == '.tif') {
    
    # Pull the year and date from the custom ODIAC filename format
    string <- substr(tiff.path, (nchar(tiff.path)-7), (nchar(tiff.path)-4))
    
    # If YYYYMM is null, the default ODIAC filename format is assumed
    # If YYYYMM is NOT null, the provided format will be used.
    if(is.null(YYYYMM)) {
      YYYY <- as.numeric(paste0('20', substr(string, 1, 2)))
      MM <- as.numeric(substr(string, 3, 4))
    } else if(!is.null(YYYYMM)) {
      YYYY <- as.numeric(substr(YYYYMM, 1, 4))
      MM <- as.numeric(substr(YYYYMM, 5, 6))
    }
    
    # Check to see if YYYY is a leap year
    # Difference in second entry of each vectory (February)
    if((YYYY %% 400 == 0) | (YYYY %% 4 == 0)) {
      month.days <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[MM]
    } else {
      month.days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[MM]
    }
    
  } else {stop('Unsure of how to deal with provided ODIAC file format.')}
  
  # 21600 rows and 43200 columns
  emiss <- raster(tiff.path) # convert to raster

  # subset spatial domain
  sel.emiss <- crop(emiss, nc.extent)

  # Method 2 -- compute area using area() function in raster package
  area.raster <- raster::area(sel.emiss) * 1E6    # convert km2 to m2
  
  # convert the unit of CO2 emiss from Tonne Carbon/cell/month to umol/m2/s
  sel.emiss <- sel.emiss * 1E6 / 12 * 1E6 # convert tonne-C to uomol-C (= umole-CO2)
  sel.emiss <- sel.emiss / month.days / 24 / 60 / 60	# convert per month to per second
  sel.emiss <- sel.emiss / area.raster		# convert per cell to per m2
  # NOW sel.co2 has unit of umole-CO2/m2/s, can be used directly with footprint

  # finally, return raster
  return(sel.emiss)
}
