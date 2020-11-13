### Source R scripts for Dien's X-STILT, 05/23/2018, DW
rsc <- dir('r', pattern = '.*\\.r$',
           full.names = T, recursive = T)
rsc <- subset(rsc, rsc != 'r/dependencies.r')

invisible(lapply(rsc, source))

# Other relevant packages
library(geosphere); library(raster); library(dplyr)
library(ncdf4)

# required STILT functions
source('X-STILT/stilt_hysplit/r/src/write_footprint.r')