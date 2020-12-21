### Source R scripts for Dien's X-STILT, 05/23/2018, DW
rsc <- dir('r/src', pattern = '.*\\.r$',
           full.names = T, recursive = T)

invisible(lapply(rsc, source))

# Other relevant packages
library(geosphere); library(raster); library(dplyr)
library(ncdf4); library(lubridate); library(stringr)
library(ggmap); library(lutz); library(reshape2)

# required STILT functions
source('X-STILT/stilt/r/src/write_footprint.r')

# required X-STILT functions
source('X-STILT/r/src/get.lon.lat.r')
