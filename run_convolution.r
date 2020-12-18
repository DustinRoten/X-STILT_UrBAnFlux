#run_convolution

user.id <- 'u1211790'
homedir <- '/uufs/chpc.utah.edu/common/home/u1211790'
setwd(file.path(homedir, 'X-STILT_UrBAnFlux'))
source('r/dependencies.r')
options(stringsAsFactors = FALSE)

#register the api key for google maps to work
api.key <- read.csv('X-STILT/insert_ggAPI.csv', header = FALSE)
register_google(key = api.key)

site <- 'Los Angeles'
footprint.directory <- '/uufs/chpc.utah.edu/common/home/lin-group11/XCO2_Climatology'

##### Universal location for emission inventory information #####
input.path  <- '/uufs/chpc.utah.edu/common/home/lin-group7/group_data'

# Identify the location of EDGAR related inputs.
# (The EDGAR inventory and temporal downscaling files)
edgar.extension <- 'EDGARv5/2018'
temporal.downscaling <- 'ext/EDGAR_TemporalProfiles'

# Identify the location of CarMA related inputs.
# (Carbon Monitoring for Action power plant files)
carma.file <- 'ext/CARMA/Plant.csv' #file name

# Identify the location of ODIAC related inputs.
odiac.extension <- 'ODIAC/ODIAC2019/2018'

# Create paths to all relevant directories.
edgar.directory <- file.path(input.path, edgar.extension)
temporal.directory <- file.path(getwd(), temporal.downscaling)
odiac.directory <- file.path(input.path, odiac.extension)
carma.filepath <- file.path(getwd(), carma.file)

slurm_options <- list(time = '48:00:00',
                      account = 'lin-kp',
                      partition = 'lin-kp')

convolution_apply(site, footprint.directory, edgar.directory,
                  temporal.directory, odiac.directory, carma.filepath,
                  slurm_options, work.dir = getwd(), user.id)