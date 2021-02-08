#run_convolution.r
#match model output to convolved data
options(stringsAsFactors = FALSE)

#run_convolution
user.id <- 'u1211790'
homedir <- '/uufs/chpc.utah.edu/common/home/u1211790'
setwd(file.path(homedir, 'X-STILT_UrBAnFlux'))
source('r/dependencies.r'); library(Metrics)

#register the api key for google maps to work
api.key <- read.csv('X-STILT/insert_ggAPI.csv', header = FALSE)
register_google(key = api.key)

##### Universal location for emission inventory information #####
#relevant user input
site <- 'Los Angeles'

#find and list all of the available footprint directories
#these should include ODIAC, EDGAR (Optional), and SMUrF
footprint.directory <-
  '/uufs/chpc.utah.edu/common/home/lin-group11/XCO2_Climatology'
available.directories <- list.files(footprint.directory,
                                    full.names = TRUE)

lon.lat <- get.lon.lat(site, dlon = 1.5, dlat = 1.5)

#supply the common input path. All filepaths will be built from here
input.path  <- '/uufs/chpc.utah.edu/common/home/lin-group11/Roten_InputData'
output.path <- footprint.directory #can be changed as needed

#inventory extensions
edgar.extension <- 'EDGARv5/2018'
odiac.extension <- 'ODIAC/ODIAC2020/2019'
smurf.extension <- 'SMUrF'
carma.extension <- 'ext/CARMA/Plant.csv'
downscaling.extension <- 'ext/EDGAR_TemporalProfiles'

### Build the Filepaths Here ###
edgar.inventory <- file.path(input.path, edgar.extension)
odiac.inventory <- file.path(input.path, odiac.extension)
smurf.inventory <- file.path(input.path, smurf.extension)
carma.inventory <- file.path(getwd(), carma.extension)
temporal.downscaling <- file.path(getwd(), downscaling.extension)

#definition of the urban area
urban.domain <- list.files('ext/SAM_Domains',
                           pattern = gsub(' ', '', site),
                           full.names = TRUE)
sector.list <- list.files('ext',
                          pattern = 'defined_sectors',
                          full.names = TRUE)

slurm_options <- list(time = '48:00:00',
                      account = 'lin-kp',
                      partition = 'lin-kp')

#next, setup a dataframe for distributed computing
convolution_apply(user.id, homedir, site, lon.lat, urban.domain,
                  available.directories, slurm_options, sector.list,
                  edgar.inventory, odiac.inventory, smurf.inventory,
                  carma.inventory, temporal.downscaling, output.path)