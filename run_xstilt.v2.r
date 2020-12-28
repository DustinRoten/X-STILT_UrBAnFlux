user.id <- 'u1211790'
homedir <- '/uufs/chpc.utah.edu/common/home/u1211790'
setwd(file.path(homedir, 'X-STILT_UrBAnFlux'))
source('r/dependencies.r')
options(stringsAsFactors = FALSE)

site <- 'Los Angeles'
input.path  <- '/uufs/chpc.utah.edu/common/home/lin-group11/Roten_InputData'
oco.sensor  <- c('OCO-2', 'OCO-3', 'Modeled', 'Interpolate')[2]
data.level <- c('L1', 'L2')[2]
oco.ver     <- c('b7rb', 'b8r', 'b9r', 'VEarlyR')[4] # retrieval algo ver
odiac.vname <- c('2016', '2017', '2018', '2019', '2020')[5] # ODIAC version
 
project <- oco.sensor   # name your project

store.path <- '/uufs/chpc.utah.edu/common/home/lin-group11/XCO2_Climatology'

### setting up the OCO sounding search grid
### These variables will be ignored if 'OCO-2/OCO-3' is not selected
urbanTF <- T; dlon.urban <- 0.75; dlat.urban <- 0.75
timestr <- c(
  NA
  # '2020022419',
  # '2020030316',
  # '2020040121',
  # '2020042320',
  # '2020050516',
  # '2020052700',
  # '2020053022',
  # '2020080420',
  # '2020080818',
  # '2020081217',
  # '2020081423',
  # '2020102021',
  # '2020102818'
)# If unsure, set as NA.

### setting up the custom X-STILT grid
### These variables will be ignored if 'Modeled' is not selected
top.left  <- list(-118.62, 34.5)  # Include the top-left corner of the custom grid
top.right <- list(-117.53, 33.94) # Include the top-right corner of the custom grid
width     <- 0.6              # Include the desired width of the custom grid

receptor.resolution <- 0.02 # Set the spacing of column-receptors on the custom grid (deg)
interpolation.resolution <- 2 # Set the size of interpolated subdomains (int >= 2)

#Input timestamps in local time
#Pacific time = 'America/Los_Angeles"
time.zone <- 'America/Los_Angeles'

#List the days and times here
timestamp <- c('20200603-070000',
               '20200603-100000',
               '20200603-130000',
               '20200603-160000',
               '20200603-190000')

#Convert the times to UTC format
time <- as.POSIXlt(timestamp, tz = time.zone, format = '%Y%m%d-%H%M%S')
time <- lubridate::with_tz(time, tzone = 'UTC')
timestamp <- strftime(time, format = '%Y%m%d-%H%M%S')
#####

# select what type of analysis is performed
run_trajec <- T     # whether to generate trajec; runs start in STEP 6
run_foot   <- T     # whether to generate footprint; runs start STEP 6

# if columnTF == F, no need to get ground height or 
# perform vertical weighting on trajec-level footprint
columnTF <- T       # column receptor (T) or fixed receptor

# whether to perform XCO2 and its error simulations
run_hor_err   <- F  # T: set error parameters in STEP 4
run_ver_err   <- F  # T: set error parameters in STEP 4
run_emiss_err <- F  # T: get XCO2 error due to prior emiss err, see STEP 4 and 8
run_sim       <- F  # T: do analysis with existing trajec/foot, see STEP 8
delt <- 2           # fixed timestep [min]; set = 0 for dynamic timestep
nhrs <- -18         # number of hours backward (-) or forward (+)

# path for the ARL format of meteo fields
# simulation_step() will find corresponding files
met             <- c('gdas0p5', 'gfs0p25', 'hrrr')[3]    # choose met fields
met_path        <- '/uufs/chpc.utah.edu/common/home/lin-group12/hrrr/hrrr' # path of met fields
met_file_format <- '%Y%m%d'                              # met file name convention
n_met_min       <- 1                                     # min number of files needed

selTF <- F

limit_recp <- T

foot.res <- 1/120

### 2) whether weighted footprint by AK and PW for column simulations (X-STILT)
# NA: no weighting performed for fixed receptor simulations
ak.wgt  <- if(oco.sensor == 'Modeled') {FALSE} else {TRUE}
pwf.wgt <- TRUE

# whether to overwrite existing wgttraj.rds files; if F, read from existing wgttraj.rds
overwrite_wgttraj <- T  

### 3) other footprint parameters using STILTv2 (Fasoli et al., 2018)
hnf_plume      <- T  # whether turn on hyper near-field (HNP) for mising hgts
smooth_factor  <- 1  # Gaussian smooth factor, 0 to disable
time_integrate <- F  # whether integrate footprint along time, T, no hourly foot
projection     <- '+proj=longlat'

## use SLURM for parallel simulation settings
# time allowed for running hymodelc before forced terminations
n_nodes  <- 4
n_cores  <- 8
timeout  <-  6 * 60 * 60  # in sec
job.time <- '06:00:00'    # total job time
account = 'lin-kp'
partition = 'lin-kp'

#########################################################################################
# Deal with timestr vs. timestamp
if(oco.sensor == 'Modeled' | oco.sensor == 'Interpolate') timestr <- NA
if(oco.sensor == 'OCO-2' | oco.sensor == 'OCO-3') timestamp <- NA

input.variables <- data.frame(user.id = user.id, account = account, partition = partition,
  homedir = homedir, site = site, input.path = input.path, oco.sensor = oco.sensor,
  data.level = data.level, oco.ver = oco.ver, odiac.vname = odiac.vname, project = oco.sensor,
  store.path = store.path, urbanTF = urbanTF, dlon.urban = dlon.urban, dlat.urban = dlat.urban,
  timestr = timestr, top.left = paste(as.character(top.left), collapse = ','),
  top.right = paste(as.character(top.right), collapse = ','), width = width,
  receptor.resolution = receptor.resolution, interpolation.resolution = interpolation.resolution,
  timestamp = timestamp, run_trajec = run_trajec, run_foot = run_foot, columnTF = columnTF,
  run_hor_err = run_hor_err, run_ver_err = run_ver_err, run_emiss_err = run_emiss_err,
  run_sim = run_sim, delt = delt, nhrs = nhrs, met = met, met_path = met_path,
  met_file_format = met_file_format, n_met_min = n_met_min, selTF = selTF, limit_recp = limit_recp,
  foot.res = foot.res, ak.wgt  = ak.wgt, pwf.wgt = pwf.wgt, overwrite_wgttraj = overwrite_wgttraj,
  hnf_plume = hnf_plume, smooth_factor = smooth_factor, time_integrate = time_integrate,
  projection = projection, n_nodes = n_nodes, n_cores = n_cores, timeout = timeout, job.time = job.time)

submit.xstilt(input.variables, user.email = '3366202544@cwwmms.com')
