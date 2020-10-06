#' This script acts as a wrapper for the latest version of X-STILT.
#' run_xstilt_UrBAnFLux_1 serves as a function and is called by the
#' top-level setup script.
#' 
#' Author: Dustin Roten 9/22/2020
#' 

run_xstilt_UrBAnFlux_2 <- function(input.variables = NULL) {
  
  #' create a namelist for running X-STILT trajectories
  #' @author Dien Wu, 04/18/2018
  #' ---------------------------------------------------------------------------
  
  #' @flags: 
  #' A. run_trajec == T or run_foot == T: run trajectory or footprint, simulations
  #'
  #' A1. if run_hor_err == T or run_ver_err == T (set error parameters in STEP 4)
  #'     X-STILT generates trajec with perturbations from wind or PBL errors.
  #'         along with traj-level CO2 and its error statistics to info.rds files
  #'      
  #' A2. if run_emiss_err == T (set error paramters in STEP 4):
  #'     X-STILT generates trajec and footprint with a resolution of 0.1deg, 
  #'     since the absolute emission error has res of 0.1deg. 
  #'
  #' ---------------------------------------------------------------------------
  #' B. run_trajec == F & run_foot == F & run_sim = T:  
  #'    Simulations will be started in STEP 8 (requires trajec and foot ready)
  #'    X-STILT models XCO2 enhancements based on FFCO2 emission from 1km ODIAC;
  #'                or XCO2 errors due to emission errors (run_emiss_err == T);
  #'                or XCO2 errors due to transport errors (run_hor_err == T).
  #'
  #' ---------------------------------------------------------------------------
  #' @updates:
  #' now build upon Ben Fasoli's STILT-R version 2 codes, DW, 05/23/2018
  #' !!! need to clear up codes for forward run, based on Ben's parallel computing
  #' Add plotting scripts for footprint and XCO2 enhancements, DW, 06/28/2018
  #' Add horizontal transport error module with flag 'run_hor_err'
  #'     with additional subroutines in /src/oco2-xstilt/trans_err, DW, 07/23/2018
  #' Add vertical trans error module, with flag 'run_ver_err', add ziscale
  #'     when scaling PBL heights and break trans error part (step 4) into
  #'     horizontal wind error and vertical trans error (via PBL perturb), 
  #'     DW, 07/25/2018
  #' simplify main code and use 'get.uverr()' to get horizontal wind error stat,
  #'     DW, 08/31/2018
  #' add emission uncertainty, DW, 09/06/2018
  #' update code according to v9 changes, DW, 10/19/2018
  #' Optimize horizontal trans error using parallel computing, DW, 10/21/2018
  #'
  #' --------------- Reconstruct X-STILT codes, DW, BF, 01/25/2019 -------------
  #' remove STILTv1 (Lin et al., 2003) from X-STILT;
  #' separate its modification from STILT; 
  #' now STILTv2 (Fasoli et al., 2018) is a SUBMODULE of X-STILT;
  #' customize before_trajec and before_footprint functions to mutate `output`. 
  #'
  #' --------------- Changes to trans error, DW, 01/29/2019 --------------------
  #' move the section of estimating trajec-level CO2 from main script to 
  #'    before_footprint_xstilt.r. So, each core will work on calculating the 
  #'    required trans error statistics (since it takes time).  DW, 01/29/2019
  #' to simplify the main script, remove calculations of lat-int signals or err, 
  #'    DW, 01/29/2019 
  #' optimize the trans error codes to remove zero footprint during the
  #'    trajec-level CO2 calculations to reduce required memories, DW
  #' allows for generating footprint with multiple reslutions at a time, 
  #'    see `foot.res2` and `before_footprint_xstilt.r` for more info, DW, 02/11/2019
  #' update all scripts for OCO-3 sensors, DW, 06/29/2020
  
  #' ---------------------------------------------------------------------------
  #' migrate X-STILT to use the latest HYSPLITv5, DW, 09/17/2020
  #' add modules for dealing with TROPOMi data, DW, 09/17/2020
  
  
  # test STILT-HYSPLIT 
  #Rscript -e "uataq::stilt_init('X-STILT', branch = 'hysplit-merge')" 
  
  # Strings as factors is FALSE
  options(stringsAsFactors = FALSE)
  
  #### source all functions and load all libraries
  # CHANGE working directory ***
  homedir <- input.variables$homedir
  xstilt_wd <- file.path(homedir, 'X-STILT_UrBAnFlux', 'X-STILT') 
  setwd(xstilt_wd)   # move to working directory
  source('r/dependencies.r') # source all functions
  
  # Please insert your API in the 'insert_ggAPI.csv' for use of ggplot and ggmap
  api.key <- readLines('insert_ggAPI.csv')
  if (api.key == '') stop('Missing googleAPI, insert your API code in insert_ggAPI.csv\n')
  register_google(key = api.key)
  
  #------------------------------ STEP 1 --------------------------------------- #
  ### 1) input dlat, dlon to get spatial domain around city center
  #site <- 'Riyadh'   # choose a city
  site <- input.variables$site
  lon.lat <- get.lon.lat(site = site, dlon = 1, dlat = 1.5)
  
  ### 2) required paths for input datasets
  # e.g., OCO-2/3 XCO2, SIF, NOAA RAOB, ODAIC emission (1km tiff files)
  input.path  <- input.variables$input.path
  raob.path   <- file.path(input.path, 'RAOB', gsub(' ', '', site))  # NOAA radiosonde
  project     <- input.variables$project   # name your project
  
  ## path for storing output or anything related to trans err
  store.path <- input.variables$store.path
  err.path   <- file.path(store.path, 'wind_err')
  dir.create(store.path, showWarnings = F, recursive = T)
  dir.create(err.path, showWarnings = F, recursive = T)
  
  #------------------------------ STEP 2 --------------------------------------- #
  # T:rerun hymodelc, even if particle location object found
  # F:re-use previously calculated particle location object
  run_trajec <- input.variables$run_trajec     # whether to generate trajec; runs start in STEP 6
  run_foot   <- input.variables$run_foot     # whether to generate footprint; runs start STEP 6
  if (run_trajec) cat('Need to generate trajec...\n')
  if (run_foot)   cat('Need to generate footprint...\n\n')
  
  # if columnTF == F, no need to get ground height or 
  # perform vertical weighting on trajec-level footprint
  columnTF <- input.variables$columnTF       # column receptor (T) or fixed receptor
  
  # whether to perform XCO2 and its error simulations
  run_hor_err   <- input.variables$run_hor_err  # T: set error parameters in STEP 4
  run_ver_err   <- input.variables$run_ver_err  # T: set error parameters in STEP 4
  run_emiss_err <- input.variables$run_emiss_err  # T: get XCO2 error due to prior emiss err, see STEP 4 and 8
  run_sim       <- input.variables$run_sim  # T: do analysis with existing trajec/foot, see STEP 8
  delt <- input.variables$delt           # fixed timestep [min]; set = 0 for dynamic timestep
  nhrs <- input.variables$nhrs         # number of hours backward (-) or forward (+)
  
  # change to Ben's definitions,  see validate_varsiwant()
  varstrajec <- c('time', 'indx', 'lati', 'long', 'zagl', 'zsfc', 'foot', 'samt',
                  'dmas', 'mlht', 'temp', 'pres', 'sigw', 'tlgr', 'dens', 'sphu')
  
  # path for the ARL format of meteo fields
  # simulation_step() will find corresponding files
  met             <- input.variables$met         # choose met fields
  met_path        <- input.variables$met_path    # path of met fields
  met_file_format <- input.variables$met_file_format  # met file name convention
  n_met_min       <- input.variables$n_met_min     # min number of files needed
  
  # OPTION for subseting met fields if met_subgrid_enable is on, 
  # useful for large met fields like GFS or HRRR
  met_subgrid_buffer <- 0.1   # Percent to extend footprint area for met subdomain
  met_subgrid_enable <- T    
  
  # if set, extracts the defined number of vertical levels from the meteorological 
  # data files to further accelerate simulations, default is NA
  met_subgrid_levels <- NA    
  
  # path to grab or store trajec, foot and potential trans err stat DW, 07/31/2018
  # ourput directory for storing traj with default convention;
  # store traj with wind err in a separate directory if run_hor_err = T
 
  timestamp <- strftime(timestamp, format = '%Y%m%d', tz = 'UTC')
  outdir <- file.path(store.path,
                      paste('out', gsub(' ', '', site),
                            timestamp, met, oco.sensor, sep = '_'))
  if (run_hor_err) outdir <- gsub('out', 'outerr', outdir)
  cat('Done with basis settings...\n')
  
  
  #------------------------------ STEP 3 --------------------------------------- #
  # select receptors --
  ### 1) Set model receptors, AGLs and particel numbers ***
  # for backward fixed-level runs OR forward fixed-level runs
  # agl can be a vector, meaning releasing particles from several fixed level
  # but if trying to represent air column, use columnTF=T, see below
  
  # if release particles from fixed levels
  agl    <- c(10, 500, 2000, 3000)[4]        # in mAGL
  numpar <- 100       # par for each time window for forward box runs
  
  # line source for column release according to HYSPLITv5, DW, 09/17/2020
  # all particles will be randomly distributed vertically
  if (columnTF) {     # insert min, max heights for STILT levels, in METERS
    minagl <- 0
    maxagl <- 3000        
    agl    <- c(minagl, maxagl)
    numpar <- 3000        
  }  # end if columnTF
  
  
  ### 2) place denser receptors within lat range with high XCO2
  # whether to select receptors; or simulate all soundings
  selTF <- input.variables$selTF  
  if (selTF) {
    
    # lat range in deg N for placing denser receptors, required for backward run
    peak.lat <- c(lon.lat$citylat - dlat.urban, lon.lat$citylat + dlat.urban)
    
    # number of points to aggregate within 1deg over small/large enhancements,
    # i.e., over background/enhancements, binwidth will be 1deg/num
    num.bg   <- 100   # e.g., every # of pts in 1 deg background lat range
    num.peak <- 500   # e.g., every # of pts in 1 deg urban lat range
    
    # recp.indx: how to pick receptors from all screened soundings (QF = 0)
    recp.indx <- c(seq(lon.lat$minlat,  peak.lat[1],     1 / num.bg),
                   seq(peak.lat[1],     peak.lat[2],     1 / num.peak),
                   seq(peak.lat[1],     lon.lat$maxlat,  1 / num.bg))
  } else { recp.indx <- NULL }
  
  # whether to subset receptors when debugging; if no subset, insert NULL
  recp.num <- NULL     # can be a number for max num of receptors or a vector, 1:20
  find.lat <- NULL     # for debug or test, model one sounding
  
  ### 3) select column-receptor locations here
  recp.info <- generate.grid(top.left = input.variables$top.left,
                             top.right = input.variables$top.right,
                             width = input.variables$width, receptor.resolution = 0.02,
                             date.time = input.variables$timestamp, agl = agl,
                             interpolation.resolution = 2, output.path = outdir)
  recp.info <- subset(recp.info, type == 'xstilt')[,c(1:3,6)] #DELETE THE ROW RESTRICTION!
  nrecp <- nrow(recp.info)
  cat(paste('Done with receptor setup...total', nrecp, 'receptors..\n'))
  
  
  #------------------------------ STEP 4 --------------------------------------- #
  # Error analysis
  # 1) get horizontal transport error component if run_hor_err = T
  # path for outputting wind error stats
  hor.err <- get.uverr(run_hor_err, site, timestr, xstilt_wd, overwrite = F,
                       raob.path, raob.format = 'fsl', nhrs, met, met.path, 
                       met.format, lon.lat, agl, err.path)
  
  ## if run_hor_err = T, require ODIAC and CT fluxes and mole fractions
  # to calculate trans error of total CO2, DW, 07/28/2018
  if (run_hor_err) {
    ct.ver      <- ifelse(substr(timestr, 1, 4) >= '2016', 'v2017', 'v2016-1')
    ct.path     <- file.path(input.path, 'CT-NRT', ct.ver)
    ctflux.path <- file.path(ct.path, 'fluxes/optimized')
    ctmole.path <- file.path(ct.path, 'molefractions/co2_total')
  } else { ct.ver <- NA; ctflux.path <- NA; ctmole.path <- NA }
  
  
  # 2) get vertical transport error component if run_ver_err = T
  # set zisf = 1 if run_ver_err = F
  zisf <- c(0.6, 0.8, 1.0, 1.2, 1.4)[3]; if (!run_ver_err) zisf <- 1.0
  pbl.err <- get.zierr(run_ver_err, nhrs.zisf = 24, const.zisf = zisf)
  
  
  ### 4) if calculating XCO2 error due to emiss error, need EDGAR and FFDAS files
  # rearrange by DW, 10/21/2018
  if (run_emiss_err) { 
    edgar.file <- file.path(input.path, 'EDGAR/v42_CO2_2008_TOT.0.1x0.1.nc')
    ffdas.path <- file.path(input.path, 'FFDAS')
    ffdas.file <- list.files(ffdas.path, 'totals')
    ffdas.file <- file.path(ffdas.path, ffdas.file[grep('2008', ffdas.file)])
  } else { edgar.file = NA; ffdas.file = NA }  # end if run_emiss_err
  
  cat('Done with inserting parameters for error estimates...\n')
  
  
  #------------------------------ STEP 5 --------------------------------------- #
  #### Settings for generating footprint maps
  ### 1) SET spatial domains and resolution for calculating footprints
  foot.res <- input.variables$foot.res  # footprint resolution, 1km for ODIAC
  
  # allow to generate footprint with different resolutions other than "foot.res"
  # foot.res2 can be a vector, foot filename will contain res info, DW, 02/11/2019
  # if no need to generate second sets of footprints, set it to NA     
  foot.res2 <- c(NA, 1/10, 1/20, 1)[1]     
  if (run_emiss_err) foot.res2 <- 1/10 # for emiss err, need to generate 0.1deg foot
  
  #' set # of hours for calculating footprint
  #' if @param foot.nhrs < @param nhrs, XSTILT will subset trajec
  foot.nhrs <- nhrs   # foot.nhrs can be different from nhrs for back-trajec
  
  # these variables will determine resoluation and spatial domain of footprint
  # 20x20 degree domain around the city center
  # one can also customize the data.frame of `foot.info`
  foot.info <- list(xmn = round(lon.lat$citylon) - 10, 
                    xmx = round(lon.lat$citylon) + 10,
                    ymn = round(lon.lat$citylat) - 10, 
                    ymx = round(lon.lat$citylat) + 10,
                    xres  = foot.res,  yres  = foot.res, 
                    xres2 = foot.res2, yres2 = foot.res2, foot.nhrs = foot.nhrs)
  
  ### and prepare ODIAC based on footprint domain 
  if (run_hor_err) {
    foot.ext <- extent(foot.info$xmn, foot.info$xmx, foot.info$ymn, foot.info$ymx)
    emiss.file <- tif2nc.odiacv3(site, timestr, vname = odiac.vname, xstilt_wd, 
                                 foot.ext, tiff.path, gzTF = F)
  } else { emiss.file = NA }
  
  
  ### 2) whether weighted footprint by AK and PW for column simulations (X-STILT)
  # NA: no weighting performed for fixed receptor simulations
  ak.wgt  <- input.variables$ak.wgt
  pwf.wgt <- input.variables$pwf.wgt
  
  # whether to overwrite existing wgttraj.rds files; if F, read from existing wgttraj.rds
  overwrite_wgttraj <- input.variables$overwrite_wgttraj  
  
  ### 3) other footprint parameters using STILTv2 (Fasoli et al., 2018)
  hnf_plume      <- input.variables$hnf_plume  # whether turn on hyper near-field (HNP) for mising hgts
  smooth_factor  <- input.variables$smooth_factor  # Gaussian smooth factor, 0 to disable
  time_integrate <- input.variables$time_integrate  # whether integrate footprint along time, T, no hourly foot
  projection     <- input.variables$projection
  cat('Done with footprint setup...\n\n')
  
  
  #------------------------------ STEP 6 --------------------------------------- #
  ## if running trajec or footprint
  if (run_trajec | run_foot) {
    
    ## use SLURM for parallel simulation settings
    # avoid using < 10 cores per node when running trans error stat (run_hor_err) 
    # along with calculating 2D foot together (run_foot), *** memory limits
    n_nodes  <- input.variables$n_nodes
    n_cores  <- input.variables$n_cores
    
    # time allowed for running hymodelc before forced terminations
    timeout  <- input.variables$timeout  # in sec
    job.time <- input.variables$job.time    # total job time
    slurm    <- n_nodes > 1
    slurm_options <- list(time = job.time, account = 'lin-kp', partition = 'lin-kp')
    jobname <- paste0('XSTILT_', gsub(' ', '', site), '_', timestr)
    
    # create a namelist including all variables
    # namelist required for generating trajec
    namelist <- list(ak.wgt = ak.wgt, columnTF = columnTF, ct.ver = ct.ver, 
                     ctflux.path = ctflux.path, ctmole.path = ctmole.path, 
                     delt = delt, emiss.file = emiss.file, foot.info = foot.info, 
                     hnf_plume = hnf_plume, hor.err = hor.err, jobname = jobname,
                     met = met, met_file_format = met_file_format, n_met_min = n_met_min,
                     met_subgrid_buffer = met_subgrid_buffer, 
                     met_subgrid_enable = met_subgrid_enable,
                     met_subgrid_levels = met_subgrid_levels,
                     met_path = met_path, nhrs = nhrs, n_cores = n_cores,
                     n_nodes = n_nodes, numpar = numpar, outdir = outdir, 
                     oco.path = oco.path, overwrite_wgttraj = overwrite_wgttraj,
                     pbl.err = pbl.err, project = project, projection = projection,
                     pwf.wgt = pwf.wgt, recp.info = recp.info, run_foot = run_foot, 
                     run_hor_err = run_hor_err, run_trajec = run_trajec, 
                     slurm = slurm, slurm_options = slurm_options, 
                     smooth_factor = smooth_factor, time_integrate = time_integrate, 
                     timeout = timeout, tropomi.speci = list(tropomi.speci), 
                     tropomi.path = list(tropomi.path), varstrajec = varstrajec, 
                     xstilt_wd = xstilt_wd)        
    cat('Done with creating namelist...\n')
    
    # call run.xstilt() to start running trajec and foot
    run.xstilt(namelist)  # see more variables defined in run.xstilt()
    q('no')
  } # end if run trajec or foot
  
  
  #------------------------------ STEP 7 --------------------------------------- #
  ### calculate XCO2 concentration and its error (need trajec and footprint ready)
  if (!run_trajec & !run_foot & run_sim) {
    
    #------------------------  Horizontal trans error -------------------------- #
    ### simulate transport error in XCO2 due to met errors, DW, 07/25/2018
    # requires two sets of trajectories before running the following section:
    if (run_hor_err) { # this does not need footprint
      
      ## call function cal.trans.err() to estimate receptor-level trans err [ppm]
      # get actual ppm error, need to have error statistics ready
      # see cal.trajfoot.stat() in called before_footprint_xstilt.r for err stat
      cat('Start simulations of XCO2 error due to horizontal trans err...\n')
      receptor <- cal.trans.err(site, timestr, xstilt_wd, outdir, store.path, met)
      if (is.null(receptor)) stop('No results calculated, check cal.trans.err()\n')
      
    } else {
      
      #--------------------- XCO2 or emiss error simulations ---------------------- #
      # requires trajec and footprints ready for running things below, DW, 06/04/2018  
      # call func to match ODIAC emissions with xfoot & sum up to get 'dxco2.ff'
      cat('Start simulations of XCO2.ff or its error due to emiss err...\n')
      receptor <- run.xco2ff.sim(site, timestr, vname = odiac.vname, tiff.path, 
                                 outdir, foot.res, xstilt_wd, store.path, nhrs, 
                                 numpar, smooth_factor, zisf, oco.ver, met, 
                                 lon.lat, run_emiss_err, edgar.file, ffdas.file, 
                                 plotTF = F, writeTF = T)
      if (is.null(receptor)) stop('No results calculated, check run.xco2ff.sim()\n')
    } # end if run_hor_err
  } # end if run_sim
  
  
  if (!run_trajec & !run_foot & !run_sim) 
    cat('NO simulations or analysis will be performed, please check run_*..\n')
  ##### end of script

}
