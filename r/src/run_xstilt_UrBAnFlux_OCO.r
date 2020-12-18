#' This script acts as a wrapper for the latest version of X-STILT.
#' run_xstilt_UrBAnFLux_OCO serves as a function and is called by the
#' top-level setup script.
#' 
#' Author: Dustin Roten 9/22/2020
#' 

run_xstilt_UrBAnFlux_OCO <- function(input.variables = NULL) {
  
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
  
  #' create a namelist for running X-STILT trajectories
  #' @author Dien Wu, 04/18/2018
  #'
  #' ---------------------------------------------------------------------------
  #' @flags
  #' A. run_trajec == T or run_foot == T: run trajectory or footprint, simulations
  #' A1. if run_hor_err == T or run_ver_err == T 
  #'     X-STILT generates trajec with perturbations from wind or PBL errors.
  #'         along with traj-level CO2 and its error statistics to info.rds files
  #' A2. if run_emiss_err == T 
  #'     X-STILT generates trajec and footprint with a resolution of 0.1deg, 
  #'     since the absolute emission error has res of 0.1deg. 
  #'
  #' B. run_trajec == F & run_foot == F & run_sim = T: requires trajec and foot
  #'    X-STILT models XCO2 enhancements based on FFCO2 emission from 1km ODIAC;
  #'                or XCO2 errors due to emission errors (run_emiss_err == T);
  #'                or XCO2 errors due to transport errors (run_hor_err == T).
  #'
  #' ---------------------------------------------------------------------------
  # latest update by DW on 11/06/2020
  # upgrade to use STILT-HYSPLIT and a brand new refactoring on tht X-STILT part 
  
  ## go to X-STILT dir and source functions and libraries
  homedir   <- input.variables$homedir
  xstilt_wd <- file.path(homedir, 'X-STILT_UrBAnFlux', 'X-STILT') 
  setwd(xstilt_wd); source('r/dependencies.r')
  
  # *** there is a current bug with the rslurm package, you may need to 
  # mannually pull the development version from github by doing:
  #devtools::install_github('SESYNC-ci/rslurm')               # DW, 11/6/2020
  
  # Please insert your API in the 'insert_ggAPI.csv' for use of ggplot and ggmap
  api.key <- readLines('insert_ggAPI.csv')
  if (api.key == '') stop('Missing googleAPI, insert your API in insert_ggAPI.csv\n')
  register_google(key = api.key)
  
  
  # ------------------------------- city params ------------------------------- #
  site <- input.variables$site
  
  # if you wanna mannually insert coordinates, set city.loc as a data frame with 
  #   lon/lat columns, e.g., city.loc = data.frame(lon = , lat = )
  lon.lat <-
    suppressWarnings(get.lon.lat(site = site, dlon = 1, dlat = 1.5, city.loc = NULL))
  
  
  # ----------------------------- OCO & ODIAC params --------------------------- #
  # as a default, all input data are stored under input.path
  input.path <- input.variables$input.path
  store.path <- input.variables$store.path
  oco.sensor <- input.variables$oco.sensor
  oco.ver    <- input.variables$oco.ver
  data.level <- input.variables$data.level
  oco.path   <- file.path(input.path, oco.sensor, data.level,
                          paste0('L2_Lite_FP_', oco.ver))
  sif.path   <- file.path(input.path, oco.sensor, data.level,
                          paste0('L2_Lite_SIF_', oco.ver))
  raob.path  <- file.path(input.path, 'RAOB', site)  # NOAA radiosonde
  odiac.ver  <- input.variables$odiac.vname         # ODIAC version
  odiac.path <- file.path(input.path, 'ODIAC', paste0('ODIAC', odiac.ver))  
  
  
  # -------------------------- get OCO2 overpass info -------------------------- #
  # whether to search for overpasses over urban region
  urbanTF    <- input.variables$urbanTF
  
  # urban box defined as city.lat +/- dlat, city.lon +/- dlon
  urban_dlon <- input.variables$dlon.urban     
  urban_dlat <- input.variables$dlat.urban    # dlat/dlon in degrees 
  plotTF     <- FALSE   # if TRUE, plot OCO XCO2 and SIF on maps and lat series
  
  if(is.na(timestr)) {
    oco.track  <- get.site.track(site, oco.sensor, oco.ver, oco.path, searchTF = F, 
                                 date.range = c('20140101', '20201231'), 
                                 thred.count.per.deg = 100, lon.lat, urbanTF, 
                                 urban_dlon, urban_dlat, 
                                 thred.count.per.deg.urban = 50, rmTF = F, 
                                 plotTF, store.path, sif.path)
    if(oco.sensor == 'OCO-3')
      oco.track <- subset(oco.track, sam.urban.count > 0)
    
    all.timestr <- oco.track$timestr; print(oco.track)
    
    message('No timestr selected. Choose an option above.')
    return()
  } else {timestr <- input.variables$timestr}

  cat(paste('Working on:', timestr, 'for city/region:', site, '...\n\n'))
  cat('Done with choosing cities & overpasses...\n')
  # ---------------------------------------------------------------------------- #
  
  
  
  # ---------------------------- Basis XSTILT flags ---------------------------- #
  # whether to generate trajec; T: may overwrite existing trajec
  run_trajec <- input.variables$run_trajec
  run_foot   <- input.variables$run_foot    # whether to generate footprint
  
  # whether to perform simulations for TROPOMI species, will only active if
  #   the TROPOMI overpass hour interval does not contain OCO overpass hour
  run_tropomi   <- F    # T: for TROPOMI species; F: for OCO
  run_hor_err   <- F    # T: set error parameters
  run_ver_err   <- F    # T: set error parameters
  run_emiss_err <- F    # T: get XCO2 error due to prior emiss err
  
  # whether to run XCO2.ff or error using existing foot; 
  # need to silent run_trajec and run_foot
  run_sim <- input.variables$run_sim
  if (run_trajec | run_foot) run_sim <- F   # when running trajec/foot, no sim allowed  
  
  # number of hours backward (-) or forward (+)
  nhrs <- input.variables$nhrs
  
  # output variable names in trajec.rds
  varstrajec <- c('time', 'indx', 'lati', 'long', 'zagl', 'zsfc', 'foot', 'samt',
                  'dmas', 'mlht', 'temz', 'pres', 'sigw', 'tlgr', 'dens', 'sphu')
  
  
  # ----------------------------- Receptor params ------------------------------ #
  # line source for column release according to HYSPLITv5, DW, 09/17/2020
  # all particles are roughly evenly distributed between minagl and maxagl
  minagl <- 0                     # min release height in meter AGL
  maxagl <- 3000                  # max release height in meter AGL
  numpar <- 3000                  # total number of particles between minagl and maxagl
  selTF  <- input.variables$selTF # whether to select OCO soundings for simulations, 
                                  # if FALSE, use all OCO soundings as receptors
  
  # for OCO soundings, what kind of data filtering do you need -
  # choose one and see get.recp.info() for more details
  data.filter <- c('QF', 0)           # will select data with QF = 0
  #data.filter <- NULL                # will NOT select any data
  #data.filter <- c('WL', 12)         # will select data with WF <= 12
  
  # params on how to select soundings, if selTF == TRUE
  # lat range in deg N for placing denser receptors, required for backward run
  peak.lat <- c(lon.lat$citylat - urban_dlat, lon.lat$citylat + urban_dlat)
  
  # number of points to aggregate within 1 degree lat range (background or enhanced)
  if (oco.sensor == 'OCO-2') { num.bg = 50; num.peak = 150 }    
  if (oco.sensor == 'OCO-3') { num.bg = 100; num.peak = 500 } 
  
  
  # -------------------------- ARL format meteo params ------------------------- #
  # see https://uataq.github.io/stilt/#/configuration?id=meteorological-data-input
  met       <- input.variables$met                    # choose met fields
  met_path  <- input.variables$met_path               # path of met fields
  n_met_min <- 1                                      # min number of files needed
  met_file_format <- input.variables$met_file_format  # met file name convention
  
  # OPTION for subseting met fields if met_subgrid_enable is on, 
  # useful for large met fields like GFS or HRRR
  met_subgrid_buffer <- 0.1   # Percent to extend footprint area for met subdomain
  met_subgrid_enable <- T    
  
  # if set, extracts the defined number of vertical levels from the meteorological 
  # data files to further accelerate simulations, default is NA
  met_subgrid_levels <- NA    
  
  
  # -------------------------- TROPOMI & CTM params ---------------------------- #
  # if tropomi.speci includes NA, no runs related to TROPOMI will be generated
  tropomi_speci <- c(NA, 'CO', 'NO2', 'CH4')[1]   
  tropomi_path  <- file.path(input.path, 'TROPOMI', paste0('L2_', tropomi_speci))
  tropomi_hir_path <- paste0(tropomi_path, '_HiR')    # higher res TROPOMI
  
  # info for chemical transport model, needed for NO2 modeling
  ctm_indx <- 1
  if ('NA' %in% tropomi_speci) ctm_indx = 1
  ctm_name <- c(NA, 'cams', 'waccm', 'tcr')[ctm_indx]
  ctm_file_format <- c(NA, '%Y%m', '%Y-%m-%d', '%Y')[ctm_indx]
  ctm_path <- file.path(input.path, toupper(ctm_name))
  cat('Done with basis flags and params for receptor + meteo + TROPOMI & CTM...\n')
  # ---------------------------------------------------------------------------- #
  
  
  
  # -------------------------- Transport error params -------------------------- #
  # if run_hor_err = T, require ODIAC and CT fluxes and mole fractions
  # to calculate horizontal transport error of total CO2, DW, 07/28/2018
  if (run_hor_err) {
    ct.ver      <- ifelse(substr(timestr, 1, 4) >= '2016', 'v2017', 'v2016-1')
    ct.path     <- file.path(input.path, 'CT-NRT', ct.ver)
    ctflux.path <- file.path(ct.path, 'fluxes/optimized')
    ctmole.path <- file.path(ct.path, 'molefractions/co2_total')
  } else ct.ver <- ctflux.path <- ctmole.path <- NA       # end if run_hor_err
  
  # if run_ver_err = T, calculate vertical transport error
  # if run_ver_err = F, set zisf = 1 (default)
  zisf <- c(0.6, 0.8, 1.0, 1.2, 1.4)[3]; if (!run_ver_err) zisf <- 1.0
  
  # if run_emiss_err = T, calculating XCO2 error due to emiss error, 
  # need EDGAR and FFDAS files to compute emission spread, DW, 10/21/2018
  if (run_emiss_err) { 
    edgar.file <- file.path(input.path, 'EDGAR/v42_CO2_2008_TOT.0.1x0.1.nc')
    ffdas.path <- file.path(input.path, 'FFDAS')
    ffdas.file <- list.files(ffdas.path, 'totals')
    ffdas.file <- file.path(ffdas.path, ffdas.file[grep('2008', ffdas.file)])
  } else edgar.file <- ffdas.file <- NA                   # end if run_emiss_err
  
  
  # ----------------------------- Footprint params ---------------------------- #
  foot_res  <- input.variables$foot.res  # footprint resolution in degree
  foot_nhrs <- nhrs   # if foot_nhrs < nhrs, subset trajec and then calc footprint
  
  # footprint spatial domain defined as city.lat +/- foot_dlat and 
  # city.lon +/- foot_dlon in degrees, 10 here meaning 20 x 20deg box around city
  foot_dlat <- 10     
  foot_dlon <- 10 
  
  # (optinal) generate footprint with different resolutions other than "foot_res"
  # foot_res2 can be a vector, foot filename will contain res info, DW, 02/11/2019
  # if no need to generate second sets of footprints, set it to NA     
  foot_res2 <- c(NA, 1/10, 1/20, 1)[1]     
  if (run_emiss_err) foot_res2 <- 1/10 # for emiss err, need to generate 0.1deg foot
  
  
  # whether weight footprint by AK and PW for column simulations
  ak.wgt  <- input.variables$ak.wgt   # if FALSE, AK will be set as 1 for regional sims
  pwf.wgt <- input.variables$pwf.wgt
  
  # other neccesary footprint params using STILTv2 (Fasoli et al., 2018)
  # whether turn on hyper near-field (HNP) for mising hgts
  hnf_plume      <- input.variables$hnf_plume
  smooth_factor  <- 1     # Gaussian smooth factor, 0 to disable
  
  # whether to integrate footprint along time; F, hourly foot
  time_integrate <- input.variables$time_integrate
  projection     <- input.variables$projection
  cat('Done with params for error analysis and footprint setup...\n')
  # ---------------------------------------------------------------------------- #
  
  
  
  # ----------------------------- SLURM params -------------------------------- #
  # avoid using < 10 cores per node when running trans error stat (run_hor_err) 
  # along with calculating 2D foot together (run_foot), *** memory limits
  n_nodes   = input.variables$n_nodes
  n_cores   = input.variables$n_cores
  timeout   = input.variables$timeout   # time allowed before forced terminations (sec)
  job.time  = input.variables$job.time  # total job time
  slurm     = T                         # if TRUE, for SLURM parallel computing
  slurm_account   = input.variables$account
  slurm_partition = input.variables$partition
  
  
  # *** IF YOU EVER RAN INTO OOM-KILL ERROR, YOU CAN ENLARGE THE MAX MEM HERE *** 
  # The ammount of memory per node you need in MB, extending to 10 GB per core
  # given 180+ GB max memory on each Caltech cluster with 32 cores
  # **** PLEASE adjust mem_per_* based on your machine
  mem_per_core = 10                                 # max memory per core in GB
  mem_per_node = n_cores * mem_per_core * 1024      # max mem per node now in MB 
  
  
  # namelist required for generating trajec
  namelist = list(ak.wgt = ak.wgt, ct.ver = ct.ver, ctflux.path = ctflux.path, 
                  ctmole.path = ctmole.path, ctm_name = ctm_name,
                  ctm_path = ctm_path, ctm_file_format = ctm_file_format, 
                  data.filter = list(data.filter), edgar.file = edgar.file, 
                  ffdas.file = ffdas.file, foot_res = foot_res, 
                  foot_res2 = list(foot_res2), foot_nhrs = foot_nhrs, 
                  foot_dlat = foot_dlat, foot_dlon = foot_dlon, 
                  hnf_plume = hnf_plume, job.time = job.time, 
                  lon.lat = list(lon.lat), mem_per_node = mem_per_node, met = met, 
                  met_file_format = met_file_format, met_path = met_path, 
                  met_subgrid_buffer = met_subgrid_buffer, 
                  met_subgrid_enable = met_subgrid_enable, 
                  met_subgrid_levels = met_subgrid_levels, 
                  minagl = minagl, maxagl = maxagl, nhrs = nhrs, n_cores = n_cores, 
                  n_met_min = n_met_min, n_nodes = n_nodes, num.bg = num.bg, 
                  num.peak = num.peak, numpar = numpar, oco.path = oco.path, 
                  oco.sensor = oco.sensor, oco.ver = oco.ver, odiac.ver = odiac.ver, 
                  odiac.path = odiac.path, projection = projection, 
                  peak.lat = peak.lat, pwf.wgt = pwf.wgt, raob.path = raob.path, 
                  run_emiss_err = run_emiss_err, run_foot = run_foot, 
                  run_hor_err = run_hor_err, run_tropomi = run_tropomi, 
                  run_sim = run_sim, run_trajec = run_trajec, 
                  run_ver_err = run_ver_err, selTF = selTF, site = site, 
                  slurm = slurm, slurm_account = slurm_account, 
                  slurm_partition = slurm_partition, smooth_factor = smooth_factor,
                  store.path = store.path, time_integrate = time_integrate, 
                  timeout = timeout, timestr = timestr, 
                  tropomi_speci = list(tropomi_speci), 
                  tropomi_path = list(tropomi_path), 
                  tropomi_hir_path = list(tropomi_hir_path), 
                  varstrajec = varstrajec, xstilt_wd = xstilt_wd, zisf = zisf)      
  
  cat('Done with creating namelist...\n\n')
  config_xstilt.chpc(namelist)  # start X-STILT, either calc traj, foot or simulation
  # end of main script
  # ---------------------------------------------------------------------------- #

} # closes run_xstilt_UrBAnFlux_OCO function
