#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   CRAM GLM CALIBRATION                                *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    14Jan2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: RUN GLM-AED for the NEON lake sites and calibrate    *
#*****************************************************************

rm(list = ls()) #let's clean up that workspace!

#remotes::install_github("CareyLabVT/glmtools")

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, reshape2, devtools, patchwork, zoo, ncdf4, glmtools, GLM3r, adagio) # --> Load the packages

setwd("C:/Users/Owner/Desktop/NEON-GLM/GLM_CRAM") # --> set the Directory to Barco Lake

source('C:/Users/Owner/Desktop/NEON-GLM/scripts/functions/calib_helpers.R') # --> source the helper functions
source('C:/Users/Owner/Desktop/NEON-GLM/scripts/functions/calibrate_sim.R') # --> source the helper functions
source('C:/Users/Owner/Desktop/NEON-GLM/scripts/functions/get_calib_init_validation.R') # --> source the helper functions
source('C:/Users/Owner/Desktop/NEON-GLM/scripts/functions/get_calib_periods.R') # --> source the helper functions
source('C:/Users/Owner/Desktop/NEON-GLM/scripts/functions/get_calib_setup.R') # --> source the helper functions

glmcmd = 'C:/Users/Owner/Desktop/NEON-GLM/GLM_CRAM/glm' # --> Set up the GLM Command for your system
glm_template = 'glm3-template.nml' 
sim_folder <- getwd()                                   # --> Set the simulation folder
out_file <- file.path(sim_folder, "output","output.nc") # --> Set the glm aed output location
file.copy(glm_template, 'glm3.nml', overwrite = TRUE)
nml_file <- file.path(sim_folder, 'glm3.nml')

field_temp<-file.path('C:/Users/Owner/Desktop/NEON-GLM/observations/CleanedObsTempCRAM.csv')
field_volume <- "C:/Users/Owner/Desktop/NEON-GLM/observations/volume_crampton.csv"

# read example configuration into memory
eg_nml <- read_nml(nml_file = file.path(sim_folder,'glm3.nml'))

glm_version()
# Run GLMr
GLM3r::run_glm(sim_folder, verbose = T)

var = 'temp'         # variable to which we apply the calibration procedure
path = getwd()       # simulation path/folder
nml_file = nml_file  # path of the nml configuration file that you want to calibrate on
glm_file = nml_file # # path of the gml configuration file
# which parameter do you want to calibrate? a sensitivity analysis helps
calib_setup <- data.frame('pars' = as.character(c('wind_factor','lw_factor','ch',
                                                  'coef_mix_hyp','Kw')),
                          'lb' = c(0.7,0.7,5e-4,0.6,0.1),
                          'ub' = c(2,2,0.004,0.4,0.8),
                          'x0' = c(1,1,0.0013,0.5,0.3))
print(calib_setup)
glmcmd = NULL        # command to be used, default applies the GLM3r function
# glmcmd = '/Users/robertladwig/Documents/AquaticEcoDynamics_gfort/GLM/glm'        # custom path to executable
# Optional variables
first.attempt = TRUE # if TRUE, deletes all local csv-files that stores the 
#outcome of previous calibration runs
period = get_calib_periods(nml_file, ratio = 1000) # define a period for the calibration, 
# this supports a split-sample calibration (e.g. calibration and validation period)
# the ratio value is the ratio of calibration period to validation period
print(period)
scaling = TRUE       # scaling of the variables in a space of [0,10]; TRUE for CMA-ES
verbose = TRUE
method = 'CMA-ES'    # optimization method, choose either `CMA-ES` or `Nelder-Mead`
metric = 'RMSE'      # objective function to be minimized, here the root-mean square error
target.fit = 1     # refers to a target fit of 2.0 degrees Celsius (stops when RMSE is below that)
target.iter = 200    # refers to a maximum run of 20 calibration iterations (stops after that many runs)
plotting = TRUE      # if TRUE, script will automatically save the contour plots
output = out_file    # path of the output file
field_file = field_temp # path of the field data
conversion.factor = 1 # conversion factor for the output, e.g. 1 for water temp.

calibrate_sim(var = 'temp', path = getwd(), 
              field_file = field_file, 
              nml_file = nml_file,
              calib_setup = calib_setup, 
              glmcmd = NULL, first.attempt = TRUE, 
              period = period, 
              scaling = TRUE, method = 'CMA-ES', metric = 'RMSE', 
              target.fit = 1, target.iter = 200, 
              plotting = TRUE, 
              output = output, 
              verbose = TRUE)


temp_rmse <- compare_to_field(nc_file = out_file, 
                              field_file = field_temp,
                              metric = 'water.temperature', 
                              as_value = FALSE, 
                              precision= 'hours')

print(paste('Total time period (calibrated):',round(temp_rmse,2),'deg C RMSE'))
plot_temp(out_file)


ice <- get_ice(out_file)
plot(ice)

volume <- get_var(var = 'Tot_V', file = out_file)
plot(volume)


glm_template = 'glm3-template.nml' 
file.copy(glm_template, 'glm3.nml', overwrite = TRUE)
nml_file <- file.path(sim_folder, 'glm3.nml')

var = 'Tot_V'         # variable to which we apply the calibration procedure
var_name = 'Tot_V'
path = getwd()       # simulation path/folder
nml_file = nml_file  # path of the nml configuration file that you want to calibrate on
glm_file = nml_file # # path of the gml configuration file
# which parameter do you want to calibrate? a sensitivity analysis helps
calib_setup <- data.frame('pars' = as.character(c('wind_factor','lw_factor','ch',
                                                  'coef_mix_hyp','Kw','rain_factor','rain_threshold','rh_factor')),
                          
                          'lb' = c(0.7,0.7,5e-4,0.6,0.1,0.5,0.001,0.8),
                          'ub' = c(2,2,0.004,0.4,0.8,1.5,0.1,1.2),
                          'x0' = c(1,1,0.0013,0.5,0.3,1,0.01,1))
print(calib_setup)
glmcmd = NULL # command to be used, default applies the GLM3r function
first.attempt = TRUE
period = get_calib_periods(nml_file, ratio = 10000)


print(period)
scaling = TRUE       # scaling of the variables in a space of [0,10]; TRUE for CMA-ES
verbose = TRUE
method = 'CMA-ES'    # optimization method, choose either `CMA-ES` or `Nelder-Mead`
metric = 'RMSE'      # objective function to be minimized, here the root-mean square error
target.fit = 120000    # refers to a target fit of 1000 cubic meters 
target.iter = 150    
plotting = T      # if TRUE, script will automatically save the contour plots
output = out_file    # path of the output file
field_file = field_volume # path of the field data
conversion.factor = 1 # conversion factor for the output, e.g. 1 for water temp.

calibrate_sim(var = 'Tot_V', path = getwd(), 
              field_file = field_volume, 
              nml_file = nml_file, 
              calib_setup = calib_setup, 
              glmcmd = NULL, first.attempt = TRUE, 
              period = period, 
              scaling = TRUE, method = 'CMA-ES', metric = 'RMSE', 
              target.fit = 12000, target.iter = 150, 
              plotting = T, 
              output = output, 
              verbose = TRUE)