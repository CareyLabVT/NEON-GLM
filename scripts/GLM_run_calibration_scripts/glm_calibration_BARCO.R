#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   GLM CALIBRATION FOR NEON LAKES                       *
#* AUTHOR:  Robert Ladwig and updated by Ryan McClure            *
#* DATE:    17Nov2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: RUN GLM-AED for the NEON lake sites and calibrate    *
#*****************************************************************
source('C:/Users/Owner/Desktop/NEON-GLM/scripts/GLM_run_calibration_scripts/calib_helpers.R')

# if you're using Rstudio:
setwd("C:/Users/Owner/Desktop/NEON-GLM/GLM_BARC/")
#setwd("C:/Users/Owner/Desktop/NEON-GLM/GLM_SUGG/")
#setwd("C:/Users/Owner/Desktop/NEON-GLM/GLM_CRAM/")
#setwd("C:/Users/Owner/Desktop/NEON-GLM/GLM_LIRO/")
#setwd("C:/Users/Owner/Desktop/NEON-GLM/GLM_PRPO/")
#setwd("C:/Users/Owner/Desktop/NEON-GLM/GLM_PRLA/")
#setwd("C:/Users/Owner/Desktop/NEON-GLM/GLM_TOOK/")

# install these packages:
install.packages("devtools")
require(devtools)
remotes::install_github("USGS-R/glmtools")

# we will need these packages
library(glmtools)
library(rLakeAnalyzer)
library(tidyverse)
library(adagio)
library(GLM3r)
library(lubridate)

# overview of glmtools functions
#   | Function       | Title           |
#   | ------------- |:-------------|
#   | `calibrate_sim` | Calibrates GLM-AED2 variables to improve fit between observed and simulated data |
#   | `compare_to_field` | compare metric for GLM vs field observations |
#   | `get_evaporation`  | get evaporation from GLM simulation |
#   | `get_hypsography` | retrieve hypsography information |
#   | `get_ice` | get ice depth from GLM simulation |
#   | `get_nml_value` | gets a nml value according to an arg_name |
#   | `get_surface_height` | get surface height from GLM simulation |
#   | `get_var` | get a variable from a GLM simulation |
#   | `get_wind` | get wind speed from GLM simulation |
#   | `model_diagnostics` | run diagnostics on model results |
#   | `plot_var_compare` | Plot matching heatmaps for modeled and observed variables |
#   | `plot_var_nc` | plot variables from a GLM simulation |
#   | `plot_var_df` | plot variables from a data.frame |
#   | `read_field_obs` | read in field data into a data.frame |
#   | `read_nml` | read in a GLM simulation `*.nml` file |
#   | `resample_sim` | get subset of time from a generic timeseries data.frame |
#   | `resample_to_field` | match GLM water temperatures with field observations |
#   | `set_nml` | sets values in nml object |
#   | `sim_metrics` | get possible metrics for comparing GLM outputs to field |
#   | `summarize_sim` | creates GLM simulation summary outputs |
#   | `validate_sim` | run diagnostics on model results vs observations |
#   | `write_nml` | write GLM `*.nml` for a GLM simulation |

# check out which R version we're currently using

sim_folder <- "C:/Users/Owner/Desktop/NEON-GLM/GLM_BARC"


#look at the .nml files to confirm the model run
nml_file <- paste0(sim_folder,"/glm3.nml")
nml <- read_nml(nml_file) 
print(nml)
glm_version()

# run GLM
system(paste0(sim_folder, "/glm.exe"))


nc_file <- file.path('C:/Users/Owner/Desktop/NEON-GLM/GLM_BARC/output/output.nc')
# visualize change of water table over time
water_height <- get_surface_height(file = nc_file)

obs_water_height <- read_csv("C:/Users/Owner/Desktop/NEON-GLM/observations/water_level_NEON_sites.csv")
obs_water_height$DateTime <- ymd(obs_water_height$DateTime)

obs_water_height <- obs_water_height %>% filter(siteID == "BARC")

ggplot(water_height, aes(DateTime, surface_height)) +
  geom_line() +
  ggtitle('Surface water level') +
  geom_point(data = obs_water_height, aes(as.POSIXct(DateTime), value))+
  xlab(label = '') + ylab(label = 'Water level (m)') +
  theme_minimal()

volume <- get_var(var = 'Tot_V', file = nc_file)
plot(volume)

#### Example 3: calibrating water volume parameters ####
field_file<-file.path('C:/Users/Owner/Desktop/NEON-GLM/observations/CleanedObsTempBARC.csv')

temp_rmse <- compare_to_field(nc_file = nc_file, 
                              field_file = field_file,
                              metric = 'water.temperature', 
                              as_value = FALSE, 
                              precision= 'hours')
print(paste('Total time period (uncalibrated):',round(temp_rmse,2),'deg C RMSE'))



var = 'Tot_V'         # variable to which we apply the calibration procedure
var_name = 'Tot_V'
path = getwd()       # simulation path/folder
nml_file = nml_file  # path of the nml configuration file that you want to calibrate on
glm_file = nml_file # # path of the gml configuration file
# which parameter do you want to calibrate? a sensitivity analysis helps
calib_setup <- data.frame('pars' = as.character(c('wind_factor','lw_factor','ch','sed_temp_mean',
                                                  'sed_temp_mean',
                                                  'coef_mix_hyp','Kw','seepage_rate','rain_factor','rain_threshold','rh_factor')),
                          
                          'lb' = c(0.7,0.7,5e-4,3,8,0.6,0.1,1e-8,0.5,0.001,0.8),
                          'ub' = c(2,2,0.002,8,20,0.4,0.8,1e-2,1.5,0.1,1.2),
                          'x0' = c(1,1,0.0013,5,13,0.5,0.3,1e-4,1,0.01,1))
print(calib_setup)
glmcmd = system(paste0(sim_folder, "/glm.exe")) # command to be used, default applies the GLM3r function

glmcmd = NULL

# Optional variables
first.attempt = T # if TRUE, deletes all local csv-files that stores the 
#outcome of previous calibration runs


period = get_calib_periods(nml_file, ratio = 20) # define a period for the calibration, 
# this supports a split-sample calibration (e.g. calibration and validation period)
# the ratio value is the ratio of calibration period to validation period
print(period)
scaling = TRUE       # scaling of the variables in a space of [0,10]; TRUE for CMA-ES
verbose = TRUE
method = 'CMA-ES'    # optimization method, choose either `CMA-ES` or `Nelder-Mead`
metric = 'RMSE'      # objective function to be minimized, here the root-mean square error
target.fit = 2.0     # refers to a target fit of 2.0 degrees Celsius (stops when RMSE is below that)
target.iter = 20    # refers to a maximum run of 20 calibration iterations (stops after that many runs)
plotting = TRUE      # if TRUE, script will automatically save the contour plots
output = out_file    # path of the output file
field_file = field_data # path of the field data
conversion.factor = 1 # conversion factor for the output, e.g. 1 for water temp.

field_data <- "C:/Users/Owner/Desktop/NEON-GLM/observations/volume_barco.csv"

calibrate_sim(var = 'Tot_V', path = wd, 
              field_file = field_data, 
              nml_file = "C:/Users/Owner/Desktop/NEON-GLM/GLM_BARC/glm3.nml", 
              calib_setup = calib_setup, 
              glmcmd = system(paste0(wd, "/glm.exe")), first.attempt = TRUE, 
              period = period, 
              scaling = TRUE, method = 'CMA-ES', metric = 'RMSE', 
              target.fit = 2.0, target.iter = 40, 
              plotting = TRUE, 
              output = output, 
              verbose = TRUE)


temp_rmse <- compare_to_field(nc_file = out_file, 
                              field_file = field_data,
                              metric = 'water.temperature', 
                              as_value = FALSE, 
                              precision= 'hours')

print(paste('BARCO calibration period:',round(temp_rmse,2),'deg C RMSE'))


# visualize change of surface water temp. over time
surface_temp <- get_var(file = out_file, 
                        var_name = 'temp',
                        reference = 'surface',
                        z_out = 2)
ggplot(surface_temp, aes(DateTime, temp_2)) +
  geom_line() +
  ggtitle('Surface water temperature') +
  xlab(label = '') + ylab(label = 'Temp. (deg C)') +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = 'temp',
                       reference = 'surface',
                       z_out = 6)
ggplot(bottom_temp, aes(DateTime, temp_6)) +
  geom_line() +
  ggtitle('Bottom water temperature') +
  xlab(label = '') + ylab(label = 'Temp. (deg C)') +
  theme_minimal()

# plot heat maps of water temperature, and compare against observed data
plot_var(out_file, 
         var_name = 'temp')
plot_var_compare(out_file, 
                 field_data, 
                 var_name = 'temp')

# use rLakeAnalyzer to calculate physical derivatives, e.g. thermocline depth
wtr_data <- get_var(file = out_file,
                    var_name = 'temp',
                    reference = 'surface')
str(wtr_data)

wtr_df <- data.frame('datetime' = wtr_data$DateTime,
                     as.matrix(wtr_data[, 2:ncol(wtr_data)]))
colnames(wtr_df) <- c('datetime',paste("wtr_", round(as.numeric(sub(".*_", "", colnames(wtr_df[-1]))),1), sep=""))
td_df <- ts.thermo.depth(wtr = wtr_df, Smin = 0.1, na.rm = TRUE)

ggplot(td_df, aes(datetime, thermo.depth)) +
  geom_line() +
  ggtitle('Thermocline depth') +
  xlab(label = '') + ylab(label = 'Depth (m)') +
  scale_y_continuous(trans = "reverse") + 
  theme_minimal()

#### Example 3: calibrating water temperature parameters ####

var = 'temp'         # variable to which we apply the calibration procedure
path = getwd()       # simulation path/folder
nml_file = nml_file  # path of the nml configuration file that you want to calibrate on
glm_file = nml_file # # path of the gml configuration file
# which parameter do you want to calibrate? a sensitivity analysis helps
calib_setup <- data.frame('pars' = as.character(c('wind_factor','lw_factor','ch','sed_temp_mean',
                                                  'sed_temp_mean',
                                                  'coef_mix_hyp','Kw')),
                          
                          'lb' = c(0.7,0.7,5e-4,3,8,0.6,0.1),
                          'ub' = c(2,2,0.002,8,20,0.4,0.8),
                          'x0' = c(1,1,0.0013,5,13,0.5,0.3))
print(calib_setup)
glmcmd = system(paste0(wd, "/glm.exe")) # command to be used, default applies the GLM3r function
# glmcmd = NULL

# Optional variables
first.attempt = TRUE # if TRUE, deletes all local csv-files that stores the 
#outcome of previous calibration runs
period = get_calib_periods(nml_file, ratio =40) # define a period for the calibration, 
# this supports a split-sample calibration (e.g. calibration and validation period)
# the ratio value is the ratio of calibration period to validation period
print(period)
scaling = TRUE       # scaling of the variables in a space of [0,10]; TRUE for CMA-ES
verbose = TRUE
method = 'CMA-ES'    # optimization method, choose either `CMA-ES` or `Nelder-Mead`
metric = 'RMSE'      # objective function to be minimized, here the root-mean square error
target.fit = 2.0     # refers to a target fit of 2.0 degrees Celsius (stops when RMSE is below that)
target.iter = 20    # refers to a maximum run of 20 calibration iterations (stops after that many runs)
plotting = TRUE      # if TRUE, script will automatically save the contour plots
output = out_file    # path of the output file
field_file = field_data # path of the field data
conversion.factor = 1 # conversion factor for the output, e.g. 1 for water temp.

calibrate_sim(var = 'temp', path = wd, 
             field_file = field_file, 
             nml_file = "C:/Users/Owner/Desktop/NEON-GLM/GLM_BARC/glm3.nml", 
             calib_setup = calib_setup, 
             glmcmd = NULL, first.attempt = TRUE, 
             period = period, 
             scaling = TRUE, method = 'CMA-ES', metric = 'RMSE', 
             target.fit = 2.0, target.iter = 40, 
             plotting = TRUE, 
             output = output, 
             verbose = TRUE)

temp_rmse <- compare_to_field(nc_file = out_file, 
                              field_file = field_data,
                              metric = 'water.temperature', 
                              as_value = FALSE, 
                              precision= 'hours')
print(paste('BARCO calibration period:',round(temp_rmse,2),'deg C RMSE'))



#### Example 4: calibrating dissovled oyxgen parameters ####
# Is parameterising water quality feasible using automatic optimization techniques? See: 
# Mi et al 2020: The formation of a metalimnetic oxygen minimum exemplifies how ecosystem dynamics shape biogeochemical processes: A modelling study
# Fenocchi et al 2019: Applicability of a one-dimensional coupled ecological-hydrodynamic numerical model to future projections in a very deep large lake (Lake Maggiore, Northern Italy/Southern Switzerland)

# visualize heat maps of oxygen, phosphate and nitrate
plot_var(nc_file = out_file, 
         var_name = 'OXY_oxy')
plot_var(nc_file = out_file, 
         var_name = 'PHS_frp')
plot_var(nc_file = out_file, 
         var_name = 'NIT_nit')

plot_var_compare(nc_file = out_file, 
                 field_file = field_data, 
                 var_name = 'OXY_oxy',
                 precision = 'hours',
                 conversion = 32/1000)

aed_template = 'aed2/aed2-template.nml' 
file.copy(aed_template, 'aed2/aed2.nml', overwrite = TRUE)
nml_file <- file.path(sim_folder, 'aed2/aed2.nml')
calib_setup <- data.frame('pars' = as.character(c('Fsed_oxy','Ksed_oxy','theta_sed_oxy')),
                          'lb' = c(-150, 10, 1.05),
                          'ub' = c(10,70,1.10),
                          'x0' = c(-100, 50, 1.08))

calibrate_sim(var = 'OXY_oxy', path = getwd(), 
              field_file = field_file, 
              nml_file = nml_file, 
              glm_file = glm_file, 
              calib_setup = calib_setup, 
              glmcmd = NULL, first.attempt = FALSE, 
              period = period, 
              scaling = TRUE, method = 'CMA-ES', metric = 'RMSE', 
              target.fit = 3.0, target.iter = 20, 
              plotting = TRUE, 
              output = output, 
              verbose = TRUE,
              conversion.factor = 32/1000)


# Example 5: check your phytoplankton ####
# Advice for calibrating phytoplankton functional groups, investigate the
# limitation functions; this example setup is not completely set up for 
# intensive water quality calculations --> this is just an example how to do it
aed_nml <- read_nml('aed2/aed2.nml')

# heat maps of phosphate, nitrate and silica
plot_var(nc_file = out_file, var_name = 'PHS_frp',reference = 'surface')
plot_var(nc_file = out_file, var_name = 'NIT_nit',reference = 'surface')
plot_var(nc_file = out_file, var_name = 'SIL_rsi',reference = 'surface')

# heat maps of cyanobacteria and diatoms
plot_var(nc_file = out_file, var_name = 'PHY_cyano',reference = 'surface')
plot_var(nc_file = out_file, var_name = 'PHY_diatom', reference = 'surface')

# visualize change of surface cyanobacteria over time
surface_cyano <- get_var(file = out_file, 
                         var_name = 'PHY_cyano',
                         reference = 'surface',
                         z_out = 2)
ggplot(surface_cyano, aes(DateTime, PHY_cyano_2)) +
  geom_line() +
  ggtitle('Cyanobacteria functional group') +
  xlab(label = '') + ylab(label = '(mmol/m3)') +
  theme_minimal()

# visualize change of surface diatoms over time
surface_diatom <- get_var(file = out_file, 
                          var_name = 'PHY_diatom',
                          reference = 'surface',
                          z_out = 2)
ggplot(surface_diatom, aes(DateTime, PHY_diatom_2)) +
  geom_line() +
  ggtitle('Diatoms functional group') +
  xlab(label = '') + ylab(label = '(mmol/m3)') +
  theme_minimal()

phyto_list <- get_nml_value(aed_nml,arg_name = 'aed2_phytoplankton::dbase')

path_phyto <- phyto_list
phyto_nml <- read_nml(path_phyto)
phyto_nam <- get_nml_value(phyto_nml,arg_name = 'pd%p_name')
names <- unlist(strsplit(phyto_nam, ","))

lim_attributes <- c('fI', 'fNit', 'fPho', 'fSil', 'fT', 'fSal')
plist <- list()
pindex <- 1
for (ii in seq_len(length(names))){
  for (jj in seq_len(length(lim_attributes))){
    
    p1 <- plot_var(nc_file = out_file, var_name = paste0('PHY_',names[ii],'_',
                                                         lim_attributes[jj]),
                   legend.title = paste(names[ii], lim_attributes[jj]))
    
    plist[[pindex]] <- p1
    pindex <- pindex + 1
  }
}

# limitation functions for cyanobacteria and diatoms
p_cyano <- plist[[1]] / plist[[2]] / plist[[3]] / plist[[4]] / plist[[5]] / plist[[6]] 
p_diatom <- plist[[7]] / plist[[8]] / plist[[9]] / plist[[10]] / plist[[11]] / plist[[12]] 

p_cyano
p_diatom