#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   GLM!                                                 *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    13Oct2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: RUN GLM-AED for the NEON lake sites                  *
#*****************************************************************

# Get packages and specify sites to download
# -----------------------------------------------------------------------------------------------------------------
remotes::install_github("CareyLabVT/GLMr")
remotes::install_github("CareyLabVT/glmtools")
remotes::install_github("GLEON/GLM3r")

download.file("https://aquatic.science.uwa.edu.au/research/models/GLM/Download/downloader.php?bin&version=3.1/glm_3.1.0a4-0_amd64.deb.gz", destfile = "./glm_3.1.0a4-0_amd64.deb.gz")


if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, reshape2, devtools, patchwork, zoo, ncdf4, GLMr, glmtools, GLM3r)

sim_folder <- getwd()

#look at the .nml files to confirm the model run
nml_file <- paste0(sim_folder,"/glm3.nml")
nml <- read_nml(nml_file) 
print(nml)

##### run glm_aed ######
system(sim_folder,"/","glm.exe", intern = T, ignore.stdout = T, ignore.stderr = T, wait = T)

nc_file <- file.path(sim_folder, 'output/output.nc') #defines the output.nc file 

field_file<-file.path('./observations/CleanedObsTempBARC.csv')
plot_temp_compare(nc_file, field_file)

#get water level
water_level<-get_surface_height(nc_file, ice.rm = TRUE, snow.rm = TRUE)
plot(water_level$DateTime,water_level$surface_height, ylim = c(5,9), main = "Barco!")
lines(as.POSIXct(water_level_barc$DateTime),water_level_barc$surface_height, type = "p", ylim = c(5,9), col = "red")
