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
remotes::install_github("hdugan/glmtools")

#download.file("https://aquatic.science.uwa.edu.au/research/models/GLM/Download/downloader.php?bin&version=3.1/glm_3.1.0a4-0_amd64.deb.gz", destfile = "./glm_3.1.0a4-0_amd64.deb.gz")

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, reshape2, devtools, patchwork, zoo, ncdf4, glmtools)


##### run glm_aed #####
glm_version()
setwd("C:/Users/Owner/Desktop/NEON-GLM/GLM_BARC")
system(paste0(sim_folder, "/GLM_BARC/glm.exe"))

nc_file <- file.path('C:/Users/Owner/Desktop/NEON-GLM/GLM_BARC/output/output.nc') #defines the output.nc file 

water_height <- get_surface_height(file = out_file)

obs_water_height <- read_csv("C:/Users/Owner/Desktop/NEON-GLM/observations/water_level_barco.csv")
obs_water_height$DateTime <- ymd(obs_water_height$DateTime)
obs_water_height$surface_height <- obs_water_height$surface_height-1

ggplot(water_height, aes(DateTime, surface_height)) +
  geom_line() +
  ggtitle('Surface water level') +
  geom_point(data = obs_water_height, aes(as.POSIXct(DateTime), surface_height))+
  xlab(label = '') + ylab(label = 'Water level (m)') +
  theme_minimal()

plot_temp(nc_file)

field_file<-file.path('C:/Users/Owner/Desktop/NEON-GLM/observations/CleanedObsTempBARC.csv')
plot_temp_compare(nc_file, field_file)





