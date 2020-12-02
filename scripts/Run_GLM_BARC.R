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

#download.file("https://aquatic.science.uwa.edu.au/research/models/GLM/Download/downloader.php?bin&version=3.1/glm_3.1.0a4-0_amd64.deb.gz", destfile = "./glm_3.1.0a4-0_amd64.deb.gz")

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, reshape2, devtools, patchwork, zoo, ncdf4, glmtools)

sim_folder <- "~/NEON-GLM"

#look at the .nml files to confirm the model run
nml_file <- paste0("~/NEON-GLM/GLM_BARC/glm3.nml")
nml <- read_nml(nml_file) 
print(nml)

##### run glm_aed #####
setwd("~/NEON-GLM/GLM_BARC")
system("~/NEON-GLM/GLM_BARC/glm.exe")

nc_file <- file.path('C:/Users/Owner/Desktop/NEON-GLM/GLM_BARC/output/output.nc') #defines the output.nc file 

plot_temp(nc_file)
field_file<-file.path('C:/Users/Owner/Desktop/NEON-GLM/observations/CleanedObsTempBARC.csv')
plot_temp_compare(nc_file, field_file)





