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
remotes::install_github("CareyLabVT/glmtools")

download.file("https://aquatic.science.uwa.edu.au/research/models/GLM/Download/downloader.php?bin&version=3.1/glm_3.1.0a4-0_amd64.deb.gz", destfile = "./glm_3.1.0a4-0_amd64.deb.gz")


if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, reshape2, devtools, patchwork, zoo, ncdf4, glmtools)

sim_folder <- "C:/Users/Owner/Desktop/NEON-GLM"


#look at the .nml files to confirm the model run
nml_file <- paste0(sim_folder,"/GLM_BARC/glm3.nml")
nml <- read_nml(nml_file) 
print(nml)

##### run glm_aed #####
setwd("C:/Users/Owner/Desktop/NEON-GLM/GLM_BARC")
system(paste0(sim_folder, "/GLM_BARC/glm.exe"))

nc_file <- file.path(sim_folder, 'GLM_BARC/output/output.nc') #defines the output.nc file 

field_file<-file.path('C:/Users/Owner/Desktop/NEON-GLM/observations/CleanedObsTempBARC.csv')
plot_temp_compare(nc_file, field_file)

plot_temp(nc_file)

EPI_RMSE <- compare_to_field(nc_file, field_file, nml_file = nml_file, metric = 'epi.temperature', as_value = F,
                 na.rm = TRUE, precision = 'days',method = 'interp')

temps <- resample_to_field(nc_file, field_file, precision="mins", method='interp')

temps<-temps[complete.cases(temps),]

temps_1 <- temps %>% filter(Depth == 1.05)
plot(temps_1$DateTime, temps_1$Observed_temp, col = "black", main = " 1m Temperature Mod = red; Obs = black")
points(temps_1$DateTime, temps_1$Modeled_temp, col = "red")

water_height <- get_surface_height(file = nc_file)

obs_water_height <- read_csv("C:/Users/Owner/Desktop/NEON-GLM/observations/water_level_barco.csv")
obs_water_height$DateTime <- ymd(obs_water_height$DateTime)
obs_water_height$surface_height <- obs_water_height$surface_height-1

ggplot(water_height, aes(DateTime, surface_height)) +
  geom_line() +
  ggtitle('Surface water level') +
  geom_point(data = obs_water_height, aes(as.POSIXct(DateTime), surface_height))+
  xlab(label = '') + ylab(label = 'Water level (m)') +
  theme_minimal()

