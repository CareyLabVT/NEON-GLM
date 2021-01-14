#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   BARCO GLM CALIBRATION                                *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    16Nov2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: RUN GLM-AED for the NEON lake sites and calibrate    *
#*****************************************************************

rm(list = ls()) #let's clean up that workspace!

remotes::install_github("CareyLabVT/glmtools")

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, reshape2, devtools, patchwork, zoo, ncdf4, glmtools) # --> Load the packages

setwd("C:/Users/Owner/Desktop/NEON-GLM/GLM_BARC") # --> set the Directory to Barco Lake

source('C:/Users/Owner/Desktop/NEON-GLM/scripts/functions/functions-glm_rpm.R') # --> source the helper functions

read.packages() 
