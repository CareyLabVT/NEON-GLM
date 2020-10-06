#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   Bathy_maps_NEON                                      *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    06Oct2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: Make bathymetry maps of each NEON lake               *
#* HELPERS: Quinn Thomas and Carl Boetiiger                      *
#*****************************************************************

# Load packages
# -------------------------------------------------------------------

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, neonstore, lubridate, reshape2)

# Lake level in meters
water_level <- neon_read(
  table = "EOS_30_min-expanded",
  product = "DP1.20016.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE)