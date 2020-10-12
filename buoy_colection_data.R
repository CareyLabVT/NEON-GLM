#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   Observed data download                               *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    08Oct2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: Downlad the met data and get it in a GLM format      *
#*****************************************************************

# Get packages
if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, reshape2, devtools, patchwork)

# Bypass the latest CRAN version of neonstore and use Carl's most recent Github push
devtools::install_github("cboettig/neonstore")

lake_sites = c("TOOK", "SUGG", "BARC", "PRPO", "PRLA", "CRAM")


# Gauge height = DP1.20267.001
# PAR below surface = DP1.20261.001
# PAR above surface = DP1.20042.001
# Periphyton, seston, phyto = DP1.20163.001
# Secchi depth = DP1.20252.001
# Dissolved gases = DP1.20097.001
# Chemical properties of surface water = DP1.20093.001
# Surface water nitrate = DP1.20254.001
# Water Quality = DP1.20288.001
# Water temperature = DP1.20264.001
# Nitrate = DP1.20033.001

water_var_product = c("DP1.20267.001", "DP1.20261.001", "DP1.20042.001",
                      "DP1.20163.001", "DP1.20252.001", "DP1.20097.001",
                      "DP1.20093.001", "DP1.20033.001", "DP1.20288.001",
                      "DP1.20264.001")

lapply(water_var_product, neon_download, site = sites, 
       start_date = "2017-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")


# Unpack the stored files to be accessible in R
# Currently using the 30min aggregated data for most variables except Wind speed
neonstore::neon_store(table = "RH_30min-expanded")
neonstore::neon_store(table = "SAAT_30min-expanded")
neonstore::neon_store(table = "SLRNR_30min-expanded")
neonstore::neon_store(table = "SECPRE_30min-expanded")
neonstore::neon_store(table = "2DWSD_1min-expanded")


# Make the stored NEON data product a data table in R
rel_hum_dat <- neonstore::neon_table(table = "RH_30min-expanded", site = lake_sites) %>%
  select(endDateTime, RHMean, siteID)

air_temp_dat <- neonstore::neon_table(table = "SAAT_30min-expanded", site = lake_sites)%>% 
  select(endDateTime, tempSingleMean, siteID)

radiation_dat <- neonstore::neon_table(table = "SLRNR_30min-expanded", site = lake_sites) %>%
  select(endDateTime, inSWMean, outSWMean, inLWMean, outLWMean, siteID)%>%
  mutate(SWMean = inSWMean - outSWMean)%>%
  mutate(LWMean = inLWMean - outLWMean)%>%
  select(endDateTime, SWMean, LWMean, siteID)

precip_dat <- neonstore::neon_table(table = "SECPRE_30min-expanded", site = lake_sites)%>% 
  select(endDateTime, secPrecipBulk, siteID)

windspeed_dat <- neonstore::neon_table(table = "2DWSD_1min-expanded", site = lake_sites) %>%
  select(endDateTime, windSpeedMean, siteID)%>%
  # ------------------------------------------------------------------------------------------  
filter(endDateTime >= hms::as.hms('23:59:00'),
       endDateTime <= hms::as.hms('00:01:00'))%>%
  filter(endDateTime >= hms::as.hms('00:59:00'),
         endDateTime <= hms::as.hms('01:01:00'))%>%
  filter(endDateTime >= hms::as.hms('01:59:00'),
         endDateTime <= hms::as.hms('02:01:00'))%>%
  filter(endDateTime >= hms::as.hms('02:59:00'),
         endDateTime <= hms::as.hms('03:01:00'))%>%
  filter(endDateTime >= hms::as.hms('03:59:00'),
         endDateTime <= hms::as.hms('04:01:00'))%>%
  filter(endDateTime >= hms::as.hms('04:59:00'),
         endDateTime <= hms::as.hms('05:01:00'))%>%
  filter(endDateTime >= hms::as.hms('05:59:00'),
         endDateTime <= hms::as.hms('06:01:00'))%>%
  filter(endDateTime >= hms::as.hms('06:59:00'),
         endDateTime <= hms::as.hms('07:01:00'))%>%
  filter(endDateTime >= hms::as.hms('07:59:00'),
         endDateTime <= hms::as.hms('08:01:00'))%>%
  filter(endDateTime >= hms::as.hms('08:59:00'),
         endDateTime <= hms::as.hms('09:01:00'))%>%
  filter(endDateTime >= hms::as.hms('09:59:00'),
         endDateTime <= hms::as.hms('10:01:00'))%>%
  filter(endDateTime >= hms::as.hms('10:59:00'),
         endDateTime <= hms::as.hms('11:01:00'))%>%
  filter(endDateTime >= hms::as.hms('11:59:00'),
         endDateTime <= hms::as.hms('12:01:00'))%>%
  filter(endDateTime >= hms::as.hms('12:59:00'),
         endDateTime <= hms::as.hms('13:01:00'))%>%
  filter(endDateTime >= hms::as.hms('13:59:00'),
         endDateTime <= hms::as.hms('14:01:00'))%>%
  filter(endDateTime >= hms::as.hms('14:59:00'),
         endDateTime <= hms::as.hms('15:01:00'))%>%
  filter(endDateTime >= hms::as.hms('15:59:00'),
         endDateTime <= hms::as.hms('15:01:00'))%>%
  filter(endDateTime >= hms::as.hms('15:59:00'),
         endDateTime <= hms::as.hms('16:01:00'))%>%
  filter(endDateTime >= hms::as.hms('16:59:00'),
         endDateTime <= hms::as.hms('17:01:00'))%>%
  filter(endDateTime >= hms::as.hms('17:59:00'),
         endDateTime <= hms::as.hms('18:01:00'))%>%
  filter(endDateTime >= hms::as.hms('18:59:00'),
         endDateTime <= hms::as.hms('19:01:00'))%>%
  filter(endDateTime >= hms::as.hms('19:59:00'),
         endDateTime <= hms::as.hms('20:01:00'))%>%
  filter(endDateTime >= hms::as.hms('20:59:00'),
         endDateTime <= hms::as.hms('21:01:00'))%>%
  filter(endDateTime >= hms::as.hms('21:59:00'),
         endDateTime <= hms::as.hms('22:01:00'))%>%
  filter(endDateTime >= hms::as.hms('22:59:00'),
         endDateTime <= hms::as.hms('23:01:00'))
# ------------------------------------------------------------------------------------------ 

