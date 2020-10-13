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

lapply(water_var_product, neonstore::neon_download, site = lake_sites, 
       start_date = "2017-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")


# Unpack the stored files to be accessible in R
neonstore::neon_store(table = "dep_secchi-basic")
neonstore::neon_store(table = "sdg_externalLabData-expanded")
neonstore::neon_store(table = "swc_externalLabDataByAnalyte-expanded")
neonstore::neon_store(table = "alg_algaeExternalLabDataPerSample-expanded")
neonstore::neon_store(table = "uPAR_30min-expanded")
neonstore::neon_store(table = "NSW_15_minute-expanded")
neonstore::neon_store(table = "waq_instantaneous-expanded")
neonstore::neon_store(table = "TSD_30_min-expanded")
neonstore::neon_store(table = "EOS_30_min-expanded")

# Make the stored NEON data product a data table in R
# Lake level in meters above sea level
water_level_obs <- neonstore::neon_read(table = "EOS_30_min-expanded",product = "DP1.20016.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),start_date = "2017-01-01", end_date = "2020-08-01",ext = "csv",timestamp = NA,
  files = NULL,sensor_metadata = TRUE,altrep = FALSE)%>%
  select(endDateTime,siteID,surfacewaterElevMean)%>%
  mutate(year_month = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m"))%>%
  arrange(siteID, endDateTime)

water_level_barc <- water_level_obs %>% filter(siteID == "BARC") %>%
  mutate(daily = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  group_by(daily)%>%
  summarise_all(funs(mean))%>%
  select(-endDateTime)%>%
  rename(DateTime = daily)%>%
  arrange(DateTime)%>%
  select(DateTime, surfacewaterElevMean)%>%
  mutate(surface_height = surfacewaterElevMean-19.2)%>%
  select(DateTime, surface_height) %>%
  filter(DateTime>="2018-01-01")%>%
  filter(DateTime<="2020-07-31")

# Water temperature by depth
water_temp_obs <- neonstore::neon_read(table = "TSD_30_min-expanded",product = "DP1.20264.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),start_date = "2017-01-01",end_date = "2020-08-01",ext = "csv",timestamp = NA,
  files = NULL,sensor_metadata = TRUE,altrep = FALSE)%>% 
  select(endDateTime, thermistorDepth, tsdWaterTempMean, siteID) %>%
  mutate(year_month = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m"))%>% 
  arrange(siteID, endDateTime, thermistorDepth)

water_temp_barc <- water_temp_obs %>% filter(siteID == "BARC") %>%
  mutate(DateTime = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  filter(DateTime>="2019-01-01") %>%
  filter(DateTime<="2020-07-31") %>%
  rename(Depth = thermistorDepth, temp = tsdWaterTempMean)%>%
  group_by(DateTime, Depth)%>%
  summarise_all(funs(mean))%>%
  filter(temp > 10)%>%
  select(DateTime, Depth, temp)%>%write_csv(.,'./observations/CleanedObsTempBARC.csv')
# ------------------------------------------------------------------------------------------ 

