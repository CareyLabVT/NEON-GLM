#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   Meteorology data collation and comparisons           *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    11Oct2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: Download the met data and get it in a GLM format     *
#*****************************************************************

# Get packages and specify sites to download
# -----------------------------------------------------------------------------------------------------------------
if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, reshape2, devtools, patchwork)

# Bypass the latest CRAN version of neonstore and use Carl's most recent Github push
devtools::install_github("cboettig/neonstore")
sites = c("TOOK", "SUGG", "BARC", "PRPO", "PRLA", "CRAM", "OSBS", "TOOL", "DCFS", "UNDE")
lake_sites = c("TOOK", "SUGG", "BARC", "PRPO", "PRLA", "CRAM")
tower_sites = c("OSBS", "TOOL", "DCFS", "UNDE")

# -----------------------------------------------------------------------------------------------------------------

# Download the newest data from NEON
# -----------------------------------------------------------------------------------------------------------------
met_product_hum = "DP1.00098.001"                                                       # Humidity
lapply(met_product_hum, neonstore::neon_download, site = sites, 
       start_date = "2017-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")

met_product_airT = "DP1.00002.001"                                                      # Air Temperature
lapply(met_product_airT, neonstore::neon_download, site = sites, 
       start_date = "2017-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")

met_product_rad = "DP1.00023.001"                                                       # SW & LW Radiation
lapply(met_product_rad, neonstore::neon_download, site = sites, 
       start_date = "2017-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")

met_product_precip = "DP1.00006.001"                                                    # Precipitation
lapply(met_product_precip, neonstore::neon_download, site = sites, 
       start_date = "2014-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")

met_product_wind = "DP1.00001.001"                                                      # 2D Windspeed
lapply(met_product_wind, neonstore::neon_download, site = sites, 
       start_date = "2017-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")
# -----------------------------------------------------------------------------------------------------------------

# Make the stored NEON data product a data table in R
# -----------------------------------------------------------------------------------------------------------------
rel_hum_dat <- neonstore::neon_read(table = "RH_30min-expanded", product = "DP1.00098.001", site = lake_sites,
  start_date = "2017-01-01", end_date = "2020-08-01", ext = "csv", timestamp = NA, files = NULL, sensor_metadata = TRUE, altrep = FALSE) %>% 
  select(endDateTime, RHMean, siteID)

air_temp_dat <- neonstore::neon_read(table = "SAAT_30min-expanded", product = "DP1.00002.001", site = lake_sites,
  start_date = "2017-01-01", end_date = "2020-08-01", ext = "csv", timestamp = NA, files = NULL, sensor_metadata = TRUE, altrep = FALSE)%>% 
  select(endDateTime, tempSingleMean, siteID)

radiation_dat <- neonstore::neon_read(table = "SLRNR_30min-expanded", product = "DP1.00023.001", site = lake_sites,
  start_date = "2017-01-01", end_date = "2020-08-01", ext = "csv", timestamp = NA, files = NULL, sensor_metadata = TRUE, altrep = FALSE) %>%
  select(endDateTime, inSWMean, outSWMean, inLWMean, outLWMean, siteID)%>%
  mutate(SWMean = inSWMean - outSWMean)%>%
  mutate(LWMean = inLWMean - outLWMean)%>%
  select(endDateTime, SWMean, LWMean, siteID)

# Read and organize the precip data
precip_dat_l <- neonstore::neon_read(table = "SECPRE_30min-expanded", product = "DP1.00006.001", site = lake_sites,
  start_date = "2017-01-01", end_date = "2020-08-01", ext = "csv", timestamp = NA, files = NULL, sensor_metadata = TRUE, altrep = FALSE)%>% 
  select(endDateTime, secPrecipBulk, siteID)

precip_dat_t <- neonstore::neon_read(table = "SECPRE_30min-expanded", product = "DP1.00006.001", site = tower_sites,
  start_date = "2017-01-01", end_date = "2020-08-01", ext = "csv", timestamp = NA, files = NULL, sensor_metadata = TRUE, altrep = FALSE)%>% 
  select(endDateTime, secPrecipBulk, siteID)

precip_dat_hour_l <- precip_dat_l %>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("secPrecipBulk"), sum, na.rm = TRUE)%>%
  arrange(siteID, time)

precip_dat_hour_t <- precip_dat_t %>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("secPrecipBulk"), sum, na.rm = TRUE)%>%
  arrange(siteID, time)

precip_dat_hour_t$siteID[precip_dat_hour_t$siteID == "UNDE"] = "CRAM"
precip_dat_hour_t$siteID[precip_dat_hour_t$siteID == "DCFS"] = "PRPO"
precip_dat_hour_t$siteID[precip_dat_hour_t$siteID == "OSBS"] = "BARC"

precip_dat_hour <- rbind(precip_dat_hour_l,precip_dat_hour_t)

windspeed_dat <- neonstore::neon_read(table = "2DWSD_2min-expanded", product = "DP1.00001.001", site = lake_sites,
  start_date = "2017-01-01", end_date = "2020-08-01", ext = "csv", timestamp = NA, files = NULL, sensor_metadata = TRUE, altrep = FALSE) %>%
  select(endDateTime, windSpeedMean, siteID)

h0 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("00:00:00"))
h1 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("01:00:00"))
h2 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("02:00:00"))
h3 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("03:00:00"))
h4 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("04:00:00"))
h5 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("05:00:00"))
h6 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("06:00:00"))
h7 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("07:00:00"))
h8 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("08:00:00"))
h9 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("09:00:00"))
h10 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("10:00:00"))
h11 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("11:00:00"))
h12 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("12:00:00"))
h13 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("13:00:00"))
h14 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("14:00:00"))
h15 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("15:00:00"))
h16 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("16:00:00"))
h17 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("17:00:00"))
h18 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("18:00:00"))
h19 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("19:00:00"))
h20 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("20:00:00"))
h21 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("21:00:00"))
h22 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("22:00:00"))
h23 <-  windspeed_dat %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
  filter(h_m_s == hms("23:00:00"))

windspeed_dat_hour <- rbind(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23) %>%
  mutate(time=paste(endDateTime, h_m_s) %>% 
           as.POSIXct(., format="%Y-%m-%d %H:%M:%S")) %>% 
  select(-endDateTime, -h_m_s)%>%
  arrange(siteID, time) %>% rename(WindSpeed = windSpeedMean)%>%
  select(time, WindSpeed, siteID)

# -----------------------------------------------------------------------------------------------------------------

# Bind all of the met data together
# -----------------------------------------------------------------------------------------------------------------
NEON_met_data_hourly <- left_join(radiation_dat, air_temp_dat, by=c('endDateTime','siteID')) %>%
  left_join(., rel_hum_dat, by=c('endDateTime','siteID'))%>%
  select(endDateTime,siteID,SWMean,LWMean,tempSingleMean,RHMean)%>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_all(funs(mean))%>%
  arrange(siteID, time)%>%
  left_join(., windspeed_dat_hour, by=c('time','siteID'))%>%
  left_join(., precip_dat_hour, by=c('time','siteID'))%>%
  mutate(Snow = ifelse(tempSingleMean <= 0, secPrecipBulk, 0))%>%
  select(time,siteID,SWMean,LWMean,tempSingleMean,WindSpeed,secPrecipBulk,RHMean,Snow)
# -----------------------------------------------------------------------------------------------------------------
