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
pacman::p_load(tidyverse, lubridate, reshape2, devtools, patchwork, zoo)

# Bypass the latest CRAN version of neonstore and use Carl's most recent Github push
devtools::install_github("cboettig/neonstore")

sites_all = c("LIRO", "TOOK", "SUGG", "BARC", "PRPO", "PRLA", "CRAM", "OSBS", "TOOL", "DCFS", "UNDE")
lake_sites = c("LIRO", "TOOK", "SUGG", "BARC", "PRPO", "PRLA", "CRAM")
tower_sites = c("OSBS", "TOOL", "DCFS", "UNDE")
# -----------------------------------------------------------------------------------------------------------------

# Download the newest data from NEON
# -----------------------------------------------------------------------------------------------------------------

met_product_hum = "DP1.00098.001"                                                       # Humidity
lapply(met_product_hum, neonstore::neon_download, site = lake_sites, 
       start_date = "2017-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")

met_product_airT = "DP1.00002.001"                                                      # Air Temperature
lapply(met_product_airT, neonstore::neon_download, site = lake_sites, 
       start_date = "2017-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")

met_product_rad = "DP1.00023.001"                                                       # SW & LW Radiation
lapply(met_product_rad, neonstore::neon_download, site = lake_sites, 
       start_date = "2017-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")

met_product_precip = "DP1.00006.001"                                                    # Precipitation
lapply(met_product_precip, neonstore::neon_download, site = lake_sites, 
       start_date = "2014-01-01",  end_date = "2020-10-01",
       file_regex = "[.]csv")

met_product_precip = "DP1.00006.001"                                                    # Precipitation
lapply(met_product_precip, neonstore::neon_download, site = lake_sites, 
       start_date = "2014-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")

met_product_wind = "DP1.00001.001"                                                      # 2D Windspeed
lapply(met_product_wind, neonstore::neon_download, site = lake_sites, 
       start_date = "2017-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")
# -----------------------------------------------------------------------------------------------------------------

# Make the stored NEON data product a data table in R
# -----------------------------------------------------------------------------------------------------------------

humidity <- neonstore::neon_read(table = "RH_30min-expanded", product = "DP1.00098.001", site = sites_all,
  start_date = "2017-01-01", end_date = "2020-08-01", ext = "csv", timestamp = NA, files = NULL, sensor_metadata = TRUE, altrep = FALSE) %>% 
  select(endDateTime, RHMean, siteID)

humidity <- humidity %>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("RHMean"), mean, na.rm = TRUE)%>%
  arrange(siteID, time)

airtemp <- neonstore::neon_read(table = "SAAT_30min-expanded", product = "DP1.00002.001", site = sites_all,
  start_date = "2017-01-01", end_date = "2020-08-01", ext = "csv", timestamp = NA, files = NULL, sensor_metadata = TRUE, altrep = FALSE)%>% 
  select(endDateTime, tempSingleMean, siteID)

airtemp <- airtemp %>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("tempSingleMean"), mean, na.rm = TRUE)%>%
  arrange(siteID, time)

radiation <- neonstore::neon_read(table = "SLRNR_30min-expanded", product = "DP1.00023.001", site = sites_all,
  start_date = "2017-01-01", end_date = "2020-08-01", ext = "csv", timestamp = NA, files = NULL, sensor_metadata = TRUE, altrep = FALSE) %>%
  select(endDateTime, inSWMean, outLWMean, siteID)

radiation <- radiation %>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("inSWMean", "outLWMean"), mean, na.rm = TRUE)%>%
  arrange(siteID, time)

precip  <- neonstore::neon_read(table = "SECPRE_30min-expanded", product = "DP1.00006.001", site = sites_all,
  start_date = "2017-01-01", end_date = "2020-08-01", ext = "csv", timestamp = NA, files = NULL, sensor_metadata = TRUE, altrep = FALSE)%>% 
  select(endDateTime, secPrecipBulk, siteID)

precip <- precip %>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("secPrecipBulk"), sum, na.rm = TRUE)%>%
  arrange(siteID, time)

# Florida site precipitation
precip_barc <- precip %>%
  filter(siteID == "OSBS") %>%
  mutate(siteID = replace(siteID, siteID=="OSBS", "BARC")) %>%
  as.data.frame()

precip_sugg <- precip %>%
  filter(siteID == "OSBS") %>%
  mutate(siteID = replace(siteID, siteID=="OSBS", "SUGG")) %>%
  as.data.frame()

# Wisconsin site precipitation
precip_cram <- precip %>%
  filter(siteID == "UNDE") %>%
  mutate(siteID = replace(siteID, siteID=="UNDE", "CRAM")) %>%
  as.data.frame()

precip_liro <- precip %>%
  filter(siteID == "UNDE") %>%
  mutate(siteID = replace(siteID, siteID=="UNDE", "LIRO")) %>%
  as.data.frame()

# Kansas site precipitation
precip_prpo <- precip %>%
  filter(siteID == "DCFS") %>%
  mutate(siteID = replace(siteID, siteID=="DCFS", "PRPO")) %>%
  as.data.frame()

precip_prla <- precip %>%
  filter(siteID == "DCFS") %>%
  mutate(siteID = replace(siteID, siteID=="DCFS", "PRLA")) %>%
  as.data.frame()

# Alaska site precipitation
precip_took <- precip %>%
  filter(siteID == "TOOK")


windspeed <- neonstore::neon_read(table = "2DWSD_30min-expanded", product = "DP1.00001.001", site = sites_all,
  start_date = "2017-01-01", end_date = "2020-08-01", ext = "csv", timestamp = NA, files = NULL, sensor_metadata = TRUE, altrep = FALSE) %>%
  select(endDateTime, windSpeedMean, siteID)

windspeed <- windspeed %>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("windSpeedMean"), sum, na.rm = TRUE)%>%
  arrange(siteID, time)

h0 <-  windspeed %>% mutate(h_m_s = hms::as.hms(endDateTime, tz = 'GMT')) %>% 
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

windspeed <- rbind(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23) %>%
  mutate(time=paste(endDateTime, h_m_s) %>% 
           as.POSIXct(., format="%Y-%m-%d %H:%M:%S")) %>% 
  select(-endDateTime, -h_m_s)%>%
  arrange(siteID, time) %>% rename(WindSpeed = windSpeedMean)%>%
  select(time, WindSpeed, siteID)

# -----------------------------------------------------------------------------------------------------------------

# Bind all of the met data together
# -----------------------------------------------------------------------------------------------------------------
NEON_met_data_hourly <- left_join(radiation, airtemp, by=c('time','siteID')) %>%
  left_join(., humidity, by=c('time','siteID'))%>%
  select(time,siteID,inSWMean,outLWMean,tempSingleMean,RHMean)
# -----------------------------------------------------------------------------------------------------------------

# Make the BARCO and SUGG Met data files for the GLM run
BARC_met <- NEON_met_data_hourly %>% filter(siteID == "BARC") %>% 
  filter(time >= as.POSIXct("2018-01-01 00:00:00", tz = "GMT")) %>% filter(time <= as.POSIXct("2020-07-31 00:00:00", tz = "GMT"))%>%
  select(-siteID)%>%
  rename(ShortWave = SWMean, LongWave = LWMean, AirTemp = tempSingleMean, RelHum = RHMean, Rain = secPrecipBulk)%>%
  select(time, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain, Snow)



# Just starting with the simple na.approx method. 
# Will download NLDAS and the tower data to build and empirical relationship to fill NAs soon
BARC_met$ShortWave <- na.approx(BARC_met$ShortWave)
BARC_met$LongWave <- na.approx(BARC_met$LongWave)
BARC_met$AirTemp <- na.approx(BARC_met$AirTemp)
BARC_met$RelHum <- na.approx(BARC_met$RelHum)
BARC_met$WindSpeed <- na.approx(BARC_met$WindSpeed)
BARC_met$Rain <- na.approx(BARC_met$Rain)
BARC_met$Snow <- na.approx(BARC_met$Snow)

#Currently assuming Barco and Suggs are similar
#Although I will be checking this soon
write_csv(BARC_met, "./driver_data/Barco_met_hourly_GMT.csv")
write_csv(BARC_met, "./driver_data/Suggs_met_hourly_GMT.csv")
