#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   Neon data collation and comparisons                  *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    30Oct2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: Download the met data and get it in a GLM format     *
#*****************************************************************

# Get packages and specify sites to download
# -----------------------------------------------------------------------------------------------------------------
if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, reshape2, devtools, patchwork, zoo)

# Bypass the latest CRAN version of neonstore and use Carl's most recent Github push
remotes::install_github("cboettig/neonstore")
# -----------------------------------------------------------------------------------------------------------------

# Download and load duckdb_r
# remember a mac is squiggle ~/ and a PC is period ./
download.file("https://github.com/cwida/duckdb/releases/download/master-builds/duckdb_r_src.tar.gz", destfile = "./duckdb_r_src.tar.gz")
install.packages("duckdb_r_src.tar.gz", repo = NULL)

neonstore::neon_dir()
Sys.setenv(NEONSTORE_HOME = "/groups/rqthomas_lab/neonstore")
neonstore::neon_dir()

sites_all = c("LIRO", "TOOK", "SUGG", "BARC", "PRPO", "PRLA", "CRAM", "OSBS", "TOOL", "DCFS", "UNDE")

lake_sites = c("LIRO", "TOOK", "SUGG", "BARC", "PRPO", "PRLA", "CRAM")

tower_sites = c("OSBS", "TOOL", "DCFS", "UNDE", "TOOK")

stream_site <- "POSE"

met_products = c("DP1.00098.001", "DP1.00002.001", "DP1.00023.001", "DP1.00006.001", "DP1.00001.001")

water_product = c("DP1.20267.001", "DP1.20261.001", "DP1.20042.001", "DP1.20163.001", "DP1.20252.001", 
                  "DP1.20097.001","DP1.20093.001", "DP1.20033.001", "DP1.20288.001")
# -----------------------------------------------------------------------------------------------------------------

# Download the newest data from NEON
# -----------------------------------------------------------------------------------------------------------------

# Download met products
neonstore::neon_download(product = met_products, site = sites_all)

# Download water products
neonstore::neon_download(product = water_product, site = stream_site)

# -----------------------------------------------------------------------------------------------------------------
# Make the stored NEON data product a data table in R
# -----------------------------------------------------------------------------------------------------------------

# Unpack the stored files to be accessible in R

# This NEEDS TO BE RUN BEFORE RUNNING NEON_TABLE

neonstore::neon_store(table = "RH_30min-expanded")
neonstore::neon_store(table = "SAAT_30min-expanded")
neonstore::neon_store(table = "SLRNR_30min-expanded")
neonstore::neon_store(table = "SECPRE_30min-expanded")
neonstore::neon_store(table = "2DWSD_2min-expanded")

#The data are too large to pullin all at once and to compartmentalize. Thus, I have pulled each site in individually

# Humidity
humidity_lakes <- neonstore::neon_table(table = "RH_30min-expanded", site = lake_sites) %>% 
  select(endDateTime, RHMean, siteID)%>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("RHMean"), mean, na.rm = TRUE)%>%
  arrange(siteID, time)

humidity_towers <- neonstore::neon_table(table = "RH_30min-expanded", site = tower_sites) %>% 
  select(endDateTime, RHMean, siteID)%>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("RHMean"), mean, na.rm = TRUE)%>%
  arrange(siteID, time)

# Airtemp
airtemp_lakes <- neonstore::neon_table(table = "SAAT_30min-expanded", site = lake_sites) %>%
  select(endDateTime, tempSingleMean, siteID)%>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("tempSingleMean"), mean, na.rm = TRUE)%>%
  arrange(siteID, time)

airtemp_towers <- neonstore::neon_table(table = "SAAT_30min-expanded", site = tower_sites) %>%
  select(endDateTime, tempSingleMean, siteID)%>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("tempSingleMean"), mean, na.rm = TRUE)%>%
  arrange(siteID, time)

# Radiation
radiation_lakes <- neonstore::neon_table(table = "SLRNR_30min-expanded", site = lake_sites) %>%
  select(endDateTime, inSWMean, outLWMean, siteID) %>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("inSWMean", "outLWMean"), mean, na.rm = TRUE)%>%
  arrange(siteID, time)

radiation_towers <- neonstore::neon_table(table = "SLRNR_30min-expanded", site = tower_sites) %>%
  select(endDateTime, inSWMean, outLWMean, siteID) %>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("inSWMean", "outLWMean"), mean, na.rm = TRUE)%>%
  arrange(siteID, time)


precip  <- neonstore::neon_table(table = "SECPRE_30min-expanded", site = tower_sites) %>%
  select(endDateTime, secPrecipBulk, siteID) %>%
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

# Windspeed
windspeed_lakes <- neonstore::neon_table(table = "2DWSD_30min-expanded", site = lake_sites)%>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("windSpeedMean"), sum, na.rm = TRUE)%>%
  arrange(siteID, time)

windspeed_towers <- neonstore::neon_table(table = "2DWSD_30min-expanded", site = tower_sites)%>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time, siteID) %>%
  summarize_at(c("windSpeedMean"), sum, na.rm = TRUE)%>%
  arrange(siteID, time)

# -----------------------------------------------------------------------------------------------------------------

# Bind all of the met data together
# Precip will be added in the site specific development
# -----------------------------------------------------------------------------------------------------------------
NEON_met_data_hourly <- left_join(radiation_lakes, windspeed_lakes, by=c('time','siteID')) %>%
  left_join(., humidity_lakes, by=c('time','siteID'))%>%
  left_join(., airtemp_lakes, by=c('time','siteID'))%>%
  select(time,siteID,inSWMean,outLWMean,tempSingleMean,RHMean,windSpeedMean)

NEON_met_data_hourly_towers <- left_join(radiation_towers, windspeed_towers, by=c('time','siteID')) %>%
  left_join(., humidity_towers, by=c('time','siteID'))%>%
  left_join(., airtemp_towers, by=c('time','siteID'))%>%
  select(time,siteID,inSWMean,outLWMean,tempSingleMean,RHMean,windSpeedMean)

NEON_met_data_hourly_all <- bind_rows(NEON_met_data_hourly, NEON_met_data_hourly_towers)
# -----------------------------------------------------------------------------------------------------------------

# Make the BARCO and SUGG Met data files for the GLM run
BARC_met <- NEON_met_data_hourly_all %>% filter(siteID == c("BARC")) %>%
  left_join(., precip_barc, by=c('time', "siteID"))%>%
  select(-siteID)%>%
  rename(ShortWave = inSWMean, 
         LongWave = outLWMean, 
         AirTemp = tempSingleMean, 
         RelHum = RHMean, 
         Rain = secPrecipBulk,
         WindSpeed = windSpeedMean)%>%
  select(time, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain)

SUGG_met <- NEON_met_data_hourly_all %>% filter(siteID == c("SUGG")) %>%
  left_join(., precip_sugg, by=c('time', "siteID"))%>%
  select(-siteID)%>%
  rename(ShortWave = inSWMean, 
         LongWave = outLWMean, 
         AirTemp = tempSingleMean, 
         RelHum = RHMean, 
         Rain = secPrecipBulk,
         WindSpeed = windSpeedMean)%>%
  select(time, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain)

OSBS_met <- NEON_met_data_hourly_all %>% filter(siteID == c("OSBS")) %>%
  left_join(., precip_sugg, by=c('time', "siteID"))%>%
  select(-siteID)%>%
  rename(ShortWave = inSWMean, 
         LongWave = outLWMean, 
         AirTemp = tempSingleMean, 
         RelHum = RHMean, 
         Rain = secPrecipBulk,
         WindSpeed = windSpeedMean)%>%
  select(time, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain)

# Make the PRPO and PRLA Met data files for the GLM run
PRPO_met <- NEON_met_data_hourly_all %>% filter(siteID == c("PRPO")) %>%
  left_join(., precip_prpo, by=c('time', "siteID"))%>%
  select(-siteID)%>%
  rename(ShortWave = inSWMean, 
         LongWave = outLWMean, 
         AirTemp = tempSingleMean, 
         RelHum = RHMean, 
         Rain = secPrecipBulk,
         WindSpeed = windSpeedMean)%>%
  select(time, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain)

PRLA_met <- NEON_met_data_hourly_all %>% filter(siteID == c("PRLA")) %>%
  left_join(., precip_prla, by=c('time', "siteID"))%>%
  select(-siteID)%>%
  rename(ShortWave = inSWMean, 
         LongWave = outLWMean, 
         AirTemp = tempSingleMean, 
         RelHum = RHMean, 
         Rain = secPrecipBulk,
         WindSpeed = windSpeedMean)%>%
  select(time, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain)

# Make the PRPO and PRLA Met data files for the GLM run
CRAM_met <- NEON_met_data_hourly_all %>% filter(siteID == c("CRAM")) %>%
  left_join(., precip_cram, by=c('time', "siteID"))%>%
  select(-siteID)%>%
  rename(ShortWave = inSWMean, 
         LongWave = outLWMean, 
         AirTemp = tempSingleMean, 
         RelHum = RHMean, 
         Rain = secPrecipBulk,
         WindSpeed = windSpeedMean)%>%
  select(time, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain)

LIRO_met <- NEON_met_data_hourly_all %>% filter(siteID == c("LIRO")) %>%
  left_join(., precip_liro, by=c('time', "siteID"))%>%
  select(-siteID)%>%
  rename(ShortWave = inSWMean, 
         LongWave = outLWMean, 
         AirTemp = tempSingleMean, 
         RelHum = RHMean, 
         Rain = secPrecipBulk,
         WindSpeed = windSpeedMean)%>%
  select(time, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain)

TOOK_met <- NEON_met_data_hourly_all %>% filter(siteID == c("TOOK")) %>%
  left_join(., precip_took, by=c('time', "siteID"))%>%
  select(-siteID)%>%
  rename(ShortWave = inSWMean, 
         LongWave = outLWMean, 
         AirTemp = tempSingleMean, 
         RelHum = RHMean, 
         Rain = secPrecipBulk,
         WindSpeed = windSpeedMean)%>%
  select(time, ShortWave, LongWave, AirTemp, RelHum, WindSpeed, Rain)
