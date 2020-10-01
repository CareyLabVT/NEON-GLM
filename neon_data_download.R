#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   Practice_script_NeonStore.R                         *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    24Sep2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: Test the new CRAN neonstore package                  *      
#*****************************************************************

# Load packages
# -------------------------------------------------------------------

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, neonstore, lubridate, reshape2)

# -------------------------------------------------------------------

# Refer to the PDF for more information on neonstore 
# -------------------------------------------------------------------
#https://cran.r-project.org/web/packages/neonstore/neonstore.pdf
# -------------------------------------------------------------------

# Cite NEON
# -------------------------------------------------------------------
neon_citation()                     # Cites NEON as a whole

neon_citation("DP1.10003.001")      # Cites the specific data product used
# -------------------------------------------------------------------


# Set up a NEON data directory  ### This might be the hardest part for people to understand
# "C:\\Users\\Owner\\AppData\\Local\\neonstore\\neonstore"##!!! is there a list of the different directories tempdir(), etc.?
# -------------------------------------------------------------------

Sys.unsetenv("NEONSTORE_HOME")
neon_dir()

# -------------------------------------------------------------------

# This  code will aggregate the data to hourly. ---------------------

# -------------------------------------------------------------------


# Download NEON data product into the local neon_dir()
# -------------------------------------------------------------------
# Primary meteorological data needed to run GLM includes 
# Shortwave, Longwave, Airtemp, Relative Humidity, 
# Windspeed, Rain, and Snow (Precipitation)

# Relative Humidity = DP1.00098.001
# Air Temperature = DP1.00002.001
# Shortwave and Longwave radiation = DP1.00023.001
# Precipitation = DP1.00006.001
# Windspeed  = DP1.00001.001

# Download NEON meteorological data that will be GLM driver data
# This is the driver data from the meteorological station adjacent to the lakes
# Tried to concatenate all the products to download at once and crashed neonstore
# A huge initial download will take 4+ hours
# Make sure to have ample space open on your PC!!!

# Relative Humidity
neon_download(
  product = c("DP1.00098.001"),
  start_date = NA, #"2013-01-01"
  end_date = "2020-10-01", # mark as today's date
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  type = "expanded",
  file_regex = "[.]csv",
  quiet = FALSE,
  verify = TRUE,
  dir = neon_dir(),
  unzip = TRUE,
  api = "https://data.neonscience.org/api/v0",
  .token = Sys.getenv("NEON_TOKEN"))

# Air temperature
neon_download(
  product = c("DP1.00002.001"),
  start_date = NA , #"2013-01-01"
  end_date = "2020-10-01", # mark as today's date
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  type = "expanded",
  file_regex = "[.]csv",
  quiet = FALSE,
  verify = TRUE,
  dir = neon_dir(),
  unzip = TRUE,
  api = "https://data.neonscience.org/api/v0",
  .token = Sys.getenv("NEON_TOKEN"))

# Shortwave and longwave radiation
neon_download(
  product = c("DP1.00023.001"),
  start_date = NA , #"2013-01-01"
  end_date = "2020-10-01", # mark as today's date
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  type = "expanded",
  file_regex = "[.]csv",
  quiet = FALSE,
  verify = TRUE,
  dir = neon_dir(),
  unzip = TRUE,
  api = "https://data.neonscience.org/api/v0",
  .token = Sys.getenv("NEON_TOKEN"))

# Windspeed
neon_download(
  product = c("DP1.00001.001"),
  start_date = NA , #"2013-01-01"
  end_date = "2020-10-01", # mark as today's date
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  type = "expanded",
  file_regex = "[.]csv",
  quiet = FALSE,
  verify = TRUE,
  dir = neon_dir(),
  unzip = TRUE,
  api = "https://data.neonscience.org/api/v0",
  .token = Sys.getenv("NEON_TOKEN"))

# Precipitation
neon_download(
  product = c("DP1.00006.001"),
  start_date = NA , #"2013-01-01"
  end_date = "2020-10-01", # mark as today's date
  site = c("UNDE","KONA","TOOK","OSBS"),
  type = "expanded",
  file_regex = "[.]csv",
  quiet = FALSE,
  verify = TRUE,
  dir = neon_dir(),
  unzip = TRUE,
  api = "https://data.neonscience.org/api/v0",
  .token = Sys.getenv("NEON_TOKEN"))
# -------------------------------------------------------------------


# Extract the downloaded data and prepare it as a dataframe for GLM-AED. 
# -------------------------------------------------------------------
# Humidity
rel_hum_dat <- neon_read(
  table = "RH_30min-expanded",
  product = "DP1.00098.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE
) %>% select(startDateTime, Month, RHMean, siteID)

# AirTemp
air_temp_dat <- neon_read(
  table = "SAAT_30min-expanded",
  product = "DP1.00002.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE
) %>% select(startDateTime, Month, tempSingleMean, siteID)

#SW LW Radiation
radiation_dat <- neon_read(
  table = "SLRNR_30min-expanded",
  product = "DP1.00023.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE
) %>% select(startDateTime, inSWMean, outSWMean, inLWMean, outLWMean, siteID)%>%
  mutate(SWMean = inSWMean - outSWMean)%>%
  mutate(LWMean = inLWMean - outLWMean)%>%
  select(startDateTime, SWMean, LWMean, siteID)

# WindSpeed
wind_speed_dat <- neon_read(
  table = "2DWSD_30min-expanded",
  product = "DP1.00001.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE
) %>% select(startDateTime, windSpeedMean, siteID)

# Precipitation
precip_dat <- neon_read(
  table = "SECPRE_30min-expanded",
  product = "DP1.00006.001",
  site = c("UNDE","KONA","TOOK","OSBS"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE
) %>% select(startDateTime, secPrecipBulk, siteID)

# Change precip names to match the actual lake siteID identifier.
# Additionally, BARC and SUGGS and PRPO and PRLA met data can be interoperable.
precip_dat$siteID[precip_dat$siteID == "UNDE"] = "CRAM"
precip_dat$siteID[precip_dat$siteID == "KONA"] = "PRPO"
precip_dat$siteID[precip_dat$siteID == "OSBS"] = "BARC"

# -------------------------------------------------------------------

# Combine all of the datasets in a joint meteorological dataset
# -------------------------------------------------------------------
NEON_lake_met <- left_join(radiation_dat, air_temp_dat, by=c('startDateTime','siteID')) %>%
  left_join(., wind_speed_dat, by=c('startDateTime','siteID'))%>%
  left_join(., precip_dat, by=c('startDateTime','siteID'))%>%
  left_join(., rel_hum_dat, by=c('startDateTime','siteID'))%>%
  select(startDateTime,siteID,SWMean,LWMean,tempSingleMean,windSpeedMean,secPrecipBulk,RHMean)%>%
  mutate(year_month = format(as.Date(startDateTime, "%Y-%m-%d"), "%Y-%m"))%>%
  arrange(startDateTime)
# -------------------------------------------------------------------

CRAM_met <- NEON_lake_met %>% filter(siteID == "CRAM") %>%
  select(startDateTime, SWMean, LWMean, tempSingleMean, RHMean, windSpeedMean, secPrecipBulk)%>%
  mutate(time = lubridate::floor_date(startDateTime, unit = "hour"))%>%
  mutate(Snow = ifelse(tempSingleMean <= 0, secPrecipBulk, 0))%>%
  select(-startDateTime)%>%
  group_by(time) %>%
  summarize_all(funs(mean))%>%
  rename(ShortWave = SWMean, LongWave = LWMean, AirTemp = tempSingleMean, RelHum = RHMean, WindSpeed = windSpeedMean, Rain = secPrecipBulk)

# Make a Heatmap plot of the different met data
# -------------------------------------------------------------------
heatmap_neon <- NEON_lake_met %>% 
  group_by(year_month, siteID)%>%
  summarize_all(funs(sum(is.na(.)) / length(.)))%>%
  select(-startDateTime)

heatmap_neon <- melt(heatmap_neon, id=c("year_month","siteID"))

jpeg("./figures/met_heatmap.jpg", width = 1200, height = 500)
ggplot(heatmap_neon, aes(as.factor(year_month), variable, fill= 1-value)) + 
  geom_tile()+
  theme_classic()+
  scale_fill_distiller(palette = "Purples") +
  ylab("")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45, size = 10, color = "black", hjust=1.2),
        axis.text.y = element_text(size = 14, color = "black"))+
  facet_wrap(~siteID)
dev.off()
# ------------------------------------------------------------------------------