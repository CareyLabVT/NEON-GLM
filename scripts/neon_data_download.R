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
# mutate(DateTime = lubridate::floor_date(startDateTime, unit = "hour"))%>%
# select(-startDateTime)%>%
# group_by(DateTime) %>%
# summarize_all(funs(mean))
# -------------------------------------------------------------------



# Download NEON data product into the local neon_dir()
# Primary meteorological data needed to run GLM includes Shortwave, Longwave, Airtemp, Relative Humidity, 
# Windspeed, Rain, and Snow (Precipitation)

# Relative Humidity = DP1.00098.001
# Air Temperature = DP1.00002.001
# Shortwave and Longwave radiation = DP1.00023.001
# Precipitation = DP1.00006.001
# Windspeed  = DP1.00001.001

# Download meteorological data that will be GLM driver data
# Relative Humidity
neon_download(
  product = c("DP1.00098.001"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  type = "expanded",
  file_regex = "[.]csv",
  quiet = FALSE,
  verify = TRUE,
  dir = neon_dir(),
  unzip = TRUE,
  api = "https://data.neonscience.org/api/v0",
  .token = Sys.getenv("NEON_TOKEN")
)

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
)  mutate(Month = lubridate::month(startDateTime))%>%
  select(startDateTime, Month, RHMean, siteID)

# Air temperature
neon_download(
  product = c("DP1.00002.001"),
  start_date = NA , #"2013-01-01"
  end_date = "2020-09-29", # mark as today's date
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  type = "expanded",
  file_regex = "[.]csv",
  quiet = FALSE,
  verify = TRUE,
  dir = neon_dir(),
  unzip = TRUE,
  api = "https://data.neonscience.org/api/v0",
  .token = Sys.getenv("NEON_TOKEN")
)

head(neon_index("DP1.00002.001"))

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
)%>%
  mutate(Month = lubridate::month(startDateTime))%>%
  select(startDateTime, Month, tempSingleMean, siteID)

# Shortwave and longwave radiation
neon_download(
  product = c("DP1.00023.001"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  type = "expanded",
  file_regex = "[.]csv",
  quiet = FALSE,
  verify = TRUE,
  dir = neon_dir(),
  unzip = TRUE,
  api = "https://data.neonscience.org/api/v0",
  .token = Sys.getenv("NEON_TOKEN")
)

head(neon_index("DP1.00023.001"))

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
)%>%
  select(startDateTime, inSWMean, outSWMean, inLWMean, outLWMean, siteID)%>%
  mutate(SWMean = inSWMean - outSWMean)%>%
  mutate(LWMean = inLWMean - outLWMean)%>%
  mutate(Month = lubridate::month(startDateTime))%>%
  select(startDateTime, Month, SWMean, LWMean, siteID)

# Windspeed
neon_download(
  product = c("DP1.00001.001"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  type = "expanded",
  file_regex = "[.]csv",
  quiet = FALSE,
  verify = TRUE,
  dir = neon_dir(),
  unzip = TRUE,
  api = "https://data.neonscience.org/api/v0",
  .token = Sys.getenv("NEON_TOKEN")
)

head(neon_index("DP1.00001.001"))

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
)%>%
  mutate(Month = lubridate::month(startDateTime))%>%
  select(startDateTime, Month, windSpeedMean, siteID)
 

# Precipitation
neon_download(
  product = c("DP1.00006.001"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  site = c("UNDE","KONA","TOOK","OSBS"),
  type = "expanded",
  file_regex = "[.]csv",
  quiet = FALSE,
  verify = TRUE,
  dir = neon_dir(),
  unzip = TRUE,
  api = "https://data.neonscience.org/api/v0",
  .token = Sys.getenv("NEON_TOKEN")
)

see <- head(neon_index("DP1.00006.001"))

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
) %>%
  mutate(Month = lubridate::month(startDateTime))%>%
  select(startDateTime, Month, secPrecipBulk, siteID)

precip_dat$siteID[precip_dat$siteID == "UNDE"] = "CRAM"
precip_dat$siteID[precip_dat$siteID == "KONA"] = "PRPO"
precip_dat$siteID[precip_dat$siteID == "OSBS"] = "BARC"

NEON_lake_met <- left_join(radiation_dat, air_temp_dat, by=c('startDateTime','siteID')) %>%
  left_join(., wind_speed_dat, by=c('startDateTime','siteID'))%>%
  left_join(., precip_dat, by=c('startDateTime','siteID'))%>%
  left_join(., rel_hum_dat, by=c('startDateTime','siteID'))%>%
  select(startDateTime,siteID,SWMean,LWMean,tempSingleMean,windSpeedMean,secPrecipBulk,RHMean)%>%
  mutate(year_month = format(as.Date(startDateTime, "%Y-%m-%d"), "%Y-%m"))%>%
  arrange(startDateTime)

# Make a Heatmap plot of the different met data -----------------------------

heatmap_neon <- NEON_lake_met %>% 
  group_by(year_month, siteID)%>%
  summarize_all(funs(sum(is.na(.)) / length(.)))%>%
  select(-startDateTime)

heatmap_neon <- melt(heatmap_neon, id=c("year_month","siteID"))

jpeg("./figures/met_heatmap.jpg", width = 7000, height = 3000)
ggplot(heatmap_neon, aes(as.factor(year_month), variable, fill= value)) + 
  geom_tile()+
  scale_fill_distiller(palette = "RdPu") +
  theme(axis.text.x = element_text(angle = 45))+
  facet_wrap(~siteID)
dev.off()
# ------------------------------------------------------------------------------