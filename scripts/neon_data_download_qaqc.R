#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   State_Variable__and_driver_download_script_NeonStore *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    02Oct2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: Test the new CRAN neonstore package                  *
#* HELPERS: Quinn Thomas and Carl Boetiiger                      *
#*****************************************************************

# Load packages
# -------------------------------------------------------------------

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, neonstore, lubridate, reshape2)

# -------------------------------------------------------------------
# Download NEON data products into the local neon_dir()
# -------------------------------------------------------------------

# Water metrics for GLM-AED
# This includes multiple data products that are both drivers and 
# state variables that are used in the general lake model. 

# The current data products include: 
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

site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK")

lapply(water_var_product, neon_download, site = site, 
       start_date = "2013-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")


# Primary meteorological data needed to run GLM includes 
# Shortwave, Longwave, Airtemp, Relative Humidity, 
# Windspeed, Rain, and Snow (Precipitation)

# Relative Humidity = DP1.00098.001
# Air Temperature = DP1.00002.001
# Shortwave and Longwave radiation = DP1.00023.001
# Precipitation = DP1.00006.001
# Windspeed  = DP1.00001.001
# Bathymetry = DP4.00132.001

met_product = c("DP1.00098.001", "DP1.00002.001", "DP1.00023.001",
                "DP1.00006.001", "DP1.00001.001", "DP4.00132.001")

site = site

lapply(met_product, neon_download, site = site, 
       start_date = "2013-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")


# -------------------------------------------------------------------
# Extract the data products from NEON directory as data frame
# -------------------------------------------------------------------

# Individually extract and tidy up each of the sensor and 
# observed variables in the lakes

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
  altrep = FALSE)%>%
  select(startDateTime,siteID,surfacewaterElevMean)%>%
  mutate(year_month = format(as.Date(startDateTime, "%Y-%m-%d"), "%Y-%m"))%>%
  arrange(siteID, startDateTime)

# Water temperature by depth
water_temp <- neon_read(
  table = "TSD_30_min-expanded",
  product = "DP1.20264.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE)%>% 
  select(startDateTime, thermistorDepth, tsdWaterTempMean, siteID) %>%
  mutate(year_month = format(as.Date(startDateTime, "%Y-%m-%d"), "%Y-%m"))%>% 
  arrange(siteID, startDateTime, thermistorDepth)

# Water quality from Sonde at surface (Conducatnce, DO, pSAT, pH, chla, turb, FDOM)
water_quality <- neon_read(
  table = c("waq_instantaneous-expanded"),
  product = "DP1.20288.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE)%>% 
  select(startDateTime, 
             sensorDepth, 
             siteID, 
             specificConductance, 
             dissolvedOxygen, 
             dissolvedOxygenSaturation, 
             pH, 
             chlorophyll,
             turbidity,
             fDOM)%>%
  mutate(year_month = format(as.Date(startDateTime, "%Y-%m-%d"), "%Y-%m"))%>% 
  arrange(siteID,sensorDepth,startDateTime)

# Surface nitrate concentrations
Nitrate <- neon_read(
  table = c("NSW_15_minute-expanded"),
  product = "DP1.20033.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE)%>% 
  select(startDateTime, siteID, surfWaterNitrateMean)


# PAR just below surface
PAR_below_surface <- neon_read(
  table = c("uPAR_30min-expanded"),
  product = "DP1.20261.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE)%>% 
  select(startDateTime, siteID, uPARMean)

# Manual data collections from NEON
# Surface water seston, carbon, and chlorophyll a
chemistry_analytes <- neon_read(
  table = c("alg_algaeExternalLabDataPerSample-expanded"),
  product = "DP1.20163.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE)%>% 
  select(collectDate, siteID, namedLocation, analyte, analyteConcentration, plantAlgaeLabUnits) %>%
  filter(grepl('buoy', namedLocation))%>%
  arrange(siteID,analyte,collectDate)%>%
  select(collectDate, siteID, analyte, analyteConcentration, plantAlgaeLabUnits)%>%
  rename(value = analyteConcentration, units = plantAlgaeLabUnits)%>%
  mutate(collectDate = format(as.Date(collectDate, "%Y-%m-%d HH:MM:SS"), "%Y-%m-%d"))


# Chemical propoerties of surface water
chemistry_surface <- neon_read(
  table = c("swc_externalLabDataByAnalyte-expanded"),
  product = "DP1.20093.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE
) %>% select(collectDate, siteID, namedLocation, analyte, analyteConcentration, analyteUnits) %>%
  filter(grepl('buoy', namedLocation))%>%
  arrange(siteID,analyte,collectDate)%>%
  rename(units = analyteUnits)%>%
  rename(value = analyteConcentration)%>%
  select(-namedLocation)%>%
  mutate(collectDate = format(as.Date(collectDate, "%Y-%m-%d HH:MM:SS"), "%Y-%m-%d"))

# Dissolved CH4, CO2, NO2, in water and air
dissolved_gases <- neon_read(
  table = c("sdg_externalLabData-expanded"),
  product = "DP1.20097.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE)%>% 
  select(collectDate, siteID, sampleID, concentrationCH4, concentrationCO2, concentrationN2O)%>%
  arrange(siteID,collectDate)%>%
  gather(greenhouseGas, concentraion, concentrationCH4:concentrationN2O, na.rm = FALSE, convert = FALSE)%>%
  filter(grepl('C0', sampleID))%>%
  mutate(year_month = format(as.Date(collectDate, "%Y-%m-%d"), "%Y-%m"))

water_surface_dissolved_ghg <- dissolved_gases %>% filter(grepl('WAT', sampleID)) %>%
  arrange(siteID, collectDate) %>% 
  select(-sampleID)
water_surface_dissolved_ghg$greenhouseGas <-  paste0("water_", water_surface_dissolved_ghg$greenhouseGas)
water_surface_dissolved_ghg <- water_surface_dissolved_ghg %>% 
  rename(analyte = greenhouseGas)%>%
  rename(value = concentraion)%>%
  mutate(units = "ppmv")%>%
  mutate(collectDate = format(as.Date(collectDate, "%Y-%m-%d HH:MM:SS"), "%Y-%m-%d"))

air_ghg <- dissolved_gases %>% filter(grepl('AIR', sampleID)) %>%
  arrange(siteID, collectDate) %>% 
  select(-sampleID)
air_ghg$greenhouseGas <-  paste0("air_", air_ghg$greenhouseGas)
air_ghg <- air_ghg %>% 
  rename(analyte = greenhouseGas)%>%
  rename(value = concentraion)%>%
  mutate(units = "ppmv")%>%
  mutate(collectDate = format(as.Date(collectDate, "%Y-%m-%d HH:MM:SS"), "%Y-%m-%d"))

# Secchi depth
secchi <- neon_read(
  table = c("dep_secchi-basic"),
  product = "DP1.20252.001",
  site = c("BARC","CRAM","PRLA", "PRPO", "SUGG", "TOOK"),
  start_date = "2013-01-01",
  end_date = "2020-08-01",
  ext = "csv",
  timestamp = NA,
  dir = neon_dir(),
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE)

# Secchi data also reports whether Ice is on the lake or not.
# Turned icePresent (Y/N) into a value (0/1)
ice_bind <- secchi %>% select(date, siteID, icePresent) %>%
  rename(collectDate = date)%>%
  mutate(units = "Unitless")%>%
  mutate(value = ifelse(grepl("N", icePresent), 0, 1))%>%
  mutate(analyte = "icePresent")%>%
  select(collectDate, siteID, analyte, value, units)%>%
  mutate(collectDate = format(as.Date(collectDate, "%Y-%m-%d HH:MM:SS"), "%Y-%m-%d"))

secchi_bind <- secchi %>% select(date, siteID, secchiMeanDepth) %>%
  rename(collectDate = date)%>%
  mutate(units = "Meters")%>%
  mutate(analyte = "secchiDepth")%>%
  rename(value = secchiMeanDepth)%>%
  select(collectDate, siteID, analyte, value, units)%>%
  mutate(collectDate = format(as.Date(collectDate, "%Y-%m-%d HH:MM:SS"), "%Y-%m-%d"))

# Append all of the manually collected data together 
manual_collection <- bind_rows(air_ghg, 
                               water_surface_dissolved_ghg, 
                               secchi_bind, 
                               ice_bind, 
                               chemistry_surface, 
                               chemistry_analytes)%>%
  arrange(siteID,analyte,collectDate)%>%
  mutate(year_month = format(as.Date(collectDate, "%Y-%m-%d"), "%Y-%m"))

# Individually exract each of the meteorological variables 
# from the weather station at the lake's shoreline

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
) %>% select(startDateTime, RHMean, siteID)

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
) %>% select(startDateTime, tempSingleMean, siteID)

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
  altrep = FALSE)%>% 
  select(startDateTime, secPrecipBulk, siteID)

# Change precip names to match the actual lake siteID identifier.
# Additionally, BARC and SUGGS and PRPO and PRLA met data can be interoperable.
precip_dat$siteID[precip_dat$siteID == "UNDE"] = "CRAM"
precip_dat$siteID[precip_dat$siteID == "KONA"] = "PRPO"
precip_dat$siteID[precip_dat$siteID == "OSBS"] = "BARC"


# -------------------------------------------------------------------
# Make a Heatmap plot of the different data products
# -------------------------------------------------------------------

# Water temperature heatmaps by depth
heatmap_water_temp <- water_temp %>% 
  group_by(year_month, siteID, thermistorDepth)%>%
  summarize_all(funs(sum(is.na(.)) / length(.)))%>%
  select(-startDateTime)

heatmap_water_temp <- melt(heatmap_water_temp, id=c("year_month","siteID","thermistorDepth"))

pdf("./figures/water_temperature_heatmap.pdf", width = 20, height = 12)
ggplot(heatmap_water_temp, aes(as.factor(year_month), as.factor(-thermistorDepth), fill= 1-value)) + 
  geom_tile()+
  theme_classic()+
  scale_fill_distiller(palette = "Spectral",trans = "reverse") +
  ylab("DEPTH")+
  xlab("YYYY-MM")+
  theme(axis.text.x = element_text(angle = 45, size = 10, color = "black", hjust=1.2),
        axis.text.y = element_text(size = 14, color = "black"))+
  facet_wrap(~siteID, scales = "free")
dev.off()



# Combine the sensored water quality variables to make a heatmap
heatmap_water_quality <- left_join(water_quality, Nitrate, by=c('startDateTime','siteID')) %>%
  left_join(., PAR_below_surface, by=c('startDateTime','siteID'))%>%
  left_join(., water_level, by=c('startDateTime','siteID'))%>%
  mutate(year_month = format(as.Date(startDateTime, "%Y-%m-%d"), "%Y-%m"))%>%
  arrange(startDateTime)%>%
  group_by(year_month, siteID)%>%
  summarize_all(funs(sum(is.na(.)) / length(.)))%>%
  select(-startDateTime, -sensorDepth, -year_month.x, -year_month.y)

heatmap_water_quality <- melt(heatmap_water_quality, id=c("year_month","siteID"))

pdf("./figures/water_quality_heatmap.pdf", width = 20, height = 12)
ggplot(heatmap_water_quality, aes(as.factor(year_month), variable, fill= 1-value)) + 
  geom_tile()+
  theme_classic()+
  scale_fill_distiller(palette = "Spectral",trans = "reverse") +
  ylab("")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45, size = 10, color = "black", hjust=1.2),
        axis.text.y = element_text(size = 14, color = "black"))+
  facet_wrap(~siteID)
dev.off()


# Manually collected data heatmap
manual_collection_heatmap <- manual_collection %>% group_by(year_month, siteID, analyte)%>%
  summarise(value = sum(is.na(value)) / length(value))

pdf("./figures/manual_collection_heatmap.pdf", width = 30, height = 22)
ggplot(manual_collection_heatmap, aes(as.factor(year_month), analyte, fill= 1-value)) + 
  geom_tile()+
  theme_classic()+
  scale_fill_distiller(palette = "Spectral",trans = "reverse") +
  ylab("")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45, size = 10, color = "black", hjust=1.2),
        axis.text.y = element_text(size = 14, color = "black"))+
  facet_wrap(~siteID)
dev.off()

# Weather driver data heatmap
heatmap_met_neon <- NEON_lake_met %>% 
  group_by(year_month, siteID)%>%
  summarize_all(funs(sum(is.na(.)) / length(.)))%>%
  select(-startDateTime)

heatmap_met_neon <- melt(heatmap_met_neon, id=c("year_month","siteID"))

pdf("./figures/met_heatmap.pdf", width = 20, height = 12)
ggplot(heatmap_met_neon, aes(as.factor(year_month), variable, fill= 1-value)) + 
  geom_tile()+
  theme_classic()+
  scale_fill_distiller(palette = "Spectral",trans = "reverse") +
  ylab("")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45, size = 10, color = "black", hjust=1.2),
        axis.text.y = element_text(size = 14, color = "black"))+
  facet_wrap(~siteID)
dev.off()

# -------------------------------------------------------------------
# Combine all of the met datasets in a joint meteorological dataset
# -------------------------------------------------------------------
NEON_lake_met <- left_join(radiation_dat, air_temp_dat, by=c('startDateTime','siteID')) %>%
  left_join(., wind_speed_dat, by=c('startDateTime','siteID'))%>%
  left_join(., precip_dat, by=c('startDateTime','siteID'))%>%
  left_join(., rel_hum_dat, by=c('startDateTime','siteID'))%>%
  select(startDateTime,siteID,SWMean,LWMean,tempSingleMean,windSpeedMean,secPrecipBulk,RHMean)%>%
  mutate(year_month = format(as.Date(startDateTime, "%Y-%m-%d"), "%Y-%m"))%>%
  arrange(startDateTime)

# -------------------------------------------------------------------
# Explore hourly Time series meteorological data
# -------------------------------------------------------------------
# Hourly met data for Crampton
CRAM_met <- NEON_lake_met %>% filter(siteID == "CRAM") %>%
  select(startDateTime, SWMean, LWMean, tempSingleMean, RHMean, windSpeedMean, secPrecipBulk)%>%
  mutate(time = lubridate::floor_date(startDateTime, unit = "hour"))%>%
  mutate(Snow = ifelse(tempSingleMean <= 0, secPrecipBulk, 0))%>%
  select(-startDateTime)%>%
  group_by(time) %>%
  summarize_all(funs(mean))%>%
  rename(ShortWave = SWMean, LongWave = LWMean, AirTemp = tempSingleMean, RelHum = RHMean, WindSpeed = windSpeedMean, Rain = secPrecipBulk)

CRAM_met_ts <- melt(CRAM_met, id=c("time"))%>% 
  ggplot(aes(time, value))+
  geom_point(size = 0.5)+
  theme_classic()+
  facet_wrap(~variable, scales = "free_y")

pdf("./figures/cram_met_time_series.pdf", width = 20, height = 12)
CRAM_met_ts
dev.off() 

# Hourly met data for Barco
BARC_met <- NEON_lake_met %>% filter(siteID == "BARC") %>%
  select(startDateTime, SWMean, LWMean, tempSingleMean, RHMean, windSpeedMean, secPrecipBulk)%>%
  mutate(time = lubridate::floor_date(startDateTime, unit = "hour"))%>%
  mutate(Snow = ifelse(tempSingleMean <= 0, secPrecipBulk, 0))%>%
  select(-startDateTime)%>%
  group_by(time) %>%
  summarize_all(funs(mean))%>%
  rename(ShortWave = SWMean, LongWave = LWMean, AirTemp = tempSingleMean, RelHum = RHMean, WindSpeed = windSpeedMean, Rain = secPrecipBulk)

BARC_met_ts <- melt(BARC_met, id=c("time"))%>% 
  ggplot(aes(time, value))+
  geom_point(size = 0.5)+
  theme_classic()+
  facet_wrap(~variable, scales = "free_y")

pdf("./figures/barc_met_time_series.pdf", width = 20, height = 12)
BARC_met_ts
dev.off() 

# Hourly met data for Suggs
SUGG_met <- NEON_lake_met %>% filter(siteID == "SUGG") %>%
  select(startDateTime, SWMean, LWMean, tempSingleMean, RHMean, windSpeedMean, secPrecipBulk)%>%
  mutate(time = lubridate::floor_date(startDateTime, unit = "hour"))%>%
  mutate(Snow = ifelse(tempSingleMean <= 0, secPrecipBulk, 0))%>%
  select(-startDateTime)%>%
  group_by(time) %>%
  summarize_all(funs(mean))%>%
  rename(ShortWave = SWMean, LongWave = LWMean, AirTemp = tempSingleMean, RelHum = RHMean, WindSpeed = windSpeedMean, Rain = secPrecipBulk)

SUGG_met_ts <- melt(SUGG_met, id=c("time"))%>% 
  ggplot(aes(time, value))+
  geom_point(size = 0.5)+
  theme_classic()+
  facet_wrap(~variable, scales = "free_y")

pdf("./figures/sugg_met_time_series.pdf", width = 20, height = 12)
SUGG_met_ts
dev.off() 

# Hourly met data for PRPO
PRPO_met <- NEON_lake_met %>% filter(siteID == "PRPO") %>%
  select(startDateTime, SWMean, LWMean, tempSingleMean, RHMean, windSpeedMean, secPrecipBulk)%>%
  mutate(time = lubridate::floor_date(startDateTime, unit = "hour"))%>%
  mutate(Snow = ifelse(tempSingleMean <= 0, secPrecipBulk, 0))%>%
  select(-startDateTime)%>%
  group_by(time) %>%
  summarize_all(funs(mean))%>%
  rename(ShortWave = SWMean, LongWave = LWMean, AirTemp = tempSingleMean, RelHum = RHMean, WindSpeed = windSpeedMean, Rain = secPrecipBulk)

PRPO_met_ts <- melt(PRPO_met, id=c("time"))%>% 
  ggplot(aes(time, value))+
  geom_point(size = 0.5)+
  theme_classic()+
  facet_wrap(~variable, scales = "free_y")

pdf("./figures/prpo_met_time_series.pdf", width = 20, height = 12)
PRPO_met_ts
dev.off() 

# Hourly met data for PRLA
PRLA_met <- NEON_lake_met %>% filter(siteID == "PRLA") %>%
  select(startDateTime, SWMean, LWMean, tempSingleMean, RHMean, windSpeedMean, secPrecipBulk)%>%
  mutate(time = lubridate::floor_date(startDateTime, unit = "hour"))%>%
  mutate(Snow = ifelse(tempSingleMean <= 0, secPrecipBulk, 0))%>%
  select(-startDateTime)%>%
  group_by(time) %>%
  summarize_all(funs(mean))%>%
  rename(ShortWave = SWMean, LongWave = LWMean, AirTemp = tempSingleMean, RelHum = RHMean, WindSpeed = windSpeedMean, Rain = secPrecipBulk)

PRLA_met_ts <- melt(PRLA_met, id=c("time"))%>% 
  ggplot(aes(time, value))+
  geom_point(size = 0.5)+
  theme_classic()+
  facet_wrap(~variable, scales = "free_y")

pdf("./figures/prla_met_time_series.pdf", width = 20, height = 12)
PRLA_met_ts
dev.off() 

# Hourly met data for TOOK
TOOK_met <- NEON_lake_met %>% filter(siteID == "TOOK") %>%
  select(startDateTime, SWMean, LWMean, tempSingleMean, RHMean, windSpeedMean, secPrecipBulk)%>%
  mutate(time = lubridate::floor_date(startDateTime, unit = "hour"))%>%
  mutate(Snow = ifelse(tempSingleMean <= 0, secPrecipBulk, 0))%>%
  select(-startDateTime)%>%
  group_by(time) %>%
  summarize_all(funs(mean))%>%
  rename(ShortWave = SWMean, LongWave = LWMean, AirTemp = tempSingleMean, RelHum = RHMean, WindSpeed = windSpeedMean, Rain = secPrecipBulk)

TOOK_met_ts <- melt(TOOK_met, id=c("time"))%>% 
  ggplot(aes(time, value))+
  geom_point(size = 0.5)+
  theme_classic()+
  facet_wrap(~variable, scales = "free_y")

pdf("./figures/took_met_time_series.pdf", width = 20, height = 12)
TOOK_met_ts
dev.off() 
