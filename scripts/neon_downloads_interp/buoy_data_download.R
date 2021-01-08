#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   Observed data download                               *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    30Oct2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: Downlad the met data and get it in a GLM format      *
#*****************************************************************

# Get packages
if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, reshape2, devtools, patchwork)

# Bypass the latest CRAN version of neonstore and use Carl's most recent Github push
devtools::install_github("cboettig/neonstore", force = F)

lake_sites = c("LIRO", "TOOK", "SUGG", "BARC", "PRPO", "PRLA", "CRAM")

water_product = c("DP1.20267.001", "DP1.20261.001", "DP1.20042.001", "DP1.20163.001", "DP1.20252.001", 
                  "DP1.20097.001","DP1.20093.001", "DP1.20033.001", "DP1.20288.001", "DP1.20016.001")

# Download water products
Sys.setenv("NEONSTORE_HOME" = "/groups/rqthomas_lab/neonstore")
neonstore::neon_download(product = "DP1.20288.001", site = lake_sites)

# Unpack the stored files to be accessible in R

# This NEEDS TO BE RUN BEFORE RUNNING NEON_TABLE
neonstore::neon_store(table = "dep_secchi-basic")
neonstore::neon_store(table = "sdg_externalLabData-expanded")
neonstore::neon_store(table = "swc_externalLabDataByAnalyte-expanded")
neonstore::neon_store(table = "alg_algaeExternalLabDataPerSample-expanded")
neonstore::neon_store(table = "uPAR_30min-expanded")
neonstore::neon_store(table = "NSW_15_minute-expanded")
neonstore::neon_store(table = "waq_instantaneous-basic")
neonstore::neon_store(table = "TSD_30_min-expanded")
neonstore::neon_store(table = "EOS_30_min-expanded")

## Secchi depth
secchi <- neonstore::neon_table(table = "dep_secchi-basic", site = lake_sites) %>%
  select(date, siteID, secchiMeanDepth) %>%
  arrange(siteID, date)%>%
  rename(value = secchiMeanDepth)%>%
  mutate(variable = "secchi")%>%
  mutate(Depth = "NA")




# Lake level in meters above sea level
# ----------------------------------------------------------------------------------------

water_level <- neonstore::neon_table(table = "EOS_30_min-expanded", site = lake_sites)%>%
  select(endDateTime,siteID,surfacewaterElevMean)%>%
  group_by(endDateTime)%>%
  arrange(siteID, endDateTime)%>%
  rename(value = surfacewaterElevMean)%>%
  mutate(variable = "waterlevel")%>%
  mutate(Depth = "NA")


# Build the observed water level data for each NEON site. 
water_level_barc <- water_level %>% filter(siteID == "BARC") %>%
  mutate(daily = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  group_by(daily)%>%
  summarise_all(funs(mean), na.rm = T)%>%
  select(-endDateTime)%>%
  rename(DateTime = daily)%>%
  arrange(DateTime)%>%
  select(DateTime, value)%>%
  mutate(surface_height = value-19.2)%>%
  select(DateTime, surface_height)

water_level_sugg <- water_level %>% filter(siteID == "SUGG") %>%
  mutate(daily = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  group_by(daily)%>%
  summarise_all(funs(mean), na.rm = T)%>%
  select(-endDateTime)%>%
  rename(DateTime = daily)%>%
  arrange(DateTime)%>%
  select(DateTime, value)%>%
  mutate(surface_height = value-25.9)%>%
  select(DateTime, surface_height)

water_level_cram <- water_level %>% filter(siteID == "CRAM") %>%
  mutate(daily = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  group_by(daily)%>%
  summarise_all(funs(mean), na.rm = T)%>%
  select(-endDateTime)%>%
  rename(DateTime = daily)%>%
  arrange(DateTime)%>%
  select(DateTime, value)%>%
  mutate(surface_height = value-25.8)%>%
  select(DateTime, surface_height)




water_level_barc <- water_level %>% filter(siteID == "BARC") %>%
  mutate(daily = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  group_by(daily)%>%
  summarise_all(funs(mean))%>%
  select(-endDateTime)%>%
  rename(DateTime = daily)%>%
  arrange(DateTime)%>%
  select(DateTime, value)%>%
  mutate(surface_height = value-19.2)%>%
  select(DateTime, surface_height)
# ----------------------------------------------------------------------------------------



# Water temperature by depth
# ----------------------------------------------------------------------------------------
water_temp <- neonstore::neon_table(table = "TSD_30_min-expanded",site = lake_sites)%>% 
  select(endDateTime, siteID, thermistorDepth, tsdWaterTempMean) %>%
  arrange(siteID, endDateTime, thermistorDepth)%>%
  rename(Depth = thermistorDepth)%>%
  rename(temp = tsdWaterTempMean)%>%
  mutate(variable = "watertemperature")

# Extract water temperatures for each NEON site
water_temp_barc <- water_temp %>% filter(siteID == "BARC") %>%
  mutate(DateTime = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  group_by(DateTime, Depth)%>%
  summarise_all(funs(mean))%>%
  filter(temp > 10)%>%
  select(DateTime, Depth, temp) %>% write_csv(.,'./observations/CleanedObsTempBARC.csv')

water_temp_sugg <- water_temp %>% filter(siteID == "SUGG") %>%
  mutate(DateTime = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  group_by(DateTime, Depth)%>%
  summarise_all(funs(mean))%>%
  filter(temp > 10)%>%
  select(DateTime, Depth, temp)%>%write_csv(.,'./observations/CleanedObsTempSUGG.csv')

water_temp_cram <- water_temp %>% filter(siteID == "CRAM") %>%
  mutate(DateTime = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  group_by(DateTime, Depth)%>%
  summarise_all(funs(mean))%>%
  filter(temp > 2)%>%
  select(DateTime, Depth, temp)%>%write_csv(.,'./observations/CleanedObsTempCRAM.csv')

water_temp_liro <- water_temp %>% filter(siteID == "LIRO") %>%
  mutate(DateTime = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  group_by(DateTime, Depth)%>%
  summarise_all(funs(mean))%>%
  filter(temp > 2)%>%
  select(DateTime, Depth, temp)%>%write_csv(.,'./observations/CleanedObsTempLIRO.csv')

water_temp_prla <- water_temp %>% filter(siteID == "PRLA") %>%
  mutate(DateTime = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  group_by(DateTime, Depth)%>%
  summarise_all(funs(mean))%>%
  filter(temp > 2)%>%
  select(DateTime, Depth, temp)%>%write_csv(.,'./observations/CleanedObsTempPRLA.csv')


water_temp_prpo <- water_temp %>% filter(siteID == "PRPO") %>%
  mutate(DateTime = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  group_by(DateTime, Depth)%>%
  summarise_all(funs(mean))%>%
  filter(temp > 2)%>%
  select(DateTime, Depth, temp)%>%write_csv(.,'./observations/CleanedObsTempPRPO.csv')

water_temp_took <- water_temp %>% filter(siteID == "TOOK") %>%
  mutate(DateTime = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  group_by(DateTime, Depth)%>%
  summarise_all(funs(mean))%>%
  filter(temp > 2)%>%
  select(DateTime, Depth, temp)%>%write_csv(.,'./observations/CleanedObsTempTOOK.csv')
# ----------------------------------------------------------------------------------------



# ----------------------------------------------------------------------------------------



# Water quality from Sonde at surface (Conducatnce, DO, pSAT, pH, chla, turb, FDOM)
surface_sonde <- neonstore::neon_table(table = "waq_instantaneous-basic", site = lake_sites)%>% 
  select(endDateTime, 
         sensorDepth, 
         siteID, 
         specificConductance, 
         dissolvedOxygen, 
         dissolvedOxygenSaturation, 
         pH, 
         chlorophyll,
         turbidity,
         fDOM)%>%
  arrange(siteID,sensorDepth,endDateTime)






water_temp_barc <- water_temp %>% filter(siteID == "BARC") %>%
  mutate(DateTime = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  filter(DateTime>="2018-01-01") %>%
  filter(DateTime<="2020-07-31") %>%
  group_by(DateTime, Depth)%>%
  summarise_all(funs(mean))%>%
  filter(temp > 10)%>%
  select(DateTime, Depth, temp) %>% write_csv(.,'./observations/CleanedObsTempBARC.csv')

water_temp_sugg <- water_temp %>% filter(siteID == "SUGG") %>%
  mutate(DateTime = format(as.Date(endDateTime, "%Y-%m-%d"), "%Y-%m-%d"))%>%
  filter(DateTime>="2018-01-01") %>%
  filter(DateTime<="2020-07-31") %>%
  group_by(DateTime, Depth)%>%
  summarise_all(funs(mean))%>%
  filter(temp > 10)%>%
  select(DateTime, Depth, temp)%>%write_csv(.,'./observations/CleanedObsTempSUGG.csv')
# ------------------------------------------------------------------------------------------ 

