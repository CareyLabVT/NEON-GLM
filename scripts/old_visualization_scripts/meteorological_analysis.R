#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   Meteorology data collation and comparisons           *
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

sites = c("TOOK", "SUGG", "BARC", "PRPO", "PRLA", "CRAM", "OSBS", "TOOL", "DCFS", "UNDE")
lake_sites = c("TOOK", "SUGG", "BARC", "PRPO", "PRLA", "CRAM")
tower_sites = c("OSBS", "TOOL", "DCFS", "UNDE")


# Download the newest data from NEON
met_product_hum = "DP1.00098.001"                                                       # Humidity
lapply(met_product_hum, neonstore::neon_download, site = sites, 
       start_date = "2014-01-01", end_date = NA,
       file_regex = "[.]csv")

met_product_airT = "DP1.00002.001"                                                      # Air Temperature
lapply(met_product_airT, neonstore::neon_download, site = sites, 
       start_date = "2014-01-01", end_date = NA,
       file_regex = "[.]csv")

met_product_rad = "DP1.00023.001"                                                       # SW & LW Radiation
lapply(met_product_rad, neonstore::neon_download, site = sites, 
       start_date = "2014-01-01", end_date = NA,
       file_regex = "[.]csv")

met_product_precip = "DP1.00006.001"                                                    # Precipitation
lapply(met_product_precip, neonstore::neon_download, site = sites, 
       start_date = "2014-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")

met_product_wind = "DP1.00001.001"                                                      # 2D Windspeed
lapply(met_product_wind, neonstore::neon_download, site = sites, 
       start_date = "2014-01-01", end_date = NA,
       file_regex = "[.]csv")


# Unpack the stored files to be accessible in R
# Currently using the 30min aggregated data for most variables except Windspeed
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












NEON_lake_met <- left_join(radiation_dat, air_temp_dat, by=c('startDateTime','siteID')) %>%
  left_join(., wind_speed_dat, by=c('startDateTime','siteID'))%>%
  left_join(., precip_dat, by=c('startDateTime','siteID'))%>%
  left_join(., rel_hum_dat, by=c('startDateTime','siteID'))%>%
  select(startDateTime,siteID,SWMean,LWMean,tempSingleMean,windSpeedMean,secPrecipBulk,RHMean)%>%
  mutate(year_month = format(as.Date(startDateTime, "%Y-%m-%d"), "%Y-%m"))%>%
  arrange(startDateTime)

# Lake hum
rel_hum_dat_BARC <- rel_hum_dat %>% filter(siteID == "BARC") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
rel_hum_dat_SUGG <- rel_hum_dat %>% filter(siteID == "SUGG") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
rel_hum_dat_TOOK <- rel_hum_dat %>% filter(siteID == "TOOK") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
rel_hum_dat_PRPO <- rel_hum_dat %>% filter(siteID == "PRPO") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
rel_hum_dat_PRLA <- rel_hum_dat %>% filter(siteID == "PRLA") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
rel_hum_dat_CRAM <- rel_hum_dat %>% filter(siteID == "CRAM") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)

# EC tower hum
rel_hum_dat_OSBS <- rel_hum_dat %>% filter(siteID == "OSBS") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
rel_hum_dat_TOOL <- rel_hum_dat %>% filter(siteID == "TOOL") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
rel_hum_dat_DCFS <- rel_hum_dat %>% filter(siteID == "DCFS") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
rel_hum_dat_UNDE <- rel_hum_dat %>% filter(siteID == "UNDE") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)

hum_compare_unde <- left_join(rel_hum_dat_UNDE, rel_hum_dat_CRAM, by=c('endDateTime')) %>%
  arrange(endDateTime) %>%
  rename(RHMean_UNDE = RHMean.x, RHMean_CRAM = RHMean.y)%>%
  mutate(state = "Michigan")

hum_compare_osbs <- left_join(rel_hum_dat_OSBS, rel_hum_dat_SUGG, by=c('endDateTime')) %>%
  left_join(., rel_hum_dat_BARC, by=c('endDateTime'))%>%
  arrange(endDateTime)%>%
  rename(RHMean_OSBS = RHMean.x, RHMean_SUGG = RHMean.y, RHMean_BARC = RHMean)%>%
  mutate(state = "Florida")

hum_compare_tool <- left_join(rel_hum_dat_TOOL, rel_hum_dat_TOOK, by=c('endDateTime')) %>%
  arrange(endDateTime)%>%
  rename(RHMean_TOOL = RHMean.x, RHMean_TOOK = RHMean.y)%>%
  mutate(state = "Alaska")

hum_compare_dcfs <- left_join(rel_hum_dat_DCFS, rel_hum_dat_PRPO, by=c('endDateTime')) %>%
  left_join(., rel_hum_dat_PRLA, by=c('endDateTime'))%>%
  arrange(endDateTime)%>%
  rename(RHMean_DCFS = RHMean.x, RHMean_PRPO = RHMean.y, RHMean_PRLA = RHMean)%>%
  mutate(state = "Kansas")

pdf("./figures/barc_osbs_humidity_compare.pdf", width = 20, height = 20)
ggplot(hum_compare_osbs, aes(RHMean_OSBS, RHMean_BARC))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("OSBS vs BARC Humidity")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
dev.off() 

pdf("./figures/sugg_osbs_humidity_compare.pdf", width = 20, height = 20)
ggplot(hum_compare_osbs, aes(RHMean_OSBS, RHMean_SUGG))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("OSBS vs SUGG Humidity")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
dev.off() 

pdf("./figures/prla_dcfs_humidity_compare.pdf", width = 20, height = 20)
ggplot(hum_compare_dcfs, aes(RHMean_DCFS, RHMean_PRLA))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("DCFS vs PRLA Humidity")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
dev.off() 

pdf("./figures/prpo_dcfs_humidity_compare.pdf", width = 20, height = 20)
ggplot(hum_compare_dcfs, aes(RHMean_DCFS, RHMean_PRPO))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("DCFS vs PRPO Humidity")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
dev.off() 

pdf("./figures/cram_unde_humidity_compare.pdf", width = 20, height = 20)
ggplot(hum_compare_unde, aes(RHMean_UNDE, RHMean_CRAM))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("UNDE vs CRAM Humidity")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
dev.off() 

pdf("./figures/took_tool_humidity_compare.pdf", width = 20, height = 20)
ggplot(hum_compare_tool, aes(RHMean_TOOL, RHMean_TOOK))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("TOOL vs TOOK Humidity")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
dev.off() 
# -------------------------------------------------------------------


# AirTemp
# -------------------------------------------------------------------

# Lake Air temp
air_temp_dat_BARC <- air_temp_dat %>% filter(siteID == "BARC") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
air_temp_dat_SUGG <- air_temp_dat %>% filter(siteID == "SUGG") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
air_temp_dat_TOOK <- air_temp_dat %>% filter(siteID == "TOOK") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
air_temp_dat_PRPO <- air_temp_dat %>% filter(siteID == "PRPO") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
air_temp_dat_PRLA <- air_temp_dat %>% filter(siteID == "PRLA") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
air_temp_dat_CRAM <- air_temp_dat %>% filter(siteID == "CRAM") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)

# Tower Air temp
air_temp_dat_OSBS <- air_temp_dat %>% filter(siteID == "OSBS") %>% filter(verticalPosition == "010") %>% select(-verticalPosition, -siteID)
air_temp_dat_TOOL <- air_temp_dat %>% filter(siteID == "TOOL") %>% filter(verticalPosition == "010") %>% select(-verticalPosition, -siteID)
air_temp_dat_DCFS <- air_temp_dat %>% filter(siteID == "DCFS") %>% filter(verticalPosition == "010") %>% select(-verticalPosition, -siteID)
air_temp_dat_UNDE <- air_temp_dat %>% filter(siteID == "UNDE") %>% filter(verticalPosition == "010") %>% select(-verticalPosition, -siteID)

temp_compare_unde <- left_join(air_temp_dat_UNDE, air_temp_dat_CRAM, by=c('endDateTime')) %>%
  arrange(endDateTime) %>%
  rename(tempSingleMean_UNDE = tempSingleMean.x, tempSingleMean_CRAM = tempSingleMean.y)

temp_compare_tool <- left_join(air_temp_dat_TOOL, air_temp_dat_TOOK, by=c('endDateTime')) %>%
  arrange(endDateTime) %>%
  rename(tempSingleMean_TOOL = tempSingleMean.x, tempSingleMean_TOOK = tempSingleMean.y)

temp_compare_osbs <- left_join(air_temp_dat_OSBS, air_temp_dat_SUGG, by=c('endDateTime')) %>%
  left_join(., air_temp_dat_BARC, by=c('endDateTime'))%>%
  arrange(endDateTime)%>%
  rename(tempSingleMean_OSBS = tempSingleMean.x, tempSingleMean_SUGG = tempSingleMean.y, tempSingleMean_BARC = tempSingleMean)

temp_compare_dcfs <- left_join(air_temp_dat_DCFS, air_temp_dat_PRPO, by=c('endDateTime')) %>%
  left_join(., air_temp_dat_PRLA, by=c('endDateTime'))%>%
  arrange(endDateTime)%>%
  rename(tempSingleMean_DCFS = tempSingleMean.x, tempSingleMean_PRPO = tempSingleMean.y, tempSingleMean_PRLA = tempSingleMean)


pdf("./figures/took_tool_temperature_compare.pdf", width = 40, height = 20)
a <- ggplot(temp_compare_tool, aes(tempSingleMean_TOOL, tempSingleMean_TOOK))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("TOOL vs TOOK Air Temp")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
b <- ggplot(temp_compare_tool, aes(endDateTime,tempSingleMean_TOOL))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,tempSingleMean_TOOK), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
c = a+b
c
dev.off() 

pdf("./figures/sugg_osbs_airtemp_compare.pdf", width = 40, height = 20)
a <- ggplot(temp_compare_osbs, aes(tempSingleMean_OSBS, tempSingleMean_SUGG))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("OSBS vs SUGG Air Temp")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

b <- ggplot(temp_compare_osbs, aes(endDateTime,tempSingleMean_OSBS))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,tempSingleMean_SUGG), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
c = a+b
c
dev.off() 

pdf("./figures/barc_osbs_airtemp_compare.pdf", width = 40, height = 20)
a <- ggplot(temp_compare_osbs, aes(tempSingleMean_OSBS, tempSingleMean_BARC))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("OSBS vs BARC Air Temp")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

b <- ggplot(temp_compare_osbs, aes(endDateTime,tempSingleMean_OSBS))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,tempSingleMean_BARC), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
c = a+b
c
dev.off() 

pdf("./figures/cram_unde_airtemp_compare.pdf", width = 40, height = 20)
a <- ggplot(temp_compare_unde, aes(tempSingleMean_UNDE, tempSingleMean_CRAM))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("UNDE vs CRAM Air Temp")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

b <- ggplot(temp_compare_unde, aes(endDateTime,tempSingleMean_UNDE))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,tempSingleMean_CRAM), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
c = a+b
c
dev.off() 

pdf("./figures/prpo_dcfs_airtemp_compare.pdf", width = 40, height = 20)
a <- ggplot(temp_compare_dcfs, aes(tempSingleMean_DCFS, tempSingleMean_PRPO))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("DCFS vs PRPO Air Temp")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

b <- ggplot(temp_compare_dcfs, aes(endDateTime,tempSingleMean_DCFS))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,tempSingleMean_PRPO), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
c = a+b
c
dev.off() 

pdf("./figures/prla_dcfs_airtemp_compare.pdf", width = 40, height = 20)
a <- ggplot(temp_compare_dcfs, aes(tempSingleMean_DCFS, tempSingleMean_PRLA))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("DCFS vs PRLA Air Temp")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

b <- ggplot(temp_compare_dcfs, aes(endDateTime,tempSingleMean_DCFS))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,tempSingleMean_PRLA), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
c = a+b
c
dev.off() 
# -------------------------------------------------------------------

# Shortwave and Longwave Radiation
# -------------------------------------------------------------------
neonstore::neon_store(table = "SLRNR_30min-expanded")
radiation_dat <- neonstore::neon_table(table = "SLRNR_30min-expanded", site = sites) %>%
  select(endDateTime, inSWMean, outSWMean, inLWMean, outLWMean,verticalPosition, siteID)%>%
  mutate(SWMean = inSWMean - outSWMean)%>%
  mutate(LWMean = inLWMean - outLWMean)%>%
  select(endDateTime, SWMean, LWMean, verticalPosition, siteID)

# Lake Air SW & LW
sw_lw_dat_BARC <- radiation_dat %>% filter(siteID == "BARC") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
sw_lw_dat_SUGG <- radiation_dat %>% filter(siteID == "SUGG") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
sw_lw_dat_TOOK <- radiation_dat %>% filter(siteID == "TOOK") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
sw_lw_dat_PRPO <- radiation_dat %>% filter(siteID == "PRPO") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
sw_lw_dat_PRLA <- radiation_dat %>% filter(siteID == "PRLA") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
sw_lw_dat_CRAM <- radiation_dat %>% filter(siteID == "CRAM") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)

# Tower Air SW & LW
sw_lw_dat_OSBS <- radiation_dat %>% filter(siteID == "OSBS") %>% filter(verticalPosition == "060") %>% select(-verticalPosition, -siteID)
sw_lw_dat_TOOL <- radiation_dat %>% filter(siteID == "TOOL") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
sw_lw_dat_DCFS <- radiation_dat %>% filter(siteID == "DCFS") %>% filter(verticalPosition == "000") %>% select(-verticalPosition, -siteID)
sw_lw_dat_UNDE <- radiation_dat %>% filter(siteID == "UNDE") %>% filter(verticalPosition == "060") %>% select(-verticalPosition, -siteID)

sw_lw_compare_unde <- left_join(sw_lw_dat_UNDE, sw_lw_dat_CRAM, by=c('endDateTime')) %>%
  arrange(endDateTime)%>%
  rename(SWMean_UNDE = SWMean.x, LWMean_UNDE = LWMean.x, SWMean_CRAM = SWMean.y, LWMean_CRAM = LWMean.y)

sw_lw_compare_tool <- left_join(sw_lw_dat_TOOL, sw_lw_dat_TOOK, by=c('endDateTime')) %>%
  arrange(endDateTime) %>%
  rename(SWMean_TOOL = SWMean.x, LWMean_TOOL = LWMean.x, SWMean_TOOK = SWMean.y, LWMean_TOOK = LWMean.y)

sw_lw_compare_osbs <- left_join(sw_lw_dat_OSBS, sw_lw_dat_SUGG, by=c('endDateTime')) %>%
  left_join(., sw_lw_dat_BARC, by=c('endDateTime'))%>%
  arrange(endDateTime)%>%
  rename(SWMean_OSBS = SWMean.x, LWMean_OSBS = LWMean.x, SWMean_SUGG = SWMean.y, LWMean_SUGG = LWMean.y,SWMean_BARC = SWMean, LWMean_BARC = LWMean)

sw_lw_compare_dcfs <- left_join(sw_lw_dat_DCFS, sw_lw_dat_PRPO, by=c('endDateTime')) %>%
  left_join(., sw_lw_dat_PRLA, by=c('endDateTime'))%>%
  arrange(endDateTime)%>%
  rename(SWMean_DCFS = SWMean.x, LWMean_DCFS = LWMean.x, SWMean_PRPO = SWMean.y, LWMean_PRPO = LWMean.y,SWMean_PRLA = SWMean, LWMean_PRLA = LWMean)


pdf("./figures/took_tool_radiation_compare.pdf", width = 40, height = 20)
a <- ggplot(sw_lw_compare_tool, aes(SWMean_TOOL, SWMean_TOOK))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("TOOL vs TOOK SW")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
b <- ggplot(sw_lw_compare_tool, aes(endDateTime,SWMean_TOOL))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,SWMean_TOOK), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
c <- ggplot(sw_lw_compare_tool, aes(LWMean_TOOL, LWMean_TOOK))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("TOOL vs TOOK LW")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
d <- ggplot(sw_lw_compare_tool, aes(endDateTime,LWMean_TOOL))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,LWMean_TOOK), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
e = (a+b)/(c+d)
e
dev.off() 

pdf("./figures/unde_cram_radiation_compare.pdf", width = 40, height = 20)
a <- ggplot(sw_lw_compare_unde, aes(SWMean_UNDE, SWMean_CRAM))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("UNDE vs CRAM SW")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
b <- ggplot(sw_lw_compare_unde, aes(endDateTime,SWMean_CRAM))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,SWMean_CRAM), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
c <- ggplot(sw_lw_compare_unde, aes(LWMean_UNDE, LWMean_CRAM))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("UNDE vs CRAM LW")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
d <- ggplot(sw_lw_compare_unde, aes(endDateTime,LWMean_UNDE))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,LWMean_CRAM), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
e = (a+b)/(c+d)
e
dev.off()  

pdf("./figures/barc_osbs_airtemp_compare.pdf", width = 40, height = 20)
a <- ggplot(temp_compare_osbs, aes(tempSingleMean_OSBS, tempSingleMean_BARC))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("OSBS vs BARC Air Temp")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

b <- ggplot(temp_compare_osbs, aes(endDateTime,tempSingleMean_OSBS))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,tempSingleMean_BARC), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
c = a+b
c
dev.off() 

pdf("./figures/cram_unde_airtemp_compare.pdf", width = 40, height = 20)
a <- ggplot(temp_compare_unde, aes(tempSingleMean_UNDE, tempSingleMean_CRAM))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("UNDE vs CRAM Air Temp")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

b <- ggplot(temp_compare_unde, aes(endDateTime,tempSingleMean_UNDE))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,tempSingleMean_CRAM), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
c = a+b
c
dev.off() 

pdf("./figures/prpo_dcfs_airtemp_compare.pdf", width = 40, height = 20)
a <- ggplot(temp_compare_dcfs, aes(tempSingleMean_DCFS, tempSingleMean_PRPO))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("DCFS vs PRPO Air Temp")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

b <- ggplot(temp_compare_dcfs, aes(endDateTime,tempSingleMean_DCFS))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,tempSingleMean_PRPO), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
c = a+b
c
dev.off() 

pdf("./figures/prla_dcfs_airtemp_compare.pdf", width = 40, height = 20)
a <- ggplot(temp_compare_dcfs, aes(tempSingleMean_DCFS, tempSingleMean_PRLA))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("DCFS vs PRLA Air Temp")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))

b <- ggplot(temp_compare_dcfs, aes(endDateTime,tempSingleMean_DCFS))+
  geom_point(pch = 21, size = 0.6)+
  geom_point(aes(endDateTime,tempSingleMean_PRLA), color = "red", pch = 21, size = 0.6)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
c = a+b
c
dev.off() 

# -------------------------------------------------------------------