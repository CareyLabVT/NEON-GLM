#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   Meteorology data collation and comparisons           *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    08Oct2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: Compare the met stations on lake to EC tower met data*
#*****************************************************************

# Get packages
if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, reshape2, devtools)

# Bypass the latest CRAN version of neonstore and use Carl's most recent Github push
devtools::install_github("cboettig/neonstore")

sites = c("TOOK", "SUGG", "BARC", "PRPO", "PRLA", "CRAM", "OSBS", "TOOL", "DCFS", "UNDE")

# Humidity
met_product_hum = "DP1.00098.001"
lapply(met_product_hum, neonstore::neon_download, site = sites, 
       start_date = "2013-01-01", end_date = NA,
       file_regex = "[.]csv")

# Air Temperature
met_product_airT = "DP1.00002.001"
lapply(met_product_airT, neonstore::neon_download, site = sites, 
       start_date = "2013-01-01", end_date = NA,
       file_regex = "[.]csv")

# Shortwave and Longwave Radiation
met_product_rad = "DP1.00023.001"
lapply(met_product_rad, neonstore::neon_download, site = sites, 
       start_date = "2013-01-01", end_date = NA,
       file_regex = "[.]csv")

# Precipitation
met_product_precip = "DP1.00006.001"
lapply(met_product_precip, neonstore::neon_download, site = sites, 
       start_date = "2013-01-01", end_date = NA,
       file_regex = "[.]csv")

# 2D Windspeed
met_product_wind = "DP1.00001.001"
lapply(met_product_wind, neonstore::neon_download, site = sites, 
       start_date = "2013-01-01", end_date = NA,
       file_regex = "[.]csv")



# Humidity
rel_hum_dat <- neonstore::neon_read(
  table = "RH_30min-expanded",
  product = "DP1.00098.001",
  site = NA,
  start_date = "2013-01-01",
  end_date = NA,
  ext = "csv",
  timestamp = NA,
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE
) %>% select(startDateTime, RHMean, verticalPosition, siteID)

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

hum_compare_unde <- left_join(rel_hum_dat_UNDE, rel_hum_dat_CRAM, by=c('startDateTime')) %>%
  arrange(startDateTime) %>%
  rename(RHMean_UNDE = RHMean.x, RHMean_CRAM = RHMean.y)%>%
  mutate(state = "Michigan")

hum_compare_osbs <- left_join(rel_hum_dat_OSBS, rel_hum_dat_SUGG, by=c('startDateTime')) %>%
  left_join(., rel_hum_dat_BARC, by=c('startDateTime'))%>%
  arrange(startDateTime)%>%
  rename(RHMean_OSBS = RHMean.x, RHMean_SUGG = RHMean.y, RHMean_BARC = RHMean)%>%
  mutate(state = "Florida")

hum_compare_tool <- left_join(rel_hum_dat_TOOL, rel_hum_dat_TOOK, by=c('startDateTime')) %>%
  arrange(startDateTime)%>%
  rename(RHMean_TOOL = RHMean.x, RHMean_TOOK = RHMean.y)%>%
  mutate(state = "Alaska")

hum_compare_dcfs <- left_join(rel_hum_dat_DCFS, rel_hum_dat_PRPO, by=c('startDateTime')) %>%
  left_join(., rel_hum_dat_PRLA, by=c('startDateTime'))%>%
  arrange(startDateTime)%>%
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

# AirTemp
air_temp_dat <- neonstore::neon_read(
  table = "SAAT_30min-expanded",
  product = "DP1.00002.001",
  site = NA,
  start_date = "2013-01-01",
  end_date = NA,
  ext = "csv",
  timestamp = NA,
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE
) %>% select(startDateTime, tempSingleMean, verticalPosition, siteID) %>%
  group_by(startDateTime)

air_temp_dat_BARC <- air_temp_dat %>% filter(siteID == "BARC")
air_temp_dat_SUGG <- air_temp_dat %>% filter(siteID == "SUGG")

temp_compare <- left_join(air_temp_dat_BARC, air_temp_dat_SUGG, by=c('startDateTime')) %>%
  left_join(., air_temp_dat_OSBS, by=c('startDateTime'))%>%
  arrange(startDateTime)%>%
  rename(air_temp_barc = tempSingleMean.x, air_temp_sugg = tempSingleMean.y, air_temp_osbs = tempSingleMean)

pdf("./figures/barc_osbs_airtemp_compare.pdf", width = 20, height = 20)
ggplot(temp_compare, aes(air_temp_osbs, air_temp_barc))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("OSBS vs BARC AirTemp")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
dev.off() 

pdf("./figures/sugg_osbs_airtemp_compare.pdf", width = 20, height = 20)
ggplot(temp_compare, aes(air_temp_osbs, air_temp_sugg))+
  geom_point(pch = 21, size = 0.6)+
  geom_smooth(method = "lm")+
  ggtitle("OSBS vs SUGG AirTemp")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"))
dev.off() 