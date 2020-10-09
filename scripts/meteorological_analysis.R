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
pacman::p_load(tidyverse, lubridate, reshape2, devtools, patchwork)

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
       start_date = "2013-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")

# Shortwave and Longwave Radiation
met_product_rad = "DP1.00023.001"
lapply(met_product_rad, neonstore::neon_download, site = sites, 
       start_date = "2013-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")

# Precipitation
met_product_precip = "DP1.00006.001"
lapply(met_product_precip, neonstore::neon_download, site = sites, 
       start_date = "2013-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")

# 2D Windspeed
met_product_wind = "DP1.00001.001"
lapply(met_product_wind, neonstore::neon_download, site = sites, 
       start_date = "2013-01-01", end_date = "2020-10-01",
       file_regex = "[.]csv")



# Humidity
# -------------------------------------------------------------------
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
) %>% select(endDateTime, RHMean, verticalPosition, siteID)

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
) %>% select(endDateTime, tempSingleMean, verticalPosition, siteID) %>%
  group_by(endDateTime)

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
radiation_dat <- neonstore::neon_read(
  table = "SLRNR_30min-expanded",
  product = "DP1.00023.001",
  site = NA,
  start_date = "2013-01-01",
  end_date = NA,
  ext = "csv",
  timestamp = NA,
  files = NULL,
  sensor_metadata = TRUE,
  altrep = FALSE)%>%
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

# -------------------------------------------------------------------