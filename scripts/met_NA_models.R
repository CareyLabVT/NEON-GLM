#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   Build empirical models to fill in NAs                *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    13Oct2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: Fill in the NAs with a better model than na.approx   *
#*****************************************************************

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(VIM, naniar, missMDA, Amelia, mice, FactoMineR, broom)

# Florida Sites
##################################################################
# Fill in Missing BARCO data
vis_miss(BARC_met, sort_miss = F) 

BARC_met <- as.data.frame(BARC_met)

#ShortWave
barc.amelia.sw <- amelia(BARC_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave")
barc_sw_imputations <- bind_rows(unclass(barc.amelia.sw$imputations), .id = "m") %>%
  select(time, ShortWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))

#LongWave
barc.amelia.lw <- amelia(BARC_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave")
barc_lw_imputations <- bind_rows(unclass(barc.amelia.lw$imputations), .id = "m") %>%
  select(time, LongWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))
plot(barc_lw_imputations$time, barc_lw_imputations$LongWave)

#AirTemp
barc.amelia.at <- amelia(BARC_met, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp")
barc_at_imputations <- bind_rows(unclass(barc.amelia.at$imputations), .id = "m") %>%
  select(time, AirTemp)%>%
  group_by(time)%>%
  summarise_all(funs(mean))
plot(barc_at_imputations$time, barc_at_imputations$AirTemp)

#Himidity
barc.amelia.rh <- amelia(BARC_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum")
barc_rh_imputations <- bind_rows(unclass(barc.amelia.rh$imputations), .id = "m") %>%
  select(time, RelHum)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))
plot(barc_rh_imputations$time, barc_rh_imputations$RelHum)

barc_imputed <- left_join(barc_sw_imputations, barc_lw_imputations, by = "time")%>%
  left_join(., barc_at_imputations, by = "time")%>%
  left_join(., barc_rh_imputations, by = "time")

BARC_met_new <- left_join(BARC_met, barc_imputed, by = "time")

BARC_met_final <- BARC_met_new %>%
  mutate(ShortWave.x = ifelse(is.na(ShortWave.x), ShortWave.y, ShortWave.x))%>%
  mutate(LongWave.x = ifelse(is.na(LongWave.x), LongWave.y, LongWave.x))%>%
  mutate(AirTemp.x = ifelse(is.na(AirTemp.x), AirTemp.y, AirTemp.x))%>%
  mutate(RelHum.x = ifelse(is.na(RelHum.x), RelHum.y, RelHum.x))%>%
  select(time, ShortWave.x, LongWave.x, AirTemp.x, RelHum.x, WindSpeed, Rain)%>%
  rename(ShortWave = ShortWave.x, LongWave = LongWave.x, AirTemp = AirTemp.x, RelHum = RelHum.x)

vis_miss(BARC_met_final, sort_miss = F) 


# Fill in Missing SUGG data
vis_miss(SUGG_met, sort_miss = F) 

SUGG_met <- as.data.frame(SUGG_met)

#ShortWave
sugg.amelia.sw <- amelia(SUGG_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave")
sugg_sw_imputations <- bind_rows(unclass(sugg.amelia.sw$imputations), .id = "m") %>%
  select(time, ShortWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))

#LongWave
sugg.amelia.lw <- amelia(SUGG_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave")
sugg_lw_imputations <- bind_rows(unclass(sugg.amelia.lw$imputations), .id = "m") %>%
  select(time, LongWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))

plot(sugg_lw_imputations$time, sugg_lw_imputations$LongWave)

#AirTemp
sugg.amelia.at <- amelia(SUGG_met, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp")
sugg_at_imputations <- bind_rows(unclass(sugg.amelia.at$imputations), .id = "m") %>%
  select(time, AirTemp)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(AirTemp = ifelse(AirTemp >= 38, 38, AirTemp))

plot(sugg_at_imputations$time, sugg_at_imputations$AirTemp)

#Himidity
sugg.amelia.rh <- amelia(SUGG_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum")
sugg_rh_imputations <- bind_rows(unclass(sugg.amelia.rh$imputations), .id = "m") %>%
  select(time, RelHum)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))

plot(SUGG_met$time, SUGG_met$RelHum)
plot(sugg_rh_imputations$time, sugg_rh_imputations$RelHum)

sugg_imputed <- left_join(sugg_sw_imputations, sugg_lw_imputations, by = "time")%>%
  left_join(., sugg_at_imputations, by = "time")%>%
  left_join(., sugg_rh_imputations, by = "time")

SUGG_met_new <- left_join(SUGG_met, sugg_imputed, by = "time")

SUGG_met_final <- SUGG_met_new %>%
  mutate(ShortWave.x = ifelse(is.na(ShortWave.x), ShortWave.y, ShortWave.x))%>%
  mutate(LongWave.x = ifelse(is.na(LongWave.x), LongWave.y, LongWave.x))%>%
  mutate(AirTemp.x = ifelse(is.na(AirTemp.x), AirTemp.y, AirTemp.x))%>%
  mutate(RelHum.x = ifelse(is.na(RelHum.x), RelHum.y, RelHum.x))%>%
  select(time, ShortWave.x, LongWave.x, AirTemp.x, RelHum.x, WindSpeed, Rain)%>%
  rename(ShortWave = ShortWave.x, LongWave = LongWave.x, AirTemp = AirTemp.x, RelHum = RelHum.x)

vis_miss(SUGG_met_final, sort_miss = F) 

##################################################################

# Wisconsin Sites
##################################################################

# Fill in Missing CRAMPTON data
vis_miss(CRAM_met, sort_miss = F) 

CRAM_met <- as.data.frame(CRAM_met)

#ShortWave
plot(CRAM_met$time, CRAM_met$ShortWave)

cram.amelia.sw <- amelia(CRAM_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave")
cram_sw_imputations <- bind_rows(unclass(cram.amelia.sw$imputations), .id = "m") %>%
  select(time, ShortWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))

plot(cram_sw_imputations$time, cram_sw_imputations$ShortWave)

#LongWave
plot(CRAM_met$time, CRAM_met$LongWave)

cram.amelia.lw <- amelia(CRAM_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave")
cram_lw_imputations <- bind_rows(unclass(cram.amelia.lw$imputations), .id = "m") %>%
  select(time, LongWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))

plot(cram_lw_imputations$time, cram_lw_imputations$LongWave)

#AirTemp
plot(CRAM_met$time, CRAM_met$AirTemp)

cram.amelia.at <- amelia(CRAM_met, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp")
cram_at_imputations <- bind_rows(unclass(cram.amelia.at$imputations), .id = "m") %>%
  select(time, AirTemp)%>%
  group_by(time)%>%
  summarise_all(funs(mean))

plot(cram_at_imputations$time, cram_at_imputations$AirTemp)

#Himidity
plot(CRAM_met$time, CRAM_met$RelHum)

cram.amelia.rh <- amelia(CRAM_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum")
cram_rh_imputations <- bind_rows(unclass(cram.amelia.rh$imputations), .id = "m") %>%
  select(time, RelHum)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))

plot(cram_rh_imputations$time, cram_rh_imputations$RelHum)

cram_imputed <- left_join(cram_sw_imputations, cram_lw_imputations, by = "time")%>%
  left_join(., cram_at_imputations, by = "time")%>%
  left_join(., cram_rh_imputations, by = "time")

CRAM_met_new <- left_join(CRAM_met, cram_imputed, by = "time")

CRAM_met_final <- CRAM_met_new %>%
  mutate(ShortWave.x = ifelse(is.na(ShortWave.x), ShortWave.y, ShortWave.x))%>%
  mutate(LongWave.x = ifelse(is.na(LongWave.x), LongWave.y, LongWave.x))%>%
  mutate(AirTemp.x = ifelse(is.na(AirTemp.x), AirTemp.y, AirTemp.x))%>%
  mutate(RelHum.x = ifelse(is.na(RelHum.x), RelHum.y, RelHum.x))%>%
  select(time, ShortWave.x, LongWave.x, AirTemp.x, RelHum.x, WindSpeed, Rain)%>%
  rename(ShortWave = ShortWave.x, LongWave = LongWave.x, AirTemp = AirTemp.x, RelHum = RelHum.x)

vis_miss(CRAM_met_final, sort_miss = F) 


# Fill in Missing LITTLE ROCK LAKE data
vis_miss(LIRO_met, sort_miss = F) 

LIRO_met <- as.data.frame(LIRO_met)

#ShortWave
plot(LIRO_met$time, LIRO_met$ShortWave)

liro.amelia.sw <- amelia(LIRO_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave")
liro_sw_imputations <- bind_rows(unclass(liro.amelia.sw$imputations), .id = "m") %>%
  select(time, ShortWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))

plot(liro_sw_imputations$time, liro_sw_imputations$ShortWave)

#LongWave
plot(LIRO_met$time, LIRO_met$LongWave)

liro.amelia.lw <- amelia(LIRO_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave")
liro_lw_imputations <- bind_rows(unclass(liro.amelia.lw$imputations), .id = "m") %>%
  select(time, LongWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))

plot(liro_lw_imputations$time, liro_lw_imputations$LongWave)

#AirTemp
plot(LIRO_met$time, LIRO_met$AirTemp)

liro.amelia.at <- amelia(LIRO_met, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp")
liro_at_imputations <- bind_rows(unclass(liro.amelia.at$imputations), .id = "m") %>%
  select(time, AirTemp)%>%
  group_by(time)%>%
  summarise_all(funs(mean))

plot(liro_at_imputations$time, liro_at_imputations$AirTemp)

#Himidity
plot(LIRO_met$time, LIRO_met$RelHum)

liro.amelia.rh <- amelia(LIRO_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum")
liro_rh_imputations <- bind_rows(unclass(liro.amelia.rh$imputations), .id = "m") %>%
  select(time, RelHum)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))

plot(liro_rh_imputations$time, liro_rh_imputations$RelHum)

liro_imputed <- left_join(liro_sw_imputations, liro_lw_imputations, by = "time")%>%
  left_join(., liro_at_imputations, by = "time")%>%
  left_join(., liro_rh_imputations, by = "time")

LIRO_met_new <- left_join(LIRO_met, liro_imputed, by = "time")

LIRO_met_final <- LIRO_met_new %>%
  mutate(ShortWave.x = ifelse(is.na(ShortWave.x), ShortWave.y, ShortWave.x))%>%
  mutate(LongWave.x = ifelse(is.na(LongWave.x), LongWave.y, LongWave.x))%>%
  mutate(AirTemp.x = ifelse(is.na(AirTemp.x), AirTemp.y, AirTemp.x))%>%
  mutate(RelHum.x = ifelse(is.na(RelHum.x), RelHum.y, RelHum.x))%>%
  select(time, ShortWave.x, LongWave.x, AirTemp.x, RelHum.x, WindSpeed, Rain)%>%
  rename(ShortWave = ShortWave.x, LongWave = LongWave.x, AirTemp = AirTemp.x, RelHum = RelHum.x)

vis_miss(LIRO_met_final, sort_miss = F) 

##################################################################

# Kansas Sites
##################################################################
# Fill in Missing PRARIE POTHOLE data
vis_miss(PRPO_met, sort_miss = F) 

PRPO_met <- as.data.frame(PRPO_met)

#AirTemp
plot(PRPO_met$time, PRPO_met$AirTemp)

prpo.amelia.at <- amelia(PRPO_met, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp")
prpo_at_imputations <- bind_rows(unclass(prpo.amelia.at$imputations), .id = "m") %>%
  select(time, AirTemp)%>%
  group_by(time)%>%
  summarise_all(funs(mean))

plot(prpo_at_imputations$time, prpo_at_imputations$AirTemp)

#Himidity
plot(PRPO_met$time, PRPO_met$RelHum)

prpo.amelia.rh <- amelia(PRPO_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum")
prpo_rh_imputations <- bind_rows(unclass(prpo.amelia.rh$imputations), .id = "m") %>%
  select(time, RelHum)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))

plot(prpo_rh_imputations$time, prpo_rh_imputations$RelHum)

prpo_imputed <- left_join(liro_at_imputations, liro_rh_imputations, by = "time")

PRPO_met_new <- left_join(PRPO_met, prpo_imputed, by = "time")

PRPO_met_final <- PRPO_met_new %>%
  mutate(AirTemp.x = ifelse(is.na(AirTemp.x), AirTemp.y, AirTemp.x))%>%
  mutate(RelHum.x = ifelse(is.na(RelHum.x), RelHum.y, RelHum.x))%>%
  select(time, ShortWave, LongWave, AirTemp.x, RelHum.x, WindSpeed, Rain)%>%
  rename(ShortWave = ShortWave, LongWave = LongWave, AirTemp = AirTemp.x, RelHum = RelHum.x)

vis_miss(PRPO_met_final, sort_miss = F)

# Fill in Missing PRARIE LAKE data
vis_miss(PRLA_met, sort_miss = F) 

PRLA_met <- as.data.frame(PRLA_met)

#ShortWave
plot(PRLA_met$time, PRLA_met$ShortWave)

prla.amelia.sw <- amelia(PRLA_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave")
prla_sw_imputations <- bind_rows(unclass(prla.amelia.sw$imputations), .id = "m") %>%
  select(time, ShortWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))

plot(prla_sw_imputations$time, prla_sw_imputations$ShortWave)

#LongWave
plot(PRLA_met$time, PRLA_met$LongWave)

prla.amelia.lw <- amelia(PRLA_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave")
prla_lw_imputations <- bind_rows(unclass(prla.amelia.lw$imputations), .id = "m") %>%
  select(time, LongWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))

plot(prla_lw_imputations$time, prla_lw_imputations$LongWave)

#AirTemp
plot(PRLA_met$time, PRLA_met$AirTemp)

prla.amelia.at <- amelia(PRLA_met, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp")
prla_at_imputations <- bind_rows(unclass(prla.amelia.at$imputations), .id = "m") %>%
  select(time, AirTemp)%>%
  group_by(time)%>%
  summarise_all(funs(mean))

plot(prla_at_imputations$time, prla_at_imputations$AirTemp)

#Himidity
plot(PRLA_met$time, PRLA_met$RelHum)

prla.amelia.rh <- amelia(PRLA_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum")
prla_rh_imputations <- bind_rows(unclass(prla.amelia.rh$imputations), .id = "m") %>%
  select(time, RelHum)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))

plot(prla_rh_imputations$time, prla_rh_imputations$RelHum)

prla_imputed <- left_join(prla_sw_imputations, prla_lw_imputations, by = "time")%>%
  left_join(., prla_at_imputations, by = "time")%>%
  left_join(., prla_rh_imputations, by = "time")

PRLA_met_new <- left_join(PRLA_met, prla_imputed, by = "time")

PRLA_met_final <- LIRO_met_new %>%
  mutate(ShortWave.x = ifelse(is.na(ShortWave.x), ShortWave.y, ShortWave.x))%>%
  mutate(LongWave.x = ifelse(is.na(LongWave.x), LongWave.y, LongWave.x))%>%
  mutate(AirTemp.x = ifelse(is.na(AirTemp.x), AirTemp.y, AirTemp.x))%>%
  mutate(RelHum.x = ifelse(is.na(RelHum.x), RelHum.y, RelHum.x))%>%
  select(time, ShortWave.x, LongWave.x, AirTemp.x, RelHum.x, WindSpeed, Rain)%>%
  rename(ShortWave = ShortWave.x, LongWave = LongWave.x, AirTemp = AirTemp.x, RelHum = RelHum.x)

vis_miss(PRLA_met_final, sort_miss = F) 
##################################################################

# Alaska Site
##################################################################
# Fill in Missing TOOLIK LAKE data
vis_miss(TOOK_met, sort_miss = F) 

TOOK_met <- as.data.frame(TOOK_met)

#ShortWave
plot(TOOK_met$time, TOOK_met$ShortWave)

took.amelia.sw <- amelia(TOOK_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave")
took_sw_imputations <- bind_rows(unclass(took.amelia.sw$imputations), .id = "m") %>%
  select(time, ShortWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))

plot(took_sw_imputations$time, took_sw_imputations$ShortWave)

#LongWave
plot(TOOK_met$time, TOOK_met$LongWave)

took.amelia.lw <- amelia(TOOK_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave")
took_lw_imputations <- bind_rows(unclass(took.amelia.lw$imputations), .id = "m") %>%
  select(time, LongWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))

plot(took_lw_imputations$time, took_lw_imputations$LongWave)

#AirTemp
plot(TOOK_met$time, TOOK_met$AirTemp)

took.amelia.at <- amelia(TOOK_met, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp")
took_at_imputations <- bind_rows(unclass(took.amelia.at$imputations), .id = "m") %>%
  select(time, AirTemp)%>%
  group_by(time)%>%
  summarise_all(funs(mean))

plot(took_at_imputations$time, took_at_imputations$AirTemp)

#Himidity
plot(TOOK_met$time, TOOK_met$RelHum)

took.amelia.rh <- amelia(TOOK_met, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum")
took_rh_imputations <- bind_rows(unclass(took.amelia.rh$imputations), .id = "m") %>%
  select(time, RelHum)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))

plot(took_rh_imputations$time, took_rh_imputations$RelHum)

took_imputed <- left_join(took_sw_imputations, took_lw_imputations, by = "time")%>%
  left_join(., took_at_imputations, by = "time")%>%
  left_join(., took_rh_imputations, by = "time")

TOOK_met_new <- left_join(TOOK_met, took_imputed, by = "time")

TOOK_met_final <- TOOK_met_new %>%
  mutate(ShortWave.x = ifelse(is.na(ShortWave.x), ShortWave.y, ShortWave.x))%>%
  mutate(LongWave.x = ifelse(is.na(LongWave.x), LongWave.y, LongWave.x))%>%
  mutate(AirTemp.x = ifelse(is.na(AirTemp.x), AirTemp.y, AirTemp.x))%>%
  mutate(RelHum.x = ifelse(is.na(RelHum.x), RelHum.y, RelHum.x))%>%
  select(time, ShortWave.x, LongWave.x, AirTemp.x, RelHum.x, WindSpeed, Rain)%>%
  rename(ShortWave = ShortWave.x, LongWave = LongWave.x, AirTemp = AirTemp.x, RelHum = RelHum.x)
##################################################################


# Save each data frame as a .csv that can be executed in the .nml files
##################################################################

write_csv(BARC_met_final, "./driver_data/BARC_met_hourly.csv")
write_csv(SUGG_met_final, "./driver_data/SUGG_met_hourly.csv")

write_csv(CRAM_met_final, "./driver_data/CRAM_met_hourly.csv")
write_csv(LIRO_met_final, "./driver_data/LIRO_met_hourly.csv")

write_csv(PRPO_met_final, "./driver_data/PRPO_met_hourly.csv")
write_csv(PRLA_met_final, "./driver_data/PRLA_met_hourly.csv")

write_csv(TOOK_met_final, "./driver_data/TOOK_met_hourly.csv")
##################################################################
