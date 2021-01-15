#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   Build volume relationship for water level in NEON    *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    14Jan2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: Test the new CRAN neonstore package                  *
#* HELPERS: Tadhg                                                *
#*****************************************************************
library(devtools)
install_github("onofriandreapg/aomisc")

library(tidyverse)
library(drc)
library(nlme)
library(aomisc)


#### Lake Crampton Volume #### 
cram_hypso <- read_csv("/groups/rqthomas_lab/neonstore/lake_bathymetry/CRAM/D05CRAM_BATH_20150707_volume.csv")
cram_hypso$Depth <- -cram_hypso$Depth
cram_hypso$Volume <- rev(cram_hypso$Volume)

cram_hypso <- cram_hypso %>% filter(Depth >= 14)

plot(cram_hypso$Volume, cram_hypso$Depth)

vol_model <- lm(Volume~Depth, data = cram_hypso)
summary(vol_model)

cram_lake_volume <- water_level %>% filter(siteID == "CRAM")%>%
  mutate(value = value - 1)%>%
  mutate(value = value*195441 - 2315254)%>%
  mutate(variable = "watervolume")

plot(cram_lake_volume$value)

write_csv(cram_lake_volume, "./observations/volume_crampton.csv")



#### Little Rock Volume #### 
liro_hypso <- read_csv("/groups/rqthomas_lab/neonstore/lake_bathymetry/LIRO/D05LIRO_BATH_20160929_volume.csv")
liro_hypso$Depth <- -liro_hypso$Depth
liro_hypso$Volume <- rev(liro_hypso$Volume)

liro_hypso <- liro_hypso %>% filter(Depth >= 14)

plot(liro_hypso$Volume, liro_hypso$Depth)

vol_model <- lm(Volume~Depth, data = cram_hypso)
summary(vol_model)

cram_lake_volume <- water_level %>% filter(siteID == "CRAM")%>%
  mutate(value = value - 1)%>%
  mutate(value = value*195441 - 2315254)%>%
  mutate(variable = "watervolume")

plot(cram_lake_volume$value)

write_csv(cram_lake_volume, "./observations/volume_crampton.csv")