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
pacman::p_load(VIM, naniar, missMDA, Amelia, mice, FactoMineR)

a <- vis_miss(BARC_met, sort_miss = F) 
b <- vis_miss(SUGG_met, sort_miss = F) 
c <- vis_miss(PRPO_met, sort_miss = F)
d <- vis_miss(PRLA_met, sort_miss = F)
e <- vis_miss(CRAM_met, sort_miss = F)
f <- vis_miss(LIRO_met, sort_miss = F)
g <- vis_miss(TOOK_met, sort_miss = F)

c <- (a+b+c+d)/(e+f+g+plot_spacer())


barc_out <- amelia(BARC_met, ts = "time", cs = "ShortWave", polytime = 1)

a.out.time <- amelia(BARC_met, ts = "time", cs = "ShortWave", polytime = 2, intercs = TRUE, p2s = 2)




FL_met_models <- left_join(BARC_met, SUGG_met, by = "time")%>%
  left_join(., OSBS_met, by = "time")

# SUGG AirTemp
sugg_fit_at_1 <- lm(AirTemp.y ~ AirTemp.x+AirTemp, data = FL_met_models)
sugg_fit_at_2 <- lm(AirTemp.y ~ AirTemp.x, data = FL_met_models)


FL_met_models$AirTemp.y <- ifelse(is.na(FL_met_models$AirTemp.y),
                                    FL_met_models$AirTemp*sugg_fit_at_1$coefficients[3]+
                                    FL_met_models$AirTemp.x*sugg_fit_at_1$coefficients[2]+
                                    sugg_fit_at_1$coefficients[1], FL_met_models$AirTemp.y)

FL_met_models$AirTemp.y <- ifelse(is.na(FL_met_models$AirTemp.y),
                                    FL_met_models$AirTemp.x*sugg_fit_at_2$coefficients[2]+
                                    sugg_fit_at_2$coefficients[1], FL_met_models$AirTemp.y)


plot(FL_met_models$time, FL_met_models$AirTemp.y)


FL_met_models$ShortWave.x <- na.spline(FL_met_models$ShortWave.x)

FL_met_models$ShortWave.x <- ifelse(FL_met_models$ShortWave.x <=0, 0, FL_met_models$ShortWave.x)
FL_met_models$ShortWave.x <- ifelse(FL_met_models$ShortWave.x >=1100, 1000, FL_met_models$ShortWave.x)


# BARC longwave
barc_fit_lw_1 <- lm(LongWave.x ~ LongWave.y, data = FL_met_models)
FL_met_models$LongWave.x <- ifelse(is.na(FL_met_models$LongWave.x),
                                    FL_met_models$LongWave.y*barc_fit_lw_1$coefficients[2]+
                                      barc_fit_lw_1$coefficients[1], FL_met_models$LongWave.x)
FL_met_models$LongWave.x <- na.spline(FL_met_models$LongWave.x)

FL_met_models$LongWave.x <- ifelse(FL_met_models$LongWave.x >=600, 600, FL_met_models$LongWave.x)

# BARC airtemp
barc_fit_at_1 <- lm(AirTemp.x ~ AirTemp.y, data = FL_met_models)
barc_fit_at_2 <- lm(AirTemp.x ~ AirTemp, data = FL_met_models)
barc_fit_at_3 <- lm(AirTemp.x ~ LongWave.x, data = FL_met_models)

FL_met_models$AirTemp.x <- ifelse(is.na(FL_met_models$AirTemp.x),
                                   FL_met_models$AirTemp.y*barc_fit_at_1$coefficients[2]+
                                     barc_fit_at_1$coefficients[1], FL_met_models$AirTemp.x)

FL_met_models$AirTemp.x <- ifelse(is.na(FL_met_models$AirTemp.x),
                                  FL_met_models$AirTemp*barc_fit_at_2$coefficients[2]-
                                    barc_fit_at_2$coefficients[1], FL_met_models$AirTemp.x)
FL_met_models$AirTemp.x <- na.approx(FL_met_models$AirTemp.x)


# BARC humidity
barc_fit_rh_1 <- lm(RelHum.x ~ RelHum.y, data = FL_met_models)
barc_fit_rh_2 <- lm(RelHum.x ~ RelHum, data = FL_met_models)

FL_met_models$RelHum.x <- ifelse(is.na(FL_met_models$RelHum.x),
                                  FL_met_models$RelHum.y*barc_fit_rh_1$coefficients[2]-
                                    barc_fit_rh_1$coefficients[1], FL_met_models$RelHum.x)

FL_met_models$RelHum.x <- ifelse(is.na(FL_met_models$RelHum.x),
                                 FL_met_models$RelHum*barc_fit_rh_2$coefficients[2]+
                                   barc_fit_rh_2$coefficients[1], FL_met_models$RelHum.x)
FL_met_models$RelHum.x <- na.spline(FL_met_models$RelHum.x)



FL_met_models$RelHum.x <- ifelse(FL_met_models$RelHum.x >=100, 100, FL_met_models$RelHum.x)



summary(FL_met_models$RelHum.x)
plot(FL_met_models$time, FL_met_models$RelHum.x)



FL_met_models$AirTemp.x <- ifelse(is.na(FL_met_models$AirTemp.x),FL_met_models$AirTemp.y * barc_fit_at_1$coefficients[2]-barc_fit_at_1$coefficients[1], FL_met_models$AirTemp.x)


# Here are all of the possible combinations for the missing data models

# Michigan and Wisconsin Lakes met NA model fills

# CRAM Humidity models
# 1st choice --> cram_hum = (unde_hum * 0.519287) + (liro_hum * 0.493274) - 0.523980
# 2nd choice --> cram_hum = (unde_hum * 0.972849) + 2.245011
# 3rd choice --> cram_hum = (liro_hum * 0.977005) + 2.251574

# LIRO Humidity models
# 1st choice --> liro_hum = (unde_hum * 0.022328) + (cram_hum * 0.918693) + 3.963503
# 2nd choice --> liro_hum = (cram_hum * 0.977005) + 2.251574
# 3rd choice --> liro_hum = (unde_hum * 0.913247) + 6.367811

# CRAM LongWave models
# 1st choice --> cram_lw = (unde_lw * 0.471172) + (liro_lw * 0.561685) - 11.876595
# 2nd choice --> cram_lw = (liro_lw * 0.986852) + 3.792107
# 3rd choice --> cram_lw = (unde_lw * 1.068111) - 24.568086

# LIRO LongWave models
# 1st choice --> liro_lw = (unde_lw * 0.397029) + (cram_lw * 0.627662) - 8.054433
# 2nd choice --> liro_lw = (cram_lw * 0.986852) + 3.792107
# 3rd choice --> liro_lw = (unde_lw * 1.069988) - 23.953770

# CRAM ShortWave models
# 1st choice --> cram_sw = (unde_sw * 0.553286) + (liro_sw * -0.181944) + 0.599674
# 2nd choice --> cram_sw = (unde_sw * 0.508289) + 0.401981
# 3rd choice --> cram_sw = (liro_sw * 1.2321) + 33.3994

# LIRO ShortWave models
# 1st choice --> liro_sw = (unde_sw * 0.265356) + (cram_sw * -0.077064) + 1.025814
# 2nd choice --> liro_sw = (unde_sw * 0.225885) + 0.993532
# 3rd choice --> liro_sw = (cram_sw * 0.314963) + 10.572628

# CRAM AirTemp models
# 1st choice --> cram_t = (unde_t * 0.573785) + (liro_t * 0.434013) + 0.106241
# 2nd choice --> cram_t = (unde_t * 1.020834) + 0.057550
# 3rd choice --> cram_t = (liro_t * 0.983979) + 0.204495

# LIRO AirTemp models
# 1st choice --> liro_t = (unde_t * -0.149927) + (cram_t * 1.155265) - 0.184073
# 2nd choice --> liro_t = (cram_t * 1.0092196) - 0.1758984
# 3rd choice --> liro_t = (unde_t * 1.0287741) - 0.1230194

# Kansas Lakes

# PRPO AirTemp models
# 1st choice --> prpo_t = (dcfs_t * 0.028725) + (prla_t * 0.968735) - 0.090413
# 2nd choice --> prpo_t = (prla_t * 0.9976141) - 0.0917582
# 3rd choice --> prpo_t = (dcfs_t * 0.9913677) - 0.0446168

# PRLA AirTemp models
# 1st choice --> prla_t = (dcfs_t * 0.572542) + (prpo_t * 0.424859) + 0.066472
# 2nd choice --> prla_t = (dcfs_t * 1.0287741) - 0.1230194
# 3rd choice --> prla_t = (prpo_t * 1.0007664) + 0.0988091

# PRPO Humidity models
# 1st choice --> prpo_h = (dcfs_h * 0.231127) + (prla_h * 0.727437) + 3.115898
# 2nd choice --> prpo_h = (prla_h * 0.957624) + 2.926002
# 3rd choice --> prpo_h = (dcfs_h * 0.949097) + 4.663283

# PRLA Humidity models
# 1st choice --> prla_h = (dcfs_h * 0.690547) + (prpo_h * 0.310355) + 0.798390
# 2nd choice --> prla_h = (dcfs_h * 0.9845556) + 2.2802131
# 3rd choice --> prla_h = (prpo_h * 1.004591) - 0.062928

# PRPO ShortWave models
# 1st choice --> prpo_sw = (dcfs_sw * 0.11480) + (prla_sw * 0.90966) + 1.73437
# 2nd choice --> prpo_sw = (prla_sw * 1.0251459) + 1.7362018
# 3rd choice --> prpo_sw = (dcfs_sw * 1.0140341) + 2.3526114

# PRLA ShortWave models
# 1st choice --> prla_sw = (dcfs_sw * 0.835099) + (prpo_sw * 0.151347) + 0.320940
# 2nd choice --> prla_sw = (dcfs_sw * 0.9885765) + 0.6765813
# 3rd choice --> prla_sw = (prpo_sw * 0.9584458) + 1.2264827

# PRPO LongWave models
# 1st choice --> prpo_lw = (dcfs_lw * 0.136864) + (prla_lw * 0.858416) + 0.386172
# 2nd choice --> prpo_lw = (prla_lw * 0.9892193) + 2.8258717
# 3rd choice --> prpo_lw = (dcfs_lw * 1.027e+00) - 1.288e+01

# PRLA Longwave models
# 1st choice --> prla_lw = (dcfs_lw * 0.413779) + (prpo_lw * 0.606779) - 7.626762
# 2nd choice --> prla_lw = (dcfs_lw * 1.037) - 1.543e+01
# 3rd choice --> prla_lw = (prpo_lw * 1.004591) - 0.7697168

# Florida Lakes

# BARC AirTemp models
# 1st choice --> barc_t = (osbs_t * 0.37029) + (sugg_t * 0.65278) - 0.29576
# 2nd choice --> barc_t = (sugg_t * 1.014810) + 0.089393
# 3rd choice --> barc_t = (osbs_t * 1.0337459) - 0.8161512

# SUGG AirTemp models
# 1st choice --> sugg_t = (osbs_t * 0.48523) + (barc_t * 0.51256) - 0.42429
# 2nd choice --> sugg_t = (barc_t * 0.976858) + 0.110030
# 3rd choice --> sugg_t = (osbs_t * 1.014450) - 0.865453

# BARC humidity models
# 1st choice --> barc_h = (osbs_h * 0.232243) + (sugg_h * 0.753246) + 0.174773
# 2nd choice --> barc_h = (sugg_h * 1.014810) - 0.977321
# 3rd choice --> barc_h = (osbs_h * 1.0337459) - 0.8161512

# SUGG humidity models
# 1st choice --> sugg_h = (osbs_h * 0.48523) + (barc_h * 0.51256) - 0.42429
# 2nd choice --> sugg_h = (barc_h * 0.976858) + 0.110030
# 3rd choice --> sugg_h = (osbs_h * 1.014450) - 0.865453

barc_hum <- humidity %>%
  filter(siteID == "BARC") %>%
  select(time, RHMean)
sugg_hum <- humidity %>%
  filter(siteID == "SUGG")%>%
  select(time, RHMean)
osbs_hum <- humidity %>%
  filter(siteID == "OSBS")%>%
  select(time, RHMean)

barc_sugg_osbs_humidity_models <- left_join(barc_hum, sugg_hum, by = "time") %>%
  left_join(., osbs_hum, by = "time") %>%
  select(time, RHMean.x, RHMean.y, RHMean) %>%
  rename(barc_h = RHMean.x, sugg_h = RHMean.y, osbs_h = RHMean)%>%
  filter(barc_h != "NaN")

barc_sugg_osbs_hum_model <- lm(barc_h ~ sugg_h, data = barc_sugg_osbs_humidity_models)
summary(barc_sugg_osbs_hum_model)


# Florida Lakes
# BARC --> SUGG
# BARC --> OSBS
# SUGG --> BARC
# SUGG --> OSBS

# Alaska site
# TOOK --> TOOL

