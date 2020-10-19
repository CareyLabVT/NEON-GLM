#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   Build empirical models to fill in NAs                *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    13Oct2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: Fill in the NAs with a better model than na.approx   *
#*****************************************************************

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
# 1st choice --> prla_lw = (dcfs_lw * 0.690547) + (prpo_lw * 0.310355) + 0.798390
# 2nd choice --> prla_lw = (dcfs_lw * 0.9845556) - 1.288e+01
# 3rd choice --> prla_lw = (prpo_lw * 1.004591) - 0.062928

prpo_longwave <- radiation %>%
  filter(siteID == "PRPO") %>%
  select(time, outLWMean)
prla_longwave <- radiation %>%
  filter(siteID == "PRLA")%>%
  select(time, outLWMean)
dcfs_longwave <- radiation %>%
  filter(siteID == "DCFS")%>%
  select(time, outLWMean)

prpo_prla_dcfs_longwave_models <- left_join(prpo_longwave, prla_longwave, by = "time") %>%
  left_join(., dcfs_longwave, by = "time") %>%
  select(time, outLWMean.x, outLWMean.y, outLWMean) %>%
  rename(prpo_lw = outLWMean.x, prla_lw = outLWMean.y, dcfs_lw = outLWMean)%>%
  filter(prpo_lw != "NaN")

prpo_prla_dcfs_lw_model <- lm(prpo_lw ~ prla_lw, data = prpo_prla_dcfs_longwave_models)
summary(prpo_prla_dcfs_lw_model)


# Florida Lakes
# BARC --> SUGG
# BARC --> OSBS
# SUGG --> BARC
# SUGG --> OSBS

# Alaska site
# TOOK --> TOOL

