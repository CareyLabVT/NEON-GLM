#*****************************************************************
#*                  CareyLabVT Blacksburg, VA USA                *
#*                                                               *
#* TITLE:   Meteorology data collation and comparisons           *
#* AUTHOR:  Ryan McClure                                         *
#* DATE:    08Oct2020                                            *
#* PROJECT: CIBR                                                 *
#* PURPOSE: Plot up met data and compare                         *
#*****************************************************************
  
# Make a large met time series plot
# -----------------------------------------------------------------------------------------------------------------
pdf("./figures/met_time_series2.pdf", width = 40, height = 30)
a <- ggplot(NEON_met_data_hourly, aes(time, SWMean))+
  geom_point(size = 0.5, pch = 19, color = "black")+
  theme(axis.text.x = element_text(size = 40, color = "black"),
        axis.text.y = element_text(size = 40, color = "black"))+
  theme_classic()+
  facet_wrap(~siteID, scales = "free")

b <- ggplot(NEON_met_data_hourly, aes(time, LWMean))+
  geom_point(size = 0.5, pch = 19, color = "black")+
  theme(axis.text.x = element_text(size = 40, color = "black"),
        axis.text.y = element_text(size = 40, color = "black"))+
  theme_classic()+
  facet_wrap(~siteID, scales = "free")

c <- ggplot(NEON_met_data_hourly, aes(time, tempSingleMean))+
  geom_point(size = 0.5, pch = 19, color = "black")+
  theme(axis.text.x = element_text(size = 40, color = "black"),
        axis.text.y = element_text(size = 40, color = "black"))+
  theme_classic()+
  facet_wrap(~siteID, scales = "free")

d <- ggplot(NEON_met_data_hourly, aes(time, WindSpeed))+
  geom_point(size = 0.5, pch = 19, color = "black")+
  theme(axis.text.x = element_text(size = 40, color = "black"),
        axis.text.y = element_text(size = 40, color = "black"))+
  theme_classic()+
  facet_wrap(~siteID, scales = "free")

e <- ggplot(NEON_met_data_hourly, aes(time, secPrecipBulk))+
  geom_point(size = 0.5, pch = 19, color = "black")+
  theme(axis.text.x = element_text(size = 40, color = "black"),
        axis.text.y = element_text(size = 40, color = "black"))+
  theme_classic()+
  facet_wrap(~siteID, scales = "free")

f <- ggplot(NEON_met_data_hourly, aes(time, RHMean))+
  geom_point(size = 0.5, pch = 19, color = "black")+
  theme(axis.text.x = element_text(size = 40, color = "black"),
        axis.text.y = element_text(size = 40, color = "black"))+
  theme_classic()+
  facet_wrap(~siteID, scales = "free")

g <- ggplot(NEON_met_data_hourly, aes(time, Snow))+
  geom_point(size = 0.5, pch = 19, color = "black")+
  theme(axis.text.x = element_text(size = 40, color = "black"),
        axis.text.y = element_text(size = 40, color = "black"))+
  theme_classic()+
  facet_wrap(~siteID, scales = "free")

gg <- (a+b+c+d)/(e+f+g+plot_spacer())
gg
dev.off() 
# -----------------------------------------------------------------------------------------------------------------

# Met heat map
# -----------------------------------------------------------------------------------------------------------------
NEON_met_data_heatmap <- NEON_met_data_hourly %>%
  mutate(year_month = format(as.Date(time, "%Y-%m-%d"), "%Y-%m"))%>%
  group_by(year_month, siteID)%>%
  summarize_all(funs(sum(is.na(.)) / length(.)))%>%
  select(-time)%>%
  melt(., id=c("year_month","siteID"))%>%
  ggplot(aes(as.factor(year_month), variable, fill= 1-value)) + 
  geom_tile()+
  theme_classic()+
  scale_fill_distiller(palette ="Spectral",trans = "reverse") +
  ylab("")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45, size = 20, color = "black", hjust=1.2),
        axis.text.y = element_text(size = 30, color = "black"))+
  facet_wrap(~siteID)

pdf("./figures/met_heatmap3.pdf", width = 40, height = 20)
NEON_met_data_heatmap
dev.off()
# -----------------------------------------------------------------------------------------------------------------