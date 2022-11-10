#################################################################################
# Correlation % Sand and % TOC in MAOC
# Objective: Discussion paper
# Date: 28/01/2022
# Author: Mercedes Roman, modification of functions by Alexandre Wadoux
# Project: SOC fraction maps for TERN

### Load packages
require(ranger)
require(raster)
library(ggplot2)
library(readr)
library(caret)
library(scales)
library(viridis)
library(viridisLite)
library(ggmap)
library(purrr)
library(magrittr)
library(tidyverse)
library(sp)
library(raster)
library(sf)
library(rgdal)
library(dplyr)
library(jsonlite)
library(viridis)
library(psych)
library(corrplot)
library(ggplot2)
library(gplots)
library(dendextend)
library(colorspace)
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(gridExtra)

### Set input and output directories
HomeDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/for_Ross/"
InputDir  <- paste0(HomeDir,"Output_files/1_Output/")
OutDir <- paste0(HomeDir,"Figures/")
OutDir2 <- paste0(HomeDir,"Output_files/9_Output/")

# 1. Prepare calibration data ------------------------------------------------
setwd(InputDir)
load("SOCfr_Prop.GSM_03092021.RData")

HOCsoildat <- HOC_p.GSM ; rm(HOC_p.GSM)
POCsoildat <- POC_p.GSM ; rm(POC_p.GSM)
ROCsoildat <- ROC_p.GSM ; rm(ROC_p.GSM)

### Constrain maximum value to 100% 
HOCsoildat$`0-5 cm` <- ifelse(HOCsoildat$`0-5 cm` >100, 100, HOCsoildat$`0-5 cm`)
HOCsoildat$`5-15 cm` <- ifelse(HOCsoildat$`5-15 cm` >100, 100, HOCsoildat$`5-15 cm`)
HOCsoildat$`15-30 cm` <- ifelse(HOCsoildat$`15-30 cm` >100, 100, HOCsoildat$`15-30 cm`)
POCsoildat$`0-5 cm` <- ifelse(POCsoildat$`0-5 cm` >100, 100, POCsoildat$`0-5 cm`)
POCsoildat$`5-15 cm` <- ifelse(POCsoildat$`5-15 cm` >100, 100, POCsoildat$`5-15 cm`)
POCsoildat$`15-30 cm` <- ifelse(POCsoildat$`15-30 cm` >100, 100, POCsoildat$`15-30 cm`)
ROCsoildat$`0-5 cm` <- ifelse(ROCsoildat$`0-5 cm` >100, 100, ROCsoildat$`0-5 cm`)
ROCsoildat$`5-15 cm` <- ifelse(ROCsoildat$`5-15 cm` >100, 100, ROCsoildat$`5-15 cm`)
ROCsoildat$`15-30 cm` <- ifelse(ROCsoildat$`15-30 cm` >100, 100, ROCsoildat$`15-30 cm`)
extrHOC <- read.csv(paste0(HomeDir,"Output_files/2_Output/extrHOC.csv"))

### Change names
names(HOCsoildat)[2:4] <- c("HOC.0.5", "HOC.5.15", "HOC.15.30")
names(POCsoildat)[2:4] <- c("POC.0.5", "POC.5.15", "POC.15.30")
names(ROCsoildat)[2:4] <- c("ROC.0.5", "ROC.5.15", "ROC.15.30")

### Bind dataframes
SOCFr <- cbind(HOCsoildat[,1:4],POCsoildat[,2:4],ROCsoildat[,2:4],HOCsoildat[,c(5:7,9)],extrHOC[,2:ncol(extrHOC)])
SOCFr_extr <- cbind(HOCsoildat[,1:4],POCsoildat[,2:4],ROCsoildat[,2:4],HOCsoildat[,c(5:7,9)])
str(SOCFr_extr)

AusMap <- get_stamenmap(bbox = c(left=110, bottom=-45, right=157, top=-8),
                        maptype="toner-lite", zoom=5,
                        source="stamen", crop=TRUE)

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=0.1), color="#fb9f0f", data = SOCFr_extr)

covar.names <- c("Clim_ADM", "Clim_EPA", "Clim_EPI", "Clim_EPX", 
                 "Clim_Prescott", "Clim_PTA", "Clim_PTI", "Clim_PTS1", "Clim_PTS2", 
                 "Clim_PTX", "Clim_RSM", "Clim_TNM", "Clim_TRA", "Clim_TXM", "Clim_WDA", 
                 "NDVI_mean_Q1", "NDVI_mean_Q2", "NDVI_mean_Q3", "NDVI_mean_Q4", 
                 "PM_Gravity", "PM_radmap_v4_2019_filtered_dose_GAPFilled", "PM_radmap_v4_2019_filtered_pctk_GAPFilled", 
                 "PM_radmap_v4_2019_filtered_ppmt_GAPFilled", "PM_radmap_v4_2019_filtered_ppmu_GAPFilled", 
                 "PM_radmap_v4_2019_ratio_tk_GAPFilled", "PM_radmap_v4_2019_ratio_u2t_GAPFilled", 
                 "PM_radmap_v4_2019_ratio_uk_GAPFilled", "PM_radmap_v4_2019_ratio_ut_GAPFilled", 
                 "PM_Weathering_Index", "relief_dems_3s_mosaic1", "Relief_mrrtf_3s", 
                 "relief_mrvbf_3s_mosaic", "relief_plan_curvature_3s", "relief_profile_curvature_3", 
                 "relief_roughness", "relief_slope_perc", "relief_twi_3s", 
                 "Veg_FC_Max_BS", "Veg_FC_Max_NPV", "Veg_FC_Max_PV", "Veg_FC_Mean_BS", 
                 "Veg_FC_Mean_NPV", "Veg_FC_Mean_PV", "Veg_FC_Min_BS", "Veg_FC_Min_NPV", 
                 "Veg_FC_Min_PV", "Veg_FC_SD_BS", "Veg_FC_SD_NPV", "Veg_FC_SD_PV", 
                 "Veg_FPAR_Max", "Veg_FPAR_Mean", "Veg_FPAR_Median", "Veg_FPAR_Min", 
                 "Veg_LandCoverTrend_evi_mean", "Veg_Persistant_green_Veg")
rm(Vp.GSM)

# Correlation % Sand & % MAOC ---------------------------------------------

### Add BIOME data
biome_Aus <- read_sf("R:/PRJ-SoilBiodSOC2019/Covariates/Ecoregions2017/EcoregionsAus.shp")
plot(biome_Aus["BIOME_NAME"])

sf_use_s2(FALSE)
SOCFr.sf <- st_as_sf(SOCFr, coords = c("Long_WGS84","Lat_WGS84"), crs = 4326)
SOCFr.sf <- st_join(SOCFr.sf, biome_Aus["BIOME_NAME"], left=TRUE)
plot(SOCFr.sf["BIOME_NAME"])
SOCFr.sf <- SOCFr.sf[!is.na(SOCFr.sf$BIOME_NAME),]

SOCFractions.coords <- as.data.frame(st_coordinates(SOCFr.sf))
SOCFr.df <- st_drop_geometry(SOCFr.sf)
SOCFr.df <- cbind(SOCFr.df,SOCFractions.coords)

### longer for MAOC
MAOC.long <- tidyr::pivot_longer(data=SOCFr.df,
                                    cols=c("HOC.0.5","HOC.5.15","HOC.15.30"),
                                    names_to = "Depth",
                                    values_to = "MAOC") %>% as.data.frame()

my.labels <-c("Montane Grasslands\n & Shrublands",
              "Temperate Broadleaf\n & Mixed Forests",
              "Temperate Grasslands,\n Savannas & Shrublands",
              "Mediterranean Forests,\n Woodlands & Scrub",
              "Deserts &\n Xeric Shrublands",
              "Tropical & Subtropical\n Grasslands, Savannas\n & Shrublands",
              "Tropical & Subtropical\n Moist Broadleaf Forests")

names(MAOC.long)[names(MAOC.long)=="BIOME_NAME"] <- "Ecoregion"

MAOC.long$Ecoregion <- factor(MAOC.long$Ecoregion,
                              levels=c("Montane Grasslands & Shrublands",
                                       "Temperate Broadleaf & Mixed Forests",
                                       "Temperate Grasslands, Savannas & Shrublands",
                                       "Mediterranean Forests, Woodlands & Scrub",
                                       "Deserts & Xeric Shrublands",
                                       "Tropical & Subtropical Grasslands, Savannas & Shrublands",
                                       "Tropical & Subtropical Moist Broadleaf Forests"))
ggplot(MAOC.long)+
  geom_boxplot(aes(x=Ecoregion, y=MAOC)) +
  ylab(label="MAOC (% SOC)") +
  xlab(label="Ecoregion")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  ylim(0,100)+ 
  scale_x_discrete(labels= my.labels)

### Now check the sand content by ecoregion and relationship with MAOC

MAOC.long1 <- MAOC.long[MAOC.long$Depth=="HOC.0.5", ] %>%
  mutate(., sand =sand_0_5)
MAOC.long2 <- MAOC.long[MAOC.long$Depth=="HOC.5.15", ] %>%
  mutate(., sand =sand_5_15)
MAOC.long3 <- MAOC.long[MAOC.long$Depth=="HOC.15.30", ] %>%
  mutate(., sand =sand_15_30)

MAOC.Sand <- rbind(MAOC.long1,MAOC.long2,MAOC.long3)
MAOC.Sand <- MAOC.Sand[!duplicated(MAOC.Sand),]

ggplot(data=MAOC.Sand) +
  ylab(label="MAOM (% TOC)") +
  xlab(label="Sand (%)") +
  geom_point(aes(x=sand, y=MAOC, color=Ecoregion, alpha=0.01)) +
  scale_colour_viridis(discrete = TRUE, option="D",direction=-1) +
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  theme(legend.title = element_text(size=16, face="bold"))+
  theme(legend.text = element_text(size=12))+ 
  ylim(0,100)+
  xlim(0,100)+ facet_wrap(~ Ecoregion)

mypal <- c(wes_palette("FantasticFox1", n = 5),
           wes_palette("GrandBudapest2", n = 2))
           #"darkolivegreen2", "antiquewhite1", "cornflowerblue",
           #"darkcyan")

ggplot(data=MAOC.Sand) +
  ylab(label="MAOM (% TOC)") +
  xlab(label="Sand (%)") +
 geom_point(aes(x=sand, y=MAOC, color=Ecoregion, alpha=1/20)) +
  scale_colour_manual(values=mypal) +
  #scale_colour_viridis(discrete = TRUE, option="C",direction=-1)
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  theme(legend.title = element_text(size=16, face="bold"))+
  theme(legend.text = element_text(size=12))+ 
  ylim(0,100)+
  xlim(0,100)


ggplot(data=MAOC.Sand) +
  ylab(label="MAOM (% TOC)") +
  xlab(label="Ecoregion") +
  geom_boxplot(aes(x=Ecoregion, y=MAOC, color=Ecoregion, alpha=1/20)) +
  scale_colour_manual(values=mypal) +
  #scale_colour_viridis(discrete = TRUE, option="C",direction=-1)
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  theme(legend.title = element_text(size=16, face="bold"))+
  theme(legend.text = element_text(size=12))+ 
  ylim(0,100)+ 
  scale_x_discrete(labels= my.labels)

#+  geom_smooth(aes(x=sand, y=MAOC, colour = Ecoregion),
   #           method="lm",
    #          linetype = 1,
     #         se = FALSE)

ggplot(SOCFr.df)+
  geom_boxplot(aes(x=Ecoregion, y=sand_0_5)) +
  ylab(label="Sand %") +
  xlab(label="Ecoregion")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  ylim(0,100)

ggplot(SOCFr.df)+
  geom_boxplot(aes(x=Ecoregion, y=sand_5_15)) +
  ylab(label="Sand %") +
  xlab(label="Ecoregion")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  ylim(0,100)

ggplot(SOCFr.df)+
  geom_boxplot(aes(x=Ecoregion, y=sand_15_30)) +
  ylab(label="Sand %") +
  xlab(label="Ecoregion")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  ylim(0,100)

summary(c(SOCFr.df$sand_0_5,SOCFr.df$sand_5_15,SOCFr.df$sand_15_30))
sd(c(SOCFr.df$sand_0_5,SOCFr.df$sand_5_15,SOCFr.df$sand_15_30))

summary(SOCFr.df$sand_0_5)
summary(SOCFr.df$sand_5_15)
summary(SOCFr.df$sand_15_30)

sd(SOCFr.df$sand_0_5)
sd(SOCFr.df$sand_5_15)
sd(SOCFr.df$sand_15_30)

### longer for Sand - summarize by ecoregion
ggplot(data=MAOC.long) +
  ylab(label="MAOM (% TOC)") +
  xlab(label="Sand (%)") +
  geom_point(aes(x=Sand, y=MAOC, color=Ecoregion, alpha=1/20)) +
  scale_colour_viridis(discrete = TRUE, option="D",direction=-1) +
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  theme(legend.title = element_text(size=16, face="bold"))+
  theme(legend.text = element_text(size=12))+ 
  ylim(0,100)+
  xlim(0,100)

setwd(OutDir2)
Sand.long %>% 
  group_by(., Ecoregion, Depth) %>%
  summarise(., N = n(),
            mean.sand= round(mean(Sand, na.rm=TRUE),1),
            sd.sand= round(sd(Sand, na.rm=TRUE),1)) %>%
  mutate(., sand = paste0(round(mean.sand,1)," \u00b1 ",round(sd.sand,1)))%>%
  write.csv(., "Sand.Mean.SD.BIOME.Depth.csv")

Sand.long %>% 
  group_by(., Ecoregion) %>%
  summarise(., N = n(),
            mean.sand= round(mean(Sand, na.rm=TRUE),1),
            sd.sand= round(sd(Sand, na.rm=TRUE),1)) %>%
  mutate(., sand = paste0(round(mean.sand,1)," \u00b1 ",round(sd.sand,1))) %>%
  write.csv(., "Sand.Mean.SD.BIOME.csv")

### what about input?
SOCFr.df %>% 
  group_by(., Ecoregion) %>%
  summarise(., N = n(),
            mean.EVI= round(mean(Veg_LandCoverTrend_evi_mean, na.rm=TRUE),5),
            sd.EVI= round(sd(Veg_LandCoverTrend_evi_mean, na.rm=TRUE),5)) %>%
  mutate(., EVI = paste0(round(mean.EVI,5)," \u00b1 ",round(sd.EVI,5)))

ggplot(SOCFr.df)+
  geom_boxplot(aes(x=Ecoregion, y=Veg_LandCoverTrend_evi_mean)) +
  ylab(label="Mean EVI (X -X)") +
  xlab(label="Ecoregion")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))
summary(SOCFr.df$Veg_LandCoverTrend_evi_mean)


p1 <- ggplot(data=SOCFr.df) +
  ylab(label="MAOM (% TOC)") +
  xlab(label="Sand (%)") +
  geom_point(aes(x=sand_0_5, y=HOC.0.5, color=Ecoregion, alpha=1/20)) +
  scale_colour_viridis(discrete = TRUE, option="D",direction=-1) +
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  theme(legend.title = element_text(size=16, face="bold"))+
  theme(legend.text = element_text(size=10))+ ylim(0,100)+xlim(0,100)

p2 <- ggplot(data=SOCFr.df) +
  ylab(label="MAOM (% TOC)") +
  xlab(label="Sand (%)") +
  geom_point(aes(x=sand_0_5, y=HOC.0.5, color=Ecoregion, alpha=1/30)) +
  geom_point(aes(x=sand_5_15, y=HOC.5.15, color=Ecoregion, alpha=1/30)) +
  geom_point(aes(x=sand_15_30, y=HOC.15.30, color=Ecoregion, alpha=1/30)) +
  scale_colour_viridis(discrete = TRUE, option="D",direction=-1) +
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  theme(legend.title = element_text(size=16, face="bold"))+
  theme(legend.text = element_text(size=10))+ ylim(0,100)+xlim(0,100)

ggplot(data=SOCFr.df) +
  ylab(label="MAOM (% TOC)") +
  xlab(label="Sand (%)") +
  geom_point(aes(x=sand_0_5, y=HOC.0.5, color=Ecoregion, alpha=1/20)) +
  geom_point(aes(x=sand_5_15, y=HOC.5.15, color=Ecoregion, alpha=1/20)) +
  geom_point(aes(x=sand_15_30, y=HOC.15.30, color=Ecoregion, alpha=1/20)) +
  scale_colour_viridis(discrete = TRUE, option="D",direction=-1) +
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  theme(legend.title = element_text(size=16, face="bold"))+
  theme(legend.text = element_text(size=14))+ ylim(0,100)+xlim(0,100)
