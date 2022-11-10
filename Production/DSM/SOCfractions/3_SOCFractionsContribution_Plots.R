########################################################################################################################################################
### Make some plots for explore the TOC concentration and distribution among fractions with depth
### Project: SOC fraction maps for TERN
### Date: 14/07/2021
### Author: Mercedes Roman Dobarco
### Load packages

library(ggmap)
library(purrr)
library(magrittr)
library(tidyverse)
library(dplyr)
library(jsonlite)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(corrplot)
library(Hmisc)
library(lattice)
library(MASS)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(viridis)
library(gplots)
library(colorspace)
library(gridExtra)
library(rasterVis)
library(lattice)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(shiny)
library(ggmap)
library(ClusterR)
library(dendsort)
library(dendextend)
library(robustbase)
#detach("package:aqp", unload=TRUE)

### Load SOC fraction data

HomeDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/for_Ross/"
InputDir  <- paste0(HomeDir,"Output_files/1_Output/")
OutDir  <- paste0(HomeDir,"Output_files/3_Output/")
setwd(InputDir)

load("1_Data_SOCfractionProportions_03092021.RData")
setwd(OutDir)

#HOC_p.GSM, POC_p.GSM, ROC_p.GSM, Vp.GSM, 
unique(POC_p.GSM$Dataset)

# ### 1. Boxplots SOC fractions contribution (%) --------
### From wide to long
HOC.GSM.long <- tidyr::pivot_longer(data=HOC_p.GSM, 
                                    cols=c("0-5 cm", "5-15 cm", "15-30 cm"),
                                    names_to = "Depth",
                                    values_to = "SOC_p") %>% as.data.frame()
ROC.GSM.long <- tidyr::pivot_longer(data=ROC_p.GSM, 
                                    cols=c("0-5 cm", "5-15 cm", "15-30 cm"),
                                    names_to = "Depth",
                                    values_to = "SOC_p") %>% as.data.frame()
POC.GSM.long <- tidyr::pivot_longer(data=POC_p.GSM, 
                                    cols=c("0-5 cm", "5-15 cm", "15-30 cm"),
                                    names_to = "Depth",
                                    values_to = "SOC_p") %>% as.data.frame()
SOCFractions.long <- rbind(HOC.GSM.long,ROC.GSM.long,POC.GSM.long)
SOCFractions.long$SOCFr <- as.factor(SOCFractions.long$SOCFr)
SOCFractions.long$Depth <- factor(SOCFractions.long$Depth, levels=c("0-5 cm", "5-15 cm", "15-30 cm"))

SOCFractions.long.Max <- SOCFractions.long
SOCFractions.long.Max$SOC_p <- ifelse(SOCFractions.long.Max$SOC_p>100,
                                      100,
                                      SOCFractions.long.Max$SOC_p)

library(wesanderson)
ggplot()+
  geom_boxplot(data=SOCFractions.long.Max, aes(x=Depth, y=SOC_p, fill=SOCFr)) +
  ylab(label="Proportion of SOC fraction (% TOC)") +
  xlab(label="Depth interval")+ 
  scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest1"), 
                    name="SOC fraction",
                    breaks=c("HOC_p", "POC_p", "ROC_p"),
                    labels=c("MAOC", "POC", "PyOC"))+
  theme(legend.title = element_text(size=16, face="bold"))+
  theme(legend.text = element_text(size=14))+
  theme(axis.text = element_text(size=14))+
  theme(axis.title = element_text(size=16))+
  ylim(0,100)

# 
# ### Now to wide, for calculating contribution of each fraction to total TOC
HOC.GSM.long1 <- tidyr::pivot_longer(data=HOC_p.GSM,
                                    cols=c("0-5 cm", "5-15 cm", "15-30 cm"),
                                    names_to = "Depth",
                                    values_to = "HOC") %>% as.data.frame()
ROC.GSM.long1 <- tidyr::pivot_longer(data=ROC_p.GSM,
                                    cols=c("0-5 cm", "5-15 cm", "15-30 cm"),
                                    names_to = "Depth",
                                    values_to = "ROC") %>% as.data.frame()
POC.GSM.long1 <- tidyr::pivot_longer(data=POC_p.GSM,
                                    cols=c("0-5 cm", "5-15 cm", "15-30 cm"),
                                    names_to = "Depth",
                                    values_to = "POC") %>% as.data.frame()
### Join together
SOCFractions.wide <- dplyr::inner_join(HOC.GSM.long1[,names(HOC.GSM.long1)!= "SOCFr"],
                                       ROC.GSM.long1[,names(ROC.GSM.long1)!= "SOCFr"])
SOCFractions.wide <- dplyr::inner_join(SOCFractions.wide,
                                       POC.GSM.long1[,names(POC.GSM.long1)!= "SOCFr"])
SOCFractions.wide <- SOCFractions.wide[!is.na(SOCFractions.wide$POC),]

### constrain max to 100 %
SOCFractions.wide$HOC <- ifelse(SOCFractions.wide$HOC>100, 100, SOCFractions.wide$HOC)
SOCFractions.wide$POC <- ifelse(SOCFractions.wide$POC>100, 100, SOCFractions.wide$POC)
SOCFractions.wide$ROC <- ifelse(SOCFractions.wide$ROC>100, 100, SOCFractions.wide$ROC)

# SOCFractions.wide$HOC_p <- (SOCFractions.wide$HOC/(SOCFractions.wide$HOC+SOCFractions.wide$ROC+SOCFractions.wide$POC))*100
# SOCFractions.wide$ROC_p <- (SOCFractions.wide$ROC/(SOCFractions.wide$HOC+SOCFractions.wide$ROC+SOCFractions.wide$POC))*100
# SOCFractions.wide$POC_p <- (SOCFractions.wide$POC/(SOCFractions.wide$HOC+SOCFractions.wide$ROC+SOCFractions.wide$POC))*100
SOCFractions.wide$Vp <- ((SOCFractions.wide$POC+0.01)/(SOCFractions.wide$HOC+SOCFractions.wide$ROC+0.01))

SOCFractions.Vp.wide <- tidyr::pivot_wider(SOCFractions.wide[,c(1:6,10)], 
                                           values_from = Vp, names_from=Depth, names_prefix="Vp.") %>% as.data.frame()
colnames(SOCFractions.Vp.wide) <- c("Dataset","Lat_WGS84","Long_WGS84","Location_ID","Spectra",
                                    "Vp.0.5","Vp.5.15","Vp.15.30")
write.csv(SOCFractions.Vp.wide, file="SOCFractions.Vp.wide.csv")

SOCFractions.wide %>% 
  group_by(.,  Depth) %>%
  summarise(., N = n(),
            mean.Vp= round(mean(Vp, na.rm=TRUE),2),
            sd.Vp= round(sd(Vp, na.rm=TRUE),2),
            mean.HOC= round(mean(HOC, na.rm=TRUE),1),
            sd.HOC= round(sd(HOC, na.rm=TRUE),1),
            mean.POC= round(mean(POC, na.rm=TRUE),1),
            sd.POC= round(sd(POC, na.rm=TRUE),1),
            mean.ROC= round(mean(ROC, na.rm=TRUE),1),
            sd.ROC= round(sd(ROC, na.rm=TRUE),1)) %>%
  mutate(., Vp = paste0(round(mean.Vp,2)," \u00b1 ",round(sd.Vp,2))) %>%
  mutate(., HOC = paste0(round(mean.HOC,1)," \u00b1 ",round(sd.HOC,1))) %>%
  mutate(., POC = paste0(round(mean.POC,1)," \u00b1 ",round(sd.POC,1))) %>%
  mutate(., ROC = paste0(round(mean.ROC,1)," \u00b1 ",round(sd.ROC,1))) %>%
  write.csv(., "SOCFractions.mean.SD.Depth.csv")


SOCFractions.wide %>% 
  group_by(.,  Depth) %>%
  summarise(., N = n(),
            median.Vp= round(median(Vp, na.rm=TRUE),2),
            sd.Vp= round(sd(Vp, na.rm=TRUE),2),
            median.HOC= round(median(HOC, na.rm=TRUE),1),
            sd.HOC= round(sd(HOC, na.rm=TRUE),1),
            median.POC= round(median(POC, na.rm=TRUE),1),
            sd.POC= round(sd(POC, na.rm=TRUE),1),
            median.ROC= round(median(ROC, na.rm=TRUE),1),
            sd.ROC= round(sd(ROC, na.rm=TRUE),1)) %>%
  mutate(., Vp = paste0(round(median.Vp,2)," \u00b1 ",round(sd.Vp,2))) %>%
  mutate(., HOC = paste0(round(median.HOC,1)," \u00b1 ",round(sd.HOC,1))) %>%
  mutate(., POC = paste0(round(median.POC,1)," \u00b1 ",round(sd.POC,1))) %>%
  mutate(., ROC = paste0(round(median.ROC,1)," \u00b1 ",round(sd.ROC,1))) %>%
  write.csv(., "SOCFractions.median.SD.Depth.csv")

SOCFractions.wide %>% 
  summarise(., N = n(),
            mean.Vp= round(mean(Vp, na.rm=TRUE),2),
            sd.Vp= round(sd(Vp, na.rm=TRUE),2),
            mean.HOC= round(mean(HOC, na.rm=TRUE),1),
            sd.HOC= round(sd(HOC, na.rm=TRUE),1),
            mean.POC= round(mean(POC, na.rm=TRUE),1),
            sd.POC= round(sd(POC, na.rm=TRUE),1),
            mean.ROC= round(mean(ROC, na.rm=TRUE),1),
            sd.ROC= round(sd(ROC, na.rm=TRUE),1)) %>%
  mutate(., Vp = paste0(round(mean.Vp,2)," \u00b1 ",round(sd.Vp,2))) %>%
  mutate(., HOC = paste0(round(mean.HOC,1)," \u00b1 ",round(sd.HOC,1))) %>%
  mutate(., POC = paste0(round(mean.POC,1)," \u00b1 ",round(sd.POC,1))) %>%
  mutate(., ROC = paste0(round(mean.ROC,1)," \u00b1 ",round(sd.ROC,1))) %>% as.data.frame()
#       N mean.Vp sd.Vp mean.HOC sd.HOC mean.POC sd.POC mean.ROC sd.ROC        Vp         HOC         POC         ROC
# 1 41896    0.69 60.42     58.8   17.5     13.1   11.1     28.2   17.5 0.69 ± 60.42 58.8 ± 17.5 13.1 ± 11.1 28.2 ± 17.5

SOCFractions.wide %>% 
  summarise(., N = n(),
            median.Vp= round(median(Vp, na.rm=TRUE),2),
            sd.Vp= round(sd(Vp, na.rm=TRUE),2),
            median.HOC= round(median(HOC, na.rm=TRUE),1),
            sd.HOC= round(sd(HOC, na.rm=TRUE),1),
            median.POC= round(median(POC, na.rm=TRUE),1),
            sd.POC= round(sd(POC, na.rm=TRUE),1),
            median.ROC= round(median(ROC, na.rm=TRUE),1),
            sd.ROC= round(sd(ROC, na.rm=TRUE),1)) %>%
  mutate(., Vp = paste0(round(median.Vp,2)," \u00b1 ",round(sd.Vp,2))) %>%
  mutate(., HOC = paste0(round(median.HOC,1)," \u00b1 ",round(sd.HOC,1))) %>%
  mutate(., POC = paste0(round(median.POC,1)," \u00b1 ",round(sd.POC,1))) %>%
  mutate(., ROC = paste0(round(median.ROC,1)," \u00b1 ",round(sd.ROC,1))) %>% as.data.frame()
#       N median.Vp sd.Vp median.HOC sd.HOC median.POC sd.POC median.ROC sd.ROC         Vp         HOC        POC         ROC
# 1 41896      0.11 60.42       61.3   17.5        9.9   11.1       25.4   17.5 0.11 ± 60.42 61.3 ± 17.5 9.9 ± 11.1 25.4 ± 17.5

# SOCFractions.Contrib.long <- tidyr::pivot_longer(data=SOCFractions.wide, 
#                                                  cols=c("HOC_p", "POC_p","ROC_p"),
#                                                  names_to = "SOC_fraction",
#                                                  values_to = "Contribution_TOC") %>% as.data.frame()
# 
# SOCFractions.Contrib.long$SOC_fraction <- factor(SOCFractions.Contrib.long$SOC_fraction,levels= c("HOC_p", "POC_p","ROC_p"))
# SOCFractions.Contrib.long$Depth <- factor(SOCFractions.Contrib.long$Depth, levels=c("0-5 cm", "5-15 cm", "15-30 cm"))

# Boxplot contribution of SOC fractions to total TOC
# ggplot()+
#   geom_boxplot(data=SOCFractions.Contrib.long, aes(x=Depth, y=Contribution_TOC, fill=SOC_fraction)) +
#   ylab(label="SOC fraction contribution to TOC (%)") +
#   xlab(label="Depth interval")+
#  scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest1"),
#                   name="SOC fraction",
#                   breaks=c("HOC_p", "POC_p", "ROC_p"),
#                   labels=c("HOC", "POC", "ROC"))+
# theme(legend.title = element_text(size=16, face="bold"))+
# theme(legend.text = element_text(size=14))+
# theme(axis.text = element_text(size=14))+
# theme(axis.title = element_text(size=16))

### Vulnerability with depth
SOCFractions.wide$Depth <- factor(SOCFractions.wide$Depth, levels=c("0-5 cm", "5-15 cm", "15-30 cm"))

ggplot()+
  geom_boxplot(data=SOCFractions.wide, 
               aes(x=Depth, y=Vp)) +
  ylab(label="TOC potential vulnerability") +
  xlab(label="Depth interval")+
  theme(axis.text = element_text(size=14))+
  theme(axis.title = element_text(size=16))+
  ylim(0,5)

ggplot()+
  geom_histogram(data=SOCFractions.wide, aes(x=Vp),binwidth = 0.01) +
  xlab(label="TOC potential vulnerability") +
  ylab(label="Counts")+
  theme(axis.text = element_text(size=14))+
  theme(axis.title = element_text(size=16))+
  facet_wrap(~Depth)+
  xlim(0,2)

### Plot the locations
AusMap <- get_stamenmap(bbox = c(left=110, bottom=-45, right=157, top=-8),
                        maptype="toner-lite", zoom=4,
                        source="stamen", crop=TRUE)

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84,color=Spectra), size=0.5,
             data = SOCFractions.wide) +
  scale_colour_manual(values=wes_palette(n=2, name="Darjeeling2"),
                       name="Spectral \ninference model",
                       breaks=c("MIR","Vis-NIR"),
                    labels=c("MIR","Vis-NIR"))+
  theme(legend.title = element_text(size=16, face="bold"))+
  theme(legend.text = element_text(size=14))
  #scale_color_viridis(discrete = TRUE, option="C", direction = -1)

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84,color=Vp), 
             data = SOCFractions.wide[SOCFractions.wide$Depth == "0-5 cm" &
                                        SOCFractions.wide$Vp <=5,]) +
  scale_color_viridis(discrete = FALSE, option="A", direction = -1,trans="pseudo_log")

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84,color=Vp), 
             data = SOCFractions.wide[SOCFractions.wide$Depth == "5-15 cm" &
                                        SOCFractions.wide$Vp <=5,]) +
  scale_color_viridis(discrete = FALSE, option="A", direction = -1,trans="pseudo_log")

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84,color=Vp), 
             data = SOCFractions.wide[SOCFractions.wide$Depth == "15-30 cm" &
                                        SOCFractions.wide$Vp <=5,]) +
  scale_color_viridis(discrete = FALSE, option="A", direction = -1,trans="pseudo_log")


vp <- ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84,color=Vp), 
             size=0.1,
             data = SOCFractions.wide[SOCFractions.wide$Vp <=5 & !is.na(SOCFractions.wide$Vp),]) +
  scale_color_viridis(discrete = FALSE, option="A", direction = -1,trans="pseudo_log")+
  facet_grid(~Depth)

POC_p <- ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84,color=POC), size=0.1,
             data = SOCFractions.wide[!is.na(SOCFractions.wide$POC),]) +
  scale_color_viridis(discrete = FALSE, option="A", direction = -1)+
  facet_grid(~Depth)

ROC_p <-ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84,color=ROC), size=0.1,
             data = SOCFractions.wide[!is.na(SOCFractions.wide$ROC),]) +
  scale_color_viridis(discrete = FALSE, option="A", direction = -1)+
  facet_grid(~Depth)

HOC_p <- ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84,color=HOC), size=0.1,
             data = SOCFractions.wide[!is.na(SOCFractions.wide$HOC),]) +
  scale_color_viridis(discrete = FALSE, option="A", direction = -1)+
  facet_grid(~Depth)

grid.arrange(HOC_p, POC_p, ROC_p, vp, nrow=4)


# ### 2. Boxplots SOC fractions concentration and contribution (%) - Biome class, agroclimatic zone, land cover
###     and agroclimatic zone
biome_Aus <- read_sf("C:/Covariates/Ecoregions2017/EcoregionsAus.shp")
plot(biome_Aus["BIOME_NAME"])

agroclim <- raster("C:/Covariates/90m/Clim_agroClim_hutch/Clim_agroClim_hutch.tif")
plot(agroclim)

### Add categories
agroclim.levels <- ratify(agroclim)
# Add some custom labels for each class.
levels(agroclim.levels)[[1]]$Agroclim <- c("B1", #1
                                           "F3", #2
                                           "F4", #3
                                           "G",  #4
                                           "H",  #5 
                                           "I1", #6
                                            "I2", #7
                                            "I3", #8
                                            "J1", #9
                                            "J2", #10
                                            "B2", #11
                                            "D5", #12
                                            "E1", #13
                                            "E2", #14
                                            "E3", #15
                                            "E4", #16
                                            "E6", #17
                                            "E7") #18
                                            
levels(agroclim.levels)
# Custom palette
#my_palette <- c("mediumseagreen","olivedrab2","orchid1","blueviolet","darkkhaki", "gold1")
par(mfrow=c(1,1))
levelplot(agroclim.levels, col.regions = viridis_pal(option = "C")(18))

##Add biome and agroclimatic information to SOC fraction data
sf_use_s2(FALSE)
SOCFractions.wide.sf <- st_as_sf(SOCFractions.wide, coords = c("Long_WGS84","Lat_WGS84"), crs = 4326)
SOCFractions.wide.sf <- st_join(SOCFractions.wide.sf, biome_Aus["BIOME_NAME"], left=TRUE)
plot(SOCFractions.wide.sf["BIOME_NAME"])
SOCFractions.wide.sf[is.na(SOCFractions.wide.sf$BIOME_NAME),]

SOCFractions.wide.sf$agroclim <- raster::extract(agroclim.levels, SOCFractions.wide.sf, sp=FALSE)
### Recode
SOCFractions.wide.sf <- SOCFractions.wide.sf %>% mutate(agroclim.l = recode(agroclim, 
                                                                            `1`="B1", #1
                                                                            `2`="F3", #2
                                                                            `3`="F4", #3
                                                                            `4`="G",  #4
                                                                            `5`="H",  #5 
                                                                            `6`="I1", #6
                                                                            `7`="I2", #7
                                                                            `8`="I3", #8
                                                                            `9`="J1", #9
                                                                            `10`="J2", #10
                                                                            `11`="B2", #11
                                                                            `12`="D5", #12
                                                                            `13`="E1", #13
                                                                            `14`="E2", #14
                                                                            `15`="E3", #15
                                                                            `16`="E4", #16
                                                                            `17`="E6", #17
                                                                            `18`="E7"))
### Land Cover
land_cover <- raster("C:/Covariates/LandCover/DLCDv1_Class.tif")
DLCD_Colours_Labels <- read_csv("C:/Covariates/LandCover/DLCD_Colours_Labels.csv")
SOCFractions.wide.sf$land_cover <- raster::extract(land_cover, SOCFractions.wide.sf, sp=FALSE)
SOCFractions.wide.sf <- dplyr::left_join(SOCFractions.wide.sf,DLCD_Colours_Labels[,c(1,5,6)], by = c("land_cover"="Class #"))

# Biome -------------------------------------------------------------------

SOCFractions.wide.sf$BIOME_NAME <-
  factor(SOCFractions.wide.sf$BIOME_NAME,
         levels=c("Montane Grasslands & Shrublands",
                  "Temperate Broadleaf & Mixed Forests",
                  "Temperate Grasslands, Savannas & Shrublands",
                  "Mediterranean Forests, Woodlands & Scrub",
                  "Deserts & Xeric Shrublands",
                  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
                  "Tropical & Subtropical Moist Broadleaf Forests"))

ggplot()+
  geom_boxplot(data=SOCFractions.wide.sf[!is.na(SOCFractions.wide.sf$BIOME_NAME),], 
               aes(x=BIOME_NAME, y=Vp)) +
  ylab(label="TOC potential vulnerability") +
  xlab(label="Biome")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  ylim(0,5)

ggplot()+
  geom_boxplot(data=SOCFractions.wide.sf[!is.na(SOCFractions.wide.sf$BIOME_NAME),], 
               aes(x=BIOME_NAME, y=HOC)) +
  ylab(label="MAOM (% SOC)") +
  xlab(label="Biome")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))

ggplot()+
  geom_boxplot(data=SOCFractions.wide.sf[!is.na(SOCFractions.wide.sf$BIOME_NAME),], 
               aes(x=BIOME_NAME, y=POC)) +
  ylab(label="POC (% SOC)") +
  xlab(label="Biome")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))

ggplot()+
  geom_boxplot(data=SOCFractions.wide.sf[!is.na(SOCFractions.wide.sf$BIOME_NAME),], 
               aes(x=BIOME_NAME, y=ROC)) +
  ylab(label="PyOC (% SOC)") +
  xlab(label="Biome")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))

### Back to dataframe
SOCFractions.wide.sf.coords <- as.data.frame(st_coordinates(SOCFractions.wide.sf))
SOCFractions.wide.df <- st_drop_geometry(SOCFractions.wide.sf)
SOCFractions.wide.df <- cbind(SOCFractions.wide.sf.coords,SOCFractions.wide.df)

#SOCFractions.wide.df[!is.finite(SOCFractions.wide.df$Vp),]$Vp <- NA

SOCFractions.wide.df %>% 
  group_by(., BIOME_NAME, Depth) %>%
  summarise(., N = n(),
            median.Vp= round(median(Vp, na.rm=TRUE),2),
            sd.Vp= round(sd(Vp, na.rm=TRUE),2),
            median.HOC= round(median(HOC, na.rm=TRUE),1),
            sd.HOC= round(sd(HOC, na.rm=TRUE),1),
            median.POC= round(median(POC, na.rm=TRUE),1),
            sd.POC= round(sd(POC, na.rm=TRUE),1),
            median.ROC= round(median(ROC, na.rm=TRUE),1),
            sd.ROC= round(sd(ROC, na.rm=TRUE),1)) %>%
    mutate(., Vp = paste0(round(median.Vp,2)," \u00b1 ",round(sd.Vp,2))) %>%
    mutate(., HOC = paste0(round(median.HOC,1)," \u00b1 ",round(sd.HOC,1))) %>%
    mutate(., POC = paste0(round(median.POC,1)," \u00b1 ",round(sd.POC,1))) %>%
    mutate(., ROC = paste0(round(median.ROC,1)," \u00b1 ",round(sd.ROC,1))) %>%
    write.csv(., "SOCFractions.Median.SD.BIOME.Depth.csv")


SOCFractions.wide.df %>% 
  group_by(., BIOME_NAME, Depth) %>%
  summarise(., N = n(),
            mean.Vp= round(mean(Vp, na.rm=TRUE),2),
            sd.Vp= round(sd(Vp, na.rm=TRUE),2),
            mean.HOC= round(mean(HOC, na.rm=TRUE),1),
            sd.HOC= round(sd(HOC, na.rm=TRUE),1),
            mean.POC= round(mean(POC, na.rm=TRUE),1),
            sd.POC= round(sd(POC, na.rm=TRUE),1),
            mean.ROC= round(mean(ROC, na.rm=TRUE),1),
            sd.ROC= round(sd(ROC, na.rm=TRUE),1)) %>%
  mutate(., Vp = paste0(round(mean.Vp,2)," \u00b1 ",round(sd.Vp,2))) %>%
  mutate(., HOC = paste0(round(mean.HOC,1)," \u00b1 ",round(sd.HOC,1))) %>%
  mutate(., POC = paste0(round(mean.POC,1)," \u00b1 ",round(sd.POC,1))) %>%
  mutate(., ROC = paste0(round(mean.ROC,1)," \u00b1 ",round(sd.ROC,1))) %>%
  write.csv(., "SOCFractions.Mean.SD.BIOME.Depth.csv")

ggplot()+
  geom_boxplot(data=SOCFractions.wide.df[!is.na(SOCFractions.wide.df$BIOME_NAME),], 
               aes(x=BIOME_NAME, y=HOC)) +
  ylab(label="HOC (% TOC)") +
  xlab(label="Biome")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))

ggplot()+
  geom_boxplot(data=SOCFractions.wide.df[!is.na(SOCFractions.wide.df$BIOME_NAME),], 
               aes(x=BIOME_NAME, y=POC)) +
  ylab(label="POC (% TOC)") +
  xlab(label="Biome")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))

ggplot() +
  geom_boxplot(data=SOCFractions.wide.df[!is.na(SOCFractions.wide.df$BIOME_NAME),], 
               aes(x=BIOME_NAME, y=ROC)) +
  ylab(label="ROC (% TOC)") +
  xlab(label="Biome")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))

### Recode agroclimatic zones
### Recode
SOCFractions.wide.df <- SOCFractions.wide.df %>% mutate(agroclim.broad = recode(agroclim, 
                                                                            `1`="B", #1
                                                                            `2`="F", #2
                                                                            `3`="F", #3
                                                                            `4`="G",  #4
                                                                            `5`="H",  #5 
                                                                            `6`="I", #6
                                                                            `7`="I", #7
                                                                            `8`="I", #8
                                                                            `9`="J", #9
                                                                            `10`="J", #10
                                                                            `11`="B", #11
                                                                            `12`="D", #12
                                                                            `13`="E", #13
                                                                            `14`="E", #14
                                                                            `15`="E", #15
                                                                            `16`="E", #16
                                                                            `17`="E", #17
                                                                            `18`="E"))

### Agroclimatic zone
ggplot()+
  geom_boxplot(data=SOCFractions.wide.df[!is.na(SOCFractions.wide.df$agroclim.broad),], 
               aes(x=agroclim.broad, y=Vp)) +
  ylab(label="TOC potential vulnerability") +
  xlab(label="Agroclimatic zone")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  ylim(0,0.5)

ggplot()+
  geom_boxplot(data=SOCFractions.wide.df[!is.na(SOCFractions.wide.df$agroclim.broad),], 
               aes(x=agroclim.broad, y=HOC)) +
  ylab(label="HOC (% TOC)") +
  xlab(label="Biome")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))

ggplot()+
  geom_boxplot(data=SOCFractions.wide.df[!is.na(SOCFractions.wide.df$agroclim.broad),], 
               aes(x=agroclim.broad, y=POC)) +
  ylab(label="POC (% TOC)") +
  xlab(label="Biome")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))

ggplot()+
  geom_boxplot(data=SOCFractions.wide.df[!is.na(SOCFractions.wide.df$agroclim.broad),], 
               aes(x=agroclim.broad, y=ROC)) +
  ylab(label="ROC (% TOC)") +
  xlab(label="Biome")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))

# SOCFractions.wide.df %>% 
#   group_by(., agroclim.broad) %>%
#   summarise(., N = n(),
#             median.Vp= round(median(Vp, na.rm=TRUE),2),
#             sd.Vp= round(sd(Vp, na.rm=TRUE),2),
#             median.HOC= round(median(HOC, na.rm=TRUE),1),
#             sd.HOC= round(sd(HOC, na.rm=TRUE),1),
#             median.POC= round(median(POC, na.rm=TRUE),1),
#             sd.POC= round(sd(POC, na.rm=TRUE),1),
#             median.ROC= round(median(ROC, na.rm=TRUE),1),
#             sd.ROC= round(sd(ROC, na.rm=TRUE),1),
#             median.HOC_p= round(median(HOC_p, na.rm=TRUE),1),
#             sd.HOC_p= round(sd(HOC_p, na.rm=TRUE),1),
#             median.POC_p= round(median(POC_p, na.rm=TRUE),1),
#             sd.POC_p= round(sd(POC_p, na.rm=TRUE),1),
#             median.ROC_p= round(median(ROC_p, na.rm=TRUE),1),
#             sd.ROC_p= round(sd(ROC_p, na.rm=TRUE),1)) %>%
#   mutate(., Vp = paste0(round(median.Vp,2)," \u00b1 ",round(sd.Vp,2))) %>%
#   mutate(., HOC = paste0(round(median.HOC,1)," \u00b1 ",round(sd.HOC,1))) %>%
#   mutate(., POC = paste0(round(median.POC,1)," \u00b1 ",round(sd.POC,1))) %>%
#   mutate(., ROC = paste0(round(median.ROC,1)," \u00b1 ",round(sd.ROC,1))) %>%
#   mutate(., HOC_p = paste0(round(median.HOC_p,1)," \u00b1 ",round(sd.HOC_p,1))) %>%
#   mutate(., POC_p = paste0(round(median.POC_p,1)," \u00b1 ",round(sd.POC_p,1))) %>%
#   mutate(., ROC_p = paste0(round(median.ROC_p,1)," \u00b1 ",round(sd.ROC_p,1))) %>%
#   write.csv(., "SOCFractions.Median.SD.agroclim.csv")

### Land cover?
ggplot()+
  geom_boxplot(data=SOCFractions.wide.df[!is.na(SOCFractions.wide.df$Label),], 
               aes(x=Label, y=Vp)) +
  ylab(label="TOC potential vulnerability") +
  xlab(label="Land Cover")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  ylim(0,0.5)

ggplot()+
  geom_boxplot(data=SOCFractions.wide.df[!is.na(SOCFractions.wide.df$Label),], 
               aes(x=Label, y=HOC)) +
  ylab(label="HOC (% TOC)") +
  xlab(label="Land Cover")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))

ggplot()+
  geom_boxplot(data=SOCFractions.wide.df[!is.na(SOCFractions.wide.df$Label),], 
               aes(x=Label, y=POC)) +
  ylab(label="POC (% TOC)") +
  xlab(label="Land Cover")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))

ggplot()+
  geom_boxplot(data=SOCFractions.wide.df[!is.na(SOCFractions.wide.df$Label),], 
               aes(x=Label, y=ROC)) +
  ylab(label="ROC (% TOC)") +
  xlab(label="Land Cover")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))

# SOCFractions.wide.df %>% 
#   group_by(., Label) %>%
#   summarise(., N = n(),
#             median.Vp= round(median(Vp, na.rm=TRUE),2),
#             sd.Vp= round(sd(Vp, na.rm=TRUE),2),
#             median.HOC= round(median(HOC, na.rm=TRUE),1),
#             sd.HOC= round(sd(HOC, na.rm=TRUE),1),
#             median.POC= round(median(POC, na.rm=TRUE),1),
#             sd.POC= round(sd(POC, na.rm=TRUE),1),
#             median.ROC= round(median(ROC, na.rm=TRUE),1),
#             sd.ROC= round(sd(ROC, na.rm=TRUE),1),
#             median.HOC_p= round(median(HOC_p, na.rm=TRUE),1),
#             sd.HOC_p= round(sd(HOC_p, na.rm=TRUE),1),
#             median.POC_p= round(median(POC_p, na.rm=TRUE),1),
#             sd.POC_p= round(sd(POC_p, na.rm=TRUE),1),
#             median.ROC_p= round(median(ROC_p, na.rm=TRUE),1),
#             sd.ROC_p= round(sd(ROC_p, na.rm=TRUE),1)) %>%
#   mutate(., Vp = paste0(round(median.Vp,2)," \u00b1 ",round(sd.Vp,2))) %>%
#   mutate(., HOC = paste0(round(median.HOC,1)," \u00b1 ",round(sd.HOC,1))) %>%
#   mutate(., POC = paste0(round(median.POC,1)," \u00b1 ",round(sd.POC,1))) %>%
#   mutate(., ROC = paste0(round(median.ROC,1)," \u00b1 ",round(sd.ROC,1))) %>%
#   mutate(., HOC_p = paste0(round(median.HOC_p,1)," \u00b1 ",round(sd.HOC_p,1))) %>%
#   mutate(., POC_p = paste0(round(median.POC_p,1)," \u00b1 ",round(sd.POC_p,1))) %>%
#   mutate(., ROC_p = paste0(round(median.ROC_p,1)," \u00b1 ",round(sd.ROC_p,1))) %>%
#   write.csv(., "SOCFractions.Median.SD.LCclass.csv")

### Create a category for cropping
unique(SOCFractions.wide.df$Label)
SOCFractions.wide.df$Cropping <- ifelse(SOCFractions.wide.df$Label %in%
                                          c("Irrigated Cropping","Rainfed Cropping",
                                            "Irrigated Pasture","Rainfed Pasture",
                                            "Irrigated Sugar","Rainfed Sugar"),
                                        "Cropping", "Natural")

###Biome/Fraction
SOCFractions.wide.df1 <- SOCFractions.wide.df[!(is.na(SOCFractions.wide.df$Cropping))|is.na(SOCFractions.wide.df$BIOME_NAME),]
SOCFractions.wide.df1$Cropping <- as.factor(SOCFractions.wide.df1$Cropping)
SOCFractions.wide.df1$BIOME_NAME <- factor(SOCFractions.wide.df1$BIOME_NAME)
dim(SOCFractions.wide.df1[is.na(SOCFractions.wide.df1$Cropping),])
dim(SOCFractions.wide.df1[is.na(SOCFractions.wide.df1$BIOME_NAME),])
SOCFractions.wide.df1 <- SOCFractions.wide.df1[!is.na(SOCFractions.wide.df1$BIOME_NAME),]


SOCFractions.SOCfr.long <- tidyr::pivot_longer(data=SOCFractions.wide.df1, 
                                                 cols=c("HOC", "POC","ROC"),
                                                 names_to = "SOC_fraction",
                                                 values_to = "TOC_fr") %>% as.data.frame()

my.labels <-c("Montane Grasslands\n & Shrublands",
                  "Temperate Broadleaf\n & Mixed Forests",
                  "Temperate Grasslands,\n Savannas & Shrublands",
                  "Mediterranean Forests,\n Woodlands & Scrub",
                  "Deserts &\n Xeric Shrublands",
                  "Tropical & Subtropical\n Grasslands, Savannas\n & Shrublands",
                  "Tropical & Subtropical\n Moist Broadleaf Forests")
# first create labels, add \n where appropriate.
library(wesanderson)
ggplot(data=SOCFractions.SOCfr.long)+
  geom_boxplot(aes(x=BIOME_NAME, y=TOC_fr, fill=SOC_fraction), lwd=0.4,outlier.size = 0.2) +
  #ylab(label= expression(TOC~(mg~C~g~soil^-1))) +
  ylab(label="SOC fraction (% TOC)") +
  xlab(label="Biome")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest1"), 
                    name="SOC fraction",
                    breaks=c("HOC", "POC", "ROC"),
                    labels=c("MAOM", "POC", "PyOC"))+
  theme(legend.title = element_text(size=16, face="bold"))+
  theme(legend.text = element_text(size=14))+
  theme(axis.text = element_text(size=14))+
  theme(axis.title = element_text(size=16))+ 
  scale_x_discrete(labels= my.labels)

SOCFractions.wide.df2 <- SOCFractions.wide.df[!is.na(SOCFractions.wide.df$Vp) ,]
SOCFractions.wide.df2 <- SOCFractions.wide.df2[!is.na(SOCFractions.wide.df2$Depth) ,]
SOCFractions.wide.df2$Depth <- factor(SOCFractions.wide.df2$Depth)
SOCFractions.wide.df2$BIOME_NAME <- factor(SOCFractions.wide.df2$BIOME_NAME)

ggplot(data= SOCFractions.wide.df[!is.na(SOCFractions.wide.df$BIOME_NAME) & SOCFractions.wide.df$Vp <=5,])+
  geom_boxplot(aes(x=BIOME_NAME, y=Vp, fill=Depth), lwd=0.4,outlier.size = 0.2) +
  scale_fill_viridis(discrete = TRUE, option="D",direction=-1) +
  ylab(label="TOC vulnerability") +
  xlab(label="Biome")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  theme(legend.title = element_text(size=16, face="bold"))+
  theme(legend.text = element_text(size=14))+
  scale_x_discrete(labels= my.labels) 

ggplot(data= SOCFractions.wide.df[!is.na(SOCFractions.wide.df$BIOME_NAME) & SOCFractions.wide.df$Vp <=2,])+
  geom_boxplot(aes(x=BIOME_NAME, y=Vp, fill=Depth), lwd=0.4,outlier.size = 0.2) +
  scale_fill_viridis(discrete = TRUE, option="D",direction=-1) +
  ylab(label="TOC vulnerability") +
  xlab(label="Ecoregion")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16))+
  theme(legend.title = element_text(size=16, face="bold"))+
  theme(legend.text = element_text(size=14))+
  scale_x_discrete(labels= my.labels)

##############################################################################################################################