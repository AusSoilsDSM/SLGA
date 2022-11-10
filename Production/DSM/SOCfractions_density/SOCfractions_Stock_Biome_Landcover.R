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

# Calculate average SOC fraction stock (Mg C/ ha)  by ecoregion (0-30 cm) --------------

# SOC fraction stocks (0-30 cm) cm ------------------------------------------------------------------
inputDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/SOCfr_density/"
setwd(inputDir)

#load all the rasters
SOCfr_Stock <- stack(paste0('./', list.files('./', pattern="0_30")))
ras <- raster::dropLayer(SOCfr_Stock,i = 4)

#### Sample regular
set.seed(1984)
samp.SOC <- raster::sampleRegular(ras, size = 500000, sp=TRUE)


# Boxplots SOC fractions stock - Biome class, land cover
biome_Aus <- read_sf("R:/PRJ-SoilBiodSOC2019/Covariates/Ecoregions2017/EcoregionsAus.shp")
plot(biome_Aus["BIOME_NAME"])

###Add biome to SOC fraction data
sf_use_s2(FALSE)
samp.SOC.sf <- st_as_sf(samp.SOC, crs = 4326)
samp.SOC.sf <- st_join(samp.SOC.sf, biome_Aus["BIOME_NAME"], left=TRUE)
plot(samp.SOC.sf["BIOME_NAME"])
samp.SOC.sf <- samp.SOC.sf[!is.na(samp.SOC.sf$BIOME_NAME),]

biome_df <- biome_Aus %>% st_drop_geometry()

tot_area <- sum(biome_area$area_total)

biome_area <- biome_df %>%
  group_by(., BIOME_NAME) %>%
  summarise(., area_total = round(sum(SHAPE_AREA,na.rm=TRUE),1)) %>%
  mutate(., Area_p = round(area_total/tot_area*100,1)) %>%
  arrange(., desc(Area_p)) %>%
  as.data.frame()
rm(tot_area)

### Land Cover
land_cover <- raster("R:/PRJ-SoilBiodSOC2019/Covariates/LandCover/DLCDv1_Class.tif")
DLCD_Colours_Labels <- read.csv("R:/PRJ-SoilBiodSOC2019/Covariates/LandCover/DLCD_Colours_Labels.csv")
plot(land_cover)

samp.SOC.sf$land_cover <- raster::extract(land_cover, samp.SOC.sf, sp=FALSE)
samp.SOC.sf <- dplyr::left_join(samp.SOC.sf,DLCD_Colours_Labels[,c(1,5,6)], by = c("land_cover"="Class.."))

### Create a category for cropping
unique(samp.SOC.sf$Label)
samp.SOC.sf$Cropping <- ifelse(samp.SOC.sf$Label %in%
                                          c("Irrigated Cropping","Rainfed Cropping",
                                            "Irrigated Pasture","Rainfed Pasture",
                                            "Irrigated Sugar","Rainfed Sugar"),
                                        "Agriculture", "Natural")

### exclude some uses
samp.SOC.sf <- samp.SOC.sf[samp.SOC.sf$Label !="Extraction Sites",]
samp.SOC.sf <- samp.SOC.sf[!is.na(samp.SOC.sf$Cropping),]

samp.SOC.sf$BIOME_NAME <-
  factor(samp.SOC.sf$BIOME_NAME,
         levels=c("Montane Grasslands & Shrublands",
                  "Temperate Broadleaf & Mixed Forests",
                  "Temperate Grasslands, Savannas & Shrublands",
                  "Mediterranean Forests, Woodlands & Scrub",
                  "Deserts & Xeric Shrublands",
                  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
                  "Tropical & Subtropical Moist Broadleaf Forests"))

my.labels <-c("Montane Grasslands\n & Shrublands",
              "Temperate Broadleaf\n & Mixed Forests",
              "Temperate Grasslands,\n Savannas & Shrublands",
              "Mediterranean Forests,\n Woodlands & Scrub",
              "Deserts &\n Xeric Shrublands",
              "Tropical & Subtropical\n Grasslands, Savannas\n & Shrublands",
              "Tropical & Subtropical\n Moist Broadleaf Forests")
my.labelsn <-c("",
              "",
              "",
              "",
              "",
              "",
              "")

p1 <- ggplot()+
  geom_boxplot(data=samp.SOC.sf, 
               aes(x=BIOME_NAME, y=HOC_0_30, fill=Cropping)) +
  ylab(label="MAOC") +
  xlab(label="Biome")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16)) + 
  scale_x_discrete(labels= my.labels)

p1 <- ggplot()+
  geom_boxplot(data=samp.SOC.sf, 
               aes(x=BIOME_NAME, y=HOC_0_30, fill=Cropping)) +
  ylab(label="MAOC") +
  xlab(label="Ecoregion")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16)) + 
  scale_x_discrete(labels= my.labels)

p2 <- ggplot()+
  geom_boxplot(data=samp.SOC.sf, 
               aes(x=BIOME_NAME, y=POC_0_30, fill=Cropping)) +
  ylab(label="POC") +
  xlab(label="Ecoregion")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16)) + 
  scale_x_discrete(labels= my.labels)

p3 <- ggplot()+
  geom_boxplot(data=samp.SOC.sf, 
               aes(x=BIOME_NAME, y=ROC_0_30, fill=Cropping)) +
  ylab(label="PyOC") +
  xlab(label="Ecoregion")+
  theme(axis.text.x = element_text(size=10, angle = 50, vjust = 1, hjust=1))+
  theme(axis.title = element_text(size=16)) + 
  scale_x_discrete(labels= my.labels)


grid.arrange(p1,p2,p3, nrow=3)


### end of the script