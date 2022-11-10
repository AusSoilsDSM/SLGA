########################################################################################################################################################
### Merge all SOC fraction data and estimate values by GSM depth intervals
### Calculate proportion of TOC in each fraction (%)
### Project: SOC fraction maps for TERN
### Date: 03/08/2021
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

# ### 1. Load tables from CSV files and join ------------------------------------------

### I have placed all input files in a directory of the University of Sydney RDS "PRJ-SoilBiodSOC2019".
### You should change the path in your computer to the parent directory:
HomeDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/for_Ross/"
InputDir  <- paste0(HomeDir,"Input_files/1_Input/")
OutDir  <- paste0(HomeDir,"Output_files/1_Output/")
setwd(InputDir)

###TOC data since 1980
load("TOC_clean_1980_23062021.RData")
TOC <- TOC[!is.na(TOC$TOC),]

# 1.1 SCaRP Brendan's csv files -------------------------------------------
Samples_soil1_fin <- read_csv("Samples_soil1_fin.csv", 
                              col_types = cols(myID = col_character(), 
                                               BDStdev = col_double(), 
                                               TC = col_double(), TN = col_double(), 
                                               IC = col_double(), POC = col_double(), 
                                               HOC = col_double(), ROC = col_double()))
Samples_soil2_fin <- read_csv("Samples_soil2_fin.csv", 
                              col_types = cols(myID = col_character(), 
                                               Clay = col_double(), Silt = col_double(), 
                                               Sand = col_double(), pH_ca = col_double(), 
                                               pH_w = col_double(), CEC = col_double(), 
                                               MED = col_double(), EC = col_double()))
Additional_Data <- read_csv("Additional_Data.csv", 
                            col_types = cols(Clay = col_double(), 
                                             Silt = col_double(), Sand = col_double(), 
                                             pH_ca = col_double(), pH_w = col_double(), 
                                             CEC = col_double(), MED = col_double(), 
                                             EC = col_double()))
Sites_matched_fin <- read_csv("Sites_matched_fin.csv", 
                              col_types = cols(myID = col_character(),Notes = col_character()))
dim(Sites_matched_fin[!is.na(Sites_matched_fin$Longitude),]) ### 4510 sites with coordinates

# 1.2 Join data -----------------------------------------------------------

### Metadata:
# SCaRP_SamID	- Unique sample ID assigned to each sample by SCaRP
# State_SamID	-	Sample ID assigned by state groups during sample collection
# SCaRP_SiteID - Unique site ID assigned to the sampling location by SCaRP

#colnames(SCaRP[,18:19]) <- c("UDepth", "LDepth")
# colnames(SCaRP)[18] <- "UDepth"
# colnames(SCaRP)[19] <- "LDepth"
colnames(Samples_soil1_fin)[colnames(Samples_soil1_fin) == "Udepth"] <- "UDepth"
colnames(Samples_soil1_fin)[colnames(Samples_soil1_fin) == "LDepth"] <- "LDepth"
colnames(Samples_soil2_fin)[colnames(Samples_soil2_fin) == "Udpeth"] <- "UDepth"
colnames(Samples_soil2_fin)[colnames(Samples_soil2_fin) == "Ldepth"] <- "LDepth" 
colnames(Additional_Data)[colnames(Additional_Data) == "Udpeth"] <- "UDepth"
colnames(Additional_Data)[colnames(Additional_Data) == "Ldepth"] <- "LDepth"

### I keep the "SCaRP_SamID" from Samples_soil1_fin
setdiff(colnames(Samples_soil2_fin), colnames(Samples_soil1_fin))
# Samples_soil_fin <- dplyr::left_join(Samples_soil1_fin, 
#                                      Samples_soil2_fin[, c("myID","myLayer","pH_ca","pH_w","CEC","MED","EC")],
#                                      by=c("myID","myLayer"))
Samples_soil_fin <- Samples_soil1_fin
### Add the site characteristics
setdiff( colnames(Sites_matched_fin),colnames(Samples_soil_fin))
Samples_soil_fin <- dplyr::left_join(Samples_soil_fin, 
                                     Sites_matched_fin[,c("myID", "State_SiteID","State","Group","Region","SCaRP_SoilType","Aust_SoilClass",
                                                          "ManageClass","ManageClass2","DateSampled","SiteType","Latitude","Longitude","Easting",
                                                          "Northing","RefGrid","Notes")], 
                                     by="myID")
SCaRP.Soil <- Samples_soil_fin
rm(Samples_soil_fin, Samples_soil2_fin,Samples_soil1_fin,Additional_Data,   Sites_matched_fin)

# 1.3 TERN surveillance and archive VisNIR samples COORDINATES ------------------------

tern_SOCfractions <- read_csv("tern_soc_carbonFraction_pred_locs.csv")
aus_archive_SOCfractions <- read_csv("aus_archive_soc_carbonFraction_pred_locs.csv")

dim(SCaRP.Soil[is.na(SCaRP.Soil$Lat),])
dim(SCaRP.Soil[is.na(SCaRP.Soil$Long),])
all.equal(SCaRP.Soil$Long, SCaRP.Soil$Longitude)
all.equal(SCaRP.Soil$Lat, SCaRP.Soil$Latitude)
dim(aus_archive_SOCfractions[is.na(aus_archive_SOCfractions$o_latitude_GDA94),])
dim(aus_archive_SOCfractions[is.na(aus_archive_SOCfractions$o_longitude_GDA94),])
dim(tern_SOCfractions[is.na(tern_SOCfractions$easting),])
dim(tern_SOCfractions[is.na(tern_SOCfractions$northing),])
dim(tern_SOCfractions[is.na(tern_SOCfractions$latitude),])
dim(tern_SOCfractions[is.na(tern_SOCfractions$longitude),])
dim(tern_SOCfractions[(is.na(tern_SOCfractions$easting) & !is.na(tern_SOCfractions$latitude)),])

## Check zone
sort(unique(tern_SOCfractions$zone))

### Plot the locations
AusMap <- get_stamenmap(bbox = c(left=110, bottom=-45, right=157, top=-8),
                        maptype="toner-lite", zoom=5,
                        source="stamen", crop=TRUE)
ggmap(AusMap) +
  geom_point(aes(y = Latitude, x =Longitude, alpha=0.1), color="azure3",
             data = TOC)+
  geom_point(aes(y = Lat, x = Long), color="#3B9AB2",
             data = SCaRP.Soil) +
  geom_point(aes(y = o_latitude_GDA94, x =o_longitude_GDA94), color= "#FD6467",
             data = aus_archive_SOCfractions) +
  geom_point(aes(y = latitude, x =longitude), color="gray21",
             data = tern_SOCfractions) 

ggmap(AusMap) +
    geom_point(aes(y = Lat, x = Long), color="#3B9AB2",
               data = SCaRP.Soil) 
length(unique(SCaRP.Soil$coord_ID))

# Warning messages:
# 1: Removed 2 rows containing missing values (geom_point). 
# 2: Removed 14841 rows containing missing values (geom_point). 

### NEED TO REPROJECT EASTING/NORTHING TO THE crs EPSG=4326
### the original CRS are GDA projected for different UTM zones: 49 50 51 52 53 54 55 56
tern_SOCfractions <- as.data.frame(tern_SOCfractions)
dim(tern_SOCfractions)
dim(tern_SOCfractions[(is.na(tern_SOCfractions$easting) & is.na(tern_SOCfractions$longitude)),])
tern_SOCfractions <- tern_SOCfractions[!(is.na(tern_SOCfractions$easting) & is.na(tern_SOCfractions$longitude)),]

### subset SOC_fractions with Lat Long (GDA94)
tern_SOCfractions.a <- tern_SOCfractions[!is.na(tern_SOCfractions$latitude),]
### the df without lat long but without zone
tern_SOCfractions.c <- tern_SOCfractions[(is.na(tern_SOCfractions$latitude)|is.na(tern_SOCfractions$longitude)),]
tern_SOCfractions.c <- tern_SOCfractions.c[is.na(tern_SOCfractions.c$zone), ]
### the df without lat long but with zone
tern_SOCfractions.b <- tern_SOCfractions[(is.na(tern_SOCfractions$latitude)|is.na(tern_SOCfractions$longitude)),]
tern_SOCfractions.b <- tern_SOCfractions.b[!is.na(tern_SOCfractions.b$zone), ]

## No data without coordinates, right?
dim(tern_SOCfractions.b[is.na(tern_SOCfractions.b$easting),])

### for loop
### New variable
tern_SOCfractions.b$Lat_WGS84 <- NA
tern_SOCfractions.b$Long_WGS84 <- NA
tern_SOCfractions.b$Lat_WGS84 <- as.numeric(tern_SOCfractions.b$Lat_WGS84)
tern_SOCfractions.b$Long_WGS84 <- as.numeric(tern_SOCfractions.b$Long_WGS84)

zones.tern <- sort(unique(tern_SOCfractions.b$zone))
zones.epsg <- c(28350,28351,28352,28353,28354,28355,28356)
# 50 51 52 53 54 55 56

for(i in 1:length(zones.tern)){
  #subset the data from that zone
  temp.df <- tern_SOCfractions.b[tern_SOCfractions.b$zone == zones.tern[[i]],]
  temp.sf <- st_as_sf(temp.df, coords = c("easting", "northing"), crs = zones.epsg[[i]]) 
  temp.sf <- st_transform(temp.sf,4326) ## to WGS84 because the covariates will be in that CRS
  #retrieve coordinates
  coords.tp <- as.data.frame(st_coordinates(temp.sf))
  
  ## Assign to lat and long
  tern_SOCfractions.b[tern_SOCfractions.b$zone == zones.tern[[i]],]$Long_WGS84 <- coords.tp$X
  tern_SOCfractions.b[tern_SOCfractions.b$zone == zones.tern[[i]],]$Lat_WGS84 <- coords.tp$Y
  print(i)
  rm(i, coords.tp, temp.df, temp.sf)
}

ggmap(AusMap) +
  geom_point(aes(y = Lat, x = Long), color="#3B9AB2",
             data = SCaRP.Soil) +
  geom_point(aes(y = o_latitude_GDA94, x =o_longitude_GDA94), color= "#FD6467",
             data = aus_archive_SOCfractions) +
  geom_point(aes(y = latitude, x =longitude), color="gray21",
             data = tern_SOCfractions.a)+
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84), color="blue",
             data = tern_SOCfractions.b)
### There seems to be many duplicates  

### so far, I imagine the CRS for the projected data without zone is GDA94 / Australian Albers, EPSG: 3577
### and for the aus_archive_SOCfractions, GDA94 is EPSG:4283.

rm(tern_SOCfractions.c)
### Better ignore this ones

### Write a little function for assigning the coordinates in WGS84
project.toWGS84 <- function(df, fromCRS, names.coords) {
  df$Lat_WGS84 <- NA
  df$Long_WGS84 <- NA
  df$Lat_WGS84 <- as.numeric(df$Lat_WGS84)
  df$Long_WGS84 <- as.numeric(df$Long_WGS84)
  
  df.sf <- st_as_sf(df, coords = names.coords, crs = as.numeric(fromCRS))
  df.sf <- st_transform(df.sf, 4326) ## to WGS84 because the covariates will be in that CRS
  
  #retrieve coordinates
  coords.tp <- as.data.frame(st_coordinates(df.sf))
  ## Assign to lat and long
  df$Long_WGS84 <- coords.tp$X
  df$Lat_WGS84 <- coords.tp$Y
  return(df)
  
}

tern_SOCfractions.a <- project.toWGS84(df=tern_SOCfractions.a, fromCRS=4283, names.coords=c("longitude", "latitude"))
SCaRP.Soil <- project.toWGS84(df=SCaRP.Soil, fromCRS=4283, names.coords =c("Longitude", "Latitude"))
aus_archive_SOCfractions <- project.toWGS84(df=aus_archive_SOCfractions, fromCRS=4283, names.coords =c("o_longitude_GDA94", "o_latitude_GDA94"))
tern_SOCfractions <- rbind(tern_SOCfractions.a, tern_SOCfractions.b)
rm(tern_SOCfractions.a, tern_SOCfractions.b, zones.epsg, zones.tern)

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84), color="#3B9AB2",
             data = SCaRP.Soil) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84), color= "#FD6467",
             data = aus_archive_SOCfractions) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84), color="darkorchid1",
             data = tern_SOCfractions)

# 1.4 Merge all data and find duplicates -----------------------------

### Change to output directory
setwd(OutDir)

### Create a unique ID based on coordinates (string)
SCaRP.Soil <- arrange(SCaRP.Soil, Lat_WGS84, Long_WGS84)
SCaRP.Soil$coord_ID <- paste(SCaRP.Soil$Lat_WGS84,sep=" ",SCaRP.Soil$Long_WGS84)
tern_SOCfractions <- arrange(tern_SOCfractions, Lat_WGS84, Long_WGS84)
tern_SOCfractions <- as.data.frame(tern_SOCfractions)
aus_archive_SOCfractions <- arrange(aus_archive_SOCfractions, Lat_WGS84, Long_WGS84)
aus_archive_SOCfractions <- as.data.frame(aus_archive_SOCfractions)

### Quick check sum of fractions and predicted TOC
### TERN
plot(x=tern_SOCfractions$`SOC (mg/g)`,
     y=(tern_SOCfractions$`HOC_median  (mg/g)` +tern_SOCfractions$`POC_median  (mg/g)`+tern_SOCfractions$`ROC_median  (mg/g)`),
     xlab="Predicted SOC (mg C / g soil)",
     ylab= "Sum of predicted SOC fractions (mg C / g soil)")
abline(0,1, col="blue")
### Aus-archive
plot(x=aus_archive_SOCfractions$`SOC (mg/g)`,
     y=(aus_archive_SOCfractions$`HOC_median  (mg/g)` +aus_archive_SOCfractions$`POC_median  (mg/g)`+aus_archive_SOCfractions$`ROC_median  (mg/g)`),
     xlab="Predicted SOC (mg C / g soil)",
     ylab= "Sum of predicted SOC fractions (mg C / g soil)")
abline(0,1, col="blue")
### SCaRP
plot(x=SCaRP.Soil$MIR_OC,
     y=(SCaRP.Soil$MIR_HOC +SCaRP.Soil$MIR_POC +SCaRP.Soil$MIR_ROC),
     xlab="Predicted SOC (mg C / g soil)",
     ylab= "Sum of predicted SOC fractions (mg C / g soil)")
abline(0,1, col="blue")

tern_SOCfractions$coord_ID <- paste(tern_SOCfractions$Lat_WGS84,sep=" ", tern_SOCfractions$Long_WGS84)
aus_archive_SOCfractions$coord_ID <- paste(aus_archive_SOCfractions$Lat_WGS84,sep=" ", aus_archive_SOCfractions$Long_WGS84)
colnames(SCaRP.Soil)
colnames(tern_SOCfractions)
colnames(aus_archive_SOCfractions)

### Are there any duplicates by coordinates in TERN surveillance and SCARP?
dup.coords.idx <- which(tern_SOCfractions$coord_ID %in% SCaRP.Soil$coord_ID); dup.coords.idx
### None based in name

### Are there any duplicates by coordinates in Aus archive and SCARP?
dup.coords.idx <- which(aus_archive_SOCfractions$coord_ID %in% SCaRP.Soil$coord_ID); dup.coords.idx
dup.coords.ids <- aus_archive_SOCfractions[dup.coords.idx,]$coord_ID

### There are two sites in Tasmania
as.data.frame(aus_archive_SOCfractions[dup.coords.idx,])
as.data.frame(SCaRP.Soil[SCaRP.Soil$coord_ID %in% dup.coords.ids,])
### Eliminate
aus_archive_SOCfractions <- aus_archive_SOCfractions[-dup.coords.idx,]
rm(dup.coords.idx,dup.coords.ids )

### And TERN and the archive?
dup.coords.idx <- which(aus_archive_SOCfractions$coord_ID %in% tern_SOCfractions$coord_ID); dup.coords.idx
### There are none
rm(dup.coords.idx)

### Arrange by site id and layer number
### For each sampling location, e.g., myID
SCaRP.Soil <- arrange(SCaRP.Soil, myID, myLayer)
SCaRP.Soil <- as.data.frame(SCaRP.Soil)
SCaRP.Soil <- SCaRP.Soil[!duplicated(SCaRP.Soil),]

## Here, we use coord_ID as site ID
tern_SOCfractions <- arrange(tern_SOCfractions, coord_ID, upper)
tern_SOCfractions <- as.data.frame(tern_SOCfractions)
tern_SOCfractions <- tern_SOCfractions[!duplicated(tern_SOCfractions),]

aus_archive_SOCfractions <- arrange(aus_archive_SOCfractions,coord_ID,h_no,samp_no)
aus_archive_SOCfractions <- as.data.frame(aus_archive_SOCfractions)
aus_archive_SOCfractions <- aus_archive_SOCfractions[!duplicated(aus_archive_SOCfractions),]

### Eliminate outside australia bbox 
inAustralia <- function(soilDF){
    if(!is.data.frame(soilDF)){return(NULL)}
    bboxExt <- extent(110,155,-45,-9)
    idxs <- which(soilDF$Long_WGS84 >= bboxExt@xmin & soilDF$Long_WGS84 <= bboxExt@xmax & soilDF$Lat_WGS84 >= bboxExt@ymin & soilDF$Lat_WGS84 <= bboxExt@ymax)
    outdf <- soilDF[idxs, ]
}
tern_SOCfractions <- inAustralia(tern_SOCfractions)
aus_archive_SOCfractions <- inAustralia(aus_archive_SOCfractions)
SCaRP.Soil <- inAustralia(SCaRP.Soil)

### Get the unique locations in each dataset
SCaRP.Soil.sites <- SCaRP.Soil[,c("Lat_WGS84","Long_WGS84","coord_ID","Latitude","Longitude",
                                  "Northing","Easting","RefGrid","SCaRP_SiteID","myID")]
SCaRP.Soil.sites <- SCaRP.Soil.sites[!duplicated(SCaRP.Soil.sites),]

tern_SOCfractions.sites <- tern_SOCfractions[,c("Lat_WGS84","Long_WGS84","coord_ID","latitude","longitude",
                                                      "northing","easting","zone")]
tern_SOCfractions.sites <- tern_SOCfractions.sites[!duplicated(tern_SOCfractions$coord_ID),]

aus_archive_SOCfractions.sites <- aus_archive_SOCfractions[,c("Lat_WGS84","Long_WGS84","coord_ID",
                                                                    "o_latitude_GDA94","o_longitude_GDA94")]
aus_archive_SOCfractions.sites <- aus_archive_SOCfractions.sites[!duplicated(aus_archive_SOCfractions$coord_ID),]

### Transform to spatial
SCaRP.Soil.sites.sf <- st_as_sf(SCaRP.Soil.sites, coords = c("Long_WGS84","Lat_WGS84"), crs = 4326)
tern.sites.sf <- st_as_sf(tern_SOCfractions.sites, coords = c("Long_WGS84","Lat_WGS84"), crs = 4326)
aus_archive.sites.sf <- st_as_sf(aus_archive_SOCfractions.sites, coords = c("Long_WGS84","Lat_WGS84"), crs = 4326)

### Find those that are too close to others
## within 2.8e-05?
## I create a buffer around TERN points, of about ~5m (in dd)
SCaRP.Soil.sites.buffer <- st_buffer(x=SCaRP.Soil.sites.sf, dist = 5e-05)
plot(SCaRP.Soil.sites.buffer["coord_ID"])

### which TERN observations intersect the buffer?
sel.tern <- tern.sites.sf[SCaRP.Soil.sites.buffer,]
rm(sel.tern)

## what about the Aus_archive?
sel.aus_archive <- aus_archive.sites.sf[SCaRP.Soil.sites.buffer,]
plot(sel.aus_archive["coord_ID"], col="black")

### Where are these duplicates?
id.dupl.sp <- sel.aus_archive$coord_ID
ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30),pch=1, color="#3B9AB2", data = SCaRP.Soil) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), pch=1, color= "gray70", data = aus_archive_SOCfractions) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84), color= "#FD6467",
             data = aus_archive_SOCfractions[aus_archive_SOCfractions$coord_ID %in% id.dupl.sp, ]) + 
  theme(legend.position = "none") 

### Exclude them from the dataset
aus_archive.sites.sf <- aus_archive.sites.sf[!(aus_archive.sites.sf$coord_ID %in% id.dupl.sp), ]
aus_archive_SOCfractions.sites <- aus_archive_SOCfractions.sites[!(aus_archive_SOCfractions.sites$coord_ID %in% id.dupl.sp),]
aus_archive_SOCfractions <- aus_archive_SOCfractions[!(aus_archive_SOCfractions$coord_ID %in% id.dupl.sp), ]
rm(id.dupl.sp, sel.aus_archive)

# 1.5 Additional samples from NatSoil and BM --------------------------
#load("C:/Users/mrom8073/Desktop/USydney/Postdoc/Projects/C_sequestration/Output/SOCfractions_Splines/Data12032021.RData")
bm2_SOCfractions <- read_csv("bm2_mir_scarp_cfs_model_extension_w_hullwork.csv")
NatSoil_SOCfractions <- read_csv("natsoil_mir_scarp_cfs_model_extension_w_hullwork.csv")

### Coordinates from Alexandre
load("datsoilspc.Rdata")

### Check the data
head(as.data.frame(bm2_SOCfractions))
head(as.data.frame(NatSoil_SOCfractions))

### Project from GDA94 to WGS84
NatSoil_SOCfractions <- project.toWGS84(df=NatSoil_SOCfractions, fromCRS=4283, names.coords =c("o_longitude_GDA94", "o_latitude_GDA94"))

### Eliminate outside australia bbox 
NatSoil_SOCfractions <- inAustralia(NatSoil_SOCfractions)

### Clarification, the MIR SOC fraction data is all in mg/g soil
NatSoil_SOCfractions_bck <- NatSoil_SOCfractions
summary(NatSoil_SOCfractions[,c("samp_upper_depth","samp_lower_depth","SOC","POC", "HOC", "ROC")])

### NatSoil
plot(x=NatSoil_SOCfractions$SOC,
     y=(NatSoil_SOCfractions$HOC +NatSoil_SOCfractions$POC +NatSoil_SOCfractions$ROC),
     xlab="Predicted SOC (mg C / g soil)",
     ylab= "Sum of predicted SOC fractions (mg C / g soil)")
abline(0,1, col="blue")

NatSoil_SOCfractions[NatSoil_SOCfractions$samp_upper_depth > 10,]
par(mar=c(2,2,2,2), oma=c(2,2,2,2))
hist(NatSoil_SOCfractions$samp_upper_depth)

### Seems like depth is in m. I convert to cm. the maximum values are in cm?
NatSoil_SOCfractions$UDepth <- NatSoil_SOCfractions$samp_upper_depth *100
NatSoil_SOCfractions$LDepth <- NatSoil_SOCfractions$samp_lower_depth *100
NatSoil_SOCfractions <- NatSoil_SOCfractions[NatSoil_SOCfractions$LDepth > NatSoil_SOCfractions$UDepth,]
plot(NatSoil_SOCfractions$LDepth, NatSoil_SOCfractions$UDepth)
abline(0,1,col="blue")
summary(NatSoil_SOCfractions[,c("UDepth","LDepth","SOC","POC", "HOC", "ROC")])

### Eliminate those deeper than... 3 m (300 cm)
NatSoil_SOCfractions <- NatSoil_SOCfractions[NatSoil_SOCfractions$UDepth < 300, ]

# 1.6 Explore duplicity between NatSoil and AusArchive
### Now eliminate duplicates from aus-archive
### do they overlap?
ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84), color="black",
             data = aus_archive_SOCfractions)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84), color= "#FD6467",
             data = NatSoil_SOCfractions)

NatSoil_SOCfractions_bck <- NatSoil_SOCfractions
NatSoil_SOCfractions <- arrange(NatSoil_SOCfractions, Lat_WGS84,Long_WGS84) %>% as.data.frame()
NatSoil_SOCfractions$coord_ID <- paste(NatSoil_SOCfractions$Lat_WGS84,sep=" ", NatSoil_SOCfractions$Long_WGS84)

### Are there any duplicates by coordinates in NatSoil and SCARP?
dup.coords.idx <- which(NatSoil_SOCfractions$coord_ID %in% SCaRP.Soil$coord_ID);dup.coords.idx # none
### And NatSoil MIR and TERN Vis-NIR?
dup.coords.idx <- which(NatSoil_SOCfractions$coord_ID %in% tern_SOCfractions$coord_ID);dup.coords.idx # None
### And NatSoil MIR and aus-archive Vis-NIR?
dup.coords.idx <- which(NatSoil_SOCfractions$coord_ID %in% aus_archive_SOCfractions$coord_ID);dup.coords.idx
dup.coords.ids <- NatSoil_SOCfractions[dup.coords.idx,]$coord_ID
length(unique(dup.coords.ids)) #646
length(unique(NatSoil_SOCfractions$coord_ID)) #700
rm(dup.coords.idx, dup.coords.ids)

### Identify the sites from aus-archive that also have predictions in the NatSoil dataset
### NatSoil MIR and aus-archive Vis-NIR, we prefer NatSoil
dup.aus_archive_NatSoil.idx <- which(aus_archive_SOCfractions$coord_ID %in% NatSoil_SOCfractions$coord_ID)
dup.coords.ids <- aus_archive_SOCfractions[dup.aus_archive_NatSoil.idx,]$coord_ID
length(unique(NatSoil_SOCfractions$coord_ID))
length(unique(aus_archive_SOCfractions$coord_ID)) # 4053
length(dup.coords.ids)
length(unique(dup.coords.ids)) #646
### 1921 observations in the aus_archive from 646 sites
dup_sites.aus_archive_NatSoil <- unique(aus_archive_SOCfractions[dup.aus_archive_NatSoil.idx,]$coord_ID)
rm(dup.coords.ids, dup.aus_archive_NatSoil.idx)
NatSoil_SOCfractions <- as.data.frame(NatSoil_SOCfractions)

### Rename variables
colnames(aus_archive_SOCfractions) <- c("agency_code","proj_code","s_id","o_id",
                                        "h_no","samp_no","spec_id","o_latitude_GDA94",  
                                        "o_longitude_GDA94","samp_upper_depth","samp_lower_depth",
                                        "specName","specID","OC","POC","HOC","ROC",
                                        "Lat_WGS84","Long_WGS84","coord_ID")
summary(aus_archive_SOCfractions[,c("samp_upper_depth","samp_lower_depth","OC","POC", "HOC", "ROC")])

### Seems like depth is in m. I convert to cm.
aus_archive_SOCfractions$UDepth <- aus_archive_SOCfractions$samp_upper_depth *100
aus_archive_SOCfractions$LDepth <- aus_archive_SOCfractions$samp_lower_depth *100
aus_archive_SOCfractions <- aus_archive_SOCfractions[aus_archive_SOCfractions$LDepth > aus_archive_SOCfractions$UDepth,]

aus_archive_SOCfractions[aus_archive_SOCfractions$coord_ID %in% dup_sites.aus_archive_NatSoil,
                         c("agency_code","proj_code","s_id","o_id",
                           "coord_ID","UDepth","LDepth","POC","HOC","ROC")]
Location_ID.dup.aus_archive <- paste0(aus_archive_SOCfractions[aus_archive_SOCfractions$coord_ID %in% dup_sites.aus_archive_NatSoil,]$agency_code,"_",
                           aus_archive_SOCfractions[aus_archive_SOCfractions$coord_ID %in% dup_sites.aus_archive_NatSoil,]$proj_code,"_",
                           aus_archive_SOCfractions[aus_archive_SOCfractions$coord_ID %in% dup_sites.aus_archive_NatSoil,]$s_id,"_",
                           aus_archive_SOCfractions[aus_archive_SOCfractions$coord_ID %in% dup_sites.aus_archive_NatSoil,]$o_id)
Location_ID.dup.aus_archive <- unique(Location_ID.dup.aus_archive) ### 860 sites

Location_ID.dup.NatSoil <- paste0(NatSoil_SOCfractions[NatSoil_SOCfractions$coord_ID %in% dup_sites.aus_archive_NatSoil,]$agency_code,"_",
                            NatSoil_SOCfractions[NatSoil_SOCfractions$coord_ID %in% dup_sites.aus_archive_NatSoil,]$proj_code,"_",
                            NatSoil_SOCfractions[NatSoil_SOCfractions$coord_ID %in% dup_sites.aus_archive_NatSoil,]$s_id,"_1")
Location_ID.dup.NatSoil <- unique(Location_ID.dup.NatSoil)

### What about spatial duplicates??
NatSoil_SOCfractions.sites <- NatSoil_SOCfractions.sites[,c("Lat_WGS84","Long_WGS84","coord_ID","o_latitude_GDA94","o_longitude_GDA94")]
NatSoil_SOCfractions.sites <- NatSoil_SOCfractions[!duplicated(NatSoil_SOCfractions$coord_ID),]

### Transform to spatial
NatSoil_SOCfractions.sites.sf <- st_as_sf(NatSoil_SOCfractions.sites, coords = c("Long_WGS84","Lat_WGS84"), crs = 4326)

### Find those that are too close to others
### which NatSoil observations intersect the buffer around SCaRP sites?
sel.NatSoil <- NatSoil_SOCfractions.sites.sf[SCaRP.Soil.sites.buffer,];sel.NatSoil ### No intersection
rm(sel.NatSoil)

### What about a buffer around NatSoil and finding too close aus-archive observations?
## I create a buffer around TERN points, of about ~5m (in dd)
NatSoil.sites.buffer <- st_buffer(x=NatSoil_SOCfractions.sites.sf, dist = 5e-05)
plot(NatSoil.sites.buffer["coord_ID"])

### which Aus_archive observations intersect the buffer?
sel.tern.NatSoil <- tern.sites.sf[NatSoil.sites.buffer,]; sel.tern.NatSoil # none
rm(sel.tern.NatSoil)

## what about the Aus_archive?
sel.aus_archive <- aus_archive.sites.sf[NatSoil.sites.buffer,]; sel.aus_archive
plot(sel.aus_archive["coord_ID"], col="black")

### are these the same or more than before?
length(dup_sites.aus_archive_NatSoil) ### 646 sites
length(unique(sel.aus_archive$coord_ID)) ### 647 sites (one more)

### Where are these duplicates?
id.dupl.sp <- sel.aus_archive$coord_ID
ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), color= "blue", data = aus_archive_SOCfractions) + 
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), color= "#3B9AB2", data = tern_SOCfractions) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), color= "#3B9AB2", data = NatSoil_SOCfractions) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), color= "#3B9AB2", data = SCaRP.Soil) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84), color= "red",
             data = aus_archive_SOCfractions[aus_archive_SOCfractions$coord_ID %in% id.dupl.sp, ]) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84), color= "magenta",
             data = aus_archive_SOCfractions[aus_archive_SOCfractions$coord_ID %in% dup_sites.aus_archive_NatSoil, ]) +
  theme(legend.position = "none") 

### We prefer MIR to vis-NIR predictions
### Eliminate spatial duplicates from Aus Archive
aus_archive.sites.sf <- aus_archive.sites.sf[!(aus_archive.sites.sf$coord_ID %in% id.dupl.sp),]
aus_archive_SOCfractions.sites <- aus_archive_SOCfractions.sites[!(aus_archive_SOCfractions.sites$coord_ID %in% id.dupl.sp),]
aus_archive_SOCfractions <- aus_archive_SOCfractions[!(aus_archive_SOCfractions$coord_ID %in% id.dupl.sp),]

###clean
rm(dup_sites.aus_archive_NatSoil, Location_ID.dup.aus_archive, Location_ID.dup.NatSoil, id.dupl.sp, sel.aus_archive)

### BM2 samples
str(bm2_SOCfractions)
str(datsoilspc)
datsoilspc$spec_id <- as.numeric(datsoilspc$spec_id)
setdiff(bm2_SOCfractions$spec_id, datsoilspc$spec_id)
setdiff(datsoilspc$spec_id, bm2_SOCfractions$spec_id)
bm2_SOCfractions <- merge(bm2_SOCfractions, datsoilspc[,c("spec_id","o_latitude_GDA94","o_longitude_GDA94","samp_upper_depth","samp_lower_depth")], by="spec_id", all.x=TRUE)

ggmap(AusMap) +
  geom_point(aes(y = o_latitude_GDA94, x = o_longitude_GDA94),
             data = bm2_SOCfractions) 
length(unique(paste0( bm2_SOCfractions$o_latitude_GDA94, bm2_SOCfractions$o_longitude_GDA94)))

### Seems like depth is in m. I convert to cm.
bm2_SOCfractions$UDepth <- bm2_SOCfractions$samp_upper_depth *100
bm2_SOCfractions$LDepth <- bm2_SOCfractions$samp_lower_depth *100

### Project to WGS84
bm2_SOCfractions <- project.toWGS84(df=bm2_SOCfractions, fromCRS=4283, names.coords =c("o_longitude_GDA94", "o_latitude_GDA94"))
### Eliminate outside australia bbox 
bm2_SOCfractions <- inAustralia(bm2_SOCfractions)
bm2_SOCfractions$coord_ID <- paste(bm2_SOCfractions$Lat_WGS84,sep=" ", bm2_SOCfractions$Long_WGS84)
bm2_SOCfractions <- bm2_SOCfractions[!duplicated(bm2_SOCfractions),]

## and also create a column "myID
bm2_SOCfractions <- bm2_SOCfractions %>% arrange(., coord_ID, UDepth, LDepth ) %>% group_by(.,coord_ID) %>% 
  mutate(., myID =  cur_group_id())
bm2_SOCfractions$myID <- as.character(bm2_SOCfractions$myID)
bm2_SOCfractions <- as.data.frame(bm2_SOCfractions)

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), color= "#3B9AB2", data = aus_archive_SOCfractions) + 
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), color= "#3B9AB2", data = tern_SOCfractions) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), color= "#3B9AB2", data = NatSoil_SOCfractions) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), color= "#3B9AB2", data = SCaRP.Soil) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84), color= "red", data = bm2_SOCfractions)+
  theme(legend.position = "none") 

### Are there duplicates between bm2 and SCaRP, aus-archive or TERN?
### Are there any duplicates by coordinates in NatSoil and SCARP?
dup.coords.idx <- which(bm2_SOCfractions$coord_ID %in% SCaRP.Soil$coord_ID); dup.coords.idx # none
### And bm2 MIR and TERN Vis-NIR?
dup.coords.idx <- which(bm2_SOCfractions$coord_ID %in% tern_SOCfractions$coord_ID); dup.coords.idx

## And bm2 MIR and NatSoil MIR?
dup.coords.idx <- which(bm2_SOCfractions$coord_ID %in% NatSoil_SOCfractions$coord_ID); dup.coords.idx
dup.coords.ids <- bm2_SOCfractions[dup.coords.idx,]$coord_ID
### do they overlap in depth?
NatSoil_SOCfractions[NatSoil_SOCfractions$coord_ID %in% dup.coords.ids,c("coord_ID","UDepth","LDepth","POC","HOC","ROC")]
bm2_SOCfractions[bm2_SOCfractions$coord_ID %in% dup.coords.ids,c("coord_ID","UDepth","LDepth","POC","HOC","ROC")]
duplicated(rbind(NatSoil_SOCfractions[NatSoil_SOCfractions$coord_ID %in% dup.coords.ids,c("coord_ID","UDepth","LDepth")],
                 bm2_SOCfractions[bm2_SOCfractions$coord_ID %in% dup.coords.ids,c("coord_ID","UDepth","LDepth")]))
### I leave just the last observation. The rest, I eliminate from bm2
dup.coords.idx <- dup.coords.idx[1:7]
### Eliminate
bm2_SOCfractions <- bm2_SOCfractions[-dup.coords.idx,]

### And bm2 MIR and aus-archive Vis-NIR?
dup.coords.idx <- which(bm2_SOCfractions$coord_ID %in% aus_archive_SOCfractions$coord_ID); dup.coords.idx
dup.coords.ids <- bm2_SOCfractions[dup.coords.idx,]$coord_ID
### do they overlap in depth?
aus_archive_SOCfractions[aus_archive_SOCfractions$coord_ID %in% dup.coords.ids,c("coord_ID","UDepth","LDepth","POC","HOC","ROC")]
bm2_SOCfractions[bm2_SOCfractions$coord_ID %in% dup.coords.ids,c("coord_ID","UDepth","LDepth","POC","HOC","ROC")]

eliminate.these <- plyr::match_df(bm2_SOCfractions[bm2_SOCfractions$coord_ID %in% dup.coords.ids,],
                                  aus_archive_SOCfractions[aus_archive_SOCfractions$coord_ID %in% dup.coords.ids,],
                                  on =c("coord_ID","UDepth","LDepth"))
bm2_SOCfractions <- bm2_SOCfractions[!(rownames(bm2_SOCfractions) %in% rownames(eliminate.these)), ]

### Rename variables
colnames(tern_SOCfractions) <- c("mylabs","state_c","nir_lab","zone","easting","northing",
                                 "matchedbarcode","upper","lower","type","pH","ec",
                                 "longitude","latitude","OC","POC", "HOC", "ROC",
                                 "Lat_WGS84","Long_WGS84","coord_ID")

###Transform also TERN depths to cm.
tern_SOCfractions$UDepth <- tern_SOCfractions$upper *100
tern_SOCfractions$LDepth <- tern_SOCfractions$lower *100
tern_SOCfractions <- tern_SOCfractions[tern_SOCfractions$LDepth > tern_SOCfractions$UDepth,]

### Clean and save image
rm(sel.NatSoil,NatSoil_SOCfractions_bck,aus_archive.sites.buffer,datsoilspc,dup.coords.idx,dup.coords.ids,eliminate.these, datsoilspc,
   dup.aus_archive_NatSoil.idx,aus_archive.sites.sf,NatSoil_SOCfractions.sites.sf,aus_archive_SOCfractions.sites,
   NatSoil.sites.buffer,NatSoil_SOCfractions.sites,NatSoil_SOCfractions.sites.sf,SCaRP.Soil.sites,SCaRP.Soil.sites.buffer,
   SCaRP.Soil.sites.sf,sel.aus_archive,tern.sites.sf,tern_SOCfractions.sites,test,test.dup,remns,dup_sites_buffer.NatSoil_aus_archive)
save.image("1_SOCFraction_Proportions_AllData_03082021.RData")

### HERE on 03/08/2021

### Values of SOC % in the literature indicate upper values of:
### ~ 300 mg C/ g soil at Forest soils in Victoria Bennett et al. Geoderma (2020)
### ~ 147 mg C/ g soil at SCaRP measured SOC
### ~ 180-200 mg C/g soil in northeast NSW (Wilson et al. 2017) in woodlands.
### POC ~ 23.9 mg C / g soil and total SOC 32 mg/g in four agricultural trials (Chan et al., 2011 Soil Use Manage)
### POC ~ 29 mg C / g soil Skjemstad et al.(2006) SR.
### POC ~ 3 mg C / g soil and HOC ~ 13 mg C / g soil Karunaratne et al. (2014) - NSW Cox's Creek catchment
### POC, HOC, ROC ~ 50 mg C/g soil (predicted with MIR models for NSW) Hobley et al. (2016)
### POC ~ 70 mg C / g soil in Tasmanian agricultural soils (Cotching et al., 2018).
### HOC ~ 70-80, POC ~ 10 mg / g soil in Victoria forest and pasture (Carter et al., 2002)

ggmap(AusMap) +
  geom_point(aes(y = Latitude, x =Longitude, alpha=1/30), color= "magenta", data = TOC) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), color= "#3B9AB2", data = aus_archive_SOCfractions) + 
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), color= "#3B9AB2", data = tern_SOCfractions) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), color= "#3B9AB2", data = NatSoil_SOCfractions) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), color= "#3B9AB2", data = SCaRP.Soil) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=1/30), color= "#3B9AB2", data = bm2_SOCfractions) +
  theme(legend.position = "none")

### The field Location_ID
NatSoil_SOCfractions$Location_ID <- paste0(NatSoil_SOCfractions$agency_code, "_", NatSoil_SOCfractions$proj_code, "_", NatSoil_SOCfractions$s_id, "_1")
aus_archive_SOCfractions$Location_ID <- paste0(aus_archive_SOCfractions$agency_code, "_", aus_archive_SOCfractions$proj_code, "_", aus_archive_SOCfractions$s_id, "_", aus_archive_SOCfractions$o_id)

# 1.6 Calculate contribution of fractions to SOC and apply splines to SCARP -----------------------------------------

### the values in the SCaRP dataset are not very extreme, there does not seem to be outliers
### Maximum SOC is 147 mg C/ g soil (14%)
SCaRP.Soil <- SCaRP.Soil[!(is.na(SCaRP.Soil$MIR_HOC)|is.na(SCaRP.Soil$MIR_POC)|is.na(SCaRP.Soil$MIR_ROC)),]
SCaRP.Soil$MIR_POC.st <- ifelse(SCaRP.Soil$MIR_POC<0, 0.01, SCaRP.Soil$MIR_POC) 
SCaRP.Soil$MIR_ROC.st <- ifelse(SCaRP.Soil$MIR_ROC<0, 0.01, SCaRP.Soil$MIR_ROC) 
SCaRP.Soil$MIR_HOC.st <- ifelse(SCaRP.Soil$MIR_HOC<0, 0.01, SCaRP.Soil$MIR_HOC) 
SCaRP.Soil$SumFracOC.st <- SCaRP.Soil$MIR_POC.st + SCaRP.Soil$MIR_ROC.st + SCaRP.Soil$MIR_HOC.st
cor.test(x=(SCaRP.Soil$SumFracOC.st), y=SCaRP.Soil$OC,method = "pearson")

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
par(resetPar())
plot(x=SCaRP.Soil$SumFracOC.st, y=SCaRP.Soil$OC,
     xlab= "Sum of predicted SOC fractions", 
     ylab="Measured TOC"); abline(0,1,col="blue")

SCaRP.Soil$POC_p <- (SCaRP.Soil$MIR_POC.st / SCaRP.Soil$SumFracOC.st) * 100
SCaRP.Soil$ROC_p <- (SCaRP.Soil$MIR_ROC.st / SCaRP.Soil$SumFracOC.st) * 100
SCaRP.Soil$HOC_p <- (SCaRP.Soil$MIR_HOC.st / SCaRP.Soil$SumFracOC.st) * 100

summary(SCaRP.Soil[, c("POC_p","ROC_p","HOC_p")])

### Fit mass-preserving splines for the depths...
sort(unique(SCaRP.Soil$LDepth))
#  [1]   5  10  15  20  30  40  50  70  75  90 100
sort(unique(SCaRP.Soil$UDepth))
# [1]  0  5 10 15 20 30 50 70 75

### For each sampling location, e.g., myID
SCaRP.Soil <- arrange(SCaRP.Soil, myID, myLayer)
SCaRP.Soil <- as.data.frame(SCaRP.Soil)

### Correct some small errors
SCaRP.Soil[SCaRP.Soil$myID == "1780", ]$UDepth
SCaRP.Soil[SCaRP.Soil$myID == "1780", ]$LDepth
SCaRP.Soil[SCaRP.Soil$myID == "1780", ]$LDepth <- SCaRP.Soil[SCaRP.Soil$myID == "1780", ]$UDepth +10

## how many samples need to be averaged by layer?
SCaRP.Soil.summary.layers <- SCaRP.Soil %>% 
  group_by(., myID, UDepth, LDepth) %>%
  summarise(., N = n(), Composite = mean(Composite))

SCaRP.Soil.summary.layers %>% filter(., N >1)

too.many <- unique(SCaRP.Soil.summary.layers[SCaRP.Soil.summary.layers$N > 1,]$myID)
kkeps <- unique(setdiff(SCaRP.Soil$myID, too.many))
length(unique(SCaRP.Soil$myID))
length(too.many) + length(kkeps)

### Split into two dataframes
SCaRP.Soil.Single <- SCaRP.Soil[SCaRP.Soil$myID %in% kkeps,]
SCaRP.Soil.Multiple <- SCaRP.Soil[SCaRP.Soil$myID %in% too.many,]
SCaRP.Soil.Multiple <- SCaRP.Soil.Multiple %>% arrange(., myID, myLayer) %>% as.data.frame()

### Average by combination MyID_UDepth_LDepth
SCaRP.Soil.Multiple.Ave <- SCaRP.Soil.Multiple[0,]
col.order <- colnames(SCaRP.Soil.Multiple.Ave)

for (i in 1:length(too.many)){
  print(i)
  df.i <- SCaRP.Soil.Multiple[SCaRP.Soil.Multiple$myID == too.many[[i]],]
  ### Summarize numeric columns
  df.numeric <- df.i %>% group_by(., myID, UDepth, LDepth) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
  df.numeric <- arrange(df.numeric, myID, UDepth, LDepth)
  df.numeric <- as.data.frame(df.numeric)
  ## Here the specIC is meaningless/incorrect
  df.numeric$specID <- NA
  
  ### Recalculate myLayer
  df.numeric$myLayer <- 1:nrow(df.numeric)
  
  ### Character variables and keep only the first rows
  df.char <- df.i %>% select_if(is.character)
  df.char <- df.char[1:nrow(df.numeric),]
  
  df.char$SCaRP_SamID <- NA
  df.char$State_SamID <- NA
  ### join both dataframes
  df.o <- cbind(df.numeric,df.char)
  ### Reorder columns
  df.o <- df.o[,col.order]
  ### Add to empty table
  SCaRP.Soil.Multiple.Ave <- rbind(SCaRP.Soil.Multiple.Ave, df.o )
}
rm(col.order,i,df.i,df.numeric,df.char,df.o,kkeps,too.many)

### Merge together
SCaRP.Soil.3 <- rbind(SCaRP.Soil.Single, SCaRP.Soil.Multiple.Ave)

### Now perform splines and keep GSM intervals (we should just keep between 0-30 cm)
colnames(SCaRP.Soil.3)
SCaRP.Soil.3 <- arrange(SCaRP.Soil.3, myID, UDepth)
SCaRP.Soil.3 <- as.data.frame(SCaRP.Soil.3)

##I forgot Vp = POC / (HOC + ROC)
SCaRP.Soil.3$Vp <- SCaRP.Soil.3$MIR_POC.st/(SCaRP.Soil.3$MIR_HOC.st + SCaRP.Soil.3$MIR_ROC.st)

### Are there any sites with only one sampled layer?
### Now, separate those that have one observations per location from those with several per location
SCaRP.Soil.summary.site <- SCaRP.Soil.3 %>% 
  group_by(., myID) %>%
  summarise(., N = n())
SCaRP.Soil.summary.site %>% filter(., N ==1) %>% as.data.frame()
### Just two. Let's check
SCaRP.Soil.3[SCaRP.Soil.3$myID %in% c("3058", "3248"),] 
### For 0-15 and 0-10. Then I don't need to worry

## Unique SiteID
sampling.profiles <- unique(SCaRP.Soil.3$myID)

### Keep info I want in a new dataframe
# SCaRP.Soil.GSM <- SCaRP.Soil.3[0,c("SCaRP_SiteID","myID", "Long", "Lat","State_SiteID","State","Group",
#                                    "Region","SCaRP_SoilType","Aust_SoilClass","ManageClass","ManageClass2",
#                                    "DateSampled","SiteType",
#                                    "Lat_WGS84","Long_WGS84","coord_ID","UDepth","LDepth",
#                                    "MIR_POC.st","MIR_ROC.st","MIR_HOC.st")]

### Load ea_spline function from Brendan
desired.properties <- c("POC_p","ROC_p","HOC_p", "Vp")
out.splines <- list()

for(j in 1:length(desired.properties)) {
  df.j <- as.data.frame(SCaRP.Soil.3[, c("myID","UDepth","LDepth",desired.properties[[j]])])
  ### Fit mass-preserving spline for j property
  eaFit <- ea_spline(obj=df.j, var.name = desired.properties[[j]], d = t(c(0,5,15,30)), lam = 0.1, vlow = 0, show.progress = TRUE)
  eaFit$obs.preds[,2] <- as.numeric(eaFit$obs.preds[,2])
  eaFit$obs.preds[,3] <- as.numeric(eaFit$obs.preds[,3])
  eaFit$obs.preds[,4] <- as.numeric(eaFit$obs.preds[,4])
  out.splines[[j]] <- eaFit
}

SCaRP.POC_p <- out.splines[[1]]
SCaRP.ROC_p <- out.splines[[2]]
SCaRP.HOC_p <- out.splines[[3]]
SCaRP.Vp <- out.splines[[4]]

### Numbers for article
dim(SCaRP.Soil.3) ### 14426 observations
length(unique(SCaRP.Soil.3$myID))

save(SCaRP.POC_p,SCaRP.ROC_p,SCaRP.HOC_p, SCaRP.Vp, SCaRP.Soil, SCaRP.Soil.3,  file="SCaRP_SOCp_05082021.RData")

problematicIDs <- setdiff(sampling.profiles, SCaRP.POC_p$harmonised$id)
rm(out.splines, desired.properties, sampling.profiles,eaFit, df.j,j,problematicIDs, resetPar)
rm(SCaRP.Soil.Multiple, SCaRP.Soil.Multiple.Ave,SCaRP.Soil.Single, SCaRP.Soil.summary.layers, SCaRP.Soil.summary.site)

### Save intermediate image
save.image("1_Data_SOCfractionProportions_05082021.RData")

# 1.7 Process TERN data --------------------------------------------

load("1_Data_SOCfractionProportions_05082021.RData")

### Clarification, the MIR SOC fraction data is all in mg/g soil!
### For each sampling location, e.g., myID
tern_SOCfractions <- arrange(tern_SOCfractions, coord_ID, UDepth, LDepth) %>% as.data.frame()

## how many are individual samples that need to be averaged?
tern_SOCfractions.summary.layers <- tern_SOCfractions %>% 
  group_by(., coord_ID, UDepth, LDepth) %>%
  summarise(., N = n())

tern_SOCfractions.summary.layers %>% filter(., N >1)

too.many <- unique(tern_SOCfractions.summary.layers[tern_SOCfractions.summary.layers$N > 1,]$coord_ID)
kkeps <- unique(setdiff(tern_SOCfractions$coord_ID, too.many))
length(unique(tern_SOCfractions$coord_ID))
length(too.many) + length(kkeps)

### Create a "myLayer" variable
tern_SOCfractions <- arrange(tern_SOCfractions, coord_ID, UDepth, LDepth)
tern_SOCfractions <- tern_SOCfractions %>% group_by(.,coord_ID) %>% arrange(., coord_ID, UDepth, LDepth) %>%
  mutate(., myLayer =  row_number())

### Split into two dataframes
tern_SOCfractions.Single <- tern_SOCfractions[tern_SOCfractions$coord_ID %in% kkeps,]
tern_SOCfractions.Multiple <- tern_SOCfractions[tern_SOCfractions$coord_ID %in% too.many,]
tern_SOCfractions.Multiple <- arrange(tern_SOCfractions.Multiple, coord_ID, UDepth, LDepth )
tern_SOCfractions.Multiple <- as.data.frame(tern_SOCfractions.Multiple)

### Average by combination MyID_UDepth_LDepth
tern_SOCfractions.Multiple.Ave <- tern_SOCfractions.Multiple[0,]
col.order <- colnames(tern_SOCfractions.Multiple.Ave)

for (i in 1:length(too.many)){
  print(i)
  df.i <- tern_SOCfractions.Multiple[tern_SOCfractions.Multiple$coord_ID == too.many[[i]],]
  ### Summarize numeric columns
  df.numeric <- df.i %>% group_by(., coord_ID, UDepth, LDepth) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
  df.numeric <- arrange(df.numeric, coord_ID, UDepth, LDepth)
  df.numeric <- as.data.frame(df.numeric)
  ## Here the mylabs is meaningless/incorrect
  df.numeric$mylabs <- NA
  
  ### Recalculate myLayer
  df.numeric$myLayer <- 1:nrow(df.numeric)
  
  ### Character variables and keep only the first rows
  df.char <- df.i %>% select_if(is.character)
  df.char <- df.char[1:nrow(df.numeric),]
  
  df.char$matchedbarcode <- NA
  df.char$nir_lab <- NA
  ### join both dataframes
  df.o <- cbind(df.numeric,df.char)
  ### Reorder columns
  df.o <- df.o[,col.order]
  ### Add to empty table
  tern_SOCfractions.Multiple.Ave <- rbind(tern_SOCfractions.Multiple.Ave, df.o )
}
rm(col.order,i,df.i,df.numeric,df.char,df.o,kkeps,too.many)

### Merge together
tern_SOCfractions.3 <- rbind(tern_SOCfractions.Single, tern_SOCfractions.Multiple.Ave)

## and also create a column "myID"
tern_SOCfractions.4 <- tern_SOCfractions.3 %>% arrange(., coord_ID, UDepth, LDepth ) %>% group_by(.,coord_ID) %>% 
  mutate(., myID =  cur_group_id())
tern_SOCfractions.4$myID <- as.character(tern_SOCfractions.4$myID)
tern_SOCfractions.4 <- as.data.frame(tern_SOCfractions.4)
rm(tern_SOCfractions.3, tern_SOCfractions.Multiple, tern_SOCfractions.Single,
   tern_SOCfractions.Multiple.Ave, tern_SOCfractions.summary.layers)

tern_SOCfractions.sf <- st_as_sf(tern_SOCfractions.4, coords = c("Long_WGS84","Lat_WGS84"), crs = 4326)
plot(tern_SOCfractions.sf["POC"])

ggmap(AusMap)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84, alpha=1/30),pch=19,color="gray20",
             data = tern_SOCfractions.4)
colnames(tern_SOCfractions.4)

tern_SOCfractions.4 <- tern_SOCfractions.4[!(is.na(tern_SOCfractions.4$HOC)|is.na(tern_SOCfractions.4$POC)|
                                           is.na(tern_SOCfractions.4$ROC)),]
tern_SOCfractions.4$HOC.st <- ifelse(tern_SOCfractions.4$HOC <0, 0.01, tern_SOCfractions.4$HOC)
tern_SOCfractions.4$POC.st <- ifelse(tern_SOCfractions.4$POC <0, 0.01, tern_SOCfractions.4$POC)
tern_SOCfractions.4$ROC.st <- ifelse(tern_SOCfractions.4$ROC <0, 0.01, tern_SOCfractions.4$ROC)
tern_SOCfractions.4$SumFracOC.st <- tern_SOCfractions.4$HOC.st + tern_SOCfractions.4$POC.st + tern_SOCfractions.4$ROC.st

summary(tern_SOCfractions.4[,c("HOC.st", "POC.st", "ROC.st", "OC", "SumFracOC.st")])

### Calculate contribution and Vp
tern_SOCfractions.4$POC_p <- (tern_SOCfractions.4$POC.st / tern_SOCfractions.4$SumFracOC.st) * 100
tern_SOCfractions.4$ROC_p <- (tern_SOCfractions.4$ROC.st / tern_SOCfractions.4$SumFracOC.st) * 100
tern_SOCfractions.4$HOC_p <- (tern_SOCfractions.4$HOC.st / tern_SOCfractions.4$SumFracOC.st) * 100
tern_SOCfractions.4$Vp <- tern_SOCfractions.4$POC.st / (tern_SOCfractions.4$HOC.st + tern_SOCfractions.4$ROC.st)
summary(tern_SOCfractions.4[,c("HOC_p", "POC_p", "ROC_p", "Vp")])
plot(x=tern_SOCfractions.4$POC.st, y=(tern_SOCfractions.4$HOC.st + tern_SOCfractions.4$ROC.st))

### Fit mass-preserving splines for the depths...
plot(tern_SOCfractions.4$lower, tern_SOCfractions.4$upper)
abline(0,1, col="blue")
sort(unique(tern_SOCfractions.4$LDepth))
#  [1]  2  3  5  6  7  8  9 10 11 12 15 16 17 20 22 25 27 28 30
sort(unique(tern_SOCfractions.4$UDepth))
# [1]  0  2  3  5  6 10 15 20
length(unique(tern_SOCfractions.4$coord_ID))
length(unique(paste(tern_SOCfractions.4$coord_ID, tern_SOCfractions.4$UDepth)))

### How many of these have only one measurement per location?
## =1 --> aqp
## >1 --> Splines

### Now, separate those that have one observations per location from those with several per location
detach("package:aqp", unload=TRUE)
tern.summary.site <- tern_SOCfractions.4 %>% 
  group_by(., myID) %>%
  summarise(., N = n())%>% as.data.frame()
tern.summary.site %>% filter(., N ==1) %>% as.data.frame()

too.many <- unique(tern.summary.site[tern.summary.site$N > 1,]$myID)
kkeps <- unique(setdiff(tern.summary.site$myID, too.many))
length(unique(tern_SOCfractions.4$myID))
length(too.many) + length(kkeps)

### Split into two dataframes
tern_SOCfractions.4.aqp <- tern_SOCfractions.4[tern_SOCfractions.4$myID %in% kkeps,]
tern_SOCfractions.4.spl <- tern_SOCfractions.4[tern_SOCfractions.4$myID %in% too.many,]
tern_SOCfractions.4.aqp <- as.data.frame(tern_SOCfractions.4.aqp)
str(tern_SOCfractions.4.aqp)
summary(tern_SOCfractions.4.aqp$LDepth)
summary(tern_SOCfractions.4.aqp$UDepth)
tern_SOCfractions.4.aqp.bck <- tern_SOCfractions.4.aqp
#tern_SOCfractions.4.aqp <- tern_SOCfractions.4.aqp.bck

library(aqp)
# upgrade to SoilProfileCollection
# 'myID' is the name of the column containing the profile ID
# 'UDepth' is the name of the column containing horizon upper boundaries
# 'LDepth' is the name of the column containing horizon lower boundaries
#depths(tern_SOCfractions.4.aqp) <- myID ~ UDepth + LDepth
#print(tern_SOCfractions.4.aqp)
#checkSPC(tern_SOCfractions.4.aqp)
#spc_in_sync(tern_SOCfractions.4.aqp)
#checkHzDepthLogic(tern_SOCfractions.4.aqp)

### change of support according to GSM depths
### https://ncss-tech.github.io/AQP/aqp/aqp-intro.html#14_Aggregating_Soil_Profile_Collections_Along_Regular_%E2%80%9CSlabs%E2%80%9D

### Doing all at the same time
gsm.depths <- c(0, 5, 15, 30)
desired.properties <- c("POC_p","ROC_p","HOC_p","Vp")
forms <- list(paste("myID ~ POC_p"), paste("myID ~ ROC_p"),paste("myID ~ HOC_p"), paste("myID ~ Vp")) 

### Remember columns in the original dataset
# [1]  "mylabs"         "state_c"        "nir_lab"        "zone"           "easting"        "northing"       "matchedbarcode" "upper"          "lower"         
# [10] "type"           "pH"             "ec"             "longitude"      "latitude"       "OC"             "POC"            "HOC"            "ROC"           
# [19] "Lat_WGS84"      "Long_WGS84"     "coord_ID"       "UDepth"         "LDepth"         "HOC.st"         "POC.st"         "ROC.st"         "myLayer"       
# [28] "myID"           "TOC"            "SumFracOC.st"

## Let's keep "myID" "Lat_WGS84" "Long_WGS84" "coord_ID"  
out.aqp <- list()
for(j in 1:length(desired.properties)) {
  
  ### Eliminate NA for each property
  nona.idx <- !is.na(tern_SOCfractions.4.aqp[,desired.properties[[j]]])
  tern_SOCfractions.4.aqp.j <- tern_SOCfractions.4.aqp[nona.idx,]
  tern_SOCfractions.4.aqp.j <- tern_SOCfractions.4.aqp.j[, c("myID","Lat_WGS84","Long_WGS84","coord_ID","UDepth","LDepth","POC_p","ROC_p","HOC_p","Vp")]
  tern_SOCfractions.4.aqp.j.bck <- tern_SOCfractions.4.aqp.j
  depths(tern_SOCfractions.4.aqp.j) <- myID ~ UDepth + LDepth
  
  # print(tern_SOCfractions.4.aqp.j)
  # checkSPC(tern_SOCfractions.4.aqp.j)
  # spc_in_sync(tern_SOCfractions.4.aqp.j)
  # checkHzDepthLogic(tern_SOCfractions.4.aqp.j)
  
  ### For one property
  tern.gsm.j <- aqp::slab(tern_SOCfractions.4.aqp.j, fm= as.formula(forms[[j]]), slab.structure = gsm.depths, slab.fun = mean, na.rm=TRUE)
  head(tern.gsm.j)
  #ids <- aqp::profile_id(tern_SOCfractions.4.aqp.j)
  
  ### when contribution is less than 25% I set it to NA, only when another layer in that location already has the value assigned
 # tern.gsm.j.rev <- tern.gsm.j[0,]
  
  # for(i in 1:length(ids)){
  #   
  #   df.i <- tern.gsm.j[tern.gsm.j$myID ==ids[[i]], ]
  #   N.profile <- sum(!is.na(df.i$value))
  #   
  #   if(N.profile ==1){
  #     df.i$value <- ifelse(is.nan(df.i$value), NA, df.i$value)
  #     tern.gsm.j.rev <- rbind(tern.gsm.j.rev,df.i)
  #   }
  #   
  #   if(N.profile > 1 & any(df.i$contributing_fraction >= 0.25)){
  #     df.i$value <- ifelse(df.i$contributing_fraction < 0.25, NA, df.i$value)
  #     tern.gsm.j.rev <- rbind(tern.gsm.j.rev,df.i)
  #   } else {
  #     max.contrib <- max(df.i$contributing_fraction)
  #     df.i$value <- ifelse(df.i$contributing_fraction < max.contrib, NA, df.i$value)
  #     tern.gsm.j.rev <- rbind(tern.gsm.j.rev,df.i)
  #   }
  # }
  tern.gsm.j$value <- ifelse(is.nan(tern.gsm.j$value), NA, tern.gsm.j$value)
  # reshape to wide format
  tern.gsm.j$GMS_layer <- paste0(tern.gsm.j$top,"-",tern.gsm.j$bottom, " cm")
  tern.gsm.j.wide <- dcast(tern.gsm.j, myID + variable ~ GMS_layer , value.var = 'value', fun.aggregate=mean)
  ## Add some columns
  tern.gsm.j.wide <- merge(tern.gsm.j.wide, 
                           tern_SOCfractions.4.aqp.bck[, c("myID","coord_ID","Lat_WGS84","Long_WGS84")],
                           by="myID", all.x=TRUE)
  out.aqp[[j]] <- tern.gsm.j.wide
}

tern.aqp <- out.aqp

rm("tern.gsm.j",  "tern.gsm.j.rev","tern.gsm.j.rev.wide", "tern_SOCfractions.4.aqp.j",
   "tern_SOCfractions.4.aqp.j.bck", df.i,i,j,nona.idx, max.contrib,N.profile, forms, ids, out.aqp)

### Now perform splines and keep GSM intervals (we should just keep between 0-30 cm)
colnames(tern_SOCfractions.4.spl)
tern_SOCfractions.4.spl <- arrange(tern_SOCfractions.4.spl, myID, UDepth, LDepth) %>% as.data.frame()

### Unique SiteID
sampling.profiles <- unique(tern_SOCfractions.4.spl$myID)
desired.properties <- c("POC_p","ROC_p","HOC_p","Vp")

out.splines <- list()
for(j in 1:length(desired.properties)) {
  df.j <- as.data.frame(tern_SOCfractions.4.spl[, c("myID","UDepth","LDepth",desired.properties[[j]])])
  ### Fit mass-preserving spline for j property
  eaFit <- ea_spline(obj=df.j, var.name = desired.properties[[j]], d = t(c(0,5,15,30)), lam = 0.1, vlow = 0, show.progress = TRUE)
  eaFit$obs.preds[,2] <- as.numeric(eaFit$obs.preds[,2])
  eaFit$obs.preds[,3] <- as.numeric(eaFit$obs.preds[,3])
  eaFit$obs.preds[,4] <- as.numeric(eaFit$obs.preds[,4])
  out.splines[[j]] <- eaFit
  
}

TERN.POC_p <- out.splines[[1]]
TERN.ROC_p <- out.splines[[2]]
TERN.HOC_p <- out.splines[[3]]
TERN.Vp <- out.splines[[4]]

problematicIDs <- setdiff(sampling.profiles, TERN.POC_p$harmonised$id)
rm(out.splines, desired.properties, sampling.profiles,eaFit, df.j,j, problematicIDs, kkeps, too.many,gsm.depths, j,
   tern.summary.site, tern_SOCfractions.4.aqp, tern_SOCfractions.4.aqp.bck, tern_SOCfractions.4.spl, tern.gsm.j.wide)

### Save intermediate TERN data
save(tern.aqp, TERN.POC_p,TERN.ROC_p,TERN.HOC_p, TERN.Vp, tern_SOCfractions, tern_SOCfractions.4, 
     file="tern_SOCp_05082021.RData")

save.image("1_Data_SOCfractionProportions_05082021.RData")
load("1_Data_SOCfractionProportions_05082021.RData")


# 1.8 Splines on Aus_archive ----------------------------------------------------

dim(aus_archive_SOCfractions)
colnames(aus_archive_SOCfractions)
aus_archive_SOCfractions <- aus_archive_SOCfractions[!is.na(aus_archive_SOCfractions$Long_WGS84),]

### Eliminate duplicates
aus_archive_SOCfractions <- aus_archive_SOCfractions[!duplicated(aus_archive_SOCfractions),]

ggmap(AusMap)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="pink",
             data = aus_archive_SOCfractions)

aus_archive_SOCfractions <- aus_archive_SOCfractions[!(is.na(aus_archive_SOCfractions$HOC)|is.na(aus_archive_SOCfractions$POC)|
                                               is.na(aus_archive_SOCfractions$ROC)),]
aus_archive_SOCfractions$HOC.st <- ifelse(aus_archive_SOCfractions$HOC <0, 0.01, aus_archive_SOCfractions$HOC)
aus_archive_SOCfractions$POC.st <- ifelse(aus_archive_SOCfractions$POC <0, 0.01, aus_archive_SOCfractions$POC)
aus_archive_SOCfractions$ROC.st <- ifelse(aus_archive_SOCfractions$ROC <0, 0.01, aus_archive_SOCfractions$ROC)
aus_archive_SOCfractions$SumFracOC.st <- aus_archive_SOCfractions$HOC.st + aus_archive_SOCfractions$POC.st + aus_archive_SOCfractions$ROC.st

summary(aus_archive_SOCfractions[,c("HOC.st", "POC.st", "ROC.st", "OC", "SumFracOC.st")])

### Calculate contribution and Vp
aus_archive_SOCfractions$POC_p <- (aus_archive_SOCfractions$POC.st / aus_archive_SOCfractions$SumFracOC.st) * 100
aus_archive_SOCfractions$ROC_p <- (aus_archive_SOCfractions$ROC.st / aus_archive_SOCfractions$SumFracOC.st) * 100
aus_archive_SOCfractions$HOC_p <- (aus_archive_SOCfractions$HOC.st / aus_archive_SOCfractions$SumFracOC.st) * 100
aus_archive_SOCfractions$Vp <- aus_archive_SOCfractions$POC.st / (aus_archive_SOCfractions$HOC.st + aus_archive_SOCfractions$ROC.st)
summary(aus_archive_SOCfractions[,c("HOC_p", "POC_p", "ROC_p", "Vp")])
plot(x=aus_archive_SOCfractions$POC.st, y=(aus_archive_SOCfractions$HOC.st + aus_archive_SOCfractions$ROC.st))

detach("package:aqp", unload=TRUE)
### Clarification, the vis-NIR SOC fraction data is all in mg/g soil!
aus_archive_SOCfractions_bck <- aus_archive_SOCfractions
#aus_archive_SOCfractions <- aus_archive_SOCfractions_bck
ggmap(AusMap)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="azure3",
             data = SCaRP.Soil.3)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="lightpink2",
             data = aus_archive_SOCfractions)+
  #geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="darkcyan",
   #          data = NatSoil_SOCfractions)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="darkgoldenrod2",
             data = tern_SOCfractions.4)#+
 # geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="blue",
  #           data = bm2_SOCfractions)

ggmap(AusMap)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="lightpink2",
             data = SCaRP.Soil.3)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="lightpink2",
             data = aus_archive_SOCfractions)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="lightpink2",
             data = NatSoil_SOCfractions)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="lightpink2",
             data = tern_SOCfractions.4)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="lightpink2",
             data = bm2_SOCfractions)
  
sort(unique(aus_archive_SOCfractions$LDepth))
sort(unique(aus_archive_SOCfractions$UDepth))
hist(aus_archive_SOCfractions$LDepth)
hist(aus_archive_SOCfractions$UDepth)

## coord_ID,h_no,samp_no
length(unique(aus_archive_SOCfractions$coord_ID))
length(unique(aus_archive_SOCfractions$Location_ID))
length(unique(paste(aus_archive_SOCfractions$coord_ID, aus_archive_SOCfractions$UDepth)))
length(unique(paste(aus_archive_SOCfractions$Location_ID, aus_archive_SOCfractions$UDepth)))

### For each sampling location, e.g., myID
aus_archive_SOCfractions <- arrange(aus_archive_SOCfractions, Location_ID, UDepth, samp_no) %>% as.data.frame()

## how many are individual samples that need to be averaged?
aus_archive_SOCfractions.summary.layers <- aus_archive_SOCfractions %>% 
  group_by(., Location_ID, UDepth, LDepth) %>%
  summarise(., N = n())

aus_archive_SOCfractions.summary.layers %>% filter(., N >1)

too.many <- unique(aus_archive_SOCfractions.summary.layers[aus_archive_SOCfractions.summary.layers$N > 1,]$Location_ID)
kkeps <- unique(setdiff(aus_archive_SOCfractions$Location_ID, too.many))
length(unique(aus_archive_SOCfractions$Location_ID))
length(too.many) + length(kkeps)

### Create a "myLayer" variable
aus_archive_SOCfractions <- arrange(aus_archive_SOCfractions, Location_ID, UDepth, LDepth)
aus_archive_SOCfractions <- aus_archive_SOCfractions %>% group_by(.,Location_ID) %>% arrange(., Location_ID, UDepth, LDepth ) %>%
  mutate(., myLayer =  row_number())

### Split into two dataframes
aus_archive_SOCfractions.Single <- aus_archive_SOCfractions[aus_archive_SOCfractions$Location_ID %in% kkeps,]
aus_archive_SOCfractions.Multiple <- aus_archive_SOCfractions[aus_archive_SOCfractions$Location_ID %in% too.many,]
aus_archive_SOCfractions.Multiple <- arrange(aus_archive_SOCfractions.Multiple, Location_ID, UDepth, LDepth)
aus_archive_SOCfractions.Multiple <- as.data.frame(aus_archive_SOCfractions.Multiple)
aus_archive_SOCfractions.Single <- as.data.frame(aus_archive_SOCfractions.Single)

### Average by combination MyID_UDepth_LDepth
aus_archive_SOCfractions.Multiple.Ave <- aus_archive_SOCfractions.Multiple[0,]
col.order <- colnames(aus_archive_SOCfractions.Multiple.Ave)

for (i in 1:length(too.many)){
  print(i)
  df.i <- aus_archive_SOCfractions.Multiple[aus_archive_SOCfractions.Multiple$Location_ID == too.many[[i]],]
  ### Summarize numeric columns
  df.numeric <- df.i %>% group_by(., Location_ID, UDepth, LDepth) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
  df.numeric <- arrange(df.numeric, Location_ID, UDepth, LDepth)
  df.numeric <- as.data.frame(df.numeric)
  ## Here I don't need this
  df.numeric$specID <- NA
  df.numeric$spec_id <- NA
  
  ### Recalculate myLayer
  df.numeric$myLayer <- 1:nrow(df.numeric)
  
  ### Character variables and keep only the first rows
  df.char <- df.i %>% select_if(is.character)
  df.char <- df.char[1:nrow(df.numeric),]
  
  df.char$specName <- NA
  #df.char$s_id <- NA
  ### join both dataframes
  df.o <- cbind(df.numeric,df.char)
  ### Reorder columns
  df.o <- df.o[,col.order]
  ### Add to empty table
  aus_archive_SOCfractions.Multiple.Ave <- rbind(aus_archive_SOCfractions.Multiple.Ave, df.o )
}
rm(col.order,i,df.i,df.numeric,df.char,df.o,kkeps,too.many)

### Merge together
aus_archive_SOCfractions.3 <- rbind(aus_archive_SOCfractions.Single, aus_archive_SOCfractions.Multiple.Ave)

## and also create a column "myID
aus_archive_SOCfractions.6 <- aus_archive_SOCfractions.3 %>% arrange(., Location_ID, UDepth, LDepth ) %>% group_by(.,Location_ID) %>% 
  mutate(., myID =  cur_group_id())
aus_archive_SOCfractions.6$myID <- as.character(aus_archive_SOCfractions.6$myID)
aus_archive_SOCfractions.6 <- as.data.frame(aus_archive_SOCfractions.6)

rm(aus_archive_SOCfractions.3, aus_archive_SOCfractions.Multiple, aus_archive_SOCfractions.Multiple.Ave, 
   aus_archive_SOCfractions.Single, aus_archive_SOCfractions.summary.layers)

### Now I apply splines and aqp for standarize depths to both datasets
#data.frame.SOCfractions <- aus_archive_SOCfractions.6

standarize.GSM <- function(data.frame.SOCfractions){
  if(!is.data.frame(data.frame.SOCfractions)){
    print("Error, input is not a dataframe")
  }
  
  df.SOCfr.summary.site <- data.frame.SOCfractions %>% 
    group_by(., myID) %>%
    summarise(., N = n())
  df.SOCfr.summary.site %>% filter(., N ==1) %>% as.data.frame()
  
  too.many <- unique(df.SOCfr.summary.site[df.SOCfr.summary.site$N > 1,]$myID)
  kkeps <- unique(setdiff(df.SOCfr.summary.site$myID, too.many))
  print(paste0("There are ",length(unique(data.frame.SOCfractions$myID)), " unique sites"))
  print(paste0("There are ",length(too.many), " sites with more than one observation, which will be processed with splines"))
  print(paste0("There are ",length(kkeps), " sites with only one observation, which will be processed with aqp"))
  print(length(too.many) + length(kkeps))
  
  ### Split into two dataframes
  data.frame.SOCfractions.aqp <- data.frame.SOCfractions[data.frame.SOCfractions$myID %in% kkeps,]
  data.frame.SOCfractions.spl <- data.frame.SOCfractions[data.frame.SOCfractions$myID %in% too.many,]
  data.frame.SOCfractions.spl <- as.data.frame(data.frame.SOCfractions.spl)
  #str(data.frame.SOCfractions.aqp)
  #summary(data.frame.SOCfractions.aqp$LDepth)
  data.frame.SOCfractions.aqp.bck <- data.frame.SOCfractions.aqp
  data.frame.SOCfractions.aqp.bck <- as.data.frame(data.frame.SOCfractions.aqp.bck)
  data.frame.SOCfractions.aqp <- as.data.frame(data.frame.SOCfractions.aqp)
 
  library(aqp)
  # upgrade to SoilProfileCollection
  # 'myID' is the name of the column containing the profile ID
  # 'UDepth' is the name of the column containing horizon upper boundaries
  # 'LDepth' is the name of the column containing horizon lower boundaries
  # depths(data.frame.SOCfractions.aqp) <- myID ~ UDepth + LDepth
  # print(data.frame.SOCfractions.aqp)
  # checkSPC(data.frame.SOCfractions.aqp)
  # spc_in_sync(data.frame.SOCfractions.aqp)
  # checkHzDepthLogic(data.frame.SOCfractions.aqp)
  
  ### change of support according to GSM depths
  ### https://ncss-tech.github.io/AQP/aqp/aqp-intro.html#14_Aggregating_Soil_Profile_Collections_Along_Regular_%E2%80%9CSlabs%E2%80%9D
  gsm.depths <- c(0, 5, 15, 30)
  # desired.properties <- c("POC.st","ROC.st","HOC.st")
  # forms <- list(paste("myID ~ POC.st"), paste("myID ~ ROC.st"),paste("myID ~ HOC.st"))
  desired.properties <- c("POC_p","ROC_p","HOC_p","Vp")
  forms <- list(paste("myID ~ POC_p"), paste("myID ~ ROC_p"),paste("myID ~ HOC_p"), paste("myID ~ Vp")) 
  
  ### Remember columns in the original dataset
  # "agency_code"       "proj_code"         "s_id"              "o_id"              "h_no"              "samp_no"           "spec_id"          
  # [8] "o_latitude_GDA94"  "o_longitude_GDA94" "samp_upper_depth"  "samp_lower_depth"  "specName"          "specID"            "OC"               
  # [15] "POC"               "HOC"               "ROC"               "Lat_WGS84"         "Long_WGS84"        "coord_ID"          "UDepth"           
  # [22] "LDepth"            "Location_ID"       "SumFracOC.st"      "myLayer"           "myID"              "DataStore"         "Dataset"          
  # [29] "Provider"          "Layer_ID"          "SampleID"          "SampleDate"        "Longitude"         "Latitude"          "TOC_LabMethod"    
  # [36] "TOC"               "TOC_Units"         "SampleYear"        "SampleYear.num"    "Biome"             "LandCover"         "ISO_CLASS"        
  # [43] "LC_Label"          "DepthInt"
  
  
  ## Let's keep "myID" "Lat_WGS84" "Long_WGS84" "coord_ID"  "Location_ID"
  library(aqp)
  out.aqp <- list()
  for(j in 1:length(desired.properties)) {
    
    ### Eliminate NA for each property
    nona.idx <- !is.na(data.frame.SOCfractions.aqp[,desired.properties[[j]]])
    data.frame.SOCfractions.aqp.j <- data.frame.SOCfractions.aqp[nona.idx,]
    data.frame.SOCfractions.aqp.j <- data.frame.SOCfractions.aqp.j[, c("myID","Lat_WGS84","Long_WGS84","coord_ID","Location_ID","UDepth","LDepth","POC_p","ROC_p","HOC_p","Vp")]
    data.frame.SOCfractions.aqp.j.bck <- data.frame.SOCfractions.aqp.j
    depths(data.frame.SOCfractions.aqp.j) <- myID ~ UDepth + LDepth

    ### For one property
    df.SOCfr.gsm.j <- aqp::slab(data.frame.SOCfractions.aqp.j, fm= as.formula(forms[[j]]), slab.structure = gsm.depths, slab.fun = mean, na.rm=TRUE)
    print(head(df.SOCfr.gsm.j))
    df.SOCfr.gsm.j$value <- ifelse(is.nan(df.SOCfr.gsm.j$value), NA, df.SOCfr.gsm.j$value)
    
    # reshape to wide format
    df.SOCfr.gsm.j$GMS_layer <- paste0(df.SOCfr.gsm.j$top,"-",df.SOCfr.gsm.j$bottom, " cm")
    df.SOCfr.gsm.j.wide <- dcast(df.SOCfr.gsm.j, myID + variable ~ GMS_layer , value.var = 'value', fun.aggregate=mean)
    ## Add some columns
    df.SOCfr.gsm.j.wide <- merge(df.SOCfr.gsm.j.wide, 
                                 data.frame.SOCfractions.aqp.bck[, c("myID","coord_ID","Lat_WGS84","Long_WGS84","Location_ID")],
                                 by="myID", all.x=TRUE)
    out.aqp[[j]] <- df.SOCfr.gsm.j.wide
  }
  
  df.SOCfr.aqp <- out.aqp
  
  # rm("df.SOCfr.gsm.j",  "df.SOCfr.gsm.j.rev","df.SOCfr.gsm.j.rev.wide", "data.frame.SOCfractions.aqp.j",
  #    "data.frame.SOCfractions.aqp.j.bck", df.i,i,j,nona.idx, max.contrib,N.profile, forms, ids, out.aqp, gsm.depths)
  detach("package:aqp", unload=TRUE)
  
  ### Now perform splines and keep GSM intervals (we should just keep between 0-30 cm)
  colnames(data.frame.SOCfractions.spl)
  data.frame.SOCfractions.spl <- arrange(data.frame.SOCfractions.spl, myID, myLayer) %>% as.data.frame()
  data.frame.SOCfractions.spl <- data.frame.SOCfractions.spl[!is.na(data.frame.SOCfractions.spl$HOC.st),]
  
  ### Unique SiteID
  sampling.profiles <- unique(data.frame.SOCfractions.spl$myID)
  desired.properties <- c("POC_p","ROC_p","HOC_p","Vp")
  #summary(data.frame.SOCfractions.spl[,desired.properties])

  out.splines <- list()
  
  for(j in 1:length(desired.properties)) {
    df.j <- as.data.frame(data.frame.SOCfractions.spl[, c("myID","UDepth","LDepth",desired.properties[[j]])])
    # myIDs <- unique(df.j$myID)
    ### check one by one, to identify where the spline fails
    # for(i in 18:length(unique(df.j$myID))){
    #   print(df.j[df.j$myID==myIDs[[i]],])
    #   print(paste0("Problem with i==",i," or myID==",myIDs[[i]]))
    #   ea_spline(obj=df.j[df.j$myID==myIDs[[i]],], var.name = desired.properties[[j]], d = t(c(0,5,15,30,60,100,200)), lam = 0.1, vlow = 0, show.progress = TRUE)
    # }
    ### Fit mass-preserving spline for j property
    eaFit <- ea_spline(obj=df.j, var.name = desired.properties[[j]], d = t(c(0,5,15,30)), lam = 0.1, vlow = 0, show.progress = TRUE)
    eaFit$obs.preds[,2] <- as.numeric(eaFit$obs.preds[,2])
    eaFit$obs.preds[,3] <- as.numeric(eaFit$obs.preds[,3])
    eaFit$obs.preds[,4] <- as.numeric(eaFit$obs.preds[,4])
    out.splines[[j]] <- eaFit
    
  }
  
  #df.SOCfr.POC <- out.splines[[1]]
  # df.SOCfr.ROC <- out.splines[[2]]
  # df.SOCfr.HOC <- out.splines[[3]]
  #problematicIDs <- setdiff(sampling.profiles, df.SOCfr.POC$harmonised$id)
  output <- list(df.SOCfr.aqp,out.splines)
  
  return(output)
  
}

#aus_archive_SOCfractions <- aus_archive_SOCfractions.6
aus_archive_SOCfractions.GSM <- standarize.GSM(aus_archive_SOCfractions.6)

rm(data.frame.SOCfractions,df.SOCfr.aqp,out.splines,problematicIDs,sampling.profiles,df.SOCfr.POC,df.j,eaFit,
   data.frame.SOCfractions.spl,df.SOCfr.aqp,out.aqp, myIDs, 
   df.SOCfr.summary.site,data.frame.SOCfractions.aqp, data.frame.SOCfractions.aqp.bck,
   aus_archive_SOCfractions.4, aus_archive_SOCfractions.5, aus_archive_SOCfractions.6.GSM)
rm(out.splines,  sampling.profiles,eaFit, df.j,j, i,myFID,problematicIDs,
   kkeps, too.many, aus_archive_SOCfractions.summary.layers, aus_archive_SOCfractions.sites,
   TOC.2.Multiple, TOC.2.Multiple.Ave, TOC.2.Single, TOC.2.summary.layers, aus_archive_SOCfractions.4)

save.image("1_Data_SOCfractionProportions_31082021.RData")
load("1_Data_SOCfractionProportions_31082021.RData")

# 1.9 Splines on NatSoil data --------------------------------------------

### NatSoil_SOCfractions
### Join with TOC data 
dim(NatSoil_SOCfractions)
colnames(NatSoil_SOCfractions)

### Subset columns of interest
NatSoil_SOCfractions <- NatSoil_SOCfractions[,c("agency_code","proj_code","s_id","spec_id","o_latitude_GDA94","o_longitude_GDA94",
                                                "samp_upper_depth","samp_lower_depth","label","POC","HOC","ROC","SOC",
                                                "Lat_WGS84","Long_WGS84","UDepth","LDepth","coord_ID","Location_ID")]
### Eliminate duplicates
NatSoil_SOCfractions <- NatSoil_SOCfractions[!duplicated(NatSoil_SOCfractions),]

ggmap(AusMap)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="azure4",
             data = SCaRP.Soil.3)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="deeppink3",
             data = aus_archive_SOCfractions.6)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="orange",
             data = NatSoil_SOCfractions)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="darkcyan",
             data = tern_SOCfractions.4)

NatSoil_SOCfractions <- NatSoil_SOCfractions[!(is.na(NatSoil_SOCfractions$HOC)|is.na(NatSoil_SOCfractions$POC)|
                                                 is.na(NatSoil_SOCfractions$ROC)),]
NatSoil_SOCfractions$HOC.st <- ifelse(NatSoil_SOCfractions$HOC <0, 0.01, NatSoil_SOCfractions$HOC)
NatSoil_SOCfractions$POC.st <- ifelse(NatSoil_SOCfractions$POC <=0, 0.01, NatSoil_SOCfractions$POC)
NatSoil_SOCfractions$ROC.st <- ifelse(NatSoil_SOCfractions$ROC <0, 0.01, NatSoil_SOCfractions$ROC)
NatSoil_SOCfractions$SumFracOC.st <- NatSoil_SOCfractions$HOC.st + NatSoil_SOCfractions$POC.st + NatSoil_SOCfractions$ROC.st
summary(NatSoil_SOCfractions[,c("HOC.st", "POC.st", "ROC.st", "SOC", "SumFracOC.st")])

### Calculate contribution and Vp
NatSoil_SOCfractions$POC_p <- (NatSoil_SOCfractions$POC.st / NatSoil_SOCfractions$SumFracOC.st) * 100
NatSoil_SOCfractions$ROC_p <- (NatSoil_SOCfractions$ROC.st / NatSoil_SOCfractions$SumFracOC.st) * 100
NatSoil_SOCfractions$HOC_p <- (NatSoil_SOCfractions$HOC.st / NatSoil_SOCfractions$SumFracOC.st) * 100
NatSoil_SOCfractions$Vp <- NatSoil_SOCfractions$POC.st / (NatSoil_SOCfractions$HOC.st + NatSoil_SOCfractions$ROC.st)
summary(NatSoil_SOCfractions[,c("HOC_p", "POC_p", "ROC_p", "Vp")])
plot(x=NatSoil_SOCfractions$POC.st, y=(NatSoil_SOCfractions$HOC.st + NatSoil_SOCfractions$ROC.st))

detach("package:aqp", unload=TRUE)
NatSoil_SOCfractions_bck <- NatSoil_SOCfractions
#NatSoil_SOCfractions <- NatSoil_SOCfractions.3
#rm(NatSoil_SOCfractions.3)
### Correct SOC fraction values
NatSoil_SOCfractions <- NatSoil_SOCfractions[!(is.na(NatSoil_SOCfractions$HOC)|is.na(NatSoil_SOCfractions$POC)|
                                                 is.na(NatSoil_SOCfractions$ROC)),]

## "spec_id", "coord_ID","Location_ID", "UDepth","LDepth"
length(unique(NatSoil_SOCfractions$coord_ID))
length(unique(NatSoil_SOCfractions$Location_ID))
length(unique(paste(NatSoil_SOCfractions$coord_ID, NatSoil_SOCfractions$UDepth)))
length(unique(paste(NatSoil_SOCfractions$Location_ID, NatSoil_SOCfractions$UDepth)))

### Aggregate by spec_id
## how many need to be averaged?
NatSoil_SOCfractions.summary.spec_id<- NatSoil_SOCfractions %>% 
  group_by(., spec_id) %>%
  summarise(., N = n())
summary(NatSoil_SOCfractions.summary.spec_id$N) ### Between 3 and 4 by spec_id

specIDs <- unique(NatSoil_SOCfractions$spec_id)
NatSoil_SOCfractions.AveSpec <- NatSoil_SOCfractions[0,]
col.order <- colnames(NatSoil_SOCfractions)

for (i in 1:length(specIDs)){
  print(i)
  df.i <- NatSoil_SOCfractions[NatSoil_SOCfractions$spec_id == specIDs[[i]],]
  ### Summarize numeric columns
  df.numeric <- df.i %>% group_by(., Location_ID, UDepth, LDepth) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
  df.numeric <- arrange(df.numeric, Location_ID, UDepth, LDepth)
  df.numeric <- as.data.frame(df.numeric)
  
  
  ### Character variables and keep only the first rows
  df.char <- df.i %>% select_if(is.character)
  df.char <- df.char[1:nrow(df.numeric),] %>% as.data.frame()
  
  df.char$label <- NA
  ### join both dataframes
  df.o <- cbind(df.numeric,df.char)
  ### Reorder columns
  df.o <- df.o[,col.order]
  ### Add to empty table
  NatSoil_SOCfractions.AveSpec <- rbind(NatSoil_SOCfractions.AveSpec, df.o)
}
rm(col.order,i,df.i,df.numeric,df.char,df.o)
rm(elim.from.aus, bm2_IDs)

### For each sampling location, e.g., Location_ID
NatSoil_SOCfractions.AveSpec <- arrange(NatSoil_SOCfractions.AveSpec, Location_ID, UDepth) %>% as.data.frame()

## how many are individual samples that need to be averaged?
NatSoil_SOCfractions.summary.layers <- NatSoil_SOCfractions.AveSpec %>% 
  group_by(., Location_ID, UDepth, LDepth) %>%
  summarise(., N = n())
summary(NatSoil_SOCfractions.summary.layers$N)
NatSoil_SOCfractions.summary.layers %>% filter(., N >1) ## Two
#rm(NatSoil_SOCfractions.summary.layers)
#NatSoil_SOCfractions.AveSpec[NatSoil_SOCfractions.AveSpec$coord_ID=="-20.09846306 145.8177643",]
#NatSoil_SOCfractions.AveSpec[NatSoil_SOCfractions.AveSpec$Location_ID %in% too.many,]
too.many <- unique(NatSoil_SOCfractions.summary.layers[NatSoil_SOCfractions.summary.layers$N > 1,]$Location_ID)
kkeps <- unique(setdiff(NatSoil_SOCfractions.AveSpec$Location_ID, too.many))
length(unique(NatSoil_SOCfractions.AveSpec$Location_ID))
length(too.many) + length(kkeps)

### Create a "myLayer" variable
NatSoil_SOCfractions.AveSpec <- NatSoil_SOCfractions.AveSpec %>% group_by(.,Location_ID) %>% arrange(., Location_ID, UDepth, LDepth ) %>%
  mutate(., myLayer =  row_number())
NatSoil_SOCfractions.AveSpec  <- as.data.frame(NatSoil_SOCfractions.AveSpec)

### Split into two dataframes
NatSoil_SOCfractions.AveSpec.Single <- NatSoil_SOCfractions.AveSpec[NatSoil_SOCfractions.AveSpec$Location_ID %in% kkeps,]
NatSoil_SOCfractions.AveSpec.Multiple <- NatSoil_SOCfractions.AveSpec[NatSoil_SOCfractions.AveSpec$Location_ID %in% too.many,]
NatSoil_SOCfractions.AveSpec.Multiple <- arrange(NatSoil_SOCfractions.AveSpec.Multiple, Location_ID, UDepth, LDepth )
NatSoil_SOCfractions.AveSpec.Multiple <- as.data.frame(NatSoil_SOCfractions.AveSpec.Multiple)

## Average by combination Location_ID_UDepth_LDepth
NatSoil_SOCfractions.AveSpec.Multiple.Ave <- NatSoil_SOCfractions.AveSpec.Multiple[0,]
col.order <- colnames(NatSoil_SOCfractions.AveSpec.Multiple)

for (i in 1:length(too.many)){
  print(i)
  df.i <- NatSoil_SOCfractions.AveSpec.Multiple[NatSoil_SOCfractions.AveSpec.Multiple$Location_ID == too.many[[i]],]
  ### Summarize numeric columns
  df.numeric <- df.i %>% group_by(., Location_ID, UDepth, LDepth) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
  df.numeric <- arrange(df.numeric, Location_ID, UDepth, LDepth)
  df.numeric <- as.data.frame(df.numeric)
  ## Here the spec_id is meaningless
  df.numeric$spec_id <- NA

  ### Recalculate myLayer
  df.numeric$myLayer <- 1:nrow(df.numeric)

  ### Character variables and keep only the first rows
  df.char <- df.i %>% select_if(is.character)
  df.char <- df.char[1:nrow(df.numeric),] %>% as.data.frame()

  ### join both dataframes
  df.o <- cbind(df.numeric,df.char)
  df.o$label <- NA
  df.o$X1 <- NA

  ### Reorder columns
  df.o <- df.o[,col.order]
  ### Add to empty table
  NatSoil_SOCfractions.AveSpec.Multiple.Ave <- rbind(NatSoil_SOCfractions.AveSpec.Multiple.Ave, df.o )
}
rm(col.order,i,df.i,df.numeric,df.char,df.o,kkeps,too.many)

### Merge together
NatSoil_SOCfractions.2 <- rbind(NatSoil_SOCfractions.AveSpec.Single, NatSoil_SOCfractions.AveSpec.Multiple.Ave)
#NatSoil_SOCfractions.2 <- NatSoil_SOCfractions.AveSpec.Single

## and also create a column "myID
NatSoil_SOCfractions.3 <- NatSoil_SOCfractions.2 %>% arrange(., Location_ID, UDepth, LDepth ) %>% group_by(.,Location_ID) %>% 
  mutate(., myID =  cur_group_id())
NatSoil_SOCfractions.3$myID <- as.character(NatSoil_SOCfractions.3$myID)
NatSoil_SOCfractions.3.bck <- NatSoil_SOCfractions.3

rm("NatSoil_SOCfractions.2", "NatSoil_SOCfractions.AveSpec" ,specIDs,
   "NatSoil_SOCfractions.AveSpec.Multiple","NatSoil_SOCfractions.AveSpec.Multiple.Ave",
 "NatSoil_SOCfractions.AveSpec.Single","NatSoil_SOCfractions.summary.layers","NatSoil_SOCfractions.summary.spec_id" )

NatSoil_SOCfractions.3 <- as.data.frame(NatSoil_SOCfractions.3)

NatSoil_SOCfractions.3 <- NatSoil_SOCfractions.3[!(is.na(NatSoil_SOCfractions.3$HOC)|
                                                     is.na(NatSoil_SOCfractions.3$POC)|
                                                     is.na(NatSoil_SOCfractions.3$ROC)),]
### Only observations from the top 2m
NatSoil_SOCfractions.3 <- NatSoil_SOCfractions.3[NatSoil_SOCfractions.3$UDepth <250,]

standarize.GSM <- function(data.frame.SOCfractions){ ### I adapt it for the dataframe NatSoil
  if(!is.data.frame(data.frame.SOCfractions)){
    print("Error, input is not a dataframe")
  }
  
 #data.frame.SOCfractions <-   NatSoil_SOCfractions.3
  
### How many of these have only one measurement per location?
## ==1 --> aqp
## >1  --> Splines
### Separate those that have one observations per location from those with several per location
#detach("package:aqp", unload=TRUE)
NatSoil.summary.site <- data.frame.SOCfractions %>% 
  group_by(., myID) %>%
  summarise(., N = n())
#NatSoil.summary.site %>% filter(., N ==1) %>% as.data.frame()
#NatSoil.summary.site %>% filter(., N > 1) %>% as.data.frame()

too.many <- unique(NatSoil.summary.site[NatSoil.summary.site$N > 1,]$myID)
kkeps <- unique(setdiff(NatSoil.summary.site$myID, too.many))
print(paste0("There are ",length(unique(data.frame.SOCfractions$myID)), " unique sites"))
print(paste0("There are ",length(too.many), " sites with more than one observation, which will be processed with splines"))
print(paste0("There are ",length(kkeps), " sites with only one observation, which will be processed with aqp"))
length(unique(data.frame.SOCfractions$myID))
length(too.many) + length(kkeps)
# as.data.frame(data.frame.SOCfractions[data.frame.SOCfractions$myID %in% kkeps,c ("UDepth", "LDepth")])

### Split into two dataframes
data.frame.SOCfractions.aqp <- data.frame.SOCfractions[data.frame.SOCfractions$myID %in% kkeps,]
data.frame.SOCfractions.spl <- data.frame.SOCfractions[data.frame.SOCfractions$myID %in% too.many,]
data.frame.SOCfractions.aqp <- as.data.frame(data.frame.SOCfractions.aqp)
data.frame.SOCfractions.aqp <- data.frame.SOCfractions.aqp[data.frame.SOCfractions.aqp$UDepth < 240,]
data.frame.SOCfractions.aqp.bck <- as.data.frame(data.frame.SOCfractions.aqp)
data.frame.SOCfractions.aqp <- as.data.frame(data.frame.SOCfractions.aqp)

library(aqp)
# upgrade to SoilProfileCollection
# 'myID' is the name of the column containing the profile ID
# 'UDepth' is the name of the column containing horizon upper boundaries
# 'LDepth' is the name of the column containing horizon lower boundaries
# depths(data.frame.SOCfractions.aqp) <- myID ~ UDepth + LDepth
# print(data.frame.SOCfractions.aqp)
# checkSPC(data.frame.SOCfractions.aqp)
# spc_in_sync(data.frame.SOCfractions.aqp)
# checkHzDepthLogic(data.frame.SOCfractions.aqp)

### change of support according to GSM depths
### https://ncss-tech.github.io/AQP/aqp/aqp-intro.html#14_Aggregating_Soil_Profile_Collections_Along_Regular_%E2%80%9CSlabs%E2%80%9D
gsm.depths <- c(0, 5, 15, 30)
desired.properties <- c("POC_p","ROC_p","HOC_p","Vp")
forms <- list(paste("myID ~ POC_p"), paste("myID ~ ROC_p"),paste("myID ~ HOC_p"),paste("myID ~ Vp")) 

### Remember columns in the original dataset
# [1] "agency_code"       "proj_code"         "s_id"              "spec_id"           "o_latitude_GDA94"  "o_longitude_GDA94" "samp_upper_depth" 
# [8] "samp_lower_depth"  "label"             "POC"               "HOC"               "ROC"               "SOC"               "Lat_WGS84"        
# [15] "Long_WGS84"        "UDepth"            "LDepth"            "coord_ID"          "Location_ID"       "HOC.st"            "POC.st"           
# [22] "ROC.st"            "SumFracOC.st"      "POC_p"             "ROC_p"             "HOC_p"             "Vp"                "myLayer"          
# [29] "myID"   

## Let's keep "myID" "Lat_WGS84" "Long_WGS84" "coord_ID" "Location_ID"  
# 
out.aqp <- list()
for(j in 1:length(desired.properties)) {

  ### Eliminate NA for each property
  nona.idx <- !is.na(data.frame.SOCfractions.aqp[,desired.properties[[j]]])
  data.frame.SOCfractions.aqp.j <- data.frame.SOCfractions.aqp[nona.idx,]
  data.frame.SOCfractions.aqp.j <- data.frame.SOCfractions.aqp.j[, c("myID","Lat_WGS84","Long_WGS84","coord_ID","Location_ID","UDepth","LDepth","POC_p","ROC_p","HOC_p","Vp")]
  data.frame.SOCfractions.aqp.j.bck <- data.frame.SOCfractions.aqp.j
  depths(data.frame.SOCfractions.aqp.j) <- myID ~ UDepth + LDepth

  # print(data.frame.SOCfractions.aqp.j)
  # checkSPC(data.frame.SOCfractions.aqp.j)
  # spc_in_sync(data.frame.SOCfractions.aqp.j)
  # checkHzDepthLogic(data.frame.SOCfractions.aqp.j)

  ### For one property
  NatSoil.gsm.j <- aqp::slab(data.frame.SOCfractions.aqp.j, fm= as.formula(forms[[j]]), slab.structure = gsm.depths, slab.fun = mean, na.rm=TRUE)
  print(head(NatSoil.gsm.j))
  #ids <- aqp::profile_id(data.frame.SOCfractions.aqp.j)

  NatSoil.gsm.j$value <- ifelse(is.nan(NatSoil.gsm.j$value), NA, NatSoil.gsm.j$value)
  
  # reshape to wide format
  NatSoil.gsm.j$GMS_layer <- paste0(NatSoil.gsm.j$top,"-",NatSoil.gsm.j$bottom, " cm")
  NatSoil.gsm.j.wide <- dcast(NatSoil.gsm.j, myID + variable ~ GMS_layer , value.var = 'value', fun.aggregate=mean)
  ## Add some columns
  NatSoil.gsm.j.wide <- merge(NatSoil.gsm.j.wide,
                                      data.frame.SOCfractions.aqp.bck[, c("myID","coord_ID","Lat_WGS84","Long_WGS84","Location_ID")],
                                      by="myID", all.x=TRUE)
  out.aqp[[j]] <- NatSoil.gsm.j.wide
}

NatSoil.aqp <- out.aqp
rm(nona.idx,data.frame.SOCfractions.aqp.j,NatSoil.gsm.j, NatSoil.gsm.j.wide, out.aqp)
detach("package:aqp", unload=TRUE)

### Now perform splines and keep GSM intervals (we should just keep between 0-30 cm)
colnames(data.frame.SOCfractions.spl)
colnames(data.frame.SOCfractions.aqp.j.bck)
data.frame.SOCfractions.spl <- arrange(data.frame.SOCfractions.spl, myID, UDepth, LDepth) %>% as.data.frame()

### Unique SiteID
sampling.profiles <- unique(data.frame.SOCfractions.spl$myID)
desired.properties <- c("POC_p","ROC_p","HOC_p","Vp")

out.splines <- list()

for(j in 1:length(desired.properties)) {
  df.j <- as.data.frame(data.frame.SOCfractions.spl[, c("myID","UDepth","LDepth",desired.properties[[j]])])
  ### Fit mass-preserving spline for j property
  eaFit <- ea_spline(obj=df.j, var.name = desired.properties[[j]], d = t(c(0,5,15,30)), lam = 0.1, vlow = 0, show.progress = TRUE)
  eaFit$obs.preds[,2] <- as.numeric(eaFit$obs.preds[,2])
  eaFit$obs.preds[,3] <- as.numeric(eaFit$obs.preds[,3])
  eaFit$obs.preds[,4] <- as.numeric(eaFit$obs.preds[,4])
  out.splines[[j]] <- eaFit
  
}

#df.SOCfr.POC <- out.splines[[1]]
#problematicIDs <- setdiff(sampling.profiles, df.SOCfr.POC$harmonised$id)
output <- list(NatSoil.aqp,out.splines)
#output <- list(out.splines,problematicIDs)
return(output)
}
#detach("package:aqp", unload=TRUE)
NatSoil_SOCfractions.GSM <- standarize.GSM(NatSoil_SOCfractions.3)

rm(data.frame.SOCfractions, NatSoil.summary.site, too.many, kkeps, 
   data.frame.SOCfractions.aqp,data.frame.SOCfractions.spl,data.frame.SOCfractions.aqp.bck,
   gsm.depths,desired.properties,forms, NatSoil.aqp,
   data.frame.SOCfractions.aqp.j.bck,out.splines,sampling.profiles,
   df.j, eaFit,output,j)
rm(NatSoil_SOCfractions.3.bck)

save.image("1_Data_SOCfractionProportions_01092021.RData")
load("1_Data_SOCfractionProportions_01092021.RData")

# 1.10 Splines on bm data -----------------------------------

ggmap(AusMap)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="blue",
             data = bm2_SOCfractions)

bm2_SOCfractions_bck <- bm2_SOCfractions
bm2_SOCfractions <- bm2_SOCfractions[, c("myID", "spec_id","coord_ID",
                                          "o_latitude_GDA94","o_longitude_GDA94",
                                          "samp_upper_depth","samp_lower_depth",
                                          "POC","HOC","ROC","SOC",
                                          "Lat_WGS84","Long_WGS84","UDepth","LDepth")]
bm2_SOCfractions <- bm2_SOCfractions[!duplicated(bm2_SOCfractions),]

ggmap(AusMap)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="azure3",
             data = SCaRP.Soil.3)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="deeppink3",
             data = aus_archive_SOCfractions)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="orange",
             data = NatSoil_SOCfractions.3)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="darkcyan",
             data = tern_SOCfractions.4)+
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84),color="blue",
             data = bm2_SOCfractions[bm2_SOCfractions$UDepth <=30,])

### Standardize the SOC fractions (mg C/ g soil) with TOC %
bm2_SOCfractions <- bm2_SOCfractions[!(is.na(bm2_SOCfractions$HOC)|is.na(bm2_SOCfractions$POC)|is.na(bm2_SOCfractions$ROC)),]

bm2_SOCfractions$HOC.st <- ifelse(bm2_SOCfractions$HOC <0, 0.01, bm2_SOCfractions$HOC)
bm2_SOCfractions$POC.st <- ifelse(bm2_SOCfractions$POC <=0, 0.01, bm2_SOCfractions$POC)
bm2_SOCfractions$ROC.st <- ifelse(bm2_SOCfractions$ROC <0, 0.01, bm2_SOCfractions$ROC)
bm2_SOCfractions$SumFracOC.st <- bm2_SOCfractions$HOC.st + bm2_SOCfractions$POC.st + bm2_SOCfractions$ROC.st
summary(bm2_SOCfractions[,c("HOC.st", "POC.st", "ROC.st", "SOC", "SumFracOC.st")])

### Calculate contribution and Vp
bm2_SOCfractions$POC_p <- (bm2_SOCfractions$POC.st / bm2_SOCfractions$SumFracOC.st) * 100
bm2_SOCfractions$ROC_p <- (bm2_SOCfractions$ROC.st / bm2_SOCfractions$SumFracOC.st) * 100
bm2_SOCfractions$HOC_p <- (bm2_SOCfractions$HOC.st / bm2_SOCfractions$SumFracOC.st) * 100
bm2_SOCfractions$Vp <- bm2_SOCfractions$POC.st / (bm2_SOCfractions$HOC.st + bm2_SOCfractions$ROC.st)
summary(bm2_SOCfractions[,c("HOC_p", "POC_p", "ROC_p", "Vp")])
plot(x=bm2_SOCfractions$POC.st, y=(bm2_SOCfractions$HOC.st + bm2_SOCfractions$ROC.st))

### Unique SiteID
sampling.profiles <- unique(bm2_SOCfractions$myID)
bm2_SOCfractions[bm2_SOCfractions$myID ==328,]
detach("package:aqp", unload=TRUE)

bm2_SOCfractions <- bm2_SOCfractions[bm2_SOCfractions$UDepth <= 50,]

### First, average when there are more than one observation per horizon
bm2.summary.layers <- bm2_SOCfractions %>% 
  group_by(., myID, UDepth, LDepth) %>%
  summarise(., N = n())

bm2.summary.layers %>% filter(., N >1) %>% as.data.frame()

too.many <- unique(bm2.summary.layers[bm2.summary.layers$N > 1,]$myID)
kkeps <- unique(setdiff(bm2.summary.layers$myID, too.many))
length(unique(bm2_SOCfractions$myID))
length(too.many) + length(kkeps)

### Create a "myLayer" variable
bm2_SOCfractions <- bm2_SOCfractions %>% group_by(.,myID) %>% arrange(., myID, UDepth, LDepth ) %>%
  mutate(., myLayer =  row_number())
bm2_SOCfractions2  <- as.data.frame(bm2_SOCfractions)

### Split into two dataframes
bm2_SOCfractions2.Single <- bm2_SOCfractions2[bm2_SOCfractions2$myID %in% kkeps,]
bm2_SOCfractions2.Multiple <- bm2_SOCfractions2[bm2_SOCfractions2$myID %in% too.many,]
bm2_SOCfractions2.Multiple <- arrange(bm2_SOCfractions2.Multiple, myID, UDepth, LDepth )
bm2_SOCfractions2.Multiple <- as.data.frame(bm2_SOCfractions2.Multiple)

### Average by combination Coord_ID_UDepth_LDepth
bm2_SOCfractions2.Multiple.Ave <- bm2_SOCfractions2.Multiple[0,]
col.order <- colnames(bm2_SOCfractions2.Multiple)

for (i in 1:length(too.many)){
  print(i)
  df.i <- bm2_SOCfractions2.Multiple[bm2_SOCfractions2.Multiple$myID == too.many[[i]],]
  ### Summarize numeric columns
  df.numeric <- df.i %>% group_by(., myID, UDepth, LDepth) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
  df.numeric <- arrange(df.numeric, myID, UDepth, LDepth)
  df.numeric <- as.data.frame(df.numeric)
  ## Here the spec_id is meaningless
  df.numeric$spec_id <- NA
  
  ### Recalculate myLayer
  df.numeric$myLayer <- 1:nrow(df.numeric)
  
  ### Character variables and keep only the first rows
  df.char <- df.i %>% select_if(is.character)
  df.char <- df.char[1:nrow(df.numeric),] %>% as.data.frame()
  
  ### join both dataframes
  df.o <- cbind(df.numeric,df.char)
  
  ### Reorder columns
  df.o <- df.o[,col.order]
  ### Add to empty table
  bm2_SOCfractions2.Multiple.Ave <- rbind(bm2_SOCfractions2.Multiple.Ave, df.o )
}
rm(col.order,i,df.i,df.numeric,df.char,df.o,kkeps,too.many)

### Merge together
bm2_SOCfractions.3 <- rbind(bm2_SOCfractions2.Single, bm2_SOCfractions2.Multiple.Ave)
rm(bm2_SOCfractions2.Multiple, bm2_SOCfractions2.Multiple.Ave, bm2_SOCfractions2.Single, bm2.summary.layers)

### Now, separate those that have one observations per location from those with several per location
bm2.summary.site <- bm2_SOCfractions.3 %>% 
  group_by(., myID) %>%
  summarise(., N = n())
bm2.summary.site %>% filter(., N >1) %>% as.data.frame()

too.many <- unique(bm2.summary.site[bm2.summary.site$N > 1,]$myID)
kkeps <- unique(setdiff(bm2.summary.site$myID, too.many))
length(unique(bm2_SOCfractions.3$myID))
length(too.many) + length(kkeps)

### Split into two dataframes
bm2_SOCfractions.3.aqp <- bm2_SOCfractions.3[bm2_SOCfractions.3$myID %in% kkeps,]
bm2_SOCfractions.3.spl <- bm2_SOCfractions.3[bm2_SOCfractions.3$myID %in% too.many,]
bm2_SOCfractions.3.aqp.bck <- bm2_SOCfractions.3.aqp
library(aqp)
summary(bm2_SOCfractions.3.aqp$LDepth)

# upgrade to SoilProfileCollection
# 'myID' is the name of the column containing the profile ID
# 'UDepth' is the name of the column containing horizon upper boundaries
# 'LDepth' is the name of the column containing horizon lower boundaries
#depths(bm2_SOCfractions.3.aqp) <- myID ~ UDepth + LDepth
# print(bm2_SOCfractions.3.aqp)
# checkSPC(bm2_SOCfractions.3.aqp)
# spc_in_sync(bm2_SOCfractions.3.aqp)
# checkHzDepthLogic(bm2_SOCfractions.3.aqp)

### change of support according to GSM depths
### https://ncss-tech.github.io/AQP/aqp/aqp-intro.html#14_Aggregating_Soil_Profile_Collections_Along_Regular_%E2%80%9CSlabs%E2%80%9D
gsm.depths <- c(0, 5, 15, 30)
desired.properties <- c("POC_p","ROC_p","HOC_p","Vp")
forms <- list(paste("myID ~ POC_p"), paste("myID ~ ROC_p"),paste("myID ~ HOC_p"),paste("myID ~ Vp")) 

out.aqp <- list()
for(j in 1:length(desired.properties)) {
  
  ### Eliminate NA for each property
  nona.idx <- !is.na(bm2_SOCfractions.3.aqp[,desired.properties[[j]]])
  bm2_SOCfractions.3.aqp.j <- bm2_SOCfractions.3.aqp[nona.idx,]
  bm2_SOCfractions.3.aqp.j <- bm2_SOCfractions.3.aqp.j[, c("myID","Lat_WGS84","Long_WGS84","coord_ID","UDepth","LDepth","POC_p","ROC_p","HOC_p","Vp")]
  bm2_SOCfractions.3.aqp.j.bck <- bm2_SOCfractions.3.aqp.j
  depths(bm2_SOCfractions.3.aqp.j) <- myID ~ UDepth + LDepth
  
  ### For one property
  bm2.gsm.j <- aqp::slab(bm2_SOCfractions.3.aqp.j, fm= as.formula(forms[[j]]), slab.structure = gsm.depths, slab.fun = mean, na.rm=TRUE)
  print(head(bm2.gsm.j))
  bm2.gsm.j$value <- ifelse(is.nan(bm2.gsm.j$value), NA, bm2.gsm.j$value)
  
  # reshape to wide format
  bm2.gsm.j$GMS_layer <- paste0(bm2.gsm.j$top,"-",bm2.gsm.j$bottom, " cm")
  bm2.gsm.j.wide <- dcast(bm2.gsm.j, myID + variable ~ GMS_layer , value.var = 'value', fun.aggregate=mean)
  ## Add some columns
  bm2.gsm.j.wide <- merge(bm2.gsm.j.wide,
                          bm2_SOCfractions.3.aqp.j.bck[, c("myID","coord_ID","Lat_WGS84","Long_WGS84")],
                          by="myID", all.x=TRUE)
  print(head(bm2.gsm.j.wide))
  out.aqp[[j]] <- bm2.gsm.j.wide
  
}

bm2.aqp <- out.aqp  
rm(nona.idx,out.aqp,bm2.gsm.j.wide,bm2.gsm.j,bm2_SOCfractions.3.aqp.j.bck,bm2_SOCfractions.3.aqp.j,forms)
### Those with more than one horizon per location --> splines

desired.properties <- c("POC_p","ROC_p","HOC_p","Vp")
out.splines <- list()

for(j in 1:length(desired.properties)) {
  df.j <- as.data.frame(bm2_SOCfractions.3.spl[, c("myID","UDepth","LDepth",desired.properties[[j]])])
  ### Fit mass-preserving spline for j property
  eaFit <- ea_spline(obj=df.j, var.name = desired.properties[[j]], d = t(c(0,5,15,30)), lam = 0.1, vlow = 0, show.progress = TRUE)
  eaFit$obs.preds[,2] <- as.numeric(eaFit$obs.preds[,2])
  eaFit$obs.preds[,3] <- as.numeric(eaFit$obs.preds[,3])
  eaFit$obs.preds[,4] <- as.numeric(eaFit$obs.preds[,4])
  out.splines[[j]] <- eaFit
}

bm2.POC_p <- out.splines[[1]]
bm2.ROC_p <- out.splines[[2]]
bm2.HOC_p <- out.splines[[3]]
bm2.Vp <- out.splines[[4]]

rm(out.splines, desired.properties,eaFit, df.j,j, kkeps, too.many, bm2.summary.site)
rm(bm2_SOCfractions.3.aqp, bm2_SOCfractions.3.aqp.bck, bm2_SOCfractions.3.spl, bm2_SOCfractions2)

save.image("1_Data_SOCfractionProportions_03092021.RData")

# 1.11 Merge the coordinates by myID ----------------------------

load("1_Data_SOCfractionProportions_03092021.RData")
detach("package:aqp", unload=TRUE)
## "bm2.aqp", "bm2.HOC","bm2.POC","bm2.ROC",bm2_SOCfractions.3                           
### Add the columns it needs in the aqp dataframes
bm2.POC_p.GSM <- dplyr::left_join(bm2.POC_p$harmonised, bm2_SOCfractions.3[, c("myID","Lat_WGS84","Long_WGS84")],by = c("id" = "myID"))
bm2.HOC_p.GSM <- dplyr::left_join(bm2.HOC_p$harmonised, bm2_SOCfractions.3[, c("myID","Lat_WGS84","Long_WGS84")],by = c("id" = "myID"))
bm2.ROC_p.GSM <- dplyr::left_join(bm2.ROC_p$harmonised, bm2_SOCfractions.3[, c("myID","Lat_WGS84","Long_WGS84")],by = c("id" = "myID"))
bm2.Vp.GSM <- dplyr::left_join(bm2.Vp$harmonised, bm2_SOCfractions.3[, c("myID","Lat_WGS84","Long_WGS84")],by = c("id" = "myID"))
bm2.POC_p.GSM<- bm2.POC_p.GSM[!duplicated(bm2.POC_p.GSM),]
bm2.HOC_p.GSM<- bm2.HOC_p.GSM[!duplicated(bm2.HOC_p.GSM),]
bm2.ROC_p.GSM<- bm2.ROC_p.GSM[!duplicated(bm2.ROC_p.GSM),]
bm2.Vp.GSM<- bm2.Vp.GSM[!duplicated(bm2.ROC_p.GSM),]

### Now add the data from the aqp processing
bm2.POC_p.aqp <- bm2.aqp[[1]]
bm2.ROC_p.aqp <- bm2.aqp[[2]]
bm2.HOC_p.aqp <- bm2.aqp[[3]]
bm2.Vp.aqp <- bm2.aqp[[4]]

names(bm2.POC_p.aqp)[names(bm2.POC_p.aqp) == "myID"] <- "id"
names(bm2.ROC_p.aqp)[names(bm2.ROC_p.aqp) == "myID"] <- "id"
names(bm2.HOC_p.aqp)[names(bm2.HOC_p.aqp) == "myID"] <- "id"
names(bm2.Vp.aqp)[names(bm2.Vp.aqp) == "myID"] <- "id"

### Create new column
bm2.POC_p.aqp$`soil depth` <- NA
bm2.ROC_p.aqp$`soil depth` <- NA
bm2.HOC_p.aqp$`soil depth` <- NA
bm2.Vp.aqp$`soil depth` <- NA

bm2.POC_p.GSM <- rbind(bm2.POC_p.GSM,bm2.POC_p.aqp[,colnames(bm2.POC_p.GSM)])
bm2.HOC_p.GSM <- rbind(bm2.HOC_p.GSM,bm2.HOC_p.aqp[,colnames(bm2.HOC_p.GSM)])
bm2.ROC_p.GSM <- rbind(bm2.ROC_p.GSM,bm2.ROC_p.aqp[,colnames(bm2.ROC_p.GSM)])
bm2.Vp.GSM    <- rbind(bm2.Vp.GSM,bm2.Vp.aqp[,colnames(bm2.Vp.GSM)])

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84, color=`5-15 cm`),
             data = bm2.HOC_p.GSM) +
  scale_color_viridis(discrete = FALSE, option="D", direction = -1)
rm(bm2.POC_p.aqp, bm2.ROC_p.aqp, bm2.HOC_p.aqp,bm2.Vp.aqp,
   bm2.POC_p, bm2.HOC_p,bm2.ROC_p, bm2.Vp, bm2.aqp, bm2_SOCfractions)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

### aus_archive
aus_archive.aqp.GSM <- aus_archive_SOCfractions.GSM[[1]]
aus_archive.spl.GSM <- aus_archive_SOCfractions.GSM[[2]]
aus_archive.POC_p <- aus_archive.spl.GSM[[1]]
aus_archive.ROC_p <- aus_archive.spl.GSM[[2]]
aus_archive.HOC_p <- aus_archive.spl.GSM[[3]]
aus_archive.Vp <- aus_archive.spl.GSM[[4]]

### Aus-archive
## aus_archive.aqp, "aus_archive.HOC", "aus_archive.POC", "aus_archive.ROC" , aus_archive_SOCfractions  ## Keep
aus_archive.POC_p.GSM <- dplyr::left_join(aus_archive.POC_p$harmonised, aus_archive_SOCfractions.6[, c("myID","Lat_WGS84","Long_WGS84","Location_ID" )], 
                                        by = c("id" = "myID"))
aus_archive.HOC_p.GSM <- dplyr::left_join(aus_archive.HOC_p$harmonised, aus_archive_SOCfractions.6[, c("myID","Lat_WGS84","Long_WGS84","Location_ID" )], 
                                        by = c("id" = "myID"))
aus_archive.ROC_p.GSM <- dplyr::left_join(aus_archive.ROC_p$harmonised, aus_archive_SOCfractions.6[, c("myID","Lat_WGS84","Long_WGS84","Location_ID" )], 
                                        by = c("id" = "myID"))
aus_archive.Vp.GSM    <- dplyr::left_join(aus_archive.Vp$harmonised, aus_archive_SOCfractions.6[, c("myID","Lat_WGS84","Long_WGS84","Location_ID" )], 
                                          by = c("id" = "myID"))
aus_archive.POC_p.GSM<- aus_archive.POC_p.GSM[!duplicated(aus_archive.POC_p.GSM),]
aus_archive.HOC_p.GSM<- aus_archive.HOC_p.GSM[!duplicated(aus_archive.HOC_p.GSM),]
aus_archive.ROC_p.GSM<- aus_archive.ROC_p.GSM[!duplicated(aus_archive.ROC_p.GSM),]
aus_archive.Vp.GSM<- aus_archive.Vp.GSM[!duplicated(aus_archive.Vp.GSM),]

### Now add the data from the aqp processing
aus_archive.POC_p.aqp <- aus_archive.aqp.GSM[[1]]
aus_archive.ROC_p.aqp <- aus_archive.aqp.GSM[[2]]
aus_archive.HOC_p.aqp <- aus_archive.aqp.GSM[[3]]
aus_archive.Vp.aqp <- aus_archive.aqp.GSM[[4]]

names(aus_archive.POC_p.aqp)[names(aus_archive.POC_p.aqp) == "myID"] <- "id"
names(aus_archive.ROC_p.aqp)[names(aus_archive.ROC_p.aqp) == "myID"] <- "id"
names(aus_archive.HOC_p.aqp)[names(aus_archive.HOC_p.aqp) == "myID"] <- "id"
names(aus_archive.Vp.aqp)[names(aus_archive.Vp.aqp) == "myID"] <- "id"

### Create new column
aus_archive.POC_p.aqp$`soil depth` <- NA
aus_archive.ROC_p.aqp$`soil depth` <- NA
aus_archive.HOC_p.aqp$`soil depth` <- NA
aus_archive.Vp.aqp$`soil depth` <- NA

aus_archive.POC_p.GSM <- rbind(aus_archive.POC_p.GSM,aus_archive.POC_p.aqp[,colnames(aus_archive.POC_p.GSM)])
aus_archive.ROC_p.GSM <- rbind(aus_archive.ROC_p.GSM,aus_archive.ROC_p.aqp[,colnames(aus_archive.ROC_p.GSM)])
aus_archive.HOC_p.GSM <- rbind(aus_archive.HOC_p.GSM,aus_archive.HOC_p.aqp[,colnames(aus_archive.HOC_p.GSM)])
aus_archive.Vp.GSM    <- rbind(aus_archive.Vp.GSM,   aus_archive.Vp.aqp[,colnames(aus_archive.Vp.GSM)])

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84, color=`5-15 cm`),
             data = aus_archive.HOC_p.GSM) +
  scale_color_viridis(discrete = FALSE, option="D", direction = -1)

rm(aus_archive.POC_p.aqp, aus_archive.ROC_p.aqp, aus_archive.HOC_p.aqp, 
   aus_archive.POC_p, aus_archive.HOC_p,aus_archive.ROC_p, aus_archive.aqp,
   aus_archive.Vp.aqp,aus_archive.Vp,
   aus_archive.aqp.GSM,aus_archive.spl.GSM)

### TERN 
# "tern.aqp" "TERN.HOC_p" "TERN.POC_p" "TERN.ROC_p" "TERN.Vp" "tern_SOCfractions" "tern_SOCfractions.4"

tern.POC_p.GSM <- dplyr::left_join(TERN.POC_p$harmonised, tern_SOCfractions.4[, c("myID","Lat_WGS84","Long_WGS84")],
                                 by = c("id" = "myID"))
tern.HOC_p.GSM <- dplyr::left_join(TERN.HOC_p$harmonised, tern_SOCfractions.4[, c("myID","Lat_WGS84","Long_WGS84")], 
                                 by = c("id" = "myID"))
tern.ROC_p.GSM <- dplyr::left_join(TERN.ROC_p$harmonised, tern_SOCfractions.4[, c("myID","Lat_WGS84","Long_WGS84")], 
                                 by = c("id" = "myID"))
tern.Vp.GSM <- dplyr::left_join(TERN.Vp$harmonised, tern_SOCfractions.4[, c("myID","Lat_WGS84","Long_WGS84")], 
                                   by = c("id" = "myID"))

tern.POC_p.GSM<- tern.POC_p.GSM[!duplicated(tern.POC_p.GSM),]
tern.HOC_p.GSM<- tern.HOC_p.GSM[!duplicated(tern.HOC_p.GSM),]
tern.ROC_p.GSM<- tern.ROC_p.GSM[!duplicated(tern.ROC_p.GSM),]
tern.Vp.GSM<- tern.Vp.GSM[!duplicated(tern.Vp.GSM),]

### Now add the data from the aqp processing
tern.POC_p.aqp <- tern.aqp[[1]]
tern.ROC_p.aqp <- tern.aqp[[2]]
tern.HOC_p.aqp <- tern.aqp[[3]]
tern.Vp.aqp <- tern.aqp[[4]]

names(tern.POC_p.aqp)[names(tern.POC_p.aqp) == "myID"] <- "id"
names(tern.ROC_p.aqp)[names(tern.ROC_p.aqp) == "myID"] <- "id"
names(tern.HOC_p.aqp)[names(tern.HOC_p.aqp) == "myID"] <- "id"
names(tern.Vp.aqp)[names(tern.Vp.aqp) == "myID"] <- "id"

### Create new column
tern.POC_p.aqp$`soil depth` <- NA
tern.ROC_p.aqp$`soil depth` <- NA
tern.HOC_p.aqp$`soil depth` <- NA
tern.Vp.aqp$`soil depth` <- NA

tern.POC_p.GSM <- rbind(tern.POC_p.GSM,tern.POC_p.aqp[,colnames(tern.POC_p.GSM)])
tern.HOC_p.GSM <- rbind(tern.HOC_p.GSM,tern.HOC_p.aqp[,colnames(tern.HOC_p.GSM)])
tern.ROC_p.GSM <- rbind(tern.ROC_p.GSM,tern.ROC_p.aqp[,colnames(tern.ROC_p.GSM)])
tern.Vp.GSM <- rbind(tern.Vp.GSM,tern.Vp.aqp[,colnames(tern.Vp.GSM)])

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84, color=`15-30 cm`),
             data = tern.HOC_p.GSM) +
  scale_color_viridis(discrete = FALSE, option="D", direction = -1)
ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84,color=`0-5 cm`),
             data = tern.POC_p.GSM) +
  scale_color_viridis(discrete = FALSE, option="D", direction = -1)

rm(tern.POC_p.aqp, tern.ROC_p.aqp, tern.HOC_p.aqp,tern.Vp.aqp, tern.POC_p, tern.HOC_p,tern.ROC_p,tern.Vp, tern.aqp)
rm(bm2_SOCfractions,NatSoil_SOCfractions,TERN.HOC_p, TERN.ROC_p, TERN.POC_p, TERN.Vp)

### HERE
save.image("1_Data_SOCfractionProportions_03092021.RData")
load("1_Data_SOCfractionProportions_03092021.RData")

### SCaRP 
#"SCaRP.HOC_p"  "SCaRP.POC_p"  "SCaRP.ROC_p" "SCaRP.Soil"  "SCaRP.Soil.3"                      
SCaRP.POC_p.GSM <- dplyr::left_join(SCaRP.POC_p$harmonised, SCaRP.Soil.3[, c("myID","Lat_WGS84","Long_WGS84","SCaRP_SiteID")],
                                  by = c("id" = "myID"))
SCaRP.HOC_p.GSM <- dplyr::left_join(SCaRP.HOC_p$harmonised, SCaRP.Soil.3[, c("myID","Lat_WGS84","Long_WGS84","SCaRP_SiteID")],
                                  by = c("id" = "myID"))
SCaRP.ROC_p.GSM <- dplyr::left_join(SCaRP.ROC_p$harmonised, SCaRP.Soil.3[, c("myID","Lat_WGS84","Long_WGS84","SCaRP_SiteID")],
                                  by = c("id" = "myID"))
SCaRP.Vp.GSM <- dplyr::left_join(SCaRP.Vp$harmonised, SCaRP.Soil.3[, c("myID","Lat_WGS84","Long_WGS84","SCaRP_SiteID")],
                                    by = c("id" = "myID"))

SCaRP.POC_p.GSM <- SCaRP.POC_p.GSM[!duplicated(SCaRP.POC_p.GSM),]
SCaRP.HOC_p.GSM <- SCaRP.HOC_p.GSM[!duplicated(SCaRP.HOC_p.GSM),]
SCaRP.ROC_p.GSM <- SCaRP.ROC_p.GSM[!duplicated(SCaRP.ROC_p.GSM),]
SCaRP.Vp.GSM <- SCaRP.Vp.GSM[!duplicated(SCaRP.Vp.GSM),]

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84,color=`0-5 cm`),
             data = SCaRP.POC_p.GSM) +
  scale_color_viridis(discrete = FALSE, option="D", direction = -1)

rm(SCaRP.POC_p, SCaRP.ROC_p, SCaRP.HOC_p, SCaRP.Vp)
save.image("1_Data_SOCfractionProportions_03092021.RData")

### NatSoil
# "NatSoil_SOCfractions.3" 
# "NatSoil_SOCfractions.GSM"
# "NatSoil_SOCfractions_bck"
NatSoil.aqp.GSM <- NatSoil_SOCfractions.GSM[[1]]
NatSoil.spl.GSM <- NatSoil_SOCfractions.GSM[[2]]
NatSoil.POC_p <- NatSoil.spl.GSM[[1]]
NatSoil.ROC_p <- NatSoil.spl.GSM[[2]]
NatSoil.HOC_p <- NatSoil.spl.GSM[[3]]
NatSoil.Vp <- NatSoil.spl.GSM[[4]]

NatSoil.POC_p.GSM <- dplyr::left_join(NatSoil.POC_p$harmonised, NatSoil_SOCfractions.3[, c("myID","Lat_WGS84","Long_WGS84","Location_ID" )], by = c("id" = "myID"))
NatSoil.HOC_p.GSM <- dplyr::left_join(NatSoil.HOC_p$harmonised, NatSoil_SOCfractions.3[, c("myID","Lat_WGS84","Long_WGS84","Location_ID" )], by = c("id" = "myID"))
NatSoil.ROC_p.GSM <- dplyr::left_join(NatSoil.ROC_p$harmonised, NatSoil_SOCfractions.3[, c("myID","Lat_WGS84","Long_WGS84","Location_ID" )], by = c("id" = "myID"))
NatSoil.Vp.GSM <- dplyr::left_join(NatSoil.Vp$harmonised, NatSoil_SOCfractions.3[, c("myID","Lat_WGS84","Long_WGS84","Location_ID" )], by = c("id" = "myID"))

### Now add the data from the aqp processing
NatSoil.POC_p.aqp <- NatSoil.aqp.GSM[[1]]
NatSoil.ROC_p.aqp <- NatSoil.aqp.GSM[[2]]
NatSoil.HOC_p.aqp <- NatSoil.aqp.GSM[[3]]
NatSoil.Vp.aqp <- NatSoil.aqp.GSM[[4]]

names(NatSoil.POC_p.aqp)[names(NatSoil.POC_p.aqp) == "myID"] <- "id"
names(NatSoil.ROC_p.aqp)[names(NatSoil.ROC_p.aqp) == "myID"] <- "id"
names(NatSoil.HOC_p.aqp)[names(NatSoil.HOC_p.aqp) == "myID"] <- "id"
names(NatSoil.Vp.aqp)[names(NatSoil.Vp.aqp) == "myID"] <- "id"

### Create new column
NatSoil.POC_p.aqp$`soil depth` <- NA
NatSoil.ROC_p.aqp$`soil depth` <- NA
NatSoil.HOC_p.aqp$`soil depth` <- NA
NatSoil.Vp.aqp$`soil depth` <- NA
colnames(NatSoil.POC_p.GSM)
colnames(NatSoil.POC_p.aqp)

NatSoil.POC_p.GSM <- rbind(NatSoil.POC_p.GSM,NatSoil.POC_p.aqp[,colnames(NatSoil.POC_p.GSM)])
NatSoil.HOC_p.GSM <- rbind(NatSoil.HOC_p.GSM,NatSoil.HOC_p.aqp[,colnames(NatSoil.HOC_p.GSM)])
NatSoil.ROC_p.GSM <- rbind(NatSoil.ROC_p.GSM,NatSoil.ROC_p.aqp[,colnames(NatSoil.ROC_p.GSM)])
NatSoil.Vp.GSM <- rbind(NatSoil.Vp.GSM,NatSoil.Vp.aqp[,colnames(NatSoil.Vp.GSM)])

NatSoil.POC_p.GSM<- NatSoil.POC_p.GSM[!duplicated(NatSoil.POC_p.GSM),]
NatSoil.HOC_p.GSM<- NatSoil.HOC_p.GSM[!duplicated(NatSoil.HOC_p.GSM),]
NatSoil.ROC_p.GSM<- NatSoil.ROC_p.GSM[!duplicated(NatSoil.ROC_p.GSM),]
NatSoil.Vp.GSM<- NatSoil.Vp.GSM[!duplicated(NatSoil.Vp.GSM),]

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84, color=`5-15 cm`),
             data = NatSoil.HOC_p.GSM) +
  scale_color_viridis(discrete = FALSE, option="D", direction = -1)

rm(NatSoil.POC_p.aqp, NatSoil.ROC_p.aqp, NatSoil.HOC_p.aqp,NatSoil.Vp.aqp,
   NatSoil.POC_p, NatSoil.HOC_p,NatSoil.ROC_p, NatSoil.Vp,
   NatSoil.aqp.GSM, NatSoil.spl.GSM)
save.image("1_Data_SOCfractionProportions_03092021.RData")

# 1.16 Merge all together ---------------

## Indicate dataset
SCaRP.POC_p.GSM$Dataset <- "SCaRP"
SCaRP.HOC_p.GSM$Dataset <- "SCaRP"
SCaRP.ROC_p.GSM$Dataset <- "SCaRP"
SCaRP.Vp.GSM$Dataset <- "SCaRP"
tern.POC_p.GSM$Dataset <- "TERN"
tern.HOC_p.GSM$Dataset <- "TERN"
tern.ROC_p.GSM$Dataset <- "TERN"
tern.Vp.GSM$Dataset <- "TERN"
aus_archive.POC_p.GSM$Dataset <- "Aus_archive"
aus_archive.HOC_p.GSM$Dataset <- "Aus_archive"
aus_archive.ROC_p.GSM$Dataset <- "Aus_archive"
aus_archive.Vp.GSM$Dataset <- "Aus_archive"
NatSoil.POC_p.GSM$Dataset <- "NatSoil"
NatSoil.HOC_p.GSM$Dataset <- "NatSoil"
NatSoil.ROC_p.GSM$Dataset <- "NatSoil"
NatSoil.Vp.GSM$Dataset <- "NatSoil"
bm2.POC_p.GSM$Dataset <- "NatSoil"
bm2.HOC_p.GSM$Dataset <- "NatSoil"
bm2.ROC_p.GSM$Dataset <- "NatSoil"
bm2.Vp.GSM$Dataset <- "NatSoil"

names(SCaRP.POC_p.GSM)[names(SCaRP.POC_p.GSM) == "SCaRP_SiteID"] <- "Location_ID"
names(SCaRP.HOC_p.GSM)[names(SCaRP.HOC_p.GSM) == "SCaRP_SiteID"] <- "Location_ID"
names(SCaRP.ROC_p.GSM)[names(SCaRP.ROC_p.GSM) == "SCaRP_SiteID"] <- "Location_ID"
names(SCaRP.Vp.GSM)[names(SCaRP.Vp.GSM) == "SCaRP_SiteID"] <- "Location_ID"
tern.POC_p.GSM$Location_ID <- NA
tern.HOC_p.GSM$Location_ID <- NA
tern.ROC_p.GSM$Location_ID <- NA
tern.Vp.GSM$Location_ID <- NA
bm2.POC_p.GSM$Location_ID <- NA
bm2.HOC_p.GSM$Location_ID <- NA
bm2.ROC_p.GSM$Location_ID <- NA
bm2.Vp.GSM$Location_ID <- NA

sel.cols <- c("Dataset","0-5 cm","5-15 cm","15-30 cm","Lat_WGS84","Long_WGS84","Location_ID")

### Together
POC_p.GSM <- rbind(SCaRP.POC_p.GSM[,sel.cols], tern.POC_p.GSM[,sel.cols], aus_archive.POC_p.GSM[,sel.cols], NatSoil.POC_p.GSM[,sel.cols], bm2.POC_p.GSM[,sel.cols])
POC_p.GSM$SOCFr <- "POC_p"
ROC_p.GSM <- rbind(SCaRP.ROC_p.GSM[,sel.cols], tern.ROC_p.GSM[,sel.cols], aus_archive.ROC_p.GSM[,sel.cols], NatSoil.ROC_p.GSM[,sel.cols], bm2.ROC_p.GSM[,sel.cols])
ROC_p.GSM$SOCFr <- "ROC_p"
HOC_p.GSM <- rbind(SCaRP.HOC_p.GSM[,sel.cols], tern.HOC_p.GSM[,sel.cols], aus_archive.HOC_p.GSM[,sel.cols], NatSoil.HOC_p.GSM[,sel.cols], bm2.HOC_p.GSM[,sel.cols])
HOC_p.GSM$SOCFr <- "HOC_p"
Vp.GSM <- rbind(SCaRP.Vp.GSM[,sel.cols], tern.Vp.GSM[,sel.cols], aus_archive.Vp.GSM[,sel.cols], NatSoil.Vp.GSM[,sel.cols], bm2.Vp.GSM[,sel.cols])
Vp.GSM$SOCFr <- "Vp"

POC_p.GSM$`15-30 cm` <- ifelse(POC_p.GSM$`15-30 cm` == -9999, NA, POC_p.GSM$`15-30 cm`)
ROC_p.GSM$`15-30 cm` <- ifelse(ROC_p.GSM$`15-30 cm` == -9999, NA, ROC_p.GSM$`15-30 cm`)
HOC_p.GSM$`15-30 cm` <- ifelse(HOC_p.GSM$`15-30 cm` == -9999, NA, HOC_p.GSM$`15-30 cm`)
Vp.GSM$`15-30 cm` <- ifelse(Vp.GSM$`15-30 cm` == -9999, NA, Vp.GSM$`15-30 cm`)

POC_p.GSM$Spectra <- ifelse(POC_p.GSM$Dataset %in% c("SCaRP","NatSoil"),"MIR", "Vis-NIR")
ROC_p.GSM$Spectra <- ifelse(ROC_p.GSM$Dataset %in% c("SCaRP","NatSoil"),"MIR", "Vis-NIR")
HOC_p.GSM$Spectra <- ifelse(HOC_p.GSM$Dataset %in% c("SCaRP","NatSoil"),"MIR", "Vis-NIR")
Vp.GSM$Spectra <- ifelse(Vp.GSM$Dataset %in% c("SCaRP","NatSoil"),"MIR", "Vis-NIR")

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84, color=Spectra, alpha=1/20),
             data = POC_p.GSM) +
  scale_color_viridis(discrete = TRUE, option="D", direction = -1)

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84, color=Dataset),
             data = POC_p.GSM) +
  scale_color_viridis(discrete = TRUE, option="B", direction = -1)

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84, color=`5-15 cm`),
             data = POC_p.GSM) +
  scale_color_viridis(discrete = FALSE, option="D", direction = -1)

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84, color=`5-15 cm`),
             data = HOC_p.GSM) +
  scale_color_viridis(discrete = FALSE, option="D", direction = -1)

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x = Long_WGS84, color=`5-15 cm`),
             data = ROC_p.GSM) +
  scale_color_viridis(discrete = FALSE, option="D", direction = -1)

### Data before homogenization by GSM depth intervals
### aus_archive_SOCfractions
### NatSoil_SOCfractions.3
### bm2_SOCfractions.3
### SCaRP.Soil.3
### tern_SOCfractions
dim(HOC_p.GSM[HOC_p.GSM$Dataset!= "SCaRP",])

save.image("1_Data_SOCfractionProportions_03092021.RData")
save(HOC_p.GSM, POC_p.GSM, ROC_p.GSM, Vp.GSM, 
     file = "SOCfr_Prop.GSM_03092021.RData")

# 2. Summary table for paper -------------------------------------------------

### All datasets before the splines

### number of observations and sites per dataset

### SCaRP
# SCaRP.Soil.3 # Has the layers with several samples (composites) averaged
dim(SCaRP.Soil.3) ### 14426 observations
length(unique(SCaRP.Soil.3$myID)) ### 4498 sites
### In the original dataset
length(unique(SCaRP.Soil$SCaRP_SiteID)) ### 4498 sites
dim(SCaRP.Soil) ### 20261 observations

### TERN
dim(tern_SOCfractions.4) # [1] 15157    33
length(unique(tern_SOCfractions.4$myID)) # 5711
length(unique(tern_SOCfractions.4$coord_ID))

### Aus_Archive (aka AusSpecNIR)
dim(aus_archive_SOCfractions.6) # 7527   33
length(unique(aus_archive_SOCfractions.6$myID)) # 3578
AusSpecNIR_N = dim(tern_SOCfractions.4)[1] + dim(aus_archive_SOCfractions.6)[1]
AusSpecNIR_Sites = length(unique(tern_SOCfractions.4$myID)) + length(unique(aus_archive_SOCfractions.6$myID))

### NatSoil (aka AusSpecMIR)
dim(NatSoil_SOCfractions.3) # [1] 3976   29
length(unique(NatSoil_SOCfractions.3$myID)) # 719

### BM2 (aka AusSpecMIR2)
dim(bm2_SOCfractions.3) # [1] 337  24
length(unique(bm2_SOCfractions.3$myID)) # 300
AusSpecMIR_N = dim(NatSoil_SOCfractions.3)[1] + dim(bm2_SOCfractions.3)[1]
AusSpecMIR_Sites = length(unique(NatSoil_SOCfractions.3$myID)) + length(unique(bm2_SOCfractions.3$myID))

### Figure
library(wesanderson)
pal <- wes_palette("Zissou1", 5, type = "discrete")
#"#3B9AB2" "#78B7C5" "#EBCC2A" "#E1AF00" "#F21A00"

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84), color= "#F21A00", data = SCaRP.Soil.3)+
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84), color= "#3B9AB2", data = aus_archive_SOCfractions.6) + 
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84), color= "#78B7C5" , data = tern_SOCfractions.4) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84), color= "#EBCC2A" , data = NatSoil_SOCfractions.3)  +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84), color= "#E1AF00", data = bm2_SOCfractions.3)  +
  theme(legend.position = "none")

### end of the script