##################################################################################################################################
### Extract coarse fragment data for SOC stocks calculation
### from the SoilDataFederator
### Calibrate RF models for volumetric coarse fragments (6 classes)

### Date: 08/12/2021
### Author: Mercedes Roman Dobarco

### Final for the paper and SOC fraction stocks calculations

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
library(quantreg)
library(MASS)
library(mgcv)
library(qgam)
library(raster)
library(rasterVis)
library(viridis)
library(parallel)
library(foreach)
library(doParallel)
library(raster)
library(rgdal)
library(ranger)
#detach("package:aqp", unload=TRUE)

### Set input and output directories
HomeDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/for_Ross/"
InputDir  <- paste0(HomeDir,"Input_files/8_Input/")
OutDir  <- paste0(HomeDir,"Output_files/8_Output/")
scriptDir <- paste0(HomeDir,"scripts/") 

### 1. Extract soil data ------------------------------------------------
root<- InputDir
library(jsonlite)
### Here change for your user and key
usr<-'user@xxx'
key<-'xxxxxxxxx'

#methods <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties')
methods <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties')
# Check what datasets are available foar coarse fragments
avail.datasets<- fromJSON(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets?usr=",usr,"&key=",key))
avail.datasets$DataSet

### coarse fragments
ExtractData1 <- c()
avail.datasets$DataSet
for (m in 1:length(avail.datasets$DataSet)){
  ExtractData1[m]<- paste0(avail.datasets$DataSet[m], "_Ext1")}

lab.methods<- c("CF_ABUN",                                                                ## volumetric rock fragments
                #"CF_NO","CF_SIZE","CF_SHAPE","CF_LITH","CF_STRENGTH","CF_DISTRIBUTION", 
                "2Z2_Grav","P10_GRAV")                                                    ## Gravimetric % gravel

ds1 <- data.frame()
for (i in 1:length(avail.datasets$DataSet)){
  print(avail.datasets$DataSet[i])
  odf <- data.frame()
  for (j in 1:length(lab.methods)){
    nml<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=",lab.methods[j],"&DataSet=",avail.datasets$DataSet[i],"&usr=",usr,"&key=",key)
    ue <-  URLencode(nml, reserved = F)
    getdata<- as.data.frame(fromJSON(ue))
    #dim(getdata)
    if (nrow(getdata) >0){
      #print(colnames(getdata))
      print(unique(getdata$ObservedProperty))
      print(nrow(getdata))
      odf<- rbind(odf, getdata)
    }
  }
  # save output
  #write.csv(odf, file = paste0(root,'/', ExtractData1[i],"_", Sys.Date(),".csv"))
  if (nrow(odf) >0){
    ds1 <-  rbind(ds1, odf)
  }
  print(i)
}

rm(root,i,j,getdata,m,ue,nml,ExtractData1, odf)

##################################################################################################################################

### 2. Clean the data ---------------------------------------
dim(ds1) ### 461749       20

### Check for missing values, and transform from character to numeric
sort(unique(ds1$ObservedProperty))
# [1] "2Z2_Grav" "cf_abun"  "CF_ABUN"  "P10_GRAV"         
ds1[ds1$Value== "N/A",]
ds1[ds1$Value== "None",]
ds1[ds1$Value== "-9999",]
ds1[ds1$Value== "9999",]

ds1[grep(x = ds1$Value, pattern=">"),] ### Leave as is for the moment
ds1[grep(x = ds1$Value, pattern="<"),] ### Leave as is for the moment
ds1[grep(x = ds1$Value, pattern="-"),] ### Leave as is for the moment
ds1[grep(x = ds1$Value, pattern=" "),]

### change variable names to make them equal
ds1[ds1$ObservedProperty == "cf_abun",]$ObservedProperty <- "CF_ABUN" 
sort(unique(ds1$ObservedProperty))
#  "2Z2_Grav" "CF_ABUN"  "P10_GRAV"

### Eliminate duplicates in each dataframe, independent of the dataset
removeDuplicateRecords <- function(soilDF){
  noDups <- soilDF[!duplicated(soilDF[,1:15]),] ## I don't include the extraction time and last variables
  return(noDups)
}
ds1 <- removeDuplicateRecords(ds1)

### Explore number of observations per method and dataset
ds1 %>% group_by(., Dataset, ObservedProperty) %>%
  summarise(., count=n()) %>% 
  arrange(., ObservedProperty,desc(count)) %>%  as.data.frame()

### NatSoil, NTGovernment have abundance and gravel (%), QLDGovernment a few (382), 
### TasGovernment (261), VicGovernment (1459)
### --> We may be able to see relationships there to convert from volumetric to gravimetric or viceversa

### Separate site observations (lithology), from lab measurements and soil profile data
cf.grav <- ds1[ds1$ObservedProperty %in% c("2Z2_Grav","P10_GRAV"),]
cf.visual <- ds1[ds1$ObservedProperty %in% c("CF_ABUN"),]


##################################################################################################################################

# 2.1 Clean data of coarse fragments abundance (% Vol) ----------------------------------------------------

dim(cf.visual[cf.visual$ObservedProperty =="CF_ABUN",]) ### Volume of coarse fragments
### 398391 20 

### what are the unique values for CF abundance?
cf.visual[cf.visual$ObservedProperty == "CF_ABUN",]$Value <- as.character(cf.visual[cf.visual$ObservedProperty == "CF_ABUN",]$Value)
unique(cf.visual[cf.visual$ObservedProperty == "CF_ABUN",]$Value) 
# [1] "0-10%"                "42648"                "45-50"                "35-40"                "0"                    "5"                    "15-20"               
# [8] "25-30"                "10-15"                "5-10"                 "42278"                "20-25"                "60-65"                "40-45"               
# [15] "50-55"                "55-60"                "75-80"                "70-75"                "65-70"                "2"                    "1"                   
# [22] "3"                    "-"                    "6"                    "4"                    "No coarse fragments"  "Very few (<2%)"       "Many (20-50%)"       
# [29] "Few (2-10%)"          "Common (10-20%)"      "Very abundant (>90%)" "Abundant (50-90%)"    "few 2-10%"            "common 10-20%"        "many 20-50%"         
# [36] "abundant 50-90%"      "very few <2%"         "very abundant >90%"   "NR"                   ""   

### Separate dataframe for abundance
cf.vol <- cf.visual[cf.visual$ObservedProperty == "CF_ABUN",]

table(cf.vol$Value)
included <- c("0","No coarse fragments","-","NR","0-10%",
              "1","very few <2%","Very few (<2%)","2",
              "5-10","Few (2-10%)","few 2-10%",
              "3","10-15","15-20","Common (10-20%)","common 10-20%",
              "4","20-25","25-30","30-35","35-40","40-45","45-50","Many (20-50%)","many 20-50%",
              "5","50-55","55-60","60-65","65-70","70-75","75-80","80-85","85-90","Abundant (50-90%)","abundant 50-90%",
              "6","Very abundant (>90%)","very abundant >90%")

table(cf.vol[!(cf.vol$Value %in% included),]$Value) ### I exclude these observations
#   42278 42648 
# 2    17    43 

### What about "-" ???
AusMap <- get_stamenmap(bbox = c(left=110, bottom=-45, right=157, top=-8),
                        maptype="toner-lite", zoom=5,
                        source="stamen", crop=TRUE)

ggmap(AusMap) + geom_point(aes(y=Latitude, x=Longitude, color=Value, alpha=1/10),
                           data=cf.vol[cf.vol$Value == "-",]) +
  scale_color_viridis(discrete = TRUE, option="A",direction=-1)

locations <- cf.vol[cf.vol$Value == "-",]$Location_ID
table(cf.vol[cf.vol$Location_ID %in% locations ,]$Value) ### it includes "0"
summary(as.numeric(cf.vol[cf.vol$Value == "-",]$LowerDepth))

### I will use the seven categories from the Australian Soil and Land Survey Field Handbook page 139 to create a new variable
cf.vol$CF.vol.class <- NA
cf.vol$CF.vol.class <- ifelse(cf.vol$Value %in% c("0","No coarse fragments","-","NR", "0-10%", "1","very few <2%","Very few (<2%)"), yes = "1", # <2 %
                              no = ifelse(cf.vol$Value %in% c("2","5-10","Few (2-10%)","few 2-10%"), yes = "2", # 2-10 %
                                                      no = ifelse(cf.vol$Value %in% c("3","10-15","15-20","Common (10-20%)","common 10-20%"), yes = "3", # 10-20 %
                                                                  no = ifelse(cf.vol$Value %in% c("4","20-25","25-30","30-35","35-40","40-45","45-50","Many (20-50%)","many 20-50%"), yes = "4", # 20-50 %
                                                                              no = ifelse(cf.vol$Value %in% c("5","50-55","55-60","60-65","65-70","70-75","75-80","80-85","85-90","Abundant (50-90%)","abundant 50-90%"), yes = "5",
                                                                                          no = ifelse(cf.vol$Value %in% c("6","Very abundant (>90%)","very abundant >90%"), yes = "6",
                                                                                                      no = NA ))))))
### I exclude the values: 42278 42648
table(cf.vol$CF.vol.class)
#      1      2      3      4      5      6 
# 204455  79910  44453  44207  20688   4616 

dim(cf.vol[is.na(cf.vol$CF.vol.class),])
cf.vol <- cf.vol[!is.na(cf.vol$CF.vol.class),]

### Transform to factor
cf.vol$CF.vol.class <- as.factor(cf.vol$CF.vol.class)

### Order
cf.vol <- arrange(cf.vol, Location_ID, desc(UpperDepth), desc(LowerDepth))
cf.vol <- arrange(cf.vol, Location_ID, (UpperDepth), (LowerDepth))

ggmap(AusMap) + geom_point(aes(y=Latitude, x=Longitude, color=CF.vol.class, alpha=1/10),
                           data=cf.vol) +
  scale_color_viridis(discrete = TRUE, option="A",direction=-1)


ggmap(AusMap) + geom_point(aes(y=Latitude, x=Longitude, color=CF.vol.class, alpha=1/10),
                           data=cf.vol) +
  scale_color_manual(values=viridis_pal(option="A", direction = -1)(7), 
                    name="Coarse fragments\n class (% volume)",
                    breaks=c("1", "2", "3", "4", "5", "6"),
                    labels=c("Very few (<2 %)", "Few (2-10 %)",
                             "Common (10-20 %)", "Many (20-50 %)",
                             "Abundant (50-90 %)", "Very abundant (>90 %)"))

### Correct soil depth
## The depths range widely. I assume the unit is meter
rownames(cf.vol[cf.vol$UpperDepth== "N/A",])
cf.vol[cf.vol$UpperDepth== "None",]
cf.vol[cf.vol$UpperDepth== "-9999",]
cf.vol[cf.vol$UpperDepth== "9999",]
cf.vol[grep(x = cf.vol$UpperDepth, pattern=">"),] ### none
cf.vol[grep(x = cf.vol$UpperDepth, pattern="<"),] ### none
cf.vol[grep(x = cf.vol$UpperDepth, pattern="-"),] ### Eliminate...
cf.vol[grep(x = cf.vol$UpperDepth, pattern=" "),] ### none

### Transform Upper and Lower depth from character to numeric
#na.values <- c("N/A","None")
# transform.na <- function(soilDF, soil.prop, y){
#   soilDF[[soil.prop]] <- dplyr::na_if(soilDF[[soil.prop]], y)
#   #soilDF[[soil.prop]] <- as.numeric(soilDF[[soil.prop]])
#   return(soilDF)
# }
# library(dplyr)
# cf.vol <- transform.na(cf.vol, soil.prop = "UpperDepth", y = "N/A")
# cf.vol <- transform.na(cf.vol, soil.prop = "UpperDepth", y = "None")
# cf.vol <- transform.na(cf.vol, soil.prop = "UpperDepth", y = "9999")
# cf.vol <- transform.na(cf.vol, soil.prop = "UpperDepth", y = "-")

## The function used to work, not it does not
cf.vol[grep(x = cf.vol$UpperDepth, pattern="-"),]$UpperDepth <- NA
cf.vol[["UpperDepth"]] <- as.numeric(cf.vol[["UpperDepth"]])
cf.vol <- cf.vol[!is.na(cf.vol$UpperDepth),]

cf.vol[["LowerDepth"]] <- as.numeric(cf.vol[["LowerDepth"]])
cf.vol <-cf.vol[!is.na(cf.vol$LowerDepth),]

### Explore number of observations per class
cf.vol %>% group_by(., CF.vol.class) %>%
  summarise(., count=n()) %>% 
  arrange(., desc(count)) %>%  as.data.frame()
summary(cf.vol)

cf.vol$Units <- "None"

### Interested in the topsoils, mainly
summary(cf.vol$UpperDepth)
summary(cf.vol$LowerDepth)
cf.vol <- cf.vol[cf.vol$UpperDepth <=2,]
cf.vol <- cf.vol[cf.vol$LowerDepth <=3,]
cf.vol <- cf.vol[cf.vol$UpperDepth >=0,]
cf.vol <- cf.vol[cf.vol$LowerDepth >=0,]

### Transform to cm
cf.vol$LowerDepth <- cf.vol$LowerDepth *100
cf.vol$UpperDepth <- cf.vol$UpperDepth *100
summary(cf.vol$LowerDepth)
summary(cf.vol$UpperDepth)

AusMap <- get_stamenmap(bbox = c(left=110, bottom=-45, right=157, top=-8),
                        maptype="toner-lite", zoom=5,
                        source="stamen", crop=TRUE)
### First meter
ggmap(AusMap) + geom_point(aes(y=Latitude, x=Longitude, color=CF.vol.class, alpha=1/10),
                           data=cf.vol[cf.vol$LowerDepth <=100 ,]) +
  #scale_color_viridis(discrete = TRUE, option="A",direction=-1)
  scale_color_manual(values=viridis_pal(option="A", direction = -1)(7), 
                     name="Coarse fragments\n class (% volume)",
                     breaks=c("1", "2", "3", "4", "5", "6"),
                     labels=c("Very few (<2 %)", "Few (2-10 %)",
                              "Common (10-20 %)", "Many (20-50 %)",
                              "Abundant (50-90 %)", "Very abundant (>90 %)"))

### Top 30 cm
ggmap(AusMap) + geom_point(aes(y=Latitude, x=Longitude, color=CF.vol.class, alpha=1/10),
                           data=cf.vol[cf.vol$UpperDepth <= 30 ,]) +
  scale_color_manual(values=viridis_pal(option="A", direction = -1)(7), 
                   name="Coarse fragments\n class (% volume)",
                   breaks=c( "1", "2", "3", "4", "5", "6"),
                   labels=c( "Very few (<2 %)", "Few (2-10 %)",
                            "Common (10-20 %)", "Many (20-50 %)",
                            "Abundant (50-90 %)", "Very abundant (>90 %)"))

### Gravel (%) grav??
ggmap(AusMap) + geom_point(aes(y=Latitude, x=Longitude, color=as.numeric(Value)*0.5, alpha=1/10),
                           data=cf.grav[as.numeric(cf.grav$LowerDepth) <= 0.6 ,]) +
  scale_color_viridis(discrete = TRUE, option="A",direction=-1)

### save just in case
setwd(OutDir)
#save.image("cf.data.13052022.RData")

##################################################################################################################################

### 3. Convert into GSM depths ------------------------------------------------

### NOTE: aqp::slab does not handle well when the upper and lower depth are the same, it returns NA. 
### So I add 1cm in these horizons. (better spatial coverage in Victoria)

### Add a cm after for lower depth
cf.vol$LowerDepth <- ifelse(test = cf.vol$LowerDepth==cf.vol$UpperDepth,
                            yes = cf.vol$UpperDepth+1, 
                            no = cf.vol$LowerDepth)

detach("package:aqp", unload=TRUE)
## how many samples need to be averaged by layer?
### I subset only the layers that ovserlap
cf.vol.0_30  <- cf.vol[cf.vol$UpperDepth <= 30,]

cf.vol.hor <- cf.vol.0_30 %>% 
  group_by(., Location_ID, UpperDepth, LowerDepth) %>%
  summarise(., N = n())

cf.vol.hor %>% filter(., N >1) %>% dim()

### There are several layers with two observations, 
### and these are categorical... should I split them into a different profiles?
### Same coordinates, different location ID?
### I just keep the first one
cf.vol.0_30[cf.vol.0_30$Location_ID == "000-212_9999_174980",]
 cf.vol.0_30[cf.vol.0_30$Location_ID == "000-SE055_0000_152332",]
# test <- cf.vol[cf.vol$Location_ID == "000-SE055_0000_152332" & cf.vol$UpperDepth <= 30, c(4,10,11,14,21)]

### Check who I use as ID
cf.vol.0_30$ID1 <- paste0(cf.vol.0_30$Latitude,"_",cf.vol.0_30$Longitude)
length(unique(cf.vol.0_30$ID1))
length(unique(cf.vol.0_30$Location_ID)) ### More unique id's here

### Order and copy
cf.vol.0_30 <- arrange(cf.vol.0_30, Location_ID, UpperDepth, LowerDepth)

### Create a "myID" variable
cf.vol.0_30 <- cf.vol.0_30 %>% arrange(., Location_ID, UpperDepth, LowerDepth ) %>% group_by(.,Location_ID,ID1) %>% 
  mutate(., myID =  cur_group_id())

cf.vol.0_30 <- arrange(cf.vol.0_30, myID, UpperDepth, LowerDepth)
cf.vol.0_30 <- as.data.frame(cf.vol.0_30)

##Make a copy
cf.vol.0_30.df <- cf.vol.0_30

library(aqp)
#depths(test) <- myID ~ UpperDepth + LowerDepth

### For one property
gsm.depths <- c(0, 5, 15, 30)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#test.out2 <- aqp::slab(test, fm= Location_ID ~ CF.vol.class, slab.structure = gsm.depths, slab.fun = Mode, na.rm=TRUE)
#test.out  <- aqp::slab(test, fm= Location_ID ~ CF.vol.class, slab.structure = gsm.depths, cpm=1, na.rm=TRUE)
#colnames(test.out2[,3:9])[apply(test.out2[,3:9],1,which.max)]

### Apply to the dataset 0-30 cm
depths(cf.vol.0_30.df) <- myID ~ UpperDepth + LowerDepth
print(cf.vol.0_30.df)
checkSPC(cf.vol.0_30.df)
spc_in_sync(cf.vol.0_30.df)
checkHzDepthLogic(cf.vol.0_30.df)
cf.vol.0_30.gsm <- aqp::slab(cf.vol.0_30.df, fm= myID ~ CF.vol.class, slab.structure = gsm.depths, slab.fun = Mode, na.rm=TRUE)

### Add the coordinates
detach("package:aqp", unload=TRUE)
setdiff(colnames(cf.vol.0_30),colnames(cf.vol.0_30.gsm))
cf.vol.0_30.coords <- cf.vol.0_30[!duplicated(cf.vol.0_30[ ,c("DataStore","Dataset","Provider","myID","Location_ID","SampleDate","Longitude","Latitude","PropertyType","ObservedProperty")]),]
cf.vol.0_30.coords <- cf.vol.0_30[ ,c("DataStore","Dataset","Provider","myID","Location_ID","SampleDate","Longitude","Latitude","PropertyType","ObservedProperty")]
cf.vol.0_30.coords <- cf.vol.0_30.coords[!duplicated(cf.vol.0_30.coords),]

### Eliminate the rows with no values
cf.vol.0_30.gsm <- cf.vol.0_30.gsm[cf.vol.0_30.gsm$contributing_fraction !=0,]

### for each prediction, keep only the dominant level
cf.vol.0_30.gsm$CF.vol.class <- colnames(cf.vol.0_30.gsm[,3:8])[apply(cf.vol.0_30.gsm[,3:8],1,which.max)]
cf.vol.0_30.gsm$CF.vol.class

# Reshape to wide format
cf.vol.0_30.gsm$GMS_layer <- paste0(cf.vol.0_30.gsm$top,"-",cf.vol.0_30.gsm$bottom, " cm")
#library(reshape2)
#cf.vol.0_30.gsm.wide <- reshape2::dcast(cf.vol.0_30.gsm, myID + variable ~ GMS_layer , value.var = 'CF.vol.class', fun.aggregate=Mode)
#head(cf.vol.0_30.gsm.wide)

## Add some columns
detach("package:aqp", unload=TRUE)
setdiff(colnames(cf.vol.0_30.coords), colnames(cf.vol.0_30.gsm))
setdiff(colnames(cf.vol.0_30.gsm),colnames(cf.vol.0_30.coords))
colnames(cf.vol.0_30.gsm)[colnames(cf.vol.0_30.gsm) %in% colnames(cf.vol.0_30.coords)]
colnames(cf.vol.0_30.coords)[colnames(cf.vol.0_30.coords) %in% colnames(cf.vol.0_30.gsm)]
cf.vol.0_30.coords$myID <- as.character(cf.vol.0_30.coords$myID)
cf.vol.0_30.gsm2 <- dplyr::left_join(cf.vol.0_30.gsm, cf.vol.0_30.coords)
cf.vol.0_30.gsm2 <- cf.vol.0_30.gsm2[!duplicated(cf.vol.0_30.gsm2),]
### eliminate duplicates based on cf.vol.0_30.gsm
cf.vol.0_30.gsm2 <- cf.vol.0_30.gsm2[!duplicated(cf.vol.0_30.gsm2[,colnames(cf.vol.0_30.gsm)]),]
cf.vol.0_30.gsm2 <- as.data.frame(cf.vol.0_30.gsm2)
cf.vol.0_30.gsm <- cf.vol.0_30.gsm2
rm(cf.vol.hor,cf.vol.0_30.gsm2, cf.vol.0_30.coords, cf.vol.0_30.df, included)
rm(odf,i,j,ds1,m,nml,ue,usr,locations,lab.methods)

cf.vol.0_30.gsm <- cf.vol.0_30.gsm[,c("myID","CF.vol.class","GMS_layer","DataStore","Dataset","Provider","Location_ID",
                                      "SampleDate","Longitude","Latitude","PropertyType","ObservedProperty")]
cf.vol.0_30.gsm <- cf.vol.0_30.gsm[!is.na(cf.vol.0_30.gsm$Longitude),]

### Eliminate outside Australia bbox 
inAustralia <- function(soilDF){
  if(!is.data.frame(soilDF)){return(NULL)}
  bboxExt <- extent(110,155,-45,-9)
  idxs <- which(soilDF$Longitude >= bboxExt@xmin & soilDF$Longitude <= bboxExt@xmax & soilDF$Latitude >= bboxExt@ymin & soilDF$Latitude <= bboxExt@ymax)
  outdf <- soilDF[idxs, ]
}
cf.vol.0_30.gsm <- inAustralia(cf.vol.0_30.gsm)

### plot by GSM depth
cf.vol.0_30.gsm$GMS_layer <- factor(cf.vol.0_30.gsm$GMS_layer, levels=c("0-5 cm","5-15 cm","15-30 cm"))
ggmap(AusMap) + geom_point(aes(y=Latitude, x=Longitude, color=CF.vol.class, alpha=1/10),
                           data=cf.vol.0_30.gsm) +
  scale_color_manual(values=viridis_pal(option="A", direction = -1)(7), 
                     name="Coarse fragments\n class (% volume)",
                     breaks=c( "X1", "X2", "X3", "X4", "X5", "X6"),
                     labels=c( "Very few (<2 %)", "Few (2-10 %)",
                              "Common (10-20 %)", "Many (20-50 %)",
                              "Abundant (50-90 %)", "Very abundant (>90 %)"))+
  facet_grid(~GMS_layer)


library(sp)
coordinates(cf.vol.0_30.gsm) <- ~Longitude + Latitude
### Give the CRS
crdref <- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4string(cf.vol.0_30.gsm) <- crdref ; rm(crdref)
plot(cf.vol.0_30.gsm)
setwd("C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/Output/8_Coarse_Fragments")
save(cf.vol.0_30.gsm, file = "cf.vol.0_30.gsm.RData")
cf.vol.0_30.gsm <- as.data.frame(cf.vol.0_30.gsm)

### As wide
cf.vol.0_30.gsm.wide <- tidyr::pivot_wider(cf.vol.0_30.gsm, 
                                           values_from = CF.vol.class, names_from=GMS_layer, names_prefix="CF.class.") %>% as.data.frame
colnames(cf.vol.0_30.gsm.wide) <- c("myID","DataStore","Dataset","Provider","Location_ID",
                                    "SampleDate","Longitude","Latitude","PropertyType","ObservedProperty",
                                    "CF.class.0_5","CF.class.5_15","CF.class.15_30")

coordinates(cf.vol.0_30.gsm.wide) <- ~Longitude + Latitude
### Give the CRS
crdref <- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4string(cf.vol.0_30.gsm.wide) <- crdref ; rm(crdref)
plot(cf.vol.0_30.gsm.wide)
save(cf.vol.0_30.gsm.wide, file = "cf.vol.0_30.gsm.wide2.RData")
rm(methods, getdata, ExtractData1, gsm.depths, key, root, Mode,avail.datasets)


setwd(OutDir)
#save.image("session_13052022.RData")

##################################################################################################################################

### 4.1 Gravimetric coarse fragment content ---------------------------------

### Process the gravimetric data
str(cf.grav)

### Eliminate duplicates
cf.grav <- cf.grav[!duplicated(cf.grav[,1:15]),]

### soil depth
cf.grav[cf.grav$UpperDepth== "None",]
cf.grav[cf.grav$UpperDepth== "-9999",]
cf.grav[cf.grav$UpperDepth== "9999",] ### eliminate
cf.grav <- cf.grav[cf.grav$UpperDepth!= "9999",]
cf.grav[grep(x = cf.grav$UpperDepth, pattern=">"),] ### none
cf.grav[grep(x = cf.grav$UpperDepth, pattern="<"),] ### none
cf.grav[grep(x = cf.grav$UpperDepth, pattern= "-"),] ### Eliminate...
cf.grav <- cf.grav[!cf.grav$UpperDepth %in% c(-0.4, -0.2),]
cf.grav[grep(x = cf.grav$UpperDepth, pattern=" "),] ### none

### Coarse fragments (% weight) values
cf.grav[cf.grav$Value == "None",]
cf.grav[cf.grav$Value== "-9999",]
cf.grav[cf.grav$Value== "9999",] 
cf.grav[grep(x = cf.grav$Value, pattern=">"),] ### Assign 1%
cf.grav[grep(x = cf.grav$Value, pattern=">"),]$Value <- 1
cf.grav[grep(x = cf.grav$Value, pattern="<"),] ### Assign 0% to those "<1%"
cf.grav[grep(x = cf.grav$Value, pattern="<"),]$Value <- 0
cf.grav[grep(x = cf.grav$Value, pattern= "-"),]

### For each sampling location, e.g., myID
cf.grav <- arrange(cf.grav, Location_ID, Layer_ID) %>% as.data.frame()

## how many observations need to be averaged by horizon?
cf.grav.summary.layers <- cf.grav %>% 
  group_by(., Location_ID, UpperDepth, LowerDepth ) %>%
  summarise(., N = n())

cf.grav.summary.layers %>% filter(., N >1)

cf.grav[cf.grav$Location_ID == "180_BASE_10614_1",]

too.many <- unique(cf.grav.summary.layers[cf.grav.summary.layers$N > 1,]$Location_ID)
kkeps <- unique(setdiff(cf.grav$Location_ID, too.many))
length(unique(cf.grav$Location_ID))
length(too.many) + length(kkeps)

### Create a "myLayer" variable
cf.grav <- arrange(cf.grav, Location_ID, UpperDepth, LowerDepth)
cf.grav <- cf.grav %>% group_by(.,Location_ID) %>% arrange(., Location_ID, UpperDepth, LowerDepth ) %>%
  mutate(., myLayer =  row_number())

### Split into two dataframes
cf.grav.Single <- cf.grav[cf.grav$Location_ID %in% kkeps,]
cf.grav.Multiple <- cf.grav[cf.grav$Location_ID %in% too.many,]
cf.grav.Multiple <- arrange(cf.grav.Multiple, Location_ID, UpperDepth, LowerDepth)
cf.grav.Multiple <- as.data.frame(cf.grav.Multiple)
cf.grav.Single <- as.data.frame(cf.grav.Single)

### Average by combination MyID_UDepth_LDepth
cf.grav.Multiple.Ave <- cf.grav.Multiple[0,]
col.order <- colnames(cf.grav.Multiple.Ave)

for (i in 1:length(too.many)){
  print(i)
  df.i <- cf.grav.Multiple[cf.grav.Multiple$Location_ID == too.many[[i]],]
  ### Summarize numeric columns
  df.numeric <- df.i %>% group_by(., Location_ID, UpperDepth, LowerDepth) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
  df.numeric <- arrange(df.numeric, Location_ID, UpperDepth, LowerDepth)
  df.numeric <- as.data.frame(df.numeric)
  
  ### Recalculate myLayer
  df.numeric$myLayer <- 1:nrow(df.numeric)
  
  ### Character variables and keep only the first rows
  df.char <- df.i %>% select_if(is.character)
  df.char <- df.char[1:nrow(df.numeric),]
  
  ### join both dataframes
  df.o <- cbind(df.numeric,df.char)
  ### Reorder columns
  df.o <- df.o[,col.order]
  ### Add to empty table
  cf.grav.Multiple.Ave <- rbind(cf.grav.Multiple.Ave, df.o )
}
rm(col.order,i,df.i,df.numeric,df.char,df.o,kkeps,too.many)

### Merge together
cf.grav.2 <- rbind(cf.grav.Single, cf.grav.Multiple.Ave)

## and also create a column "myID
cf.grav.2 <- cf.grav.2 %>% arrange(.,Location_ID,UpperDepth,LowerDepth) %>% group_by(.,Location_ID) %>% 
  mutate(., myID =  cur_group_id())
cf.grav.2$myID <- as.character(cf.grav.2$myID)
cf.grav.2 <- as.data.frame(cf.grav.2)
rm( cf.grav.Multiple, cf.grav.Multiple.Ave,cf.grav.Single, cf.grav.summary.layers)

### Transform depth into numeric
cf.grav.2[["UpperDepth"]] <- as.numeric(cf.grav.2[["UpperDepth"]])
cf.grav.2 <- cf.grav.2[!is.na(cf.grav.2$UpperDepth),]

cf.grav.2[["LowerDepth"]] <- as.numeric(cf.grav.2[["LowerDepth"]])
cf.grav.2 <-cf.grav.2[!is.na(cf.grav.2$LowerDepth),]

### Transform to cm
cf.grav.2$LowerDepth <- cf.grav.2$LowerDepth *100
cf.grav.2$UpperDepth <- cf.grav.2$UpperDepth *100

### Transform value into numeric
cf.grav.2[["Value"]] <- as.numeric(cf.grav.2[["Value"]])
cf.grav.2 <- cf.grav.2[!is.na(cf.grav.2$Value),]
cf.grav.2 <- cf.grav.2[cf.grav.2$Value <= 100,]

### use only observations from 1 m
cf.grav.2 <- cf.grav.2[cf.grav.2$UpperDepth < 100, ]
cf.grav.2 <- cf.grav.2[cf.grav.2$LowerDepth < 150, ]

### Now I apply splines and aqp for standarize depths to both datasets
# #dataframeCF <- aus_archive_SOCfractions.6
# 
# standarize.GSM <- function(dataframeCF){
#   if(!is.data.frame(dataframeCF)){
#     print("Error, input is not a dataframe")
#   }

df.CF.summary.site <- cf.grav.2 %>% 
  group_by(., myID) %>%
  summarise(., N = n())
df.CF.summary.site %>% filter(., N ==1) %>% as.data.frame()

too.many <- unique(df.CF.summary.site[df.CF.summary.site$N > 1,]$myID)
kkeps <- unique(setdiff(df.CF.summary.site$myID, too.many))
print(paste0("There are ",length(unique(cf.grav.2$myID)), " unique sites"))
print(paste0("There are ",length(too.many), " sites with more than one observation, which will be processed with splines"))
print(paste0("There are ",length(kkeps), " sites with only one observation, which will be processed with aqp"))
print(length(too.many) + length(kkeps))

dataframeCF <- cf.grav.2
hist(dataframeCF$Value)

### Split into two dataframes
dataframeCF.aqp <- dataframeCF[dataframeCF$myID %in% kkeps,]
dataframeCF.spl <- dataframeCF[dataframeCF$myID %in% too.many,]
dataframeCF.spl <- as.data.frame(dataframeCF.spl)
#str(dataframeCF.aqp)
#summary(dataframeCF.aqp$LDepth)
dataframeCF.aqp.bck <- dataframeCF.aqp
dataframeCF.aqp.bck <- as.data.frame(dataframeCF.aqp.bck)
dataframeCF.aqp <- as.data.frame(dataframeCF.aqp)

library(aqp)
# upgrade to SoilProfileCollection
# 'myID' is the name of the column containing the profile ID
# 'UDepth' is the name of the column containing horizon upper boundaries
# 'LDepth' is the name of the column containing horizon lower boundaries
# depths(dataframeCF.aqp) <- myID ~ UDepth + LDepth
# print(dataframeCF.aqp)
# checkSPC(dataframeCF.aqp)
# spc_in_sync(dataframeCF.aqp)
# checkHzDepthLogic(dataframeCF.aqp)

### change of support according to GSM depths
### https://ncss-tech.github.io/AQP/aqp/aqp-intro.html#14_Aggregating_Soil_Profile_Collections_Along_Regular_%E2%80%9CSlabs%E2%80%9D
gsm.depths <- c(0, 5, 15, 30)
depths(dataframeCF.aqp) <- myID ~ UpperDepth + LowerDepth

### For one property
df.CF.gsm <- aqp::slab(dataframeCF.aqp, fm= myID ~ Value, slab.structure = gsm.depths, slab.fun = mean, na.rm=TRUE)
print(head(df.CF.gsm))
df.CF.gsm$value <- ifelse(is.nan(df.CF.gsm$value), NA, df.CF.gsm$value)


# rm("df.CF.gsm.j",  "df.CF.gsm.j.rev","df.CF.gsm.j.rev.wide", "dataframeCF.aqp.j",
#    "dataframeCF.aqp.j.bck", df.i,i,j,nona.idx, max.contrib,N.profile, forms, ids, out.aqp, gsm.depths)
detach("package:aqp", unload=TRUE)

### Now perform splines and keep GSM intervals (we should just keep between 0-30 cm)
setwd(HomeDir)
source('./scripts/1_ea_spline.R')

colnames(dataframeCF.spl)
dataframeCF.spl <- arrange(dataframeCF.spl, myID, myLayer) %>% as.data.frame()
dataframeCF.spl <- dataframeCF.spl[!is.na(dataframeCF.spl$Value),]

### Unique SiteID
sampling.profiles <- unique(dataframeCF.spl$myID)
df.j <- as.data.frame(dataframeCF.spl[, c("myID","UpperDepth","LowerDepth","Value")])
eaFit <- ea_spline(obj=df.j, var.name = "Value", d = t(c(0,5,15,30)), lam = 0.1, vlow = 0, show.progress = TRUE)
eaFit$obs.preds[,2] <- as.numeric(eaFit$obs.preds[,2])
eaFit$obs.preds[,3] <- as.numeric(eaFit$obs.preds[,3])
eaFit$obs.preds[,4] <- as.numeric(eaFit$obs.preds[,4])

### Merge both datasets
cf.grav.3 <- dplyr::left_join(eaFit$harmonised, dataframeCF.spl[, c("myID","Latitude","Longitude","Location_ID" )], by = c("id" = "myID"))
cf.grav.3 <- cf.grav.3[!duplicated(cf.grav.3),]

### aqp to wide
# reshape to wide format
df.CF.gsm$GMS_layer <- paste0(df.CF.gsm$top,"-",df.CF.gsm$bottom, " cm")
library(reshape2)
df.CF.gsm.wide <- dcast(df.CF.gsm, myID + variable ~ GMS_layer , value.var = 'value', fun.aggregate=mean)
## Add some columns
tern.gsm.j.wide <- merge(df.CF.gsm.wide, 
                         dataframeCF.aqp.bck[, c("myID","Latitude","Longitude","Location_ID")],
                         by="myID", all.x=TRUE)

names(tern.gsm.j.wide)[names(tern.gsm.j.wide) == "myID"] <- "id"

## bind by rows
cf.grav.4 <- rbind(cf.grav.3[,-5],
                   tern.gsm.j.wide[,-2])

AusMap <- get_stamenmap(bbox = c(left=110, bottom=-45, right=157, top=-8),
                        maptype="toner-lite", zoom=5,
                        source="stamen", crop=TRUE)

ggmap(AusMap) + geom_point(aes(y=Latitude, x=Longitude, color=`0-5 cm`, alpha=1/10),
                           data=cf.grav.4) +
  scale_color_viridis(discrete = FALSE, option="A",direction=-1)

ggmap(AusMap) + geom_point(aes(y=Latitude, x=Longitude, color=`5-15 cm`, alpha=1/10),
                           data=cf.grav.4) +
  scale_color_viridis(discrete = FALSE, option="A",direction=-1) 

ggmap(AusMap) + geom_point(aes(y=Latitude, x=Longitude, color=`15-30 cm`, alpha=1/10),
                           data=cf.grav.4) +
  scale_color_viridis(discrete = FALSE, option="A",direction=-1) 


### In those with observations in 5-15 but missing in 0-5 cm I assing those from 5-15
# cf.grav.4$`0-5 cm` <- ifelse(test = (is.na(cf.grav.4$`0-5 cm`) & !is.na(cf.grav.4$`5-15 cm`)),
#                              yes = cf.grav.4$`5-15 cm`, 
#                              no = cf.grav.4$`0-5 cm`)

ls()
rm("plot_soilProfile","removeDuplicateRecords","sampling.profiles","tern.gsm.j.wide","too.many",
   "kkeps","plot_ea_spline","gsm.depths","df.CF.summary.site","df.j","ea_spline","eaFit",
   "cf.grav","cf.grav.2","cf.grav.3","cf.visual","cf.vol","dataframeCF","dataframeCF.aqp",
   "dataframeCF.aqp.bck","dataframeCF.spl","df.CF.gsm","df.CF.gsm.wide")

setwd(OutDir)
#save.image("session_13052022.RData")

########################################################################################################################

# 4.2 extract covariates --------------------------------------------------

### extract values from covariates
### Extract covariates values @ 90m
covariates.dir <- "R:/PRJ-SoilBiodSOC2019/Covariates/Australia90m_filled/"
setwd(covariates.dir)
# list all the raster files available 
list_ras <- list.files(covariates.dir, pattern="tif$")

# load all the rasters
covariates <- stack(paste0(covariates.dir, list_ras))
options(rasterMaxMemory = 6e10) 

# extract the raster stack values to the dataframe
# Coarse fragments

### Eliminate outside australia bbox 
inAustralia <- function(soilDF){
  if(!is.data.frame(soilDF)){return(NULL)}
  bboxExt <- extent(110,155,-45,-9)
  idxs <- which(soilDF$Longitude >= bboxExt@xmin & soilDF$Longitude <= bboxExt@xmax & soilDF$Latitude >= bboxExt@ymin & soilDF$Latitude <= bboxExt@ymax)
  outdf <- soilDF[idxs, ]
}
cf.grav.4 <- inAustralia(cf.grav.4)
cf.grav.4.sp <- cf.grav.4
cf.grav.4.sp <- cf.grav.4.sp[!is.na(cf.grav.4.sp$Latitude),]
colnames(cf.grav.4.sp)
coordinates(cf.grav.4.sp) <- ~ Longitude + Latitude
plot(cf.grav.4.sp)
### Give the CRS
crdref <- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4string(cf.grav.4.sp) <- crdref ; rm(crdref)

## save and extract in Artemis
save(cf.grav.4.sp, file = "cf.grav.4.sp.RData")
extr <- raster::extract(covariates, cf.grav.4.sp)
extr <- as.data.frame(extr)
cf.grav.4 <- cbind(cf.grav.4, extr)
cf.grav.4 <- cf.grav.4[!is.na(cf.grav.4$Clim_ADM),]
load("cf.grav_extr.RData")
rm(extr, list_ras)
#save.image("session_13052022.RData")

###########################################################################################################################################

### 4.3. Extract Bulk density data for gravimetric dataset -----------------------
load("session_13052022.RData")
### soildat (volumetric data) I can use directly

### cf.grav.4 extract BD and transform into gravimetric
str(cf.grav.4)
str(cf.grav.4.sp)
plot(cf.grav.4.sp)

### Give the CRS
crdref <- CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4string(cf.grav.4.sp) <- crdref ; rm(crdref)

## save and extract in Artemis
save(cf.grav.4.sp, file = "cf.grav.4.sp.RData")

### Extraction done in Artemis and back here - Script "extract_BD2.R"
load("extr_bd_cfgrav.RData")
extr_bd <- as.data.frame(extr_bd)
cf.grav.4 <- cbind(cf.grav.4,extr_bd)
rm(extr_bd)
#save.image("session_13052022.RData")

###########################################################################################################################

### 4.4 Transform from gravimetric to volumetric ------------------------------------------------

### Rename some variables
names(cf.grav.4)[names(cf.grav.4)=="id"] <- "myID"
names(cf.grav.4)[names(cf.grav.4)=="0-5 cm"] <- "cf_grav.0_5"
names(cf.grav.4)[names(cf.grav.4)=="5-15 cm"] <- "cf_grav.5_15"
names(cf.grav.4)[names(cf.grav.4)=="15-30 cm"] <- "cf_grav.15_30"

### in a for loop for the three depths
#covar.depth.specific.list <- list(c("clay_0_5", "sand_0_5"),c("clay_5_15", "sand_5_15"), c("clay_15_30", "sand_15_30") )
#bd.depth.specifics <- c("BDW_000_005_EV", "BDW_005_015_EV", "BDW_015_030_EV")
# depths <- c("0_5", "5_15", "15_30")
# cf.vol.depth <- c("CF.class.0_5","CF.class.5_15","CF.class.15_30")
# df.grav.depth <- c("cf_grav.0_5","cf_grav.5_15","cf_grav.15_30")
### Keep coordinates and Location_ID too!!!

### function to transform into volumetric
### simple equation
func.trans.grav.vol <- function(x,bd){
  CFvol <- ifelse(test= (is.na(bd)|is.na(x)), yes=NA, no= x*(bd/2.65))
  return(CFvol)
}

### Create new variables for each depth
cf.grav.4$cf_vol.0_5 <- func.trans.grav.vol(x = cf.grav.4$cf_grav.0_5, bd = cf.grav.4$BDW_000_005_EV)
cf.grav.4$cf_vol.5_15 <- func.trans.grav.vol(x = cf.grav.4$cf_grav.5_15, bd = cf.grav.4$BDW_005_015_EV)
cf.grav.4$cf_vol.15_30 <- func.trans.grav.vol(x = cf.grav.4$cf_grav.15_30, bd = cf.grav.4$BDW_015_030_EV)

### Assign to volumetric classes
assignCFclass <- function(x) { 
  cfClass <- ifelse(x < 2, yes = "X1", # <2 %
                                no = ifelse((x>=2 & x<10), yes = "X2", # 2-10 %
                                            no = ifelse((x>=10 & x<20), yes = "X3", # 10-20 %
                                                        no = ifelse((x>=20 & x<50), yes = "X4", # 20-50 %
                                                                    no = ifelse((x>=50 & x<90), yes = "X5", # 50-90 %
                                                                                no = ifelse((x>=90 & x<=100), yes = "X6", # >90%
                                                                                            no = NA ))))))
  return(cfClass)
}

cf.grav.4$CF.class.0_5 <- assignCFclass(cf.grav.4$cf_vol.0_5)
cf.grav.4$CF.class.5_15 <- assignCFclass(cf.grav.4$cf_vol.5_15)
cf.grav.4$CF.class.15_30 <- assignCFclass(cf.grav.4$cf_vol.15_30)

### Join with original volumetric data
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
#save.image("session_13052022.RData")


### 5. DSM model for categories of coarse fragments ------------------------------------------------

### Extract covariates values @ 90m
covariates.dir <- "R:/PRJ-SoilBiodSOC2019/Covariates/Australia90m_filled/"
setwd(covariates.dir)
#list all the raster files available
list_ras <- list.files(covariates.dir, pattern="tif$")

# load all the rasters
covariates <- stack(paste0(covariates.dir, list_ras))
# extract the raster stack values to the dataframe
# Coarse fragments
plot(covariates[[40]])
#plot(cf.vol.0_30.gsm.wide, add=TRUE)
plot(cf.vol.0_30.gsm.wide)
#extr <- raster::extract(covariates, cf.vol.0_30.gsm.wide)
### note: it does not work in my computer, perhaps because I have the rasters in the RDS
### the extraction is done in Artemis
load("extr_cf.vol.gsm.wide.Rdata")
cf.vol.0_30.gsm.wide <- as.data.frame(cf.vol.0_30.gsm.wide)
extr <- as.data.frame(extr)

### Sometime in the past, some columns are duplicate here, but character/factor
soildat <- cbind(cf.vol.0_30.gsm.wide, extr)
colnames(soildat)
names(covariates)

##### How is the output of a probability RF model?
covar.names <- c("Clim_ADM", "Clim_EPA", "Clim_EPI", "Clim_EPX","Clim_Prescott", "Clim_PTA", "Clim_PTI", 
                 "Clim_PTS1", "Clim_PTS2", 
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

#save.image("session_23052022.RData")

##################################################################################################################################

# 5.1 Depth 0-5 cm  -------------------------------------------------------

### Functions to perform cross-validation
setwd(scriptDir)
source("./8_make_cv_RF_coarse_fragments.R")
setwd(OutDir)

### Subset data for this depth
covar.depth.specific <- c('clay_0_5', 'sand_0_5')
data.0_5 <- soildat[,c('CF.class.0_5',"Latitude","Longitude", covar.names, covar.depth.specific)]
data.0_5 <- data.0_5[complete.cases(data.0_5),]
covars <- c(covar.names, covar.depth.specific)

### Select from cf.grav.4
data.0_5.2 <- cf.grav.4[,c('CF.class.0_5',"Latitude","Longitude", covar.names, covar.depth.specific)]
data.0_5.2 <- data.0_5.2[complete.cases(data.0_5.2),]

### join
data.0_5 <- rbind(data.0_5,data.0_5.2);rm(data.0_5.2)

form <- as.formula(paste("CF.class.0_5", paste(covars, collapse=" + "), sep=" ~ "))
unique(data.0_5$CF.class.0_5)
data.0_5$CF.class.0_5 <- as.factor(data.0_5$CF.class.0_5)

### Plot
ggmap(AusMap) + geom_point(aes(y=Latitude, x=Longitude, color=CF.class.0_5, alpha=1/10),
                           data=data.0_5) +
  scale_color_manual(values=viridis_pal(option="A", direction = -1)(6), 
                     name="Coarse fragments\n class (% volume)",
                     breaks=c("X1", "X2", "X3", "X4", "X5", "X6"),
                     labels=c("Very few (<2 %)", "Few (2-10 %)",
                              "Common (10-20 %)", "Many (20-50 %)",
                              "Abundant (50-90 %)", "Very abundant (>90 %)"))

ggmap(AusMap) + geom_point(aes(y=Latitude, x=Longitude, color=CF.class.5_15, alpha=1/10),
                           data=data.5_15) +
  scale_color_manual(values=viridis_pal(option="A", direction = -1)(6), 
                     name="Coarse fragments\n class (% volume)",
                     breaks=c("X1", "X2", "X3", "X4", "X5", "X6"),
                     labels=c("Very few (<2 %)", "Few (2-10 %)",
                              "Common (10-20 %)", "Many (20-50 %)",
                              "Abundant (50-90 %)", "Very abundant (>90 %)"))

ggmap(AusMap) + geom_point(aes(y=Latitude, x=Longitude, color=CF.class.15_30, alpha=1/10),
                           data=data.15_30) +
  scale_color_manual(values=viridis_pal(option="A", direction = -1)(6), 
                     name="Coarse fragments\n class (% volume)",
                     breaks=c("X1", "X2", "X3", "X4", "X5", "X6"),
                     labels=c("Very few (<2 %)", "Few (2-10 %)",
                              "Common (10-20 %)", "Many (20-50 %)",
                              "Abundant (50-90 %)", "Very abundant (>90 %)"))

### Let's try in cross-validation
set.seed(2022)
cf_vol_cv_0_5 <- make_cv_RF_cf_prob(data = data.0_5,
                                    property = 'CF.class.0_5',
                                    covars = c(covar.names, covar.depth.specific),
                                    nfolds = 10)
cv.results <- cf_vol_cv_0_5[[2]]

### for each prediction, keep only the dominant level
colnames(cv.results) <- c("CF.class.0_5","X1","X2","X3","X4","X5","X6","pred") 
cv.results$pred <- colnames(cv.results[,2:7])[apply(cv.results[,2:7],1,which.max)]

### pkg/R/goofcat.R
# Purpose        : Goodness of fit statistics for categorical varibles
# Maintainer     : Brendan Malone (brendan.malone@sydney.edu.au); 
# Contributions  : 
# Status         : working
# Note           : 

goofcat<- function(observed = NULL, predicted = NULL, conf.mat, imp=FALSE){
  
  if (imp==TRUE){
    if(class(conf.mat)!="matrix"){
      stop("Entered data is NOT a matrix")}
    if(nrow(conf.mat)!= ncol(conf.mat)) {
      stop("Entered data is NOT a confusion matrix")}
    
    else {OA<- ceiling(sum(diag(conf.mat))/sum(colSums(conf.mat)) * 100)
    PA<- ceiling(diag(conf.mat)/colSums(conf.mat) * 100)
    UA<- ceiling(diag(conf.mat)/rowSums(conf.mat) * 100)
    
    PE_mat <- matrix(NA, ncol = 1, nrow = length(rowSums(conf.mat)))
    for (i in 1:length(rowSums(conf.mat))) {
      PE_mat[i, 1] <- (rowSums(conf.mat)[i]/sum(colSums(conf.mat))) * (colSums(conf.mat)[i]/sum(colSums(conf.mat)))}
    KS <- (sum(diag(conf.mat))/sum(colSums(conf.mat)) - sum(PE_mat))/(1 - sum(PE_mat))}}
  
  if (imp==FALSE) {obsMat<- table(observed,observed)
  df<- data.frame(observed, predicted)
  names(df)<- c("observed", "predicted")
  #make a confusion matrix
  cfuM<- function(df,obsMat){
    c.Mat<- as.matrix(obsMat)
    snames1<- c(colnames(c.Mat))
    for (i in 1:nrow(c.Mat)){
      for (j in 1:nrow(c.Mat)){
        c.Mat[j,i]<- nrow(subset(df, df$observed ==snames1[i]  & df$predicted ==snames1[j]))}}
    fmat<- matrix(NA, nrow=nrow(c.Mat), ncol=ncol(c.Mat))
    rownames(fmat)<- rownames(c.Mat)
    colnames(fmat)<- colnames(c.Mat)
    for (i in 1:nrow(c.Mat)){
      fmat[i,]<- c(c.Mat[i,])}
    return(fmat)}
  conf.mat<- cfuM(df, obsMat)
  
  OA<- ceiling(sum(diag(conf.mat))/sum(colSums(conf.mat)) * 100)
  PA<- ceiling(diag(conf.mat)/colSums(conf.mat) * 100)
  UA<- ceiling(diag(conf.mat)/rowSums(conf.mat) * 100)
  
  PE_mat <- matrix(NA, ncol = 1, nrow = length(rowSums(conf.mat)))
  for (i in 1:length(rowSums(conf.mat))) {
    PE_mat[i, 1] <- (rowSums(conf.mat)[i]/sum(colSums(conf.mat))) * (colSums(conf.mat)[i]/sum(colSums(conf.mat)))}
  KS <- (sum(diag(conf.mat))/sum(colSums(conf.mat)) - sum(PE_mat))/(1 - sum(PE_mat))}
  retval<- list(conf.mat,OA, PA, UA, KS)
  names(retval)<- c("confusion_matrix", "overall_accuracy", "producers_accuracy", "users_accuracy", "kappa")
  return(retval)}

cv.stats.0_5.prob <- goofcat(observed = cv.results$CF.class.0_5, predicted = cv.results$pred)
write.csv(cv.stats.0_5.prob$confusion_matrix, file = "cv.stats.0_5.prob.cm.csv")
table(cv.results$CF.class.0_5)

### Let's try classification model
set.seed(2022)
cf_vol_cv_class_0_5 <- make_cv_RF_cf_class(data = data.0_5,
                                           property = 'CF.class.0_5',
                                           covars = c(covar.names, covar.depth.specific),
                                           nfolds = 10)
cv.results_class <- cf_vol_cv_class_0_5[[2]]
head(cv.results_class)
cv.results_class$obs.char <- as.character(cv.results_class$CF.class.0_5)
cv.results_class$pred.cf.class <- ifelse(cv.results_class$pred==1, "X1", 
                                         ifelse(cv.results_class$pred==2, "X2",
                                                ifelse(cv.results_class$pred==3, "X3",
                                                       ifelse(cv.results_class$pred==4, "X4",
                                                              ifelse(cv.results_class$pred==5, "X5", 
                                                                     ifelse(cv.results_class$pred==6, "X6", NA))))))
#cv.results_class$pred.cf.class <- factor(cv.results_class$pred.cf.class)
cv.stats.0_5.class<-goofcat(observed = cv.results_class$obs.char , predicted = cv.results_class$pred.cf.class)
write.csv(cv.stats.0_5.class$confusion_matrix, file = "cv.stats.0_5.class.cm.csv")

### save
#save.image("cf.data.25052022.RData")

#### Map at 1km to have an idea?
####  Extract the model
cf_rf_class_0_5 <- cf_vol_cv_class_0_5[[1]]

# path to the covariates raster
setwd('R:/PRJ-SoilBiodSOC2019/Covariates/Australia1km_filled/')

# list all the raster files available 
list_ras <- list.files( pattern="tif$")

# load all the rasters
covariates.stack <- raster::stack(paste0('R:/PRJ-SoilBiodSOC2019/Covariates/Australia1km_filled/', list_ras))

# make blocks
bs <- blockSize(covariates.stack, minblocks=50)

dir.create(paste0(OutDir,"/Maps1km/"))
dir.create(paste0(OutDir,"/Maps1km/Vol_6classes/"))
outTiles <- paste0(OutDir,"/Maps1km/Vol_6classes/")
setwd(outTiles)

cl <- makeCluster(6)   
registerDoParallel(cl)
    
cf_rast_list <- foreach(i=1:bs$n, .packages=c("raster", "ranger"), .export = c("cf_rf_class_0_5")) %dopar% {
  
      ### Get one tile of the raster stack
      tile <- crop(covariates.stack, extent(covariates.stack, bs$row[[i]], bs$row[[i]]+bs$nrows[[i]], 1, ncol(covariates.stack)))
      ### Predict coarse fragments class
      #filename= paste0("cf_rf_class_0_5.tile",i,".tif")
      model=cf_rf_class_0_5
      pred <- raster::predict(object = tile, 
                              model = model, 
                              fun = function(model, ...) predict(model, ...)$predictions)
      pred # Return this
    }
    
stopCluster(cl)
    
## Assign function to mosaic
cf_rast_list$na.rm <- TRUE
cf_rast_list$fun <- min
## Create mosaic for whole NSW
k.raster <- do.call(mosaic, cf_rast_list)
names(k.raster) <- "cf_vol_class_0_5"
plot(k.raster)
    
## Write to file
writeRaster(k.raster, filename= "cf_vol_class_0_5.tif", na.rm=T,inf.rm=T, format="GTiff", overwrite=TRUE )
gc()
rm(cf_rast_list,i,list_ras,k.raster)
cf_vol_class_0_5 <- raster("cf_vol_class_0_5.tif")
cf_vol_class_0_5 <- ratify(cf_vol_class_0_5)
rat <- levels(cf_vol_class_0_5)[[1]]
rat$CF_class <- c("Very few (<2 %)", "Few (2-10 %)",
                  "Common (10-20 %)", "Many (20-50 %)",
                  "Abundant (50-90 %)")
levels(cf_vol_class_0_5) <- rat

myPal <- RColorBrewer::brewer.pal('YlOrBr', n=6)[1:5]

par(xpd=TRUE)
plot(cf_vol_class_0_5, col=myPal,legend=FALSE)
legend(145,-10,inset=c(10,-5), cex=0.9,
       legend=c("Very few (<2 %)", "Few (2-10 %)",
                "Common (10-20 %)", "Many (20-50 %)", "Abundant (50-90 %)"),
       fill=myPal, bg="white", bty="n")

##################################################################################################################################

# 5.1 Depth 0-5 cm Probability maps  -------------------------------------------------------

## Now, we predict the probabilities for each class
cf_rf_probs_0_5 <- cf_vol_cv_0_5[[1]]

cl <- makeCluster(5)   
registerDoParallel(cl)

cf_rast_list <- foreach(i=1:bs$n, .packages=c("raster", "ranger")) %dopar% {
  
  ### Get one tile of the raster stack
  tile <- crop(covariates.stack, extent(covariates.stack, bs$row[[i]], bs$row[[i]]+bs$nrows[[i]], 1, ncol(covariates.stack)))
  ### Predict coarse fragments class
  #filename= paste0("cf_rf_class_0_5.tile",i,".tif")
  model=cf_rf_probs_0_5
  pred <- raster::predict(object = tile, 
                          model = model, index=1:6,
                          fun = function(model, ...) predict(model, ...)$predictions)
  pred # Return this
}

stopCluster(cl)

## Assign function to mosaic
### Divide by layers
cf.classes.levels <- levels(data.0_5$CF.class.0_5)

#### Mosaic by cf class
for(var in 1:length(cf.classes.levels)){
  rast.list <- list()
  for(j in 1:bs$n){
    rast.list[[j]] <- cf_rast_list[[j]][[var]]
  }
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  pred <- do.call(mosaic, rast.list)
  names(pred) <- names(cf.classes.levels[[var]])
  print(pred)
  #print(paste0(OutDirTiles,names.write[[var]],i,".rds"))
  writeRaster(pred, filename= paste0(outTiles,"cf_vol_prob_",cf.classes.levels[[var]],".0_5.tif"), 
              na.rm=T,inf.rm=T, format="GTiff", overwrite=TRUE)
}
rm(cf_rast_list, var,j,cl, tile, rat, pred, rast.list, model)

rast.prob <- stack(list.files(pattern="prob"))
#rast.prob <- dropLayer(rast.prob,1)
levelplot(rast.prob+0.01,zscaleLog=TRUE)
levelplot(rast.prob)
### save
save.image("cf.data.25052022.RData")

##################################################################################################################################

# 5.2 Depth 5-15 cm  ------------------------------------

covar.depth.specific <- c('clay_5_15', 'sand_5_15')
data.5_15 <- soildat[,c('CF.class.5_15', "Latitude","Longitude", covar.names, covar.depth.specific)]
data.5_15 <- data.5_15[complete.cases(data.5_15),]
covars <- c(covar.names, covar.depth.specific)

### Select from cf.grav.4
data.5_15.2 <- cf.grav.4[,c('CF.class.5_15',"Latitude","Longitude", covar.names, covar.depth.specific)]
data.5_15.2 <- data.5_15.2[complete.cases(data.5_15.2),]

### join
data.5_15 <- rbind(data.5_15,data.5_15.2);rm(data.5_15.2)

form <- as.formula(paste("CF.class.5_15", paste(covars, collapse=" + "), sep=" ~ "))
unique(data.5_15$CF.class.5_15)
data.5_15$CF.class.5_15 <- as.factor(data.5_15$CF.class.5_15)

### Let's try in cross-validation
set.seed(2022)
cf_vol_cv_5_15 <- make_cv_RF_cf_prob(data = data.5_15,
                                    property = 'CF.class.5_15',
                                    covars = c(covar.names, covar.depth.specific),
                                    nfolds = 10)
cv.results <- cf_vol_cv_5_15[[2]]

### for each prediction, keep only the dominant level
colnames(cv.results) <- c("CF.class.5_15","X1","X2","X3","X4","X5","X6","pred") 
cv.results$pred <- colnames(cv.results[,2:7])[apply(cv.results[,2:7],1,which.max)]

cv.stats.5_15.prob <- goofcat(observed = cv.results$CF.class.5_15, predicted = cv.results$pred);cv.stats.5_15.prob
write.csv(cv.stats.5_15.prob$confusion_matrix, file = "cv.stats.5_15.prob.cm.csv")
table(cv.results$CF.class.5_15)

### Let's try a classification model
set.seed(2022)
cf_vol_cv_class_5_15 <- make_cv_RF_cf_class(data = data.5_15,
                                           property = 'CF.class.5_15',
                                           covars = c(covar.names, covar.depth.specific),
                                           nfolds = 10)
cv.results_class <- cf_vol_cv_class_5_15[[2]]
head(cv.results_class)
cv.results_class$obs.char <- as.character(cv.results_class$CF.class.5_15)
cv.results_class$pred.cf.class <- ifelse(cv.results_class$pred==1, "X1", 
                                         ifelse(cv.results_class$pred==2, "X2",
                                                ifelse(cv.results_class$pred==3, "X3",
                                                       ifelse(cv.results_class$pred==4, "X4",
                                                              ifelse(cv.results_class$pred==5, "X5", 
                                                                     ifelse(cv.results_class$pred==6, "X6",NA))))))
#cv.results_class$pred.cf.class <- factor(cv.results_class$pred.cf.class)
cv.stats.5_15.class<-goofcat(observed = cv.results_class$obs.char , predicted = cv.results_class$pred.cf.class)
cv.stats.5_15.class
write.csv(cv.stats.5_15.class$confusion_matrix, file = "cv.stats.5_15.class.cm.csv")

#### Map at 1km to have an idea?
####  Extract the model
cf_rf_class_5_15 <- cf_vol_cv_class_5_15[[1]]
# # path to the covariates raster
# setwd('R:/PRJ-SoilBiodSOC2019/Covariates/Australia1km_filled/')
# # list all the raster files available 
# list_ras <- list.files( pattern="tif$")
# # load all the rasters
# covariates.stack <- stack(paste0('R:/PRJ-SoilBiodSOC2019/Covariates/Australia1km_filled/', list_ras))

# make blocks
bs <- blockSize(covariates.stack, minblocks=50)
setwd(outTiles)
cl <- makeCluster(5)   
registerDoParallel(cl)
#model=cf_rf_class_5_15
cf_rast_list <- foreach(i=1:bs$n, .packages=c("raster", "ranger")) %dopar% {
  ### Get one tile of the raster stack
  tile <- crop(covariates.stack, extent(covariates.stack, bs$row[[i]], bs$row[[i]]+bs$nrows[[i]], 1, ncol(covariates.stack)))
  ### Predict coarse fragments class
  model=cf_rf_class_5_15
  pred <- raster::predict(object = tile, 
                          model = model, 
                          fun = function(model, ...) predict(model, ...)$predictions)
  pred # Return this
}
stopCluster(cl)

## Assign function to mosaic
cf_rast_list$na.rm <- TRUE
cf_rast_list$fun <- min
## Create mosaic for whole NSW
k.raster <- do.call(mosaic, cf_rast_list)
names(k.raster) <- "cf_vol_class_5_15"
plot(k.raster)

## Write to file
writeRaster(k.raster, filename= "cf_vol_class_5_15.tif", na.rm=T,inf.rm=T, format="GTiff", overwrite=TRUE )
gc()
rm(cf_rast_list,i,list_ras,k.raster)
cf_vol_class_5_15 <- raster("cf_vol_class_5_15.tif")
cf_vol_class_5_15 <- ratify(cf_vol_class_5_15)
rat <- levels(cf_vol_class_5_15)[[1]]
#rat <- rbind(rat,7)
rat$CF_class <- c("Very few (<2 %)", "Few (2-10 %)",
                  "Common (10-20 %)", "Many (20-50 %)",
                  "Abundant (50-90 %)")
levels(cf_vol_class_5_15) <- rat

myPal <- RColorBrewer::brewer.pal('YlOrBr', n=6)[1:5]

par(mfrow=c(1,1))
par(xpd=FALSE)
plot(cf_vol_class_5_15, col=myPal,legend=FALSE)
legend(145,-10,inset=c(10,-5), cex=0.9,
       legend=c("Very few (<2 %)", "Few (2-10 %)",
                "Common (10-20 %)", "Many (20-50 %)", "Abundant (50-90 %)"),
       fill=myPal, bg="white", bty="n")

##################################################################################################################################

# 5.2 Depth 5-15 cm Probability maps --------------------------------------

### Now, we predict the probabilities for each class
cf_rf_probs_5_15 <- cf_vol_cv_5_15[[1]]

cl <- makeCluster(5)   
registerDoParallel(cl)
cf_rast_list <- foreach(i=1:bs$n, .packages=c("raster", "ranger"), .export = "cf_rf_probs_5_15") %dopar% {
  ### Get one tile of the raster stack
  tile <- crop(covariates.stack, extent(covariates.stack, bs$row[[i]], bs$row[[i]]+bs$nrows[[i]], 1, ncol(covariates.stack)))
  ### Predict coarse fragments class
  model=cf_rf_probs_5_15
  pred <- raster::predict(object = tile, 
                          model = model, index=1:6,
                          fun = function(model, ...) predict(model, ...)$predictions)
  pred # Return this
}
stopCluster(cl)

## Assign function to mosaic
### Divide by layers
cf.classes.levels <- levels(data.5_15$CF.class.5_15)

#### Mosaic by cf class
for(var in 1:length(cf.classes.levels)){
  rast.list <- list()
  for(j in 1:bs$n){
    rast.list[[j]] <- cf_rast_list[[j]][[var]]
  }
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  pred <- do.call(mosaic, rast.list)
  names(pred) <- names(cf.classes.levels[[var]])
  print(pred)
  #print(paste0(OutDirTiles,names.write[[var]],i,".rds"))
  writeRaster(pred, filename= paste0(outTiles,"cf_vol_prob_",cf.classes.levels[[var]],".5_15.tif"), 
              na.rm=T,inf.rm=T, format="GTiff", overwrite=TRUE)
}
rm(cf_rast_list, var,j,cl, rat, pred, rast.list, model)

rast.prob <- stack(list.files(pattern="5_15")[2:8])
rast.prob<-dropLayer(rast.prob,1)
levelplot(rast.prob+0.01,zscaleLog=TRUE)
levelplot(rast.prob)

##################################################################################################################################

# 4.3 Depth 15-30 cm ----------------------------------------------------

covar.depth.specific <- c('clay_15_30', 'sand_15_30')
data.15_30 <- soildat[,c('CF.class.15_30',"Latitude","Longitude", covar.names, covar.depth.specific)]
data.15_30 <- data.15_30[complete.cases(data.15_30),]

### Select from cf.grav.4
data.15_30.2 <- cf.grav.4[,c('CF.class.15_30',"Latitude","Longitude", covar.names, covar.depth.specific)]
data.15_30.2 <- data.15_30.2[complete.cases(data.15_30.2),]

### join
data.15_30 <- rbind(data.15_30,data.15_30.2);rm(data.15_30.2)

covars <- c(covar.names, covar.depth.specific)
form <- as.formula(paste("CF.class.15_30", paste(covars, collapse=" + "), sep=" ~ "))
unique(data.15_30$CF.class.15_30)
data.15_30$CF.class.15_30 <- as.factor(data.15_30$CF.class.15_30)

### Let's try in cross-validation
set.seed(2022)
cf_vol_cv_15_30 <- make_cv_RF_cf_prob(data = data.15_30,
                                    property = 'CF.class.15_30',
                                    covars = c(covar.names, covar.depth.specific),
                                    nfolds = 10)
cv.results <- cf_vol_cv_15_30[[2]]

### for each prediction, keep only the dominant level
colnames(cv.results) <- c("CF.class.15_30","X1","X2","X3","X4","X5","X6","pred") 
cv.results$pred <- colnames(cv.results[,2:7])[apply(cv.results[,2:7],1,which.max)]
cv.stats.15_30.prob <- goofcat(observed = cv.results$CF.class.15_30, predicted = cv.results$pred);cv.stats.15_30.prob
write.csv(cv.stats.15_30.prob$confusion_matrix, file = "cv.stats.15_30.prob.cm.csv")
table(cv.results$CF.class.15_30)

### Let's try in cross-validation
set.seed(2022)
cf_vol_cv_class_15_30 <- make_cv_RF_cf_class(data = data.15_30,
                                           property = 'CF.class.15_30',
                                           covars = c(covar.names, covar.depth.specific),
                                           nfolds = 10)
cv.results_class <- cf_vol_cv_class_15_30[[2]]
head(cv.results_class)
cv.results_class$obs.char <- as.character(cv.results_class$CF.class.15_30)
cv.results_class$pred.cf.class <- ifelse(cv.results_class$pred==1, "X1", 
                                         ifelse(cv.results_class$pred==2, "X2",
                                                ifelse(cv.results_class$pred==3, "X3",
                                                       ifelse(cv.results_class$pred==4, "X4",
                                                              ifelse(cv.results_class$pred==5, "X5", 
                                                                     ifelse(cv.results_class$pred==6, "X6",NA))))))
#cv.results_class$pred.cf.class <- factor(cv.results_class$pred.cf.class)
cv.stats.15_30.class<-goofcat(observed = cv.results_class$obs.char , predicted = cv.results_class$pred.cf.class);cv.stats.15_30.class
write.csv(cv.stats.15_30.class$confusion_matrix, file = "cv.stats.15_30.class.cm.csv")

#### Map at 1km to have an idea?
####  Extract the model
cf_rf_class_15_30 <- cf_vol_cv_class_15_30[[1]]

# # path to the covariates raster
# setwd('R:/PRJ-SoilBiodSOC2019/Covariates/Australia1km_filled/')
# # list all the raster files available 
# list_ras <- list.files( pattern="tif$")
# # load all the rasters
# covariates.stack <- stack(paste0('R:/PRJ-SoilBiodSOC2019/Covariates/Australia1km_filled/', list_ras))

# make blocks
bs <- blockSize(covariates.stack, minblocks=50)

setwd(outTiles)
cl <- makeCluster(5)   
registerDoParallel(cl)
cf_rast_list <- foreach(i=1:bs$n, .packages=c("raster", "ranger"), .export = c("cf_rf_class_15_30")) %dopar% {
  ### Get one tile of the raster stack
  tile <- crop(covariates.stack, extent(covariates.stack, bs$row[[i]], bs$row[[i]]+bs$nrows[[i]], 1, ncol(covariates.stack)))
  ### Predict coarse fragments class
  #filename= paste0("cf_rf_class_15_30.tile",i,".tif")
  model=cf_rf_class_15_30
  pred <- raster::predict(object = tile, 
                          model = model, 
                          fun = function(model, ...) predict(model, ...)$predictions)
  pred # Return this
}
stopCluster(cl)

## Assign function to mosaic
cf_rast_list$na.rm <- TRUE
cf_rast_list$fun <- min
## Create mosaic for whole NSW
k.raster <- do.call(mosaic, cf_rast_list)
names(k.raster) <- "cf_vol_class_15_30"
plot(k.raster)

## Write to file
writeRaster(k.raster, filename= "cf_vol_class_15_30.tif", na.rm=T,inf.rm=T, format="GTiff", overwrite=TRUE )
gc()
rm(cf_rast_list,i,list_ras,k.raster)
cf_vol_class_15_30 <- raster("cf_vol_class_15_30.tif")
cf_vol_class_15_30 <- ratify(cf_vol_class_15_30)
rat <- levels(cf_vol_class_15_30)[[1]]
rat$CF_class <- c("Very few (<2 %)", "Few (2-10 %)",
                  "Common (10-20 %)", "Many (20-50 %)",
                  "Abundant (50-90 %)")
levels(cf_vol_class_15_30) <- rat

myPal <- RColorBrewer::brewer.pal('YlOrBr', n=6)[1:6]

par(xpd=FALSE)
plot(cf_vol_class_15_30, col=myPal,legend=FALSE)
legend(145,-10,inset=c(10,-5),cex=0.9,
       legend=c("Very few (<2 %)", "Few (2-10 %)",
                "Common (10-20 %)", "Many (20-50 %)", "Abundant (50-90 %)"),
       fill=myPal, bg="white", bty="n")

##################################################################################################################################

# 4.3 Depth 15-30 cm Probability maps -------------------------------------

### Now, we predict the probabilities for each class
cf_rf_probs_15_30 <- cf_vol_cv_15_30[[1]]

cl <- makeCluster(6)   
registerDoParallel(cl)
#model=cf_rf_class_15_30
cf_rast_list <- foreach(i=1:bs$n, .packages=c("raster", "ranger"), .export = c("cf_rf_probs_15_30")) %dopar% {
  
  ### Get one tile of the raster stack
  tile <- crop(covariates.stack, extent(covariates.stack, bs$row[[i]], bs$row[[i]]+bs$nrows[[i]], 1, ncol(covariates.stack)))
  ### Predict coarse fragments class
  #filename= paste0("cf_rf_class_15_30.tile",i,".tif")
  model=cf_rf_probs_15_30
  pred <- raster::predict(object = tile, 
                          model = model, index=1:6,
                          fun = function(model, ...) predict(model, ...)$predictions)
  pred # Return this
}

stopCluster(cl)

## Assign function to mosaic
### Divide by layers
cf.classes.levels <- levels(data.15_30$CF.class.15_30)

#### Mosaic by cf class
for(var in 1:length(cf.classes.levels)){
  rast.list <- list()
  for(j in 1:bs$n){
    rast.list[[j]] <- cf_rast_list[[j]][[var]]
  }
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  pred <- do.call(mosaic, rast.list)
  names(pred) <- names(cf.classes.levels[[var]])
  print(pred)
  #print(paste0(OutDirTiles,names.write[[var]],i,".rds"))
  writeRaster(pred, filename= paste0(outTiles,"cf_vol_prob_",cf.classes.levels[[var]],".15_30.tif"), 
              na.rm=T,inf.rm=T, format="GTiff", overwrite=TRUE)
}
rm(cf_rast_list, var,j,cl, rat, pred, rast.list, model)

rast.prob <- stack(list.files(pattern="30")[3:8])
#rast.prob<- dropLayer(rast.prob,1)
levelplot(rast.prob+0.01,zscaleLog=TRUE)
#levelplot(rast.prob)

### save
save(cf_rf_probs_15_30, file="cf_rf_probs_15_30.RData")
save(cf_rf_probs_5_15, file="cf_rf_probs_5_15.RData")
save(cf_rf_probs_0_5, file="cf_rf_probs_0_5.RData")


# Figure for Paper - Variable Importance ----------------------------------

require(ranger)
library(readr)
library(ggplot2)
library(shapper)
library(ggplot2)
library(scales)
library(viridis)
library(viridisLite)

load("cf.data.26052022.RData")

### Redo the models with 5000 trees

covar.depth.specific <- c('clay_0_5', 'sand_0_5')
covars <- c(covar.names, covar.depth.specific)
property = 'CF.class.0_5'
form <- as.formula(paste("CF.class.0_5", paste(covars, collapse=" + "), sep=" ~ "))
data = data.0_5
form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))

set.seed(1991)
rf.CFprob_0_5 <- ranger(form, data = data.0_5, 
                        num.trees = 5000,
                        importance='impurity_corrected',
                        probability = TRUE)

### Plot varImp
DF<-data.frame(covariates=names(rf.CFprob_0_5$variable.importance),
               Importance=as.vector(rf.CFprob_0_5$variable.importance))
DF <- DF %>% dplyr::arrange(., desc(Importance)) %>% as.data.frame()
DF[1:15,]
DF <- DF %>% dplyr::arrange(., Importance) %>% as.data.frame()
DF$covariates <- factor(DF$covariates, levels = DF$covariates)

ggplot(DF, aes(x=Importance, y= covariates, fill=Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Variable Importance") +
  ylab("") +
  ggtitle("Coarse fragments (0-5 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")

DF[1:10,]
CF_0_5 <- DF


covar.depth.specific <- c('clay_5_15', 'sand_5_15')
covars <- c(covar.names, covar.depth.specific)
property = 'CF.class.5_15'
form <- as.formula(paste("CF.class.5_15", paste(covars, collapse=" + "), sep=" ~ "))
data = data.5_15
form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))

set.seed(1984)
rf.CFprob_5_15 <- ranger(form, data = data.5_15, 
                        num.trees = 5000,
                        importance='impurity_corrected',
                        probability = TRUE)

### Plot varImp
DF<-data.frame(covariates=names(rf.CFprob_5_15$variable.importance),
               Importance=as.vector(rf.CFprob_5_15$variable.importance))
DF <- DF %>% dplyr::arrange(., desc(Importance)) %>% as.data.frame()
DF[1:15,]
DF <- DF %>% dplyr::arrange(., Importance) %>% as.data.frame()
DF$covariates <- factor(DF$covariates, levels = DF$covariates)

ggplot(DF, aes(x=Importance, y= covariates, fill=Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Variable Importance") +
  ylab("") +
  ggtitle("Coarse fragments (5-15 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")

DF[1:10,]
CF_5_15 <- DF


covar.depth.specific <- c('clay_15_30', 'sand_15_30')
covars <- c(covar.names, covar.depth.specific)
property = 'CF.class.15_30'
form <- as.formula(paste("CF.class.15_30", paste(covars, collapse=" + "), sep=" ~ "))
data = data.15_30
form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))

set.seed(1984)
rf.CFprob_15_30 <- ranger(form, data = data.15_30, 
                         num.trees = 5000,
                         importance='impurity_corrected',
                         probability = TRUE)

### Plot varImp
DF<-data.frame(covariates=names(rf.CFprob_15_30$variable.importance),
               Importance=as.vector(rf.CFprob_15_30$variable.importance))
DF <- DF %>% dplyr::arrange(., desc(Importance)) %>% as.data.frame()
DF[1:15,]
DF <- DF %>% dplyr::arrange(., Importance) %>% as.data.frame()
DF$covariates <- factor(DF$covariates, levels = DF$covariates)

ggplot(DF, aes(x=Importance, y= covariates, fill=Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Variable Importance") +
  ylab("") +
  ggtitle("Coarse fragments (15-30 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")

DF[1:10,]
CF_15_30 <- DF

### bind all of them
varImp <- merge(CF_0_5, CF_5_15 , by="covariates", all=TRUE)
colnames(varImp) <- c("covariates",   "Importance.CF.0_5", "Importance.CF.5_15")
varImp <- merge(varImp, CF_15_30 , by="covariates", all=TRUE)
colnames(varImp) <- c("covariates",   "Importance.CF.0_5", "Importance.CF.5_15", "Importance.CF.15_30")

library(tidyverse)
covar.order <-c(covar.names, "clay_0_5", "sand_0_5", "clay_5_15", "sand_5_15", "clay_15_30", "sand_15_30")
varImp2 <- dplyr::left_join(data.frame(covariates=covar.order), varImp, by="covariates")
rownames(varImp2) <- varImp2$covariates

varImp2["clay_0_5","Importance.CF.5_15"]  <- varImp2["clay_5_15","Importance.CF.5_15"]
varImp2["clay_0_5","Importance.CF.15_30"] <- varImp2["clay_15_30","Importance.CF.15_30"]
varImp2["sand_0_5","Importance.CF.5_15"]  <- varImp2["sand_5_15","Importance.CF.5_15"]
varImp2["sand_0_5","Importance.CF.15_30"] <- varImp2["sand_15_30","Importance.CF.15_30"]

dim(varImp2)
varImp2 <- varImp2[1:57,]
varImp2[56:57,]$covariates <- c("clay","sand")
rownames(varImp2) <- varImp2$covariates

library(ggplot2)
library(gplots)
library(tidyverse)
heatmap(t(as.matrix(varImp2[,2:4])),
        cexRow = 0.6, 
        cexCol = 0.8,
        margins = c(10, 6))
mypal <-viridis_pal(option = "A",direction=-1)(15)

heatmap.2(t(as.matrix(varImp2[,2:4])),dendrogram = "none",
          Colv = FALSE,Rowv = FALSE,
          cexRow = 0.8, notecol="transparent", tracecol = "transparent", 
          cexCol = 0.8,scale="none",col=mypal,
          margins = c(10, 6),
          key.title="Variable importance")

myorder <-c("Clim_ADM", "Clim_EPA", "Clim_EPI", "Clim_EPX", 
            "Clim_Prescott", "Clim_PTA", "Clim_PTI", "Clim_PTS1", "Clim_PTS2", 
            "Clim_PTX", "Clim_RSM", "Clim_TNM", "Clim_TRA", "Clim_TXM", "Clim_WDA", 
            "NDVI_mean_Q1", "NDVI_mean_Q2", "NDVI_mean_Q3", "NDVI_mean_Q4",
            "Veg_FC_Max_BS", "Veg_FC_Max_NPV", "Veg_FC_Max_PV", "Veg_FC_Mean_BS", 
            "Veg_FC_Mean_NPV", "Veg_FC_Mean_PV", "Veg_FC_Min_BS", "Veg_FC_Min_NPV", 
            "Veg_FC_Min_PV", "Veg_FC_SD_BS", "Veg_FC_SD_NPV", "Veg_FC_SD_PV", 
            "Veg_FPAR_Max", "Veg_FPAR_Mean", "Veg_FPAR_Median", "Veg_FPAR_Min", 
            "Veg_LandCoverTrend_evi_mean", "Veg_Persistant_green_Veg",
            "relief_dems_3s_mosaic1","relief_slope_perc",
            "Relief_mrrtf_3s", "relief_mrvbf_3s_mosaic", 
            "relief_plan_curvature_3s", "relief_profile_curvature_3", 
            "relief_roughness", "relief_twi_3s",
            "PM_Gravity", 
            "PM_radmap_v4_2019_filtered_dose_GAPFilled",
            "PM_radmap_v4_2019_filtered_pctk_GAPFilled", 
            "PM_radmap_v4_2019_filtered_ppmt_GAPFilled", 
            "PM_radmap_v4_2019_filtered_ppmu_GAPFilled", 
            "PM_radmap_v4_2019_ratio_tk_GAPFilled", 
            "PM_radmap_v4_2019_ratio_u2t_GAPFilled", 
            "PM_radmap_v4_2019_ratio_uk_GAPFilled", 
            "PM_radmap_v4_2019_ratio_ut_GAPFilled", 
            "PM_Weathering_Index",
            "clay","sand")
varImp3 <- dplyr::left_join(data.frame(covariates=myorder),varImp2, by="covariates")
rownames(varImp3) <- varImp3$covariates

### Rename the covariates for the plot
rownames(varImp3) <-c("ADM", "EPA", "EPI", "EPX", "Prescott", "PTA", "PTI", "PTS1", "PTS2", 
                      "PTX", "RSM", "TNM", "TRA", "TXM", "WDA", 
                      "NDVI_Q1", "NDVI_Q2", "NDVI_Q3", "NDVI_Q4",
                      "FC_Max_BS", "FC_Max_NPV", "FC_Max_PV", "FC_Mean_BS", 
                      "FC_Mean_NPV", "FC_Mean_PV", "FC_Min_BS", "FC_Min_NPV", 
                      "FC_Min_PV", "FC_SD_BS", "FC_SD_NPV", "FC_SD_PV", 
                      "FPAR_Max", "FPAR_Mean", "FPAR_Median", "FPAR_Min", 
                      "LC_EVI", "Pers_Green_Veg",
                      "DEM","Slope",
                      "MRRTF", "MRVBF", 
                      "Plan_curv", "Prof_curv", 
                      "Roughness", "TWI",
                      "Gravity", 
                      "Dose",
                      "K", 
                      "Th", 
                      "U", 
                      "Th_K", 
                      "U2_Th", 
                      "U_K", 
                      "U_Th", 
                      "WII",
                      "Clay","Sand")

heatmap.2(t(as.matrix(varImp3[,2:4])), dendrogram = "none",
          Colv = FALSE,Rowv = FALSE,
          cexRow = 0.8, notecol="transparent", tracecol = "transparent", 
          cexCol = 0.8, scale="none",col=mypal,
          margins = c(12,12),
          key.title="Variable importance")

rm(covar.names, covar.depth.specific, covar.order,mypal, properties)
write.csv(varImp3, file ="varImp_CF_Vars3depths.csv" )

##################################################################################################################################

### End of the script