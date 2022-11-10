########################################################################################################################################################
### Model examination - SOC fractions in Australia
### Project: SOC fraction maps for TERN
### Date: 14/10/2021
### Author: Mercedes Roman Dobarco

require(ranger)
library(sf)
require(raster)
library(readr)
library(ggplot2)
library(shapper)
library(ggplot2)
library(scales)
library(viridis)
library(viridisLite)

# ### 1. Load SOC fraction data, Biome and Covariates ------------------------

HomeDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/for_Ross/"
InputDir  <- paste0(HomeDir,"Output_files/4_Output/")
OutDir  <- paste0(HomeDir,"Output_files/5_Output/")
setwd(InputDir)
load("4_SOCfr_RF_cv_ilr_HPR.RData")

### Load the data: SOC fractions and extracted covariates
 
# HOCsoildat <- HOC_p.GSM ; rm(HOC_p.GSM)
# POCsoildat <- POC_p.GSM ; rm(POC_p.GSM)
# ROCsoildat <- ROC_p.GSM ; rm(ROC_p.GSM)
# 
# # check for remaining very large values
# par(mfrow=c(1,3))
# boxplot(HOCsoildat[,c(2:4)])
# boxplot(ROCsoildat[,c(2:4)])
# boxplot(POCsoildat[,c(2:4)])
# 
# ### constrain to 100% maximum value
# HOCsoildat$`0-5 cm` <- ifelse(HOCsoildat$`0-5 cm` >100, 100, HOCsoildat$`0-5 cm`)
# HOCsoildat$`5-15 cm` <- ifelse(HOCsoildat$`5-15 cm` >100, 100, HOCsoildat$`5-15 cm`)
# HOCsoildat$`15-30 cm` <- ifelse(HOCsoildat$`15-30 cm` >100, 100, HOCsoildat$`15-30 cm`)
# POCsoildat$`0-5 cm` <- ifelse(POCsoildat$`0-5 cm` >100, 100, POCsoildat$`0-5 cm`)
# POCsoildat$`5-15 cm` <- ifelse(POCsoildat$`5-15 cm` >100, 100, POCsoildat$`5-15 cm`)
# POCsoildat$`15-30 cm` <- ifelse(POCsoildat$`15-30 cm` >100, 100, POCsoildat$`15-30 cm`)
# ROCsoildat$`0-5 cm` <- ifelse(ROCsoildat$`0-5 cm` >100, 100, ROCsoildat$`0-5 cm`)
# ROCsoildat$`5-15 cm` <- ifelse(ROCsoildat$`5-15 cm` >100, 100, ROCsoildat$`5-15 cm`)
# ROCsoildat$`15-30 cm` <- ifelse(ROCsoildat$`15-30 cm` >100, 100, ROCsoildat$`15-30 cm`)
# 
# ### Add column with Spectral function
# HOCsoildat$SpectralFun <- ifelse(HOCsoildat$Dataset == "SCaRP", "MIR_PLSR_2013",
#                                  ifelse(HOCsoildat$Dataset %in% c("TERN","Aus_archive"),"Vis-NIR_PLSR_2021","MIR_PLSR_2021"))
# POCsoildat$SpectralFun <- ifelse(POCsoildat$Dataset == "SCaRP", "MIR_PLSR_2013",
#                                  ifelse(POCsoildat$Dataset %in% c("TERN","Aus_archive"),"Vis-NIR_PLSR_2021","MIR_PLSR_2021"))
# ROCsoildat$SpectralFun <- ifelse(ROCsoildat$Dataset == "SCaRP", "MIR_PLSR_2013",
#                                  ifelse(ROCsoildat$Dataset %in% c("TERN","Aus_archive"),"Vis-NIR_PLSR_2021","MIR_PLSR_2021"))
# 
# ### Change col names
# colnames(HOCsoildat) <- c("Dataset","HOC.0.5","HOC.5.15","HOC.15.30",
#                           "Lat_WGS84","Long_WGS84","Location_ID","SOCFr","Spectra","SpectralFun")
# colnames(POCsoildat) <- c("Dataset","POC.0.5","POC.5.15","POC.15.30",
#                           "Lat_WGS84","Long_WGS84","Location_ID","SOCFr","Spectra","SpectralFun")
# colnames(ROCsoildat) <- c("Dataset","ROC.0.5","ROC.5.15","ROC.15.30",
#                           "Lat_WGS84","Long_WGS84","Location_ID","SOCFr","Spectra","SpectralFun")
# 
# ### Extract BIOME
# biome_Aus <- read_sf("C:/Covariates/Ecoregions2017/EcoregionsAus.shp")
# plot(biome_Aus["BIOME_NAME"])
# 
# attach.biome <- function(soildat) {
#   # Join biome data
#   soildat.sf <- st_as_sf(soildat, coords = c("Long_WGS84","Lat_WGS84"), crs = 4326)
#   soildat.sf <- st_join(soildat.sf, biome_Aus["BIOME_NAME"], left=TRUE)
#   # Back to dataframe
#   soildat.sf.coords <- as.data.frame(st_coordinates(soildat.sf))
#   soildat.df <- st_drop_geometry(soildat.sf)
#   soildat <- cbind(soildat.sf.coords,soildat.df)
#   colnames(soildat)[colnames(soildat) == "BIOME_NAME"] <- "Biome"
#   colnames(soildat)[colnames(soildat) == "X"] <- "Long_WGS84"
#   colnames(soildat)[colnames(soildat) == "Y"] <- "Lat_WGS84"
#   return(soildat)
# }
# 
# HOCsoildat <- attach.biome(HOCsoildat)
# POCsoildat <- attach.biome(POCsoildat)
# ROCsoildat <- attach.biome(ROCsoildat)
# 
# ### Load extracted covariates and join tables
# extrHOC <- read_csv("C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/data/extrHOC.csv")
# extrPOC <- read_csv("C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/data/extrPOC.csv")
# extrROC <- read_csv("C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/data/extrROC.csv")
# HOCsoildat <- cbind(HOCsoildat, as.data.frame(extrHOC[,colnames(extrHOC) != "X1"]))
# POCsoildat <- cbind(POCsoildat, as.data.frame(extrPOC[,colnames(extrPOC) != "X1"]))
# ROCsoildat <- cbind(ROCsoildat, as.data.frame(extrROC[,colnames(extrROC) != "X1"]))
# 
# SOCFr <- cbind(HOCsoildat[,c(1,2,4:6)], POCsoildat[,4:6], ROCsoildat[,4:6], HOCsoildat[,c(3,7,9:ncol(HOCsoildat))])
# 
# covar.names <- c("Clim_ADM", "Clim_EPA", "Clim_EPI", "Clim_EPX", 
#                  "Clim_Prescott", "Clim_PTA", "Clim_PTI", "Clim_PTS1", "Clim_PTS2", 
#                  "Clim_PTX", "Clim_RSM", "Clim_TNM", "Clim_TRA", "Clim_TXM", "Clim_WDA", 
#                  "NDVI_mean_Q1", "NDVI_mean_Q2", "NDVI_mean_Q3", "NDVI_mean_Q4", 
#                  "PM_Gravity", "PM_radmap_v4_2019_filtered_dose_GAPFilled", "PM_radmap_v4_2019_filtered_pctk_GAPFilled", 
#                  "PM_radmap_v4_2019_filtered_ppmt_GAPFilled", "PM_radmap_v4_2019_filtered_ppmu_GAPFilled", 
#                  "PM_radmap_v4_2019_ratio_tk_GAPFilled", "PM_radmap_v4_2019_ratio_u2t_GAPFilled", 
#                  "PM_radmap_v4_2019_ratio_uk_GAPFilled", "PM_radmap_v4_2019_ratio_ut_GAPFilled", 
#                  "PM_Weathering_Index", "relief_dems_3s_mosaic1", "Relief_mrrtf_3s", 
#                  "relief_mrvbf_3s_mosaic", "relief_plan_curvature_3s", "relief_profile_curvature_3", 
#                  "relief_roughness", "relief_slope_perc", "relief_twi_3s", 
#                  "Veg_FC_Max_BS", "Veg_FC_Max_NPV", "Veg_FC_Max_PV", "Veg_FC_Mean_BS", 
#                  "Veg_FC_Mean_NPV", "Veg_FC_Mean_PV", "Veg_FC_Min_BS", "Veg_FC_Min_NPV", 
#                  "Veg_FC_Min_PV", "Veg_FC_SD_BS", "Veg_FC_SD_NPV", "Veg_FC_SD_PV", 
#                  "Veg_FPAR_Max", "Veg_FPAR_Mean", "Veg_FPAR_Median", "Veg_FPAR_Min", 
#                  "Veg_LandCoverTrend_evi_mean", "Veg_Persistant_green_Veg")
# 
# # ### 2. Do errors depend on the spectral model? -----------------------------
# 
# ### HOC 0-5 cm
# #load("C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/data/HOC_0_5_cm_model_evaluationCV.Rdata")
# load("C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/data/October2021/ilr_0_5_cm_model_evaluationCV.Rdata")
# 
# SOC_0_5.pr <- rf.ilr0_5[[3]]
# SOC_0_5.pr$HOCe0.5 <- SOC_0_5.pr$HOC.0.5 - SOC_0_5.pr$pred.HOC
# SOC_0_5.pr$POCe0.5 <- SOC_0_5.pr$POC.0.5 - SOC_0_5.pr$pred.POC
# SOC_0_5.pr$ROCe0.5 <- SOC_0_5.pr$ROC.0.5 - SOC_0_5.pr$pred.ROC
# 
# par(mfrow=c(1,3))
# hist(SOC_0_5.pr$HOCe0.5, breaks=50)
# hist(SOC_0_5.pr$POCe0.5, breaks=50)
# hist(SOC_0_5.pr$ROCe0.5, breaks=50)
# 
# ### Join with covariates
# covar.depth.specific <- c("clay_0_5", "sand_0_5")
# HOCsoildat.0_5 <- cbind(HOCsoildat[complete.cases(HOCsoildat[,c("HOC.0.5",covar.names, covar.depth.specific)]),], SOC_0_5.pr)
# 
# ### Boxplot
# boxplot(HOCsoildat.0_5$HOCe0.5 ~ HOCsoildat.0_5$SpectralFun)
# boxplot(HOCsoildat.0_5$HOCe0.5 ~ HOCsoildat.0_5$Dataset)
# boxplot(HOCsoildat.0_5$HOCe0.5 ~ HOCsoildat.0_5$Spectra)
# lm.HOCe.0.5 <- lm(HOCe0.5~SpectralFun, data = HOCsoildat.0_5)
# summary(lm.HOCe.0.5)
# anova(lm.HOCe.0.5)
# par(mfrow=c(2,2))
# plot(lm.HOCe.0.5)
# 
# boxplot(HOCsoildat.0_5$HOC.0.5 ~ HOCsoildat.0_5$SpectralFun)
# boxplot(HOCsoildat.0_5$HOC.0.5 ~ HOCsoildat.0_5$Dataset)
# boxplot(HOCsoildat.0_5$HOC.0.5 ~ HOCsoildat.0_5$Spectra)
# 
# 
# ### 3. Variable importance - RandomForest varImp -----------------------------------------------------
# 
# ### By BIOME
# SOCFr$Biome <-  factor(SOCFr$Biome,
#                        levels=c("Montane Grasslands & Shrublands",
#                                 "Temperate Broadleaf & Mixed Forests",
#                                 "Temperate Grasslands, Savannas & Shrublands",
#                                 "Mediterranean Forests, Woodlands & Scrub",
#                                 "Deserts & Xeric Shrublands",
#                                 "Tropical & Subtropical Grasslands, Savannas & Shrublands",
#                                 "Tropical & Subtropical Moist Broadleaf Forests"))


### Change working directory
setwd("OutDir")

### 0-5 cm

### properties
properties <- c("HOC.0.5","POC.0.5","ROC.0.5")
covar.depth.specific <- c('clay_0_5','sand_0_5')
covars <- c(covar.names, covar.depth.specific)
SOCFr.0_5.rf <-  SOCFr[complete.cases(SOCFr[,c(properties,covars)]),]

### Perform the ilr transformation
library(compositions)
ilr.properties <- ilr(SOCFr.0_5.rf[,properties])
ilr.properties <- as.data.frame(ilr.properties)
SOCFr.0_5.rf$ilr1 <- ilr.properties$V1
SOCFr.0_5.rf$ilr2 <- ilr.properties$V2
# make the formula for ilr1 and ilr2
form1 <- as.formula(paste("ilr1", paste(covars, collapse=" + "), sep=" ~ "))
form2 <- as.formula(paste("ilr2", paste(covars, collapse=" + "), sep=" ~ "))

#set.seed(2445)
set.seed(3899)
rf.0_5.ilr1<- ranger(form1, 
                     data = SOCFr.0_5.rf,
                     num.trees = 5000,
                     importance = 'permutation', 
                     scale.permutation.importance = TRUE)
rf.0_5.ilr1$variable.importance


### Plot varImp
DF<-data.frame(covariates=names(rf.0_5.ilr1$variable.importance),
               Importance=as.vector(rf.0_5.ilr1$variable.importance))
DF <- DF %>% dplyr::arrange(., desc(Importance)) %>% as.data.frame()
DF[1:15,]
DF <- DF %>% dplyr::arrange(., Importance) %>% as.data.frame()
DF$covariates <- factor(DF$covariates, levels = DF$covariates)

ggplot(DF, aes(x=Importance, y= covariates, fill=Importance))+ 
   geom_bar(stat="identity", position="dodge") + 
     xlab("Covariate Importance") +
     ylab("") +
     ggtitle("ilr1 (0-5 cm)")+
     guides(fill=F)+
     scale_fill_gradient(low="gray80", high="blue")

DF[1:10,]
VarImp_ilr1_0_5 <- DF

### Now ilr2
#set.seed(2445)
set.seed(4037)
rf.0_5.ilr2<- ranger(form2, 
                     data = SOCFr.0_5.rf,
                     num.trees = 5000,
                     importance = 'permutation', 
                     scale.permutation.importance = TRUE)
rf.0_5.ilr2$variable.importance

#rf.0_5.ilr2 <- rf.ilr2.0_5

### Plot varImp
DF<-data.frame(covariates=names(rf.0_5.ilr2$variable.importance),
               Importance=as.vector(rf.0_5.ilr2$variable.importance))
DF <- DF %>% dplyr::arrange(., desc(Importance)) %>% as.data.frame()
DF[1:15,]
DF <- DF %>% dplyr::arrange(., Importance) %>% as.data.frame()
DF$covariates <- factor(DF$covariates, levels = DF$covariates)

ggplot(DF, aes(x=Importance, y= covariates, fill=Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("ilr2 (0-5 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")
DF[1:10,]
VarImp_ilr2_0_5 <- DF


### 5-15 cm

### properties
properties <- c("HOC.5.15","POC.5.15","ROC.5.15")
covar.depth.specific <- c('clay_5_15','sand_5_15')
covars <- c(covar.names, covar.depth.specific)
SOCFr.5_15.rf <-  SOCFr[complete.cases(SOCFr[,c(properties,covars)]),]

### Perform the ilr transformation
library(compositions)
ilr.properties <- ilr(SOCFr.5_15.rf[,properties])
ilr.properties <- as.data.frame(ilr.properties)
SOCFr.5_15.rf$ilr1 <- ilr.properties$V1
SOCFr.5_15.rf$ilr2 <- ilr.properties$V2
# make the formula for ilr1 and ilr2
form1 <- as.formula(paste("ilr1", paste(covars, collapse=" + "), sep=" ~ "))
form2 <- as.formula(paste("ilr2", paste(covars, collapse=" + "), sep=" ~ "))

#set.seed(2446)
set.seed(3892)
rf.5_15.ilr1<- ranger(form1, 
                     data = SOCFr.5_15.rf,
                     num.trees = 5000,
                     importance = 'permutation', 
                     scale.permutation.importance = TRUE)
rf.5_15.ilr1$variable.importance


#rf.5_15.ilr1 <- rf.ilr1.5_15

### Plot varImp
DF<-data.frame(covariates=names(rf.5_15.ilr1$variable.importance),
               Importance=as.vector(rf.5_15.ilr1$variable.importance))
DF <- DF %>% dplyr::arrange(., desc(Importance)) %>% as.data.frame()
DF[1:15,]
DF <- DF %>% dplyr::arrange(., Importance) %>% as.data.frame()
DF$covariates <- factor(DF$covariates, levels = DF$covariates)

ggplot(DF, aes(x=Importance, y= covariates, fill=Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("ilr1 (5-15 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")

DF[1:10,]
VarImp_ilr1_5_15 <- DF

### Now ilr2
#set.seed(2446)
set.seed(4030)
rf.5_15.ilr2<- ranger(form2, 
                     data = SOCFr.5_15.rf,
                     num.trees = 5000,
                     importance = 'permutation', 
                     scale.permutation.importance = TRUE)
rf.5_15.ilr2$variable.importance

#rf.5_15.ilr2 <- rf.ilr2.5_15

### Plot varImp
DF<-data.frame(covariates=names(rf.5_15.ilr2$variable.importance),
               Importance=as.vector(rf.5_15.ilr2$variable.importance))
DF <- DF %>% dplyr::arrange(., desc(Importance)) %>% as.data.frame()
DF[1:15,]
DF <- DF %>% dplyr::arrange(., Importance) %>% as.data.frame()
DF$covariates <- factor(DF$covariates, levels = DF$covariates)

ggplot(DF, aes(x=Importance, y= covariates, fill=Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("ilr2 (5-15 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")
DF[1:10,]
VarImp_ilr2_5_15 <- DF


### 15-30 cm

### properties
properties <- c("HOC.15.30","POC.15.30","ROC.15.30")
covar.depth.specific <- c('clay_15_30','sand_15_30')
covars <- c(covar.names, covar.depth.specific)
SOCFr.15_30.rf <-  SOCFr[complete.cases(SOCFr[,c(properties,covars)]),]

### Perform the ilr transformation
library(compositions)
ilr.properties <- ilr(SOCFr.15_30.rf[,properties])
ilr.properties <- as.data.frame(ilr.properties)
SOCFr.15_30.rf$ilr1 <- ilr.properties$V1
SOCFr.15_30.rf$ilr2 <- ilr.properties$V2
# make the formula for ilr1 and ilr2
form1 <- as.formula(paste("ilr1", paste(covars, collapse=" + "), sep=" ~ "))
form2 <- as.formula(paste("ilr2", paste(covars, collapse=" + "), sep=" ~ "))

#set.seed(2447)
set.seed(3854)
rf.15_30.ilr1<- ranger(form1, 
                      data = SOCFr.15_30.rf,
                      num.trees = 5000,
                      importance = 'permutation', 
                      scale.permutation.importance = TRUE)
rf.15_30.ilr1$variable.importance


#rf.15_30.ilr1 <- rf.ilr1.15_30

### Plot varImp
DF<-data.frame(covariates=names(rf.15_30.ilr1$variable.importance),
               Importance=as.vector(rf.15_30.ilr1$variable.importance))
DF <- DF %>% dplyr::arrange(., desc(Importance)) %>% as.data.frame()
DF[1:15,]
DF <- DF %>% dplyr::arrange(., Importance) %>% as.data.frame()
DF$covariates <- factor(DF$covariates, levels = DF$covariates)

ggplot(DF, aes(x=Importance, y= covariates, fill=Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("ilr1 (15-30 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")

DF[1:10,]
VarImp_ilr1_15_30 <- DF

### Now ilr2
#set.seed(2447)
set.seed(3992)
rf.15_30.ilr2<- ranger(form2, 
                      data = SOCFr.15_30.rf,
                      num.trees = 5000,
                      importance = 'permutation', 
                      scale.permutation.importance = TRUE)
rf.15_30.ilr2$variable.importance
#rf.15_30.ilr2 <- rf.ilr2.15_30

### Plot varImp
DF<-data.frame(covariates=names(rf.15_30.ilr2$variable.importance),
               Importance=as.vector(rf.15_30.ilr2$variable.importance))
DF <- DF %>% dplyr::arrange(., desc(Importance)) %>% as.data.frame()
DF[1:15,]
DF <- DF %>% dplyr::arrange(., Importance) %>% as.data.frame()
DF$covariates <- factor(DF$covariates, levels = DF$covariates)

ggplot(DF, aes(x=Importance, y= covariates, fill=Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("ilr2 (15-30 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")
DF[1:10,]
VarImp_ilr2_15_30 <- DF
VarImp_ilr2_15_30

### bind all of them
varImp <- merge(VarImp_ilr1_0_5, VarImp_ilr2_0_5 , by="covariates", all=TRUE)
colnames(varImp) <- c("covariates",   "Importance.ilr1.0_5", "Importance.ilr2.0_5")
varImp <- merge(varImp, VarImp_ilr1_5_15 , by="covariates", all=TRUE)
varImp <- merge(varImp, VarImp_ilr2_5_15 , by="covariates",all=TRUE)
colnames(varImp) <- c("covariates",   "Importance.ilr1.0_5", "Importance.ilr2.0_5",
                      "Importance.ilr1.5_15", "Importance.ilr2.5_15")
varImp <- merge(varImp, VarImp_ilr1_15_30 , by="covariates",all=TRUE)
varImp <- merge(varImp, VarImp_ilr2_15_30 , by="covariates",all=TRUE)
colnames(varImp) <- c("covariates",   "Importance.ilr1.0_5", "Importance.ilr2.0_5",
                      "Importance.ilr1.5_15", "Importance.ilr2.5_15",
                      "Importance.ilr1.15_30", "Importance.ilr2.15_30")
#varImp <-  dplyr::arrange(varImp, desc(covariates)) %>% as.data.frame()
library(tidyverse)
covar.order <-c(covar.names, "clay_0_5", "sand_0_5", "clay_5_15", "sand_5_15", "clay_15_30", "sand_15_30")
varImp2 <- dplyr::left_join(data.frame(covariates=covar.order),varImp, by="covariates")
rownames(varImp2) <- varImp2$covariates

varImp2["clay_0_5","Importance.ilr1.5_15"]  <- varImp2["clay_5_15","Importance.ilr1.5_15"]
varImp2["clay_0_5","Importance.ilr1.15_30"] <- varImp2["clay_15_30","Importance.ilr1.15_30"]
varImp2["sand_0_5","Importance.ilr1.5_15"]  <- varImp2["sand_5_15","Importance.ilr1.5_15"]
varImp2["sand_0_5","Importance.ilr1.15_30"] <- varImp2["sand_15_30","Importance.ilr1.15_30"]

varImp2["clay_0_5","Importance.ilr2.5_15"]  <- varImp2["clay_5_15","Importance.ilr2.5_15"]
varImp2["clay_0_5","Importance.ilr2.15_30"] <- varImp2["clay_15_30","Importance.ilr2.15_30"]
varImp2["sand_0_5","Importance.ilr2.5_15"]  <- varImp2["sand_5_15","Importance.ilr2.5_15"]
varImp2["sand_0_5","Importance.ilr2.15_30"] <- varImp2["sand_15_30","Importance.ilr2.15_30"]

dim(varImp2)
varImp2 <- varImp2[1:57,]
varImp2[56:57,]$covariates <- c("clay","sand")
rownames(varImp2) <- varImp2$covariates

library(ggplot2)
library(gplots)
library(tidyverse)
heatmap(t(as.matrix(varImp2[,2:7])),
        cexRow = 0.6, 
        cexCol = 0.8,
        margins = c(10, 6))

mypal <-viridis_pal(option = "A",direction=-1)(15)

rm(DF, extrHOC, extrPOC, extrROC, ilr.properties, rf.0_5.ilr1, rf.0_5.ilr2, rf.15_30.ilr1, rf.15_30.ilr2, rf.5_15.ilr1, rf.5_15.ilr2)
rm(VarImp_ilr1_0_5, VarImp_ilr1_15_30, VarImp_ilr1_5_15, VarImp_ilr2_0_5, VarImp_ilr2_15_30, VarImp_ilr2_5_15)
rm(wh_heatmap)

heatmap.2(t(as.matrix(varImp2[,c(2,4,6,3,5,7)])),dendrogram = "none",
          Colv = FALSE,Rowv = FALSE,
        cexRow = 0.8, notecol="transparent", tracecol = "transparent", 
        cexCol = 0.8,scale="none",col=mypal,
        margins = c(10, 6),
        key.title="Variable importance")
#nrow(t(as.matrix(varImp2[,2:7])))

### Reorder covariates by soil-forming factor
# myorder <-c("Clim_ADM", "Clim_EPA", "Clim_EPI", "Clim_EPX", 
#   "Clim_Prescott", "Clim_PTA", "Clim_PTI", "Clim_PTS1", "Clim_PTS2", 
#   "Clim_PTX", "Clim_RSM", "Clim_TNM", "Clim_TRA", "Clim_TXM", "Clim_WDA", 
#   "NDVI_mean_Q1", "NDVI_mean_Q2", "NDVI_mean_Q3", "NDVI_mean_Q4",
#   "Veg_FC_Max_BS", "Veg_FC_Max_NPV", "Veg_FC_Max_PV", "Veg_FC_Mean_BS", 
#   "Veg_FC_Mean_NPV", "Veg_FC_Mean_PV", "Veg_FC_Min_BS", "Veg_FC_Min_NPV", 
#   "Veg_FC_Min_PV", "Veg_FC_SD_BS", "Veg_FC_SD_NPV", "Veg_FC_SD_PV", 
#   "Veg_FPAR_Max", "Veg_FPAR_Mean", "Veg_FPAR_Median", "Veg_FPAR_Min", 
#   "Veg_LandCoverTrend_evi_mean", "Veg_Persistant_green_Veg",
#   "relief_dems_3s_mosaic1","relief_slope_perc",
#   "Relief_mrrtf_3s", "relief_mrvbf_3s_mosaic", 
#   "relief_plan_curvature_3s", "relief_profile_curvature_3", 
#   "relief_roughness", "relief_twi_3s",
#   "PM_Gravity", 
#   "PM_radmap_v4_2019_filtered_dose_GAPFilled",
#   "PM_radmap_v4_2019_filtered_pctk_GAPFilled", 
#   "PM_radmap_v4_2019_filtered_ppmt_GAPFilled", 
#   "PM_radmap_v4_2019_filtered_ppmu_GAPFilled", 
#   "PM_radmap_v4_2019_ratio_tk_GAPFilled", 
#   "PM_radmap_v4_2019_ratio_u2t_GAPFilled", 
#   "PM_radmap_v4_2019_ratio_uk_GAPFilled", 
#   "PM_radmap_v4_2019_ratio_ut_GAPFilled", 
#   "PM_Weathering_Index",
#   "clay","sand")

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

heatmap.2(t(as.matrix(varImp3[,c(2,4,6,3,5,7)])), dendrogram = "none",
          Colv = FALSE,Rowv = FALSE,
          cexRow = 0.8, notecol="transparent", tracecol = "transparent", 
          cexCol = 0.8, scale="none",col=mypal,
          margins = c(12,12),
          key.title="Variable importance")

rm(covar.names, covar.depth.specific, covar.order,mypal, properties)
write.csv(varImp3, file ="varImp_IlrVars3depths_REDOsameSeeds.csv" )

### compositions done correctly?
library(compositions)
ilrBase(D=3)
head(ilr(x = SOCFr[,c("ROC.0.5","HOC.0.5","POC.0.5")]))


# ### Variable Importance individual models -------------------------------


### 0-5 cm

### HOC
property <- "HOC.0.5"
covar.depth.specific <- c('clay_0_5','sand_0_5')
covars <- c(covar.names, covar.depth.specific)
HOC.0_5.df <-  SOCFr[complete.cases(SOCFr[,c(property,covars)]),c(property,covars)]

### Calibrate model
form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))

set.seed(2021)
HOC.0_5.rf<- ranger(form, 
                    data = HOC.0_5.df,
                    num.trees = 5000,
                    importance = 'permutation', 
                    scale.permutation.importance = TRUE)
HOC.0_5.rf$variable.importance

### Plot varImp
DF<-data.frame(covariates=names(HOC.0_5.rf$variable.importance),
               HOC.0_5.Importance=as.vector(HOC.0_5.rf$variable.importance))
DF %>% dplyr::arrange(., desc(HOC.0_5.Importance)) %>% as.data.frame() %>%
  ggplot(., aes(x=HOC.0_5.Importance, y= covariates, fill=HOC.0_5.Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("HOC (0-5 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")
# DF[1:15,]
# DF$covariates <- factor(DF$covariates, levels = DF$covariates)

ggplot(DF, aes(x=Importance, y= covariates, fill=Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("ilr1 (0-5 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")

### POC
property <- "POC.0.5"
covar.depth.specific <- c('clay_0_5','sand_0_5')
covars <- c(covar.names, covar.depth.specific)
POC.0_5.df <-  SOCFr[complete.cases(SOCFr[,c(property,covars)]),c(property,covars)]

### Calibrate model
form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))

set.seed(2046)
POC.0_5.rf<- ranger(form, 
                    data = POC.0_5.df,
                    num.trees = 5000,
                    importance = 'permutation', 
                    scale.permutation.importance = TRUE)
POC.0_5.rf$variable.importance


### Plot varImp
DF.poc <-data.frame(covariates=names(POC.0_5.rf$variable.importance),
                    POC.0_5.Importance=as.vector(POC.0_5.rf$variable.importance))
DF <- merge(DF, DF.poc, by="covariates")
rm(DF.poc)

DF %>% dplyr::arrange(., desc(POC.0_5.Importance)) %>% as.data.frame() %>% 
  ggplot(., aes(x=POC.0_5.Importance, y= covariates, fill=POC.0_5.Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("POC (0-5 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")


### ROC
property <- "ROC.0.5"
covar.depth.specific <- c('clay_0_5','sand_0_5')
covars <- c(covar.names, covar.depth.specific)
ROC.0_5.df <-  SOCFr[complete.cases(SOCFr[,c(property,covars)]),c(property,covars)]

### Calibrate model
form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))

set.seed(2046)
ROC.0_5.rf<- ranger(form, 
                    data = ROC.0_5.df,
                    num.trees = 5000,
                    importance = 'permutation', 
                    scale.permutation.importance = TRUE)
ROC.0_5.rf$variable.importance


### Plot varImp
DF.ROC <-data.frame(covariates=names(ROC.0_5.rf$variable.importance),
                    ROC.0_5.Importance=as.vector(ROC.0_5.rf$variable.importance))
DF <- merge(DF, DF.ROC, by="covariates")
rm(DF.ROC)

DF %>% dplyr::arrange(., desc(ROC.0_5.Importance)) %>% as.data.frame() %>% 
  ggplot(., aes(x=ROC.0_5.Importance, y= covariates, fill=ROC.0_5.Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("ROC (0-5 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")

########################################################################################################################

### 5-15 cm

### HOC
property <- "HOC.5.15"
covar.depth.specific <- c('clay_5_15','sand_5_15')
covars <- c(covar.names, covar.depth.specific)
HOC.5_15.df <-  SOCFr[complete.cases(SOCFr[,c(property,covars)]),c(property,covars)]

### Calibrate model
form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))

set.seed(2046)
HOC.5_15.rf<- ranger(form, 
                    data = HOC.5_15.df,
                    num.trees = 5000,
                    importance = 'permutation', 
                    scale.permutation.importance = TRUE)
HOC.5_15.rf$variable.importance

### Plot varImp
DF$covariates[1] <- "clay"
DF$covariates[39] <- "sand"

DF.hoc <-data.frame(covariates=names(HOC.5_15.rf$variable.importance),
                    HOC.5_15.Importance=as.vector(HOC.5_15.rf$variable.importance))
DF.hoc$covariates[DF.hoc$covariates %in% c("clay_5_15", "sand_5_15")] <- c("clay", "sand")
DF <- merge(DF, DF.hoc, by="covariates")
rm(DF.hoc)

DF %>% dplyr::arrange(., desc(HOC.5_15.Importance)) %>% as.data.frame() %>%
  ggplot(., aes(x=HOC.5_15.Importance, y= covariates, fill=HOC.5_15.Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("HOC (5-15 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")


### POC
property <- "POC.5.15"
covar.depth.specific <- c('clay_5_15','sand_5_15')
covars <- c(covar.names, covar.depth.specific)
POC.5_15.df <-  SOCFr[complete.cases(SOCFr[,c(property,covars)]),c(property,covars)]

### Calibrate model
form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))

set.seed(2046)
POC.5_15.rf<- ranger(form, 
                    data = POC.5_15.df,
                    num.trees = 5000,
                    importance = 'permutation', 
                    scale.permutation.importance = TRUE)
POC.5_15.rf$variable.importance


### Plot varImp
DF.poc <-data.frame(covariates=names(POC.5_15.rf$variable.importance),
                    POC.5_15.Importance=as.vector(POC.5_15.rf$variable.importance))
DF.poc$covariates[DF.poc$covariates %in% c("clay_5_15", "sand_5_15")] <- c("clay", "sand")
DF <- merge(DF, DF.poc, by="covariates")
rm(DF.poc)

DF %>% dplyr::arrange(., desc(POC.5_15.Importance)) %>% as.data.frame() %>% 
  ggplot(., aes(x=POC.5_15.Importance, y= covariates, fill=POC.5_15.Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("POC (5-15 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")


### ROC
property <- "ROC.5.15"
covar.depth.specific <- c('clay_5_15','sand_5_15')
covars <- c(covar.names, covar.depth.specific)
ROC.5_15.df <-  SOCFr[complete.cases(SOCFr[,c(property,covars)]),c(property,covars)]

### Calibrate model
form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))

set.seed(2046)
ROC.5_15.rf<- ranger(form, 
                     data = ROC.5_15.df,
                     num.trees = 5000,
                     importance = 'permutation', 
                     scale.permutation.importance = TRUE)
ROC.5_15.rf$variable.importance

### Plot varImp
DF.roc <-data.frame(covariates=names(ROC.5_15.rf$variable.importance),
                    ROC.5_15.Importance=as.vector(ROC.5_15.rf$variable.importance))
DF.roc$covariates[DF.roc$covariates %in% c("clay_5_15", "sand_5_15")] <- c("clay", "sand")
DF <- merge(DF, DF.roc, by="covariates")
rm(DF.roc)

DF %>% dplyr::arrange(., desc(ROC.5_15.Importance)) %>% as.data.frame() %>% 
  ggplot(., aes(x=ROC.5_15.Importance, y= covariates, fill=ROC.5_15.Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("ROC (5-15 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")


########################################################################################################################

### 15-30 cm

### HOC
property <- "HOC.15.30"
covar.depth.specific <- c('clay_15_30','sand_15_30')
covars <- c(covar.names, covar.depth.specific)
HOC.15_30.df <-  SOCFr[complete.cases(SOCFr[,c(property,covars)]),c(property,covars)]

### Calibrate model
form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))

set.seed(2046)
HOC.15_30.rf<- ranger(form, 
                      data = HOC.15_30.df,
                      num.trees = 5000,
                      importance = 'permutation', 
                      scale.permutation.importance = TRUE)
HOC.15_30.rf$variable.importance

### Plot varImp
DF.hoc <-data.frame(covariates=names(HOC.15_30.rf$variable.importance),
                    HOC.15_30.Importance=as.vector(HOC.15_30.rf$variable.importance))
DF.hoc$covariates[DF.hoc$covariates %in% c("clay_15_30", "sand_15_30")] <- c("clay", "sand")
DF <- merge(DF, DF.hoc, by="covariates")
rm(DF.hoc)

DF %>% dplyr::arrange(., desc(HOC.15_30.Importance)) %>% as.data.frame() %>%
  ggplot(., aes(x=HOC.15_30.Importance, y= covariates, fill=HOC.15_30.Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("HOC (15-30 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")

### POC
property <- "POC.15.30"
covar.depth.specific <- c('clay_15_30','sand_15_30')
covars <- c(covar.names, covar.depth.specific)
POC.15_30.df <-  SOCFr[complete.cases(SOCFr[,c(property,covars)]),c(property,covars)]

### Calibrate model
form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))

set.seed(2046)
POC.15_30.rf<- ranger(form, 
                     data = POC.15_30.df,
                     num.trees = 5000,
                     importance = 'permutation', 
                     scale.permutation.importance = TRUE)
POC.15_30.rf$variable.importance

### Plot varImp
DF.poc <-data.frame(covariates=names(POC.15_30.rf$variable.importance),
                    POC.15_30.Importance=as.vector(POC.15_30.rf$variable.importance))
DF.poc$covariates[DF.poc$covariates %in% c("clay_15_30", "sand_15_30")] <- c("clay", "sand")
DF <- merge(DF, DF.poc, by="covariates")
rm(DF.poc)

DF %>% dplyr::arrange(., desc(POC.15_30.Importance)) %>% as.data.frame() %>% 
  ggplot(., aes(x=POC.15_30.Importance, y= covariates, fill=POC.15_30.Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("POC (15-30 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")

### ROC
property <- "ROC.15.30"
covar.depth.specific <- c('clay_15_30','sand_15_30')
covars <- c(covar.names, covar.depth.specific)
ROC.15_30.df <-  SOCFr[complete.cases(SOCFr[,c(property,covars)]),c(property,covars)]

### Calibrate model
form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))

set.seed(2046)
ROC.15_30.rf<- ranger(form, 
                      data = ROC.15_30.df,
                      num.trees = 5000,
                      importance = 'permutation', 
                      scale.permutation.importance = TRUE)
ROC.15_30.rf$variable.importance

### Plot varImp
DF.roc <-data.frame(covariates=names(ROC.15_30.rf$variable.importance),
                    ROC.15_30.Importance=as.vector(ROC.15_30.rf$variable.importance))
DF.roc$covariates[DF.roc$covariates %in% c("clay_15_30", "sand_15_30")] <- c("clay", "sand")
DF <- merge(DF, DF.roc, by="covariates")
rm(DF.roc)

DF %>% dplyr::arrange(., desc(ROC.15_30.Importance)) %>% as.data.frame() %>% 
  ggplot(., aes(x=ROC.15_30.Importance, y= covariates, fill=ROC.15_30.Importance))+ 
  geom_bar(stat="identity", position="dodge") + 
  xlab("Covariate Importance") +
  ylab("") +
  ggtitle("ROC (15-30 cm)")+
  guides(fill=F)+
  scale_fill_gradient(low="gray80", high="blue")


###################################################################################################

#### Create Heatmap figure
### Reorder covariates by soil-forming factor
library(tidyverse)
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

varImp <- dplyr::left_join(data.frame(covariates=myorder),DF, by="covariates")
rownames(varImp) <- varImp$covariates

### Reorder columns by SOC fraction
varImp <- varImp[,c("covariates",
                    "HOC.0_5.Importance","HOC.5_15.Importance","HOC.15_30.Importance",
                    "ROC.0_5.Importance","ROC.5_15.Importance","ROC.15_30.Importance",
                    "POC.0_5.Importance","POC.5_15.Importance","POC.15_30.Importance")]
library(ggplot2)
library(gplots)
library(tidyverse)
heatmap(t(as.matrix(varImp[,2:ncol(varImp)])),
        cexRow = 0.6, 
        cexCol = 0.8,
        margins = c(10, 6))

mypal <-viridis_pal(option = "A",direction=-1)(20)

heatmap.2(t(as.matrix(varImp[,2:ncol(varImp)])),
          dendrogram = "none",
          Colv = FALSE,Rowv = FALSE,
          cexRow = 0.8, notecol="transparent", tracecol = "transparent", 
          cexCol = 0.8,scale="none",col=mypal,
          margins = c(10, 6),
          key.title="Variable importance")


heatmap.2(t(as.matrix(varImp[,c("POC.0_5.Importance","POC.5_15.Importance","POC.15_30.Importance")])), 
          dendrogram = "none",
          Colv = FALSE, Rowv = FALSE,
          cexRow = 0.8, notecol="transparent", tracecol = "transparent", 
          cexCol = 0.8, scale="none",col=mypal,
          margins = c(10, 6),
          key.title="Variable importance")

### End of the script