#################################################################################
# 4_SOCfr_RF_cv_ilr_HPR

# Project: SOC fraction maps for TERN
# Objective: Calculate validation statistics for ilr-models, 
# Calculate PICP for several confidence levels
# keep the predictions to check spatial structure of the residuals
# Date: 04/11/2021
# Author: Mercedes Roman

### Load packages
require(ranger)
require(raster)
library(readr)
library(caret)
library(scales)
library(viridis)
library(ggplot2)
library(viridisLite)
library(ggmap)


### Set input and output directories
HomeDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/for_Ross/"
InputDir  <- paste0(HomeDir,"Output_files/1_Output/")
OutDir  <- paste0(HomeDir,"Output_files/4_Output/")
scriptDir <- paste0(HomeDir,"scripts/") 

setwd(HomeDir)
source('./scripts/2_make_cv_RF_ilr.R')


# 1. Prepare calibration data ------------------------------------------------

load(paste0(InputDir,"SOCfr_Prop.GSM_03092021.RData"))

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

### Bring covariate data
extrHOC <- read.csv(paste0(HomeDir,"Output_files/2_Output/extrHOC.csv"))

### Change names
names(HOCsoildat)[2:4] <- c("HOC.0.5", "HOC.5.15", "HOC.15.30")
names(POCsoildat)[2:4] <- c("POC.0.5", "POC.5.15", "POC.15.30")
names(ROCsoildat)[2:4] <- c("ROC.0.5", "ROC.5.15", "ROC.15.30")

### Bind dataframes
SOCFr <- cbind(HOCsoildat[,1:4],POCsoildat[,2:4],ROCsoildat[,2:4],HOCsoildat[,c(5:7,9)],extrHOC[,2:ncol(extrHOC)])
SOCFr_extr <- cbind(HOCsoildat[,1:4],POCsoildat[,2:4],ROCsoildat[,2:4],HOCsoildat[,c(5:7,9)])
str(SOCFr_extr)

### Plot the locations
library(ggmap)
AusMap <- get_stamenmap(bbox = c(left=110, bottom=-45, right=157, top=-8),
                        maptype="toner-lite", zoom=4,
                        source="stamen", crop=TRUE)

ggmap(AusMap) +
  geom_point(aes(y = Lat_WGS84, x =Long_WGS84, alpha=0.1), color="#fb9f0f", data = SOCFr_extr)

setwd(OutDir)
write.csv(SOCFr_extr, file="SOCFr_extr.csv")
rm(extrHOC)

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


# 2. Cross-validation of ilr-models  ------------------------------------------------------------

### What this loop will do:
### 1. Fit rf models for the two ilr variables, 
###    for a soil depths and a particular order of the three SOC fractions as input to the ilr transformation.
### 2. Cross-validation (10-fold) and calculate statistics on the ilr and backtransformed variables.
### 3. Plot the cross-validation results
### 4. Calibrate RF models with all data
### 5. Sample from the conditional probability distributions of ilr1 and ilr2
### 6. Estimate prediction interval limits and calculate PICP
### 7. Return the cross-validation data and RF models

### For loop, changing soil depth

depths <- c("0-5 cm","5-15 cm","15-30 cm")

dfilenames <- c("0_5","5_15","15_30")

covar.depth.specific.list <- list(c("clay_0_5", "sand_0_5"),
                                  c("clay_5_15", "sand_5_15"),
                                  c("clay_15_30", "sand_15_30"))

properties.list <- list(c("HOC.0.5","POC.0.5","ROC.0.5"),
                        c("HOC.5.15","POC.5.15","ROC.5.15"),
                        c("HOC.15.30","POC.15.30","ROC.15.30"))

orderProps <- c("HOC","POC","ROC")
combi.name <- "HPR"
some.seeds <- c(1991,1984,1946,1948,2013,2016)

setwd(OutDir)

output.list <- list("0-5 cm"=list(), "5-15 cm"=list(), "15-30 cm"=list())
picp90.list <- list("0-5 cm"=list(), "5-15 cm"=list(), "15-30 cm"=list())

for(d in 1:length(depths)) {
  
  print(paste0("Cross-validation for depth ",depths[[d]]))

  ### 1.Perform cross-validation
  rf.ilr.cv <- make_cv_RF_ilr_perc(data= SOCFr,     ### Dataframe with covariates and SOC fractions
                                  properties= properties.list[[d]], ### Vector with the names of SOC fractions
                                  orderProps= orderProps,### Vector with HOC, POC, ROC, in the same order as in "properties"
                                  covars= c(covar.names, covar.depth.specific.list[[d]]),      ### Name of covariates
                                  nfolds= 10,     ### Number of k-folds for cross-validation
                                  thisseed = some.seeds[[d]],    ### Number for set.seed, to have the same partition into train and test, in case we test a different set of covariates, etc.
                                  ntreefinal= 500,  ### Number of trees for the final quantile regression forest calibrated with all data (for mapping or checking variable importance with permutation)
                                  nsamp= 500,       ### Number of samples to take from the distribution of ilr1 and ilr2, used later for back-transformation
                                  ### Vector with confidence levels for validating the prediction intervals with PICP
                                  Conf.Levels = c(5,10,20,30,40,50,60,70,75,80,85,90,95,97.5,99))
  
  output.list[[d]] = rf.ilr.cv
  
  ### 2.Validation statistics table
  tab.res <- data.frame(matrix(data=NA, nrow = 5, ncol = 8))
  tab.res[,3] <- c("ilr1","ilr2", "HOC","POC","ROC")
  tab.res[,2] <- depths[[d]]
  tab.res[,1] <- "HPR"
  colnames(tab.res) <- c("Combination","Depth",'SOC fraction', 'ME', 'RMSE', 'r2', 'R2', 'rhoC')
  
  ### Extract table with CV results
  testTable <- rf.ilr.cv[[3]]
  
  tab.res[1,][4:8] <- eval(testTable$ilr1, testTable$pred.ilr1, obj = 'quant')[1:5]
  tab.res[2,][4:8] <- eval(testTable$ilr2, testTable$pred.ilr2, obj = 'quant')[1:5]
  
  HOC.index <- grep(names(testTable),pattern = "HOC.")
  POC.index <- grep(names(testTable),pattern = "POC.")
  ROC.index <- grep(names(testTable),pattern = "ROC.")
  
  tab.res[3,][4:8] <- eval(testTable[,HOC.index], testTable$pred.HOC, obj = 'quant')[1:5]
  tab.res[4,][4:8] <- eval(testTable[,POC.index], testTable$pred.POC, obj = 'quant')[1:5]
  tab.res[5,][4:8] <- eval(testTable[,ROC.index], testTable$pred.ROC, obj = 'quant')[1:5]
  tab.res
  
  ### Write the table into a file
  write.csv(tab.res, file=paste0("./TableCV_ilr_",dfilenames[[d]],"_",combi.name,".csv"))
  
  ### Make cross-validation ggplots
  p4 <- ggplot(testTable, aes(ilr1, pred.ilr1)) + geom_hex(binwidth = c(0.1,0.1), aes(colour = ..count..)) +
    scale_fill_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    scale_colour_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    guides(fill = guide_colourbar(title = "Count")) +
    geom_abline(intercept = 0, slope = 1, color="steelblue3", size=1)+
    xlab("Observed ilr1")+
    ylab("Predicted ilr1")+
    ggtitle(paste0("ilr1 (",depths[[d]],")"))
  
  p5 <- ggplot(testTable, aes(ilr2, pred.ilr2)) + geom_hex(binwidth = c(0.1,0.1), aes(colour = ..count..)) +
    scale_fill_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    scale_colour_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    guides(fill = guide_colourbar(title = "Count")) +
    geom_abline(intercept = 0, slope = 1, color="steelblue3", size=1)+
    xlab("Observed ilr2")+
    ylab("Predicted ilr2")+
    ggtitle(paste0("ilr2 (",depths[[d]],")"))
  
  ### List of variables
  #aes.list <- paste0(c("HOC.", "POC.", "ROC."), gsub(pattern="_", replacement = ".", x = dfilenames))
  aes.list <- names(testTable)[1:3]
  
  p1 <- ggplot(testTable, aes_string(aes.list[1], "pred.HOC"))+
    geom_hex(binwidth = c(1,1), aes(colour = ..count..)) +
    scale_fill_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    scale_colour_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    guides(fill = guide_colourbar(title = "Count")) +
    geom_abline(intercept = 0, slope = 1, color="steelblue3", size=1)+
    xlab("Observed HOC (%)")+
    ylab("Predicted HOC (%)")+
    ggtitle(paste0("HOC (",depths[[d]],")"))+
    xlim(0,100)+
    ylim(0,100)
  
  p2 <- ggplot(testTable, aes_string(aes.list[2], "pred.POC"))+
    geom_hex(binwidth = c(1,1), aes(colour = ..count..)) +
    scale_fill_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    scale_colour_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    guides(fill = guide_colourbar(title = "Count")) +
    geom_abline(intercept = 0, slope = 1, color="steelblue3", size=1)+
    xlab("Observed POC (%)")+
    ylab("Predicted POC (%)")+
    ggtitle(paste0("POC (",depths[[d]],")"))+
    xlim(0,100)+
    ylim(0,100)
  
  p3 <- ggplot(testTable, aes_string(aes.list[3], "pred.ROC"))+
    geom_hex(binwidth = c(1,1), aes(colour = ..count..)) +
    scale_fill_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    scale_colour_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    guides(fill = guide_colourbar(title = "Count")) +
    geom_abline(intercept = 0, slope = 1, color="steelblue3", size=1)+
    xlab("Observed ROC (%)")+
    ylab("Predicted ROC (%)")+
    ggtitle(paste0("ROC (",depths[[d]],")"))+
    xlim(0,100)+
    ylim(0,100)
  
  pdf(paste0("CVplots.",dfilenames[[d]],"_",combi.name,".pdf"),width = 14,height = 8)
  print(gridExtra::grid.arrange(p1,p2,p3,p4,p5,nrow=2))
  dev.off()
  
  
  ### 3. Create PICP plot
  
  Conf.Levels = c(5,10,20,30,40,50,60,70,75,80,85,90,95,97.5,99) # confidence level
  
  bMat.HOC <- matrix(NA, nrow = nrow(testTable), ncol = length(Conf.Levels))
  for (i in 1:ncol(bMat.HOC)) {
    bMat.HOC[, i] <- as.numeric(testTable[,HOC.index] <= rf.ilr.cv[[4]][, i] & testTable[,HOC.index]  >= rf.ilr.cv[[7]][, i])
  }
  ### Here I save the PICP for 90% confidence level, but it could be as well the whole vector of PICP values
  picp90.list[[d]] [[1]] <- sum(bMat.HOC[,which(Conf.Levels==90)],na.rm=TRUE)/nrow(bMat.HOC)*100
  
  # make plot
  pdf(paste0("PICP.",dfilenames[[d]],".HOC.pdf"),width = 6, height = 6)
  plot(Conf.Levels, colSums(bMat.HOC)/nrow(bMat.HOC) *100, pch=19, col="black", ylab="PICP (%)", xlab="Confidence level (%))", 
       main=paste0("HOC (",depths[[d]],")"))
  abline(0,1, col="red", lty=2)
  dev.off()
  
  bMat.POC <- matrix(NA, nrow = nrow(testTable), ncol = length(Conf.Levels))
  for (i in 1:ncol(bMat.POC)) {
    bMat.POC[, i] <- as.numeric(testTable[,POC.index]  <= rf.ilr.cv[[5]][, i] & testTable[,POC.index] >= rf.ilr.cv[[8]][, i])
  }
  picp90.list[[d]] [[2]] <- sum(bMat.POC[,which(Conf.Levels==90)],na.rm=TRUE)/nrow(bMat.POC)*100
  
  
  # make plot
  pdf(paste0("PICP.",dfilenames[[d]],".POC.pdf"),width = 6, height = 6)
  plot(Conf.Levels, colSums(bMat.POC)/nrow(bMat.POC) *100, pch=19, col="black", ylab="PICP (%)", xlab="Confidence level (%))", 
       main=paste0("POC (",depths[[d]],")"))
  abline(0,1, col="red", lty=2)
  dev.off()
  
  bMat.ROC <- matrix(NA, nrow = nrow(testTable), ncol = length(Conf.Levels))
  for (i in 1:ncol(bMat.ROC)) {
    bMat.ROC[, i] <- as.numeric(testTable[,ROC.index]  <= rf.ilr.cv[[6]][, i] & testTable[,ROC.index] >= rf.ilr.cv[[9]][, i])
  }
  picp90.list[[d]] [[3]] <- sum(bMat.ROC[,which(Conf.Levels==90)],na.rm=TRUE)/nrow(bMat.ROC)*100
  
  # make plot
  pdf(paste0("PICP.",dfilenames[[d]],".ROC.pdf"),width = 6, height = 6)
  plot(Conf.Levels, colSums(bMat.ROC)/nrow(bMat.ROC) *100, pch=19, col="black", ylab="PICP (%)", xlab="Confidence level (%))", 
       main=paste0("ROC (",depths[[d]],")"))
  abline(0,1, col="red", lty=2)
  dev.off()
  
}

### Clean
rm( "aes.list","bMat.HOC","bMat.POC","bMat.ROC",
    "covar.depth.specific.list","d","depths","dfilenames","HOC.index","i",
    "p1","p2","p3","p4","p5","POC.index","properties.list","rf.ilr.cv","ROC.index",
    "some.seeds","tab.res","testTable")

save.image("4_SOCfr_RF_cv_ilr_HPR.RData")

### Depth 0-5 cm
rf.ilr1.0_5 <- output.list$`0-5 cm`[[1]]
rf.ilr2.0_5 <- output.list$`0-5 cm`[[2]]

### Depth 5-15 cm
rf.ilr1.5_15 <- output.list$`5-15 cm`[[1]]
rf.ilr2.5_15 <- output.list$`5-15 cm`[[2]]

### Depth 15-30 cm
rf.ilr1.15_30 <- output.list$`15-30 cm`[[1]]
rf.ilr2.15_30 <- output.list$`15-30 cm`[[2]]

### Save into different RData
save(rf.ilr1.0_5,rf.ilr2.0_5, file = "ilr_models.HPR.0_5.RData")
save(rf.ilr1.5_15,rf.ilr2.5_15, file = "ilr_models.HPR.5_15.RData")
save(rf.ilr1.15_30,rf.ilr2.15_30, file = "ilr_models.HPR.15_30.RData")


# ### PICP for ilr variables ----------------------------------------------

### I forgot to compute this before, so I add it here now:

picp90.ilr.list <- list("0-5 cm"=list(), "5-15 cm"=list(), "15-30 cm"=list())

for(d in 1:length(depths)) {
  
  print(paste0("Cross-validation for depth ",depths[[d]]))
  
  ### 1.Perform cross-validation
  rf.ilr.cv <- make_cv_RF_ilr_PICP(data= SOCFr,     ### Dataframe with covariates and SOC fractions
                                   properties= properties.list[[d]], ### Vector with the names of SOC fractions
                                   orderProps= orderProps,### Vector with HOC, POC, ROC, in the same order as in "properties"
                                   covars= c(covar.names, covar.depth.specific.list[[d]]),      ### Name of covariates
                                   nfolds= 10,     ### Number of k-folds for cross-validation
                                   thisseed = some.seeds[[d]]    ### Number for set.seed, to have the same partition into train and test, in case we test a different set of covariates, etc.
  )
  output.list[[d]] = rf.ilr.cv
  
  ### 2.Validation statistics table
  tab.res <- data.frame(matrix(data=NA, nrow = 2, ncol = 4))
  tab.res[,3] <- c("ilr1","ilr2")
  tab.res[,2] <- depths[[d]]
  tab.res[,1] <- "HPR"
  colnames(tab.res) <- c("Combination","Depth",'SOC fraction', 'PICP')
  
  ### Extract table with CV results
  testTable <- rf.ilr.cv
  
  ### 3. Calculate PICP
  
  #Conf.Levels = 90 # confidence level
  bMat.ilr1 <- as.numeric(testTable$ilr1 <= testTable$pred.ilr1.UPL & testTable$ilr1  >= rf.ilr.cv$pred.ilr1.LPL)
  picp <- sum(bMat.ilr1,na.rm=TRUE)/length(bMat.ilr1)*100
  tab.res[1,'PICP'] <- picp
  
  bMat.ilr2 <- as.numeric(testTable$ilr2 <= testTable$pred.ilr2.UPL & testTable$ilr2  >= rf.ilr.cv$pred.ilr2.LPL)
  picp <- sum(bMat.ilr2,na.rm=TRUE)/length(bMat.ilr2)*100
  tab.res[2,'PICP'] <- picp
  
  picp90.ilr.list[[d]] <- tab.res
  
}

### End of script