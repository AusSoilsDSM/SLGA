#################################################################################
# Objective: Calculate SOC fraction density and their uncertainties, as well as Sobol indices (Global Sensitivity analysis)
# Date: 09/06/2022
# Author: Mercedes Roman
### personal email: mercetadzio@gmail.com
### University of Sydney: mercedes.romandobarco@sydney.edu.au
# Project: SOC fraction maps for TERN
# map at 90 m

### Note: this script is as an example for the essential calculations. 
### The actual computations were not done at the University of Sydney (Artemis HPC), but in the supercomputer of CSIRO
### with an script adapted by Brendan Malone 
### In this example, the depth is 0-5 cm. some changes awould be needed for other depths (line 45)

### Load packages
library(sp)
library(raster)
library(rgdal)
library(ranger)
library(compositions)
library(parallel)
library(doParallel)
library(foreach)
require(truncnorm)
library(sensitivity)
library(rasterVis)

### Set the parameters and bring the data (for Artemis)

### Number of samples to estimate the prediction interval
nsamp <- 500

### Bring argument (tile number) from the PBS script
### this needs to be adapted
args <- commandArgs(trailingOnly = T)
i <- as.numeric(args[1])
print(paste0("row tile ",i))

### Job array
job_array <- "j1"

### Depth we are predicting for and parameters to calculate SOC fraction stocks
### in this example, it is for depth 0-5 cm
GSM.layer <- 1

depths <-  c("0_5", "5_15", "15_30")
#thickness.gsm <- c(5,10,15)
depth <- depths[[GSM.layer]]
#thickness <- thickness.gsm[[GSM.layer]]
orderProps <- c("HOC","POC","ROC")
combi.name <- "HPR"

# path to the covariates raster
InputDir    <-"/project/RDS-FSC-SoilBiodSOC2019-RW/SOCfractions/Input/"
OutDirTiles <-paste0("/project/RDS-FSC-SoilBiodSOC2019-RW/SOCfractions/Output/SOC_stocks_PI/",depth,"/",job_array,"/")

# list all the raster files available
setwd(InputDir)
#covariates.dir <- "R:/PRJ-SoilBiodSOC2019/Covariates/Australia90m_filled/"
#setwd(covariates.dir)

# load all the rasters
list_ras <- list.files(pattern="tif")
covariates.stack <- stack(paste0(InputDir,list_ras))
#covariates.stack <- stack(paste0(covariates.dir,list_ras))
print(covariates.stack)

### Create tiles
# dimensions of the stack: 
# nrow = 40800, 
# ncol = 49200, 
# ncell = 2007360000, 
# make blocks
bs <- blockSize(covariates.stack, minblocks = 100, minrows = 4) ### 4 rows per tile, or 10200 tiles

### Make the same for columns manually, although we leave the last one just to make it fit with the last column number of the raster stack
### 1480 cells is ok. --> 1480/4=370
### We try 2005 cells (5 rows X 400 cols)
bc <- list()
bc$col <- seq(from=1, to=ncol(covariates.stack), by=400)
bc$ncol <- c(rep(400,length(bc$col)-1),(ncol(covariates.stack)-((length(bc$col)-1)*400))-1)
bc$n <- length(bc$col)

########################################################################################################

### Load models for SOC fractions (% TOC)
load(paste0('ilr_models.',combi.name,'.',depth,'.RData'))
#load("C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/Output/6_ilr_HPR_map_quality_indices/ilr_models.HPR.0_5.RData")
model.ilr1 <- rf.ilr1.0_5
model.ilr2 <- rf.ilr2.0_5 
rm(rf.ilr1.0_5,rf.ilr2.0_5)

### Load the models for TOC -------------------------------------------------
### Load the models for TOC 
load(paste0("TOC_",depth,"_cm_model_evaluationCV.Rdata"))
model.TOC <- res.model[[1]]

### Load the models for Coarse fragments class probability ------------------------------
### Load the models for Coarse fragments class probability
load(paste0("cf_rf_probs_",depth,".RData"))
CFvol.model <- cf_rf_probs_0_5
rm(cf_rf_probs_0_5)

# Covariates --------------------------------------------------------------

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

covar.depth.specific.list <- list(c("clay_0_5", "sand_0_5"),
                                  c("clay_5_15", "sand_5_15"),
                                  c("clay_15_30", "sand_15_30"))
covar.depth <- covar.depth.specific.list[[GSM.layer]]
setdiff(covariates.stack, c(covar.names,covar.depth))

#### Bulk density for SOC stocks calculation - specific to this layer
bd.depth.specific.list <- list(c("BDW_000_005_EV","BDW_000_005_05", "BDW_000_005_95"),
                               c("BDW_005_015_EV","BDW_005_015_05", "BDW_005_015_95"),
                               c("BDW_015_030_EV","BDW_015_030_05", "BDW_015_030_95"))
bd.depth.specific <- bd.depth.specific.list[[GSM.layer]]


#############################################################################################################

### Start the mapping

### We are going to do a foreach to divide the block into tiles (bc)

cl <- makeCluster(10)   ### Create cluster
registerDoParallel(cl)
getDoParWorkers()

OutTiles <-foreach(j=1:length(bc$col), .packages=c("compositions","raster","ranger", "sensitivity", "truncnorm"),
                    .export = c("i","bs","bc","covariates.stack", "nsamp"))  %dopar% {
                      
### Crop the tile in question 
tile <- crop(covariates.stack, extent(covariates.stack,
                                      bs$row[[i]],
                                      bs$row[[i]]+ bs$nrows[[i]],
                                      bc$col[[j]],
                                      bc$col[[j]]+ bc$ncol[[j]]))
tile 
### Transform into a dataframe
tile.df <- as.data.frame(tile, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)
    
## Copy dataframe for outputs 
### We are only interested in the upper and lower prediction intervals of SOC FRACTION density ***
### for the tests we also calculated expected values (calculation of bulk density with the mean predictions)
### to compare with the mean predicted with the mean of all components
### First-order and total effects Sobol sensitivity indices are calculated and mapped - if possible
tile.df.out <- tile.df

### SOC fraction stock density - average and percentiles of simulations
tile.df.out$pred.HOCd.m<- NA
tile.df.out$pred.POCd.m<- NA
tile.df.out$pred.ROCd.m<- NA
tile.df.out$pred.HOCd.05p <- NA
tile.df.out$pred.POCd.05p <- NA
tile.df.out$pred.ROCd.05p <- NA
tile.df.out$pred.HOCd.95p <- NA
tile.df.out$pred.POCd.95p <- NA
tile.df.out$pred.ROCd.95p <- NA

### Sensitivity indices
### HOC stock density
tile.df.out$Sobol1.HOC.SOCfr <- NA
tile.df.out$Sobol1.HOC.TOC <- NA
tile.df.out$Sobol1.HOC.BD <- NA
tile.df.out$Sobol1.HOC.CF <- NA
tile.df.out$SobolT.HOC.SOCfr <- NA
tile.df.out$SobolT.HOC.TOC <- NA
tile.df.out$SobolT.HOC.BD <- NA
tile.df.out$SobolT.HOC.CF <- NA

### POC stock density
tile.df.out$Sobol1.POC.SOCfr <- NA
tile.df.out$Sobol1.POC.TOC <- NA
tile.df.out$Sobol1.POC.BD <- NA
tile.df.out$Sobol1.POC.CF <- NA
tile.df.out$SobolT.POC.SOCfr <- NA
tile.df.out$SobolT.POC.TOC <- NA
tile.df.out$SobolT.POC.BD <- NA
tile.df.out$SobolT.POC.CF <- NA

### ROC stock density
tile.df.out$Sobol1.ROC.SOCfr <- NA
tile.df.out$Sobol1.ROC.TOC <- NA
tile.df.out$Sobol1.ROC.BD <- NA
tile.df.out$Sobol1.ROC.CF <- NA
tile.df.out$SobolT.ROC.SOCfr <- NA
tile.df.out$SobolT.ROC.TOC <- NA
tile.df.out$SobolT.ROC.BD <- NA
tile.df.out$SobolT.ROC.CF <- NA

### Store in separate dataframe
Sobol.df <- tile.df.out[,grep(x=colnames(tile.df.out),pattern="Sobol")]
tile.df.out <- tile.df.out[,-grep(x=colnames(tile.df.out),pattern="Sobol")]

### Extract the index of the dataframe rows that are na/nan/Inf
df.na <- which(apply(tile.df, MARGIN = 1, FUN = function(x) {any(is.na(x))}))

### Different cases
### If there is no missing data
if(length(df.na)==0){

  print("no missing data in this tile")
  
  ### Sample nsamp values of ilr1 and ilr2 from the quantile distribution to estimate the prediction intervals of SOC fraction density
  pred.ilr1.sample <- predict(object=model.ilr1,
                              data = tile.df, 
                              type = "quantiles",
                              what = function(x) sample(x, nsamp, replace = TRUE))$predictions
  
  pred.ilr2.sample <- predict(object=model.ilr2,
                              data = tile.df, 
                              type = "quantiles",
                              what = function(x) sample(x, nsamp, replace = TRUE))$predictions
  
  ### Create array with both matrices
  ### Dimnames
  column.names <- paste0("Samp", c(1:nsamp))
  row.names <- paste0("row", c(1:nrow(tile.df)))
  matrix.names <- c("ilr1","ilr2")
  
  zSOC <- array(c(pred.ilr1.sample, pred.ilr2.sample),
             dim = c(nrow(tile.df), nsamp, 2),
             dimnames = list(row.names, column.names, matrix.names)) 
  
  ### Back-transform to HOC, POC, ROC (%) with apply over the array
  backtf <- function(x){Backt <- compositions::ilrInv(x); return(Backt)}
  Backt <- apply(X = zSOC, MARGIN = c(1,2), FUN = backtf)
 
  dimnames(Backt)[[1]] <- orderProps #c("HOC", "POC","ROC")
  ### Store in separate matrices
  HOC.sample <- Backt["HOC",,]
  POC.sample <- Backt["POC",,]
  ROC.sample <- Backt["ROC",,]
  
  ### Predicts TOC concentration in mg C / g soil
  ### Sample from the distribution of TOC (%)
  pred.TOC.sample <- predict(object=model.TOC,
                              data = tile.df, 
                              type = "quantiles",
                              what = function(x) sample(x, nsamp, replace = TRUE))$predictions
  
  ### Name dimensions
  column.names <- paste0("Samp", c(1:nsamp))
  row.names <- paste0("row", c(1:nrow(tile.df)))
  zTOC <- matrix(c(pred.TOC.sample*10), ### Transform from % to mg C/g soil
                 nrow = nrow(tile.df), ncol = nsamp,
                 dimnames = list(row.names, column.names))
  
  ### Predict the probability of the volumetric coarse fragments classes for each pixel (df row) 
  CFvol_prob <- predict(CFvol.model, data = tile.df)$predictions 
  CFvol_prob <- as.data.frame(CFvol_prob)
  colnames(CFvol_prob) <- c("X1p", "X2p", "X3p", "X4p", "X5p", "X6p")
  
  ### Sample from each class should be proportional to the class probability
  CFsamp.n <- round(nsamp*CFvol_prob, digits = 0)
  CFsamp.nTot <- rowSums(CFsamp.n, na.rm = TRUE)
  CFsamp.n[CFsamp.nTot<nsamp,] <- ceiling(nsamp*CFvol_prob[CFsamp.nTot<nsamp,])
  rm(CFsamp.nTot)
  
  ### function to generate sample from uniform distribution
  func.sample.vol <- function(x){
    #set.seed(1)
    x1 <- runif(n=x[["X1p"]], min = 0, max = 2) 
    x2 <- runif(n=x[["X2p"]], min = 2, max = 10) 
    x3 <- runif(n=x[["X3p"]], min = 10, max = 20) 
    x4 <- runif(n=x[["X4p"]], min = 20, max = 50) 
    x5 <- runif(n=x[["X5p"]], min = 50, max = 90) 
    x6 <- runif(n=x[["X6p"]], min = 90, max = 100) 
    out <- c(x1,x2,x3,x4,x5,x6)
    ### in case the ceiling made it > nsamp
    ### but shuffle to not favor necessarily the smaller classes
    out2 <- sample(out, size=nsamp, replace = FALSE)
    return(out2)
  }
  
  ### Apply the function over the rows
  cfvol <- apply(X =CFsamp.n, MARGIN = 1,  FUN = func.sample.vol)
  ### Transpose so we have nsamp cols and tile.df number of rows as rows
  cfvol <- t(cfvol)

  ### save into a matrix
  column.names <- paste0("Samp", c(1:nsamp))
  row.names <- paste0("row", c(1:nrow(tile.df)))
  zCF <- matrix(c(cfvol/100),
                nrow = nrow(tile.df), ncol = nsamp,
                dimnames = list(row.names, column.names))
 
  ### clean space
  rm(pred.TOC.sample,cfvol,CFsamp.n,pred.ilr1.sample,pred.ilr2.sample,CFvol_prob,row.names,column.names)
  
  ### Bulk density is in the dataframe tile.df
  bd.df <- tile.df[, bd.depth.specific]
  ### the standard deviation is calculated from the prediction interval
  bd.df$BD_sd <- (bd.df[,3]-bd.df[,2])/(2*1.64)
  colnames(bd.df) <- c("BD_mean", "BD_05", "BD_95", "BD_sd" )
  
  ### Generate truncated sample assuming a normal distribution
  
  ### Note, I think Brendan changed the limits (1 g/cm3 was too high, probably he left it at 0.5 or so?)
  func.sample.bd <- function(x){
    bdsamp <- truncnorm::rtruncnorm(n=nsamp, a=1, b=1.8, mean = x[["BD_mean"]], sd = x[["BD_sd"]])
    return(bdsamp)
  }
  
  ### Apply the function over the rows
  set.seed(122)
  bdsamp <- apply(X = bd.df, MARGIN = 1,  FUN = func.sample.bd)
  ### Transpose so we have nsamp cols and df.nrow as rows (same as with CF)
  bdsamp <- t(bdsamp)

  ### save into a matrix
  column.names <- paste0("Samp", c(1:nsamp))
  row.names <- paste0("row", c(1:nrow(tile.df)))
  zBD <- matrix(bdsamp, byrow = FALSE,
                nrow = nrow(tile.df), ncol = nsamp,
                dimnames = list(row.names, column.names))

  ### Calculate SOC density [exclude layer thickness (cm)]
  ###   Backt * (zTOC/100) * zBD * (1- zCF) 
  ###   SOC stock Mg C-fraction/ha cm = 
  ###   SOC fraction (%/100) x TOC content mg C/g<2mm soil x bulk density (g soil/cm3 soil) x
  ###   gravel correction (g<2mm/g soil) x  0.1 correction factor (Mg C cm2 / mg C ha)
  HOCs <- HOC.sample * zTOC * zBD * (1- zCF) * 0.1
  POCs <- POC.sample * zTOC * zBD * (1- zCF) * 0.1
  ROCs <- ROC.sample * zTOC * zBD * (1- zCF) * 0.1
  
  ### Calculate mean and percentiles and assign to dataframe with all rows
  tile.df.out$pred.HOCd.m  <- apply(X = HOCs, MARGIN = 1, FUN = function(x) mean(x, na.rm=TRUE))
  tile.df.out$pred.HOCd.95p <- apply(X = HOCs, MARGIN = 1, FUN = function(x) quantile(x, c(.95)))
  tile.df.out$pred.HOCd.05p <- apply(X = HOCs, MARGIN = 1, FUN = function(x) quantile(x, c(.05)))
  
  tile.df.out$pred.POCd.m  <- apply(X = POCs, MARGIN = 1, FUN = function(x) mean(x, na.rm=TRUE))
  tile.df.out$pred.POCd.95p <- apply(X = POCs, MARGIN = 1, FUN = function(x) quantile(x, c(.95)))
  tile.df.out$pred.POCd.05p <- apply(X = POCs, MARGIN = 1, FUN = function(x) quantile(x, c(.05)))
  
  tile.df.out$pred.ROCd.m  <- apply(X = ROCs, MARGIN = 1, FUN = function(x) mean(x, na.rm=TRUE))
  tile.df.out$pred.ROCd.95p <- apply(X = ROCs, MARGIN = 1, FUN = function(x) quantile(x, c(.95)))
  tile.df.out$pred.ROCd.05p <- apply(X = ROCs, MARGIN = 1, FUN = function(x) quantile(x, c(.05)))
  
 
  ### Assign values to raster files
  pred.HOCd.m<- setValues(tile[[1]], tile.df.out$pred.HOCd.m)
  names(pred.HOCd.m) <- paste0("HOCd.m.",depth)
  
  pred.POCd.m<- setValues(tile[[1]], tile.df.out$pred.POCd.m)
  names(pred.POCd.m) <- paste0("POCd.m.",depth)
  
  pred.ROCd.m<- setValues(tile[[1]], tile.df.out$pred.ROCd.m)
  names(pred.ROCd.m) <- paste0("ROCd.m.",depth)
  
  pred.HOCd.05p <- setValues(tile[[1]], tile.df.out$pred.HOCd.05p)
  names(pred.HOCd.05p) <- paste0("HOCd.05p.",depth)
  
  pred.POCd.05p <- setValues(tile[[1]], tile.df.out$pred.POCd.05p)
  names(pred.POCd.05p) <- paste0("POCd.05p.",depth)
  
  pred.ROCd.05p <- setValues(tile[[1]], tile.df.out$pred.ROCd.05p)
  names(pred.ROCd.05p) <- paste0("ROCd.05p.",depth)
  
  pred.HOCd.95p <- setValues(tile[[1]], tile.df.out$pred.HOCd.95p)
  names(pred.HOCd.95p) <- paste0("HOCd.95p.",depth)
  
  pred.POCd.95p <- setValues(tile[[1]], tile.df.out$pred.POCd.95p)
  names(pred.POCd.95p) <- paste0("POCd.95p.",depth)
  
  pred.ROCd.95p <- setValues(tile[[1]], tile.df.out$pred.ROCd.95p)
  names(pred.ROCd.95p) <- paste0("ROCd.95p.",depth)
  
  ### Create a raster stack
  stack.out <- stack(pred.HOCd.m,pred.POCd.m,pred.ROCd.m,
                     pred.HOCd.05p,pred.POCd.05p,pred.ROCd.05p,
                     pred.HOCd.95p,pred.POCd.95p,pred.ROCd.95p)
  
  ### Function to calculate sensitivity on pixel by pixel
  sobolPixelFunc <- function(k) {
  ### HOC
  X <- data.frame(HOC=as.vector(HOC.sample[k,]),
                  TOC=as.vector(zTOC[k,]),
                  bd=as.vector(zBD[k,]),
                  CF=as.vector(zCF[k,]))
  modelSOCfr <- function(x){y <- x$HOC * x$TOC * x$bd * (1- x$CF) * 0.1 }
  sobolPixelHOC <- sobolmartinez(X1=X[1:(nsamp/2),],
                                 X2=X[(nsamp/2+1):nsamp,],
                                 model = modelSOCfr)
  ### POC
  X <- data.frame(POC=as.vector(POC.sample[k,]),
                  TOC=as.vector(zTOC[k,]),
                  bd=as.vector(zBD[k,]),
                  CF=as.vector(zCF[k,]))
  modelSOCfr <- function(x){y <- x$POC * x$TOC * x$bd * (1- x$CF) * 0.1 }
  sobolPixelPOC <- sobolmartinez(X1=X[1:(nsamp/2),],
                              X2=X[(nsamp/2+1):nsamp,],
                              model = modelSOCfr)
  ### ROC
  X <- data.frame(ROC=as.vector(ROC.sample[k,]),
                  TOC=as.vector(zTOC[k,]),
                  bd=as.vector(zBD[k,]),
                  CF=as.vector(zCF[k,]))
  modelSOCfr <- function(x){y <- x$ROC * x$TOC * x$bd * (1- x$CF) * 0.1 }
  sobolPixelROC <- sobolmartinez(X1=X[1:(nsamp/2),],
                                 X2=X[(nsamp/2+1):nsamp,],
                                 model = modelSOCfr)
  
  sobolPixel <- c(sobolPixelHOC$S$original,sobolPixelHOC$T$original,
                  sobolPixelPOC$S$original,sobolPixelPOC$T$original,
                  sobolPixelROC$S$original,sobolPixelROC$T$original)
  return(sobolPixel)
  }
  
  for(k in 1:nrow(Sobol.df)){
  print(k)
  sobol.k <- sobolPixelFunc(k)
  Sobol.df[k,] <- sobol.k
  }  
  
gc()
rm(sobol.k)

### Assign to raster
Sobol.list <- list()
for(r in 1:ncol(Sobol.df)){
  Sobol.r <- setValues(tile[[1]], Sobol.df[,r])
  names(Sobol.r) <- colnames(Sobol.df)[r]
  Sobol.list[[r]] <- Sobol.r

}

### stack Sobol indices
Sobol.stack <- stack(Sobol.list)

} else if (nrow(tile.df) - length(df.na) > 1) {
  print("some missing data in tile")
  
  ### Sample nsamp values of ilr1 and ilr2 from the quantile distribution
  pred.ilr1.sample <- predict(object=model.ilr1,
                              data = tile.df[-df.na, ], 
                              type = "quantiles",
                              what = function(x) sample(x, nsamp, replace = TRUE))$predictions
  
  pred.ilr2.sample <- predict(object=model.ilr2,
                              data = tile.df[-df.na, ], 
                              type = "quantiles",
                              what = function(x) sample(x, nsamp, replace = TRUE))$predictions
  
  ### Create array with both matrices
  ### Dimnames
  column.names <- paste0("Samp", c(1:nsamp))
  row.names <- paste0("row", c(1:nrow(tile.df[-df.na,])))
  matrix.names <- c("ilr1","ilr2")
    
  zSOC <- array(c(pred.ilr1.sample, pred.ilr2.sample), 
             dim = c(nrow(tile.df[-df.na, ]), nsamp, 2), 
             dimnames = list(row.names, column.names, matrix.names)) 
    
  ### Back-transform to HOC, POC, ROC (%) with apply over the array
  backtf <- function(x){Backt <- compositions::ilrInv(x); return(Backt)}
  Backt <- apply(X = zSOC, MARGIN = c(1,2) , FUN = backtf)
  dimnames(Backt)[[1]] <- orderProps #c("HOC", "POC","ROC")
    
  ### Store in separate matrices
  HOC.sample <- Backt["HOC",,]
  POC.sample <- Backt["POC",,]
  ROC.sample <- Backt["ROC",,]
    
  ### Sample from the distribution of TOC 
  pred.TOC.sample <- predict(object=model.TOC,
                             data = tile.df[-df.na, ],  
                             type = "quantiles",
                             what = function(x) sample(x, nsamp, replace = TRUE))$predictions
    
  ### Name dimensions
  column.names <- paste0("Samp", c(1:nsamp))
  row.names <- paste0("row", c(1:nrow(tile.df[-df.na,])))
  zTOC <- matrix(c(pred.TOC.sample*10),
                 nrow = nrow(tile.df[-df.na,]), ncol = nsamp,
                 dimnames = list(row.names, column.names))
  
  ### Predict the probability of the volumetric coarse fragments classes for each pixel (df row) 
  CFvol_prob <- predict(CFvol.model, data = tile.df[-df.na, ])$predictions 
  CFvol_prob <- as.data.frame(CFvol_prob)
  colnames(CFvol_prob) <- c("X1p", "X2p", "X3p", "X4p", "X5p", "X6p")
    
  ### Sample from each class should be proportional to the class probability
  CFsamp.n <- round(nsamp*CFvol_prob, digits = 0)
  CFsamp.nTot <- rowSums(CFsamp.n, na.rm = TRUE)
  CFsamp.n[CFsamp.nTot<nsamp,] <- ceiling(nsamp*CFvol_prob[CFsamp.nTot<nsamp,])
  rm(CFsamp.nTot)
    
  ### function to generate sample from uniform distribution
  func.sample.vol <- function(x){
    #set.seed(1)
    x1 <- runif(n=x[["X1p"]], min = 0, max = 2) 
    x2 <- runif(n=x[["X2p"]], min = 2, max = 10) 
    x3 <- runif(n=x[["X3p"]], min = 10, max = 20) 
    x4 <- runif(n=x[["X4p"]], min = 20, max = 50) 
    x5 <- runif(n=x[["X5p"]], min = 50, max = 90) 
    x6 <- runif(n=x[["X6p"]], min = 90, max = 100) 
    out <- c(x1,x2,x3,x4,x5,x6)
    ### in case the ceiling made it > nsamp
    ### but shuffle to not favor necessarily the smaller classes
    out2 <- sample(out, size=nsamp, replace = FALSE)
    return(out2)
  }
    
  ### Apply the function over the rows
  cfvol <- apply(X =CFsamp.n, MARGIN = 1,  FUN = func.sample.vol)
  
  ### Transpose so we have nsamp cols and tile.df number of rows as rows
  cfvol <- t(cfvol)
    
  ### save into a matrix
  column.names <- paste0("Samp", c(1:nsamp))
  row.names <- paste0("row", c(1:nrow(tile.df[-df.na,])))
  zCF <- matrix(c(cfvol/100),
                nrow = nrow(tile.df[-df.na,]), ncol = nsamp,
                dimnames = list(row.names, column.names))
 
  ### clean space
  rm(pred.TOC.sample,cfvol,CFsamp.n,pred.ilr1.sample,pred.ilr2.sample,CFvol_prob,row.names,column.names)
  
  ### Bulk density is in the dataframe tile.df
  bd.df <- tile.df[-df.na, bd.depth.specific]
  ### the standard deviation is calculated from the prediction interval
  bd.df$BD_sd <- (bd.df[,3]-bd.df[,2])/(2*1.64)
  colnames(bd.df) <- c("BD_mean", "BD_05", "BD_95", "BD_sd" )
  
  ### Generate truncated sample assuming a normal distribution
  func.sample.bd <- function(x){
    bdsamp <- truncnorm::rtruncnorm(n=nsamp, a=1, b=1.8, mean = x[["BD_mean"]], sd = x[["BD_sd"]])
    return(bdsamp)
  }
   
  ### Apply the function over the rows
  set.seed(122)
  bdsamp <- apply(X = bd.df, MARGIN = 1,  FUN = func.sample.bd)
  ### Transpose so we have nsamp cols and df.nrow as rows (same as with CF)
  bdsamp <- t(bdsamp)
    
  ### save into a matrix
  column.names <- paste0("Samp", c(1:nsamp))
  row.names <- paste0("row", c(1:nrow(tile.df[-df.na,])))
  zBD <- matrix(bdsamp, byrow = FALSE,
                nrow = nrow(tile.df[-df.na,]), ncol = nsamp,
                dimnames = list(row.names, column.names))

  ### Calculate SOC density [I do not include layer thickness (cm)]
  ### Backt * (zTOC) * zBD * (1- zCF) * 0.1
  ###   SOC stock Mg C-fraction/ha cm = 
  ###   SOC fraction (%/100) x TOC content mg C/g soil x bulk density (g soil/cm3 soil) x
  ###   gravel correction (g<2mm/g soil) x  0.1 correction factor
   
  HOCs <- HOC.sample * zTOC * zBD * (1- zCF) * 0.1
  POCs <- POC.sample * zTOC * zBD * (1- zCF) * 0.1
  ROCs <- ROC.sample * zTOC * zBD * (1- zCF) * 0.1
    
  ### Calculate mean and percentiles and assign to dataframe with all rows
  tile.df.out[-df.na,]$pred.HOCd.m   <- apply(X = HOCs, MARGIN = 1, FUN = function(x) mean(x, na.rm=TRUE))
  tile.df.out[-df.na,]$pred.HOCd.95p <- apply(X = HOCs, MARGIN = 1, FUN = function(x) quantile(x, c(.95)))
  tile.df.out[-df.na,]$pred.HOCd.05p <- apply(X = HOCs, MARGIN = 1, FUN = function(x) quantile(x, c(.05)))
    
  tile.df.out[-df.na,]$pred.POCd.m   <- apply(X = POCs, MARGIN = 1, FUN = function(x) mean(x, na.rm=TRUE))
  tile.df.out[-df.na,]$pred.POCd.95p <- apply(X = POCs, MARGIN = 1, FUN = function(x) quantile(x, c(.95)))
  tile.df.out[-df.na,]$pred.POCd.05p <- apply(X = POCs, MARGIN = 1, FUN = function(x) quantile(x, c(.05)))
    
  tile.df.out[-df.na,]$pred.ROCd.m   <- apply(X = ROCs, MARGIN = 1, FUN = function(x) mean(x, na.rm=TRUE))
  tile.df.out[-df.na,]$pred.ROCd.95p <- apply(X = ROCs, MARGIN = 1, FUN = function(x) quantile(x, c(.95)))
  tile.df.out[-df.na,]$pred.ROCd.05p <- apply(X = ROCs, MARGIN = 1, FUN = function(x) quantile(x, c(.05)))
    
  ### I use tile as template for the coordinates
  pred.HOCd.m<- setValues(tile[[1]], tile.df.out$pred.HOCd.m)
  names(pred.HOCd.m) <- paste0("HOCd.m.",depth)
  
  pred.POCd.m<- setValues(tile[[1]], tile.df.out$pred.POCd.m)
  names(pred.POCd.m) <- paste0("POCd.m.",depth)
    
  pred.ROCd.m<- setValues(tile[[1]], tile.df.out$pred.ROCd.m)
  names(pred.ROCd.m) <- paste0("ROCd.m.",depth)
  
  pred.HOCd.05p <- setValues(tile[[1]], tile.df.out$pred.HOCd.05p)
  names(pred.HOCd.05p) <- paste0("HOCd.05p.",depth)
    
  pred.POCd.05p <- setValues(tile[[1]], tile.df.out$pred.POCd.05p)
  names(pred.POCd.05p) <- paste0("POCd.05p.",depth)
    
  pred.ROCd.05p <- setValues(tile[[1]], tile.df.out$pred.ROCd.05p)
  names(pred.ROCd.05p) <- paste0("ROCd.05p.",depth)
    
  pred.HOCd.95p <- setValues(tile[[1]], tile.df.out$pred.HOCd.95p)
  names(pred.HOCd.95p) <- paste0("HOCd.95p.",depth)
    
  pred.POCd.95p <- setValues(tile[[1]], tile.df.out$pred.POCd.95p)
  names(pred.POCd.95p) <- paste0("POCd.95p.",depth)
    
  pred.ROCd.95p <- setValues(tile[[1]], tile.df.out$pred.ROCd.95p)
  names(pred.ROCd.95p) <- paste0("ROCd.95p.",depth)
  
  ### Stack SOC fraction density
  stack.out <- stack(pred.HOCd.m,pred.POCd.m,pred.ROCd.m,
                     pred.HOCd.05p,pred.POCd.05p,pred.ROCd.05p,
                     pred.HOCd.95p,pred.POCd.95p,pred.ROCd.95p)
  
    
  ### Function to calculate sensitivity on pixel by pixel
  sobolPixelFunc <- function(k) {
    ### HOC
    X <- data.frame(HOC=as.vector(HOC.sample[k,]),
                    TOC=as.vector(zTOC[k,]),
                    bd=as.vector(zBD[k,]),
                    CF=as.vector(zCF[k,]))
    modelSOCfr <- function(x){y <- x$HOC * x$TOC * x$bd * (1- x$CF) * 0.1 }
    sobolPixelHOC <- sobolmartinez(X1=X[1:(nsamp/2),],
                                   X2=X[(nsamp/2+1):nsamp,],
                                   model = modelSOCfr)
    ### POC
    X <- data.frame(POC=as.vector(POC.sample[k,]),
                    TOC=as.vector(zTOC[k,]),
                    bd=as.vector(zBD[k,]),
                    CF=as.vector(zCF[k,]))
    modelSOCfr <- function(x){y <- x$POC * x$TOC * x$bd * (1- x$CF) * 0.1 }
    sobolPixelPOC <- sobolmartinez(X1=X[1:(nsamp/2),],
                                   X2=X[(nsamp/2+1):nsamp,],
                                   model = modelSOCfr)
    ### ROC
    X <- data.frame(ROC=as.vector(ROC.sample[k,]),
                    TOC=as.vector(zTOC[k,]),
                    bd=as.vector(zBD[k,]),
                    CF=as.vector(zCF[k,]))
    modelSOCfr <- function(x){y <- x$ROC * x$TOC * x$bd * (1- x$CF) * 0.1 }
    sobolPixelROC <- sobolmartinez(X1=X[1:(nsamp/2),],
                                   X2=X[(nsamp/2+1):nsamp,],
                                   model = modelSOCfr)
    sobolPixel <- c(sobolPixelHOC$S$original,sobolPixelHOC$T$original,
                    sobolPixelPOC$S$original,sobolPixelPOC$T$original,
                    sobolPixelROC$S$original,sobolPixelROC$T$original)
    return(sobolPixel)
    }
    
    
  Sobol.temp <- Sobol.df[-df.na,]
  for(k in 1:nrow(Sobol.temp)){
    print(k)
    sobol.k <- sobolPixelFunc(k)
    Sobol.temp[k,] <- sobol.k
  }  
  gc()
  rm(sobol.k)
  Sobol.df[-df.na,] <- Sobol.temp
  rm(Sobol.temp)
    
  ### Assign to raster
  Sobol.list <- list()
  for(r in 1:ncol(Sobol.df)){
    Sobol.r <- setValues(tile[[1]], Sobol.df[,r])
    names(Sobol.r) <- colnames(Sobol.df)[r]
    Sobol.list[[r]] <- Sobol.r
      
  }
    
  ### stack Sobol indices
  Sobol.stack <- stack(Sobol.list)
  
  } else if (nrow(tile.df) - length(df.na) == 1) {
    print("only one observation")
    
    
    ### Sample 500 values of ilr1 and ilr2 from the quantile distribution to estimate the prediction intervals
    pred.ilr1.sample <- predict(object=model.ilr1,
                                 data = tile.df[-df.na, ], 
                                 type = "quantiles",
                                 what = function(x) sample(x, nsamp, replace = TRUE))$predictions
     
    pred.ilr2.sample <- predict(object=model.ilr2,
                                 data = tile.df[-df.na, ], 
                                 type = "quantiles",
                                 what = function(x) sample(x, nsamp, replace = TRUE))$predictions
     
   ### Create array with both matrices
   ### Dimnames
   zSOC <- data.frame(ilr1=t(pred.ilr1.sample), ilr2=t(pred.ilr2.sample))
   rownames(zSOC) <- paste0("Samp", c(1:nsamp))
 
   ### Back-transform to HOC, POC, ROC (%) with apply over the array
   backtf <- function(x){Backt <- compositions::ilrInv(x); return(Backt)}
   Backt <- backtf(zSOC)
   names(Backt) <- orderProps #c("HOC", "POC","ROC")
   SOCfr <- as.data.frame(Backt)
     
   ### Sample from the distribution of TOC 
   SOCfr$TOC <- as.vector(predict(object=model.TOC,
                              data = tile.df[-df.na, ],  
                              type = "quantiles",
                              what = function(x) sample(x, nsamp, replace = TRUE))$predictions*10)
     
   ### Predict the probability of the volumetric coarse fragments classes for each pixel (df row) 
   CFvol_prob <- predict(CFvol.model, data = tile.df[-df.na, ])$predictions 
   CFvol_prob <- as.data.frame(CFvol_prob)
   colnames(CFvol_prob) <- c("X1p", "X2p", "X3p", "X4p", "X5p", "X6p")
     
   ### Sample from each class should be proportional to the class probability
   CFsamp.n <- round(nsamp*CFvol_prob, digits = 0)
   CFsamp.nTot <- rowSums(CFsamp.n, na.rm = TRUE)
   CFsamp.n[CFsamp.nTot<nsamp,] <- ceiling(nsamp*CFvol_prob[CFsamp.nTot<nsamp,])
   rm(CFsamp.nTot)
     
   ### function to generate sample from uniform distribution
   func.sample.vol <- function(x){
     #set.seed(1)
     x1 <- runif(n=x[["X1p"]], min = 0, max = 2) 
     x2 <- runif(n=x[["X2p"]], min = 2, max = 10) 
     x3 <- runif(n=x[["X3p"]], min = 10, max = 20) 
     x4 <- runif(n=x[["X4p"]], min = 20, max = 50) 
     x5 <- runif(n=x[["X5p"]], min = 50, max = 90) 
     x6 <- runif(n=x[["X6p"]], min = 90, max = 100) 
     out <- c(x1,x2,x3,x4,x5,x6)
     ### in case the ceiling made it > nsamp
     ### but shuffle to not favor necessarily the smaller classes
     out2 <- sample(out, size=nsamp, replace = FALSE)
     return(out2)
   }
     
   ### Apply the function over the rows
   cfvol <- apply(X =CFsamp.n, MARGIN = 1,  FUN = func.sample.vol)
     
   ### attach to dataframe
   SOCfr$CF <- as.vector(cfvol/100)
     
   ### clean space
   rm(cfvol,CFsamp.n,pred.ilr1.sample,pred.ilr2.sample,CFvol_prob)
     
   ### Bulk density is in the dataframe tile.df
   bd.df <- tile.df[-df.na, bd.depth.specific]
   ### the standard deviation is calculated from the prediction interval
   bd.df$BD_sd <- (bd.df[,3]-bd.df[,2])/(2*1.64)
   colnames(bd.df) <- c("BD_mean", "BD_05", "BD_95", "BD_sd" )
     
   ### Generate truncated sample assuming a normal distribution
   func.sample.bd <- function(x){
     bdsamp <- truncnorm::rtruncnorm(n=nsamp, a=1, b=1.8, mean = x[["BD_mean"]], sd = x[["BD_sd"]])
     return(bdsamp)
   }
     
   ### Apply the function over the rows
   set.seed(122)
   bdsamp <- apply(X = bd.df, MARGIN = 1,  FUN = func.sample.bd)
   ### attach to dataframe
   SOCfr$BD <- as.vector(bdsamp)
     
   ### Calculate SOC density [I do not include layer thickness (cm)]
   ### Backt * (zTOC) * zBD * (1- zCF) * 0.1
   ###   SOC stock Mg C-fraction/ha cm = 
   ###   SOC fraction (%/100) x TOC content mg C/g soil x bulk density (g soil/cm3 soil) x
   ###   gravel correction (g<2mm/g soil) x  0.1 correction factor
     
   SOCfr$HOCs <- SOCfr$HOC * SOCfr$TOC * SOCfr$BD * (1- SOCfr$CF) * 0.1
   SOCfr$POCs <- SOCfr$POC * SOCfr$TOC * SOCfr$BD * (1- SOCfr$CF) * 0.1
   SOCfr$ROCs <- SOCfr$ROC * SOCfr$TOC * SOCfr$BD * (1- SOCfr$CF) * 0.1
     
   ### Calculate mean and percentiles and assign to dataframe with all rows
   tile.df.out[-df.na,]$pred.HOCd.m   <- mean(SOCfr$HOCs, na.rm=TRUE)
   tile.df.out[-df.na,]$pred.HOCd.95p <- quantile(SOCfr$HOCs, c(.95))
   tile.df.out[-df.na,]$pred.HOCd.05p <- quantile(SOCfr$HOCs, c(.05))
     
   tile.df.out[-df.na,]$pred.POCd.m   <- mean(SOCfr$POCs, na.rm=TRUE)
   tile.df.out[-df.na,]$pred.POCd.95p <- quantile(SOCfr$POCs, c(.95))
   tile.df.out[-df.na,]$pred.POCd.05p <- quantile(SOCfr$POCs, c(.05))
     
   tile.df.out[-df.na,]$pred.ROCd.m   <- mean(SOCfr$ROCs, na.rm=TRUE)
   tile.df.out[-df.na,]$pred.ROCd.95p <- quantile(SOCfr$ROCs, c(.95))
   tile.df.out[-df.na,]$pred.ROCd.05p <- quantile(SOCfr$ROCs, c(.05))
     
   
   ### Tile as template
   pred.HOCd.m<- setValues(tile[[1]], tile.df.out$pred.HOCd.m)
   names(pred.HOCd.m) <- paste0("HOCd.m.",depth)
     
   pred.POCd.m<- setValues(tile[[1]], tile.df.out$pred.POCd.m)
   names(pred.POCd.m) <- paste0("POCd.m.",depth)
     
   pred.ROCd.m<- setValues(tile[[1]], tile.df.out$pred.ROCd.m)
   names(pred.ROCd.m) <- paste0("ROCd.m.",depth)
   
   pred.HOCd.05p <- setValues(tile[[1]], tile.df.out$pred.HOCd.05p)
   names(pred.HOCd.05p) <- paste0("HOCd.05p.",depth)
     
   pred.POCd.05p <- setValues(tile[[1]], tile.df.out$pred.POCd.05p)
   names(pred.POCd.05p) <- paste0("POCd.05p.",depth)
     
   pred.ROCd.05p <- setValues(tile[[1]], tile.df.out$pred.ROCd.05p)
   names(pred.ROCd.05p) <- paste0("ROCd.05p.",depth)
     
   pred.HOCd.95p <- setValues(tile[[1]], tile.df.out$pred.HOCd.95p)
   names(pred.HOCd.95p) <- paste0("HOCd.95p.",depth)
     
   pred.POCd.95p <- setValues(tile[[1]], tile.df.out$pred.POCd.95p)
   names(pred.POCd.95p) <- paste0("POCd.95p.",depth)
     
   pred.ROCd.95p <- setValues(tile[[1]], tile.df.out$pred.ROCd.95p)
   names(pred.ROCd.95p) <- paste0("ROCd.95p.",depth)
   
   ### Stack with SOC fraction density
   ### Create a raster stack, and resturn it
   stack.out <- stack(pred.HOCd.m,pred.POCd.m,pred.ROCd.m,
                      pred.HOCd.05p,pred.POCd.05p,pred.ROCd.05p,
                      pred.HOCd.95p,pred.POCd.95p,pred.ROCd.95p)
   
   ### Function to calculate sensitivity on pixel by pixel
   
   sobolPixelFunc <- function(x) {
     
     modelHOC <- function(x){y <- x$HOC * x$TOC * x$BD * (1- x$CF) * 0.1 }
     modelPOC <- function(x){y <- x$POC * x$TOC * x$BD * (1- x$CF) * 0.1 }
     modelROC <- function(x){y <- x$ROC * x$TOC * x$BD * (1- x$CF) * 0.1 }
       
     sobolPixelHOC <- sobolmartinez(X1=x[1:(nsamp/2),c("HOC","TOC","BD", "CF")], X2=x[(nsamp/2+1):nsamp,c("HOC","TOC","BD", "CF")], model = modelHOC)
     sobolPixelPOC <- sobolmartinez(X1=x[1:(nsamp/2),c("POC","TOC","BD", "CF")], X2=x[(nsamp/2+1):nsamp,c("POC","TOC","BD", "CF")], model = modelPOC)
     sobolPixelROC <- sobolmartinez(X1=x[1:(nsamp/2),c("ROC","TOC","BD", "CF")], X2=x[(nsamp/2+1):nsamp,c("ROC","TOC","BD", "CF")], model = modelROC)
     
     sobolPixel <- c(sobolPixelHOC$S$original,sobolPixelHOC$T$original,
                     sobolPixelPOC$S$original,sobolPixelPOC$T$original,
                     sobolPixelROC$S$original,sobolPixelROC$T$original)
     
     return(sobolPixel)
     }
     
  ### Apply function for that pixel
  Sobol.df[-df.na,] <- sobolPixelFunc(SOCfr)
   
   ### Assign to raster
   Sobol.list <- list()
   for(r in 1:ncol(Sobol.df)){
     Sobol.r <- setValues(tile[[1]], Sobol.df[,r])
     names(Sobol.r) <- colnames(Sobol.df)[r]
     Sobol.list[[r]] <- Sobol.r
     
   }
     
   ### stack with Sobol indices
   Sobol.stack <- stack(Sobol.list)
   
   } else {
     
     print("Empty tile")
       
     pred.HOCd.m<- setValues(tile[[1]], tile.df.out$pred.HOCd.m)
     names(pred.HOCd.m) <- paste0("HOCd.m.",depth)
       
     pred.POCd.m<- setValues(tile[[1]], tile.df.out$pred.POCd.m)
     names(pred.POCd.m) <- paste0("POCd.m.",depth)
       
     pred.ROCd.m<- setValues(tile[[1]], tile.df.out$pred.ROCd.m)
     names(pred.ROCd.m) <- paste0("ROCd.m.",depth)
       
     pred.HOCd.05p <- setValues(tile[[1]], tile.df.out$pred.HOCd.05p)
     names(pred.HOCd.05p) <- paste0("HOCd.05p.",depth)
       
     pred.POCd.05p <- setValues(tile[[1]], tile.df.out$pred.POCd.05p)
     names(pred.POCd.05p) <- paste0("POCd.05p.",depth)
       
     pred.ROCd.05p <- setValues(tile[[1]], tile.df.out$pred.ROCd.05p)
     names(pred.ROCd.05p) <- paste0("ROCd.05p.",depth)
       
     pred.HOCd.95p <- setValues(tile[[1]], tile.df.out$pred.HOCd.95p)
     names(pred.HOCd.95p) <- paste0("HOCd.95p.",depth)
       
     pred.POCd.95p <- setValues(tile[[1]], tile.df.out$pred.POCd.95p)
     names(pred.POCd.95p) <- paste0("POCd.95p.",depth)
       
     pred.ROCd.95p <- setValues(tile[[1]], tile.df.out$pred.ROCd.95p)
     names(pred.ROCd.95p) <- paste0("ROCd.95p.",depth)
       
       
     ### Stack with SOC fraction density
     ### Create a raster stack, and resturn it
     stack.out <- stack(pred.HOCd.m,pred.POCd.m,pred.ROCd.m,
                        pred.HOCd.05p,pred.POCd.05p,pred.ROCd.05p,
                        pred.HOCd.95p,pred.POCd.95p,pred.ROCd.95p)
       
     Sobol.list <- list()
     for(r in 1:ncol(Sobol.df)){
       Sobol.r <- setValues(tile[[1]], Sobol.df[,r])
       names(Sobol.r) <- colnames(Sobol.df)[r]
       Sobol.list[[r]] <- Sobol.r
     }
       
     ### stack Sobol indices
     Sobol.stack <- stack(Sobol.list)
   
     gc()
   }

### Return all together, better
stack.out <- stack(stack.out,Sobol.stack)
return(stack.out)
}

stopCluster(cl)
names.write <- c("pred.HOCd.m",  "pred.POCd.m",  "pred.ROCd.m",
                 "pred.HOCd.05p","pred.POCd.05p","pred.ROCd.05p",
                 "pred.HOCd.95p","pred.POCd.95p","pred.ROCd.95p",
                 "Sobol1.HOC.SOCfr","Sobol1.HOC.TOC","Sobol1.HOC.BD",
                 "Sobol1.HOC.CF","SobolT.HOC.SOCfr","SobolT.HOC.TOC",  
                 "SobolT.HOC.BD","SobolT.HOC.CF","Sobol1.POC.SOCfr",
                 "Sobol1.POC.TOC","Sobol1.POC.BD","Sobol1.POC.CF",   
                 "SobolT.POC.SOCfr","SobolT.POC.TOC","SobolT.POC.BD",
                 "SobolT.POC.CF","Sobol1.ROC.SOCfr","Sobol1.ROC.TOC", 
                 "Sobol1.ROC.BD","Sobol1.ROC.CF","SobolT.ROC.SOCfr",
                 "SobolT.ROC.TOC","SobolT.ROC.BD","SobolT.ROC.CF")


OutTiles <- unlist(OutTiles)
OutDirTiles <-  "C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/Output/TEST/"

#### Mosaic each variable into just the horizontal tile and save into TIFF file
for(var in 1:length(names.write)){
  rast.list <- list()
  for(j in 1:length(OutTiles)){
    rast.list[[j]] <- OutTiles[[j]][[var]]
  }
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  pred <- do.call(mosaic, rast.list)
  names(pred) <- names(names.write[[var]])
  print(pred)
  print(paste0(OutDirTiles,names.write[[var]],depth,".tif"))
  writeRaster(pred, filename= paste0(OutDirTiles,names.write[[var]],".",depth,".500sim.tif"),
              na.rm=T,inf.rm=T, format="GTiff", overwrite=TRUE )
}

### End of the script for this tile :)