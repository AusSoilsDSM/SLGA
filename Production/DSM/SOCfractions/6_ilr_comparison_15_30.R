#################################################################################

# Objective: For RF models and cross-validation results - Compare how order of variables affects modelling results 
# Date: 10/2021
# Author: Mercedes Roman
# Project: SOC fraction maps for TERN

### Load packages
require(ranger)
require(raster)
library(blockCV)
library(readr)
library(caret)
library(scales)
library(viridis)
library(viridisLite)

setwd("C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/")
source('./codes/make_cv_RF_ilr.R')
dir.create("C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/data/October2021/comparison_order/")
outDir <- "C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/data/October2021/comparison_order/"


# 1. Prepare calibration data ------------------------------------------------

load("C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/data/SOCfr_Prop.GSM_03092021.RData")

HOCsoildat <- HOC_p.GSM ; rm(HOC_p.GSM)
POCsoildat <- POC_p.GSM ; rm(POC_p.GSM)
ROCsoildat <- ROC_p.GSM ; rm(ROC_p.GSM)

### constrain to 100% maximum value
HOCsoildat$`0-5 cm` <- ifelse(HOCsoildat$`0-5 cm` >100, 100, HOCsoildat$`0-5 cm`)
HOCsoildat$`5-15 cm` <- ifelse(HOCsoildat$`5-15 cm` >100, 100, HOCsoildat$`5-15 cm`)
HOCsoildat$`15-30 cm` <- ifelse(HOCsoildat$`15-30 cm` >100, 100, HOCsoildat$`15-30 cm`)
POCsoildat$`0-5 cm` <- ifelse(POCsoildat$`0-5 cm` >100, 100, POCsoildat$`0-5 cm`)
POCsoildat$`5-15 cm` <- ifelse(POCsoildat$`5-15 cm` >100, 100, POCsoildat$`5-15 cm`)
POCsoildat$`15-30 cm` <- ifelse(POCsoildat$`15-30 cm` >100, 100, POCsoildat$`15-30 cm`)
ROCsoildat$`0-5 cm` <- ifelse(ROCsoildat$`0-5 cm` >100, 100, ROCsoildat$`0-5 cm`)
ROCsoildat$`5-15 cm` <- ifelse(ROCsoildat$`5-15 cm` >100, 100, ROCsoildat$`5-15 cm`)
ROCsoildat$`15-30 cm` <- ifelse(ROCsoildat$`15-30 cm` >100, 100, ROCsoildat$`15-30 cm`)
extrHOC <- read.csv("C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/data/extrHOC.csv")

### Change names
names(HOCsoildat)[2:4] <- c("HOC.0.5", "HOC.5.15", "HOC.15.30")
names(POCsoildat)[2:4] <- c("POC.0.5", "POC.5.15", "POC.15.30")
names(ROCsoildat)[2:4] <- c("ROC.0.5", "ROC.5.15", "ROC.15.30")

### Bind dataframes
SOCFr <- cbind(HOCsoildat[,1:4],POCsoildat[,2:4],ROCsoildat[,2:4],HOCsoildat[,c(5:7,9)],extrHOC[,2:ncol(extrHOC)])
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

# mydata <- data.frame(HOC=SOCFr$HOC.5.15, POC=SOCFr$POC.5.15, ROC=SOCFr$ROC.5.15)
# mydata <- data.frame(HOC=SOCFr$HOC.5.15, POC=SOCFr$POC.0.5, ROC=SOCFr$ROC.0.5)
# mydata <- data.frame(HOC=SOCFr$HOC.15.30, POC=SOCFr$POC.15.30, ROC=SOCFr$ROC.15.30)
# mydata <- mydata[complete.cases(mydata),]
# 
# library(Ternary)
# par(mfrow = c(1, 1), mar = rep(0.3, 4))
# TernaryPlot(alab = "HOC (%)", blab = "POC (%)", clab = "ROC (%)",
#             lab.col = c('red', 'green', 'blue'),
#             point = 'right', lab.cex = 0.8, grid.minor.lines = 0,
#             grid.lty = 'solid', col = rgb(0.9, 0.9, 0.9), grid.col = 'white', 
#             axis.col = rgb(0.6, 0.6, 0.6), ticks.col = rgb(0.6, 0.6, 0.6),
#             axis.rotate = FALSE,
#             padding = 0.08)
# 
# rgbWhite <- function (r, g, b) {
#   highest <- apply(rbind(r, g, b), 2L, max)
#   rgb(r/highest, g/highest, b/highest)
# }
# 
# values <- TernaryPointValues(rgbWhite, resolution = 20)
# ColourTernary(values, spectrum = NULL)
# TernaryPoints(mydata[, c('HOC', 'POC', 'ROC')], cex=0.4)
# rm(mydata, values, rgbWhite)

#########################################################################################################################################

### Load covariate stack

# path to the covariates raster
CovarDir    <-"R:/PRJ-SoilBiodSOC2019/Covariates/Australia1km_filled/"

# list all the raster files available
setwd(CovarDir)
list_ras <- list.files(pattern="tif$")

# load all the rasters
covariates.stack <- stack(paste0(CovarDir,list_ras))
plot(covariates.stack)

### Crop two tiles of interest

### Crop the tile in question 
tile1 <- crop(covariates.stack, extent(covariates.stack,600,700,400,500))
tile2 <- crop(covariates.stack, extent(covariates.stack,900,1000,1200,1300))
tiles <- list(tile1, tile2)

### Where are these tiles?
e <- extent(tile1)
# coerce to a SpatialPolygons object
p <- as(e, 'SpatialPolygons')
proj4string(p2) <- "+proj=longlat +datum=WGS84 +no_defs" 

e <- extent(tile2)
# coerce to a SpatialPolygons object
p2 <- as(e, 'SpatialPolygons')
proj4string(p2) <- "+proj=longlat +datum=WGS84 +no_defs" 

#Plot
plot(covariates.stack[[1]])
plot(p, lwd=2, border='black', add=TRUE)
plot(covariates.stack[[1]])
plot(p2, lwd=2, border='black', add=TRUE)


### What this loop will do:

### 1. Fit rf models for the two ilr variables, 
###    for a certain order of the three SOC fractions as input to the ilr transformation.
### 2. Cross-validation (10-fold) and calculate statistics on the ilr and backtransformed variables
### 3. Predict the SOC fractions in two tiles
### 4. Plot the three fractions as rgb

# 2. Calibrate and compare ilr-models for the depth 0-5 cm ------------------------------------------------------------

# ### For loop, changing order of combinations

OutDir <- "C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/Output_Comparison/"
dir.create(OutDir)

depth <- "15-30 cm"
dfilenames <- "15_30"
covar.depth.specific <- c("clay_15_30", "sand_15_30")

possible.combis <- list(c("HOC.15.30","POC.15.30","ROC.15.30"),
                        c("HOC.15.30","ROC.15.30","POC.15.30"),
                        c("ROC.15.30","POC.15.30","HOC.15.30"),
                        c("ROC.15.30","HOC.15.30","POC.15.30"),
                        c("POC.15.30","ROC.15.30","HOC.15.30"),
                        c("POC.15.30","HOC.15.30","ROC.15.30"))

orderProps.list <- list(c("HOC","POC","ROC"),
                        c("HOC","ROC","POC"),
                        c("ROC","POC","HOC"),
                        c("ROC","HOC","POC"),
                        c("POC","ROC","HOC"),
                        c("POC","HOC","ROC"))

combi.names <- c("HPR","HRP","RPH","RHP","PRH","PHR")
some.seeds <- c(1991,1984,1946,1948,2013,2016)

SolarDiagram.list <- list("HPR"=list(),"HRP"=list(),"RPH"=list(),
                          "RHP"=list(),"PRH"=list(),"PHR"=list())

rho.list <- list("HPR"=list(),"HRP"=list(),"RPH"=list(),
                 "RHP"=list(),"PRH"=list(),"PHR"=list())

setwd(OutDir)
dir.create(paste0(OutDir,dfilenames,"/"))
setwd(paste0(OutDir,dfilenames,"/"))

for(i in 1: length(possible.combis)) {
  
  print(paste0("Testing combination ",combi.names[[i]]))
  
  
  dir.create(paste0(OutDir,dfilenames,"/",combi.names[i],"/"))
  setwd(paste0(OutDir,dfilenames,"/",combi.names[i],"/"))
  
  ### Dataset
  data = SOCFr
  properties = possible.combis[[i]]
  orderProps = orderProps.list[[i]]
  covars = c(covar.names, covar.depth.specific)
 
  ### Model settings
  nfolds = 10
  thisseed <- some.seeds[[i]]
  ntreefinal <- 500
  ### Number of samples to estimate the prediction interval
  nsamp <- 500
  
  ### 1. Prepare the data
  ### remove rows with NA
  data.depth <-  data[complete.cases(data[,c(properties,covars)]), c(properties,covars)]
  
  ### Perform the ilr transformation
  library(compositions)
  ilr.properties <- ilr(data.depth[,properties])
  ilr.properties <- as.data.frame(ilr.properties)
  data.depth$ilr1 <- ilr.properties$V1
  data.depth$ilr2 <- ilr.properties$V2
  
  
  ### 2. Perform cross-validation
  # create a data.frame to store the prediction of each fold (record) - Five columns, three for the original fractions,
  # two for the ilr
  testTable  <- data.depth[,c(properties,"ilr1","ilr2")]
  predsTable <- data.frame(matrix(ncol = 5,nrow = nrow(testTable), data = NA))
  colnames(predsTable) <- c("pred.ilr1","pred.ilr2","pred.HOC","pred.POC","pred.ROC")
  testTable <- cbind(testTable,predsTable)
  
  # make the formula for ilr1
  form1 <- as.formula(paste("ilr1", paste(covars, collapse=" + "), sep=" ~ "))
  form2 <- as.formula(paste("ilr2", paste(covars, collapse=" + "), sep=" ~ "))
  
  # samples that will be used for the validation
  set.seed(2013)
  sp <- sample(1:nfolds, nrow(data.depth), replace = T)
  sp <- sample(sp)
  data.depth$sp <- sp
  
  for(j in 1:nfolds){
    # extracting the training and testing indices
    # this way works with folds list (but not foldID)
    dat.train <- data.depth[data.depth$sp != j,][c(properties, "ilr1", "ilr2", covars)]
    testSet   <- data.depth[data.depth$sp == j,covars]
    
    #################### 
    ## model 
    #library(caret)
    library(ranger)
    # rf calibration and prediction
    rf.ilr1 <- ranger(form1, 
                      data = dat.train, 
                      num.trees = 500, 
                      quantreg = TRUE) # model fitting on training set
    testTable$pred.ilr1[data.depth$sp == j] <- predict(rf.ilr1, data = testSet)$predictions # predict the test set
    
    rf.ilr2 <- ranger(form2, 
                      data = dat.train, 
                      num.trees = 500, 
                      quantreg = TRUE) # model fitting on training set
    testTable$pred.ilr2[data.depth$sp == j] <- predict(rf.ilr2, data = testSet)$predictions # predict the test set
    
  }
  
  ### Back-transform to composition (in %)
  Backt <- compositions::ilrInv(testTable[,6:7])
  Backt <- as.data.frame(Backt)
  colnames(Backt) <- orderProps
  testTable$pred.HOC <- Backt$HOC * 100
  testTable$pred.POC <- Backt$POC * 100
  testTable$pred.ROC <- Backt$ROC * 100
  
  ### Store predictions for Solar diagram
  SolarDiagram.list[[i]] = testTable
  
  ##### Validation statistics table
  tab.res <- data.frame(matrix(data=NA, nrow = 5, ncol = 8))
  tab.res[,3] <- c("ilr1","ilr2", "HOC","POC","ROC")
  tab.res[,2] <- depth
  tab.res[,1] <- combi.names[i]
  colnames(tab.res) <- c("Combination","Depth",'SOC fraction', 'ME', 'RMSE', 'r2', 'R2', 'rhoC')
  
  tab.res[1,][4:8] <- eval(testTable$ilr1, testTable$pred.ilr1, obj = 'quant')[1:5]
  tab.res[2,][4:8] <- eval(testTable$ilr2, testTable$pred.ilr2, obj = 'quant')[1:5]
  
  ### Here, in case I use this for different depths
  HOC.index <- grep(names(testTable),pattern = "HOC.")
  POC.index <- grep(names(testTable),pattern = "POC.")
  ROC.index <- grep(names(testTable),pattern = "ROC.")
  
  tab.res[3,][4:8] <- eval(testTable[,HOC.index], testTable$pred.HOC, obj = 'quant')[1:5]
  tab.res[4,][4:8] <- eval(testTable[,POC.index], testTable$pred.POC, obj = 'quant')[1:5]
  tab.res[5,][4:8] <- eval(testTable[,ROC.index], testTable$pred.ROC, obj = 'quant')[1:5]
  tab.res
  
  rho.list[[i]] <- tab.res[3:5, "rhoC"]
  
 
  write.csv(tab.res, file=paste0("./TableCV_",dfilenames,"_",combi.names[i],".csv"))
  
  ### Make cross-validation ggplots
  p4 <- ggplot(testTable, aes(ilr1, pred.ilr1)) + geom_hex(binwidth = c(0.1,0.1), aes(colour = ..count..)) +
    scale_fill_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    scale_colour_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    guides(fill = guide_colourbar(title = "Count")) +
    geom_abline(intercept = 0, slope = 1, color="steelblue3", size=1)+
    xlab("Observed ilr1")+
    ylab("Predicted ilr1")+
    ggtitle(paste0("ilr1 (",depth,")"))
  
  p5 <- ggplot(testTable, aes(ilr2, pred.ilr2)) + geom_hex(binwidth = c(0.1,0.1), aes(colour = ..count..)) +
    scale_fill_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    scale_colour_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans())+
    guides(fill = guide_colourbar(title = "Count")) +
    geom_abline(intercept = 0, slope = 1, color="steelblue3", size=1)+
    xlab("Observed ilr2")+
    ylab("Predicted ilr2")+
    ggtitle(paste0("ilr2 (",depth,")"))
  
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
    ggtitle(paste0("HOC (",depth,")"))+
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
    ggtitle(paste0("POC (",depth,")"))+
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
    ggtitle(paste0("ROC (",depth,")"))+
    xlim(0,100)+
    ylim(0,100)

  pdf(paste0("CVplots.",dfilenames,"_",combi.names[i],".pdf"),width = 14,height = 8)
  print(gridExtra::grid.arrange(p1,p2,p3,p4,p5,nrow=2))
  dev.off()
  
  
  #### 3. Map the predictions on two tiles
  
  # Fit RF model using all data
  set.seed(thisseed+1998)
  rf.ilr1 <- ranger(form1, 
                    data = data.depth,
                    num.trees = ntreefinal, 
                    quantreg = TRUE)
  
  # Fit RF model using all data
  set.seed(thisseed+2046)
  rf.ilr2 <- ranger(form2, 
                    data = data.depth,
                    num.trees = ntreefinal,
                    quantreg = TRUE)
  
  
  for (t in 1:length(tiles)) {
    
    
    # if(length(df.na)==0){
    #   print("no missing data in this tile")
    # 
    
  ### Transform into a dataframe
  tile.df <- as.data.frame(tiles[[t]], row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)
  
  ## Copy dataframe for outputs
  tile.df.out <- tile.df
  tile.df.out$pred.HOC <- NA
  tile.df.out$pred.POC <- NA
  tile.df.out$pred.ROC <- NA
  tile.df.out$pred.HOC.05 <- NA
  tile.df.out$pred.POC.05 <- NA
  tile.df.out$pred.ROC.05 <- NA
  tile.df.out$pred.HOC.95 <- NA
  tile.df.out$pred.POC.95 <- NA
  tile.df.out$pred.ROC.95 <- NA
  
  ### Extract the index of the dataframe rows that are na/nan/Inf
  df.na <- which(apply(tile.df, MARGIN = 1, FUN = function(x) {any(is.na(x))}))
  
  if(length(df.na)==0){
    print("no missing data in this tile")
    
  ### Predict mean SOC fractions (back-transformed ilr predictions)
  pred.ilr1 <- predict(object=rf.ilr1,
                       data = tile.df,
                       type = "response")$predictions
  
  pred.ilr2 <- predict(object=rf.ilr2,
                       data = tile.df,
                       type = "response")$predictions
  
  preds.ilr <- data.frame(pred.ilr1=pred.ilr1, pred.ilr2=pred.ilr2)
    
    ### Back-transform to HOC, POC, ROC (%)
    Backt <- compositions::ilrInv(preds.ilr)
    Backt <- as.data.frame(Backt)
    colnames(Backt) <- orderProps 
    tile.df.out$pred.HOC <- Backt$HOC * 100
    tile.df.out$pred.POC <- Backt$POC * 100
    tile.df.out$pred.ROC <- Backt$ROC * 100
    
    ### Sample 500 values of ilr1 and ilr2 from the quantile distribution to estimate the prediction intervals
    pred.ilr1.sample <- predict(object=rf.ilr1,
                                data = tile.df, 
                                type = "quantiles",
                                what = function(x) sample(x, nsamp, replace = TRUE))$predictions
    
    pred.ilr2.sample <- predict(object=rf.ilr2,
                                data = tile.df, 
                                type = "quantiles",
                                what = function(x) sample(x, nsamp, replace = TRUE))$predictions
    
    ### Create array with both matrices
    ### Dimnames
    column.names <- paste0("Samp", c(1:nsamp))
    row.names <- paste0("row", c(1:nrow(tile.df)))
    matrix.names <- c("ilr1","ilr2")
    
    z <- array(c(pred.ilr1.sample, pred.ilr2.sample),
               dim = c(nrow(tile.df), nsamp, 2),
               dimnames = list(row.names, column.names, matrix.names)) 
    
    ### Back-transform to HOC, POC, ROC (%) with apply over the array
    backtf <- function(x){Backt <- compositions::ilrInv(x); return(Backt)}
    Backt <- apply(X = z, MARGIN = c(1,2) , FUN = backtf)
    dimnames(Backt)[[1]] <- orderProps 
    
    ### Calculate percentiles
    SOCfr.95 <- apply(X = Backt, MARGIN = c(1,2) , FUN = function(x) quantile(x, c(.95)))
    SOCfr.05 <- apply(X = Backt, MARGIN = c(1,2) , FUN = function(x) quantile(x, c(.05)))
    
    ### Transpose and transform into %
    UPL <-as.data.frame(t(SOCfr.95)); UPL <- UPL*100
    LPL <-as.data.frame(t(SOCfr.05)); LPL <- LPL*100
    
    ### Assign to dataframe with all rows
    tile.df.out$pred.HOC.05 <- LPL$HOC 
    tile.df.out$pred.POC.05 <- LPL$POC 
    tile.df.out$pred.ROC.05 <- LPL$ROC 
    
    tile.df.out$pred.HOC.95 <- UPL$HOC 
    tile.df.out$pred.POC.95 <- UPL$POC     
    tile.df.out$pred.ROC.95 <- UPL$ROC 
    
    gc()
    
    } else if (nrow(tile.df) - length(df.na) > 1) {
      
      print("some missing data in tile")
    
    ### Predict mean SOC fractions (back-transformed ilr predictions)
    pred.ilr1 <- predict(object=rf.ilr1,
                         data = tile.df[-df.na, ],
                         type = "response")$predictions
    
    pred.ilr2 <- predict(object=rf.ilr2,
                         data = tile.df[-df.na, ],
                         type = "response")$predictions
    
    preds.ilr <- data.frame(pred.ilr1=pred.ilr1, pred.ilr2=pred.ilr2)
    
    ### Back-transform to HOC, POC, ROC (%)
    Backt <- compositions::ilrInv(preds.ilr)
    Backt <- as.data.frame(Backt)
    colnames(Backt) <- orderProps
    tile.df.out[-df.na, ]$pred.HOC <- Backt$HOC * 100
    tile.df.out[-df.na, ]$pred.POC <- Backt$POC * 100
    tile.df.out[-df.na, ]$pred.ROC <- Backt$ROC * 100
    
    
    ### Sample 500 values of ilr1 and ilr2 from the quantile distribution to estimate the prediction intervals
    pred.ilr1.sample <- predict(object=rf.ilr1,
                                data = tile.df[-df.na, ], 
                                type = "quantiles",
                                what = function(x) sample(x, nsamp, replace = TRUE))$predictions
    
    pred.ilr2.sample <- predict(object=rf.ilr2,
                                data = tile.df[-df.na, ], 
                                type = "quantiles",
                                what = function(x) sample(x, nsamp, replace = TRUE))$predictions
    
    ### Create array with both matrices
    ### Dimnames
    column.names <- paste0("Samp", c(1:nsamp))
    row.names <- paste0("row", c(1:nrow(tile.df[-df.na,])))
    matrix.names <- c("ilr1","ilr2")
    
    z <- array(c(pred.ilr1.sample, pred.ilr2.sample), 
               dim = c(nrow(tile.df[-df.na, ]), nsamp, 2), 
               dimnames = list(row.names, column.names, matrix.names)) 
    
    ### Back-transform to HOC, POC, ROC (%) with apply over the array
    backtf <- function(x){Backt <- compositions::ilrInv(x); return(Backt)}
    Backt <- apply(X = z, MARGIN = c(1,2) , FUN = backtf)
    dimnames(Backt)[[1]] <- orderProps 
    
    ### Calculate percentiles
    SOCfr.95 <- apply(X = Backt, MARGIN = c(1,2) , FUN = function(x) quantile(x, c(.95)))
    SOCfr.05 <- apply(X = Backt, MARGIN = c(1,2) , FUN = function(x) quantile(x, c(.05)))
    
    ### Transpose and transform into %
    UPL <-as.data.frame(t(SOCfr.95)); UPL <- UPL*100
    LPL <-as.data.frame(t(SOCfr.05)); LPL <- LPL*100
    
    ### Assign to dataframe with all rows
    tile.df.out[-df.na, ]$pred.HOC.05 <- LPL$HOC 
    tile.df.out[-df.na, ]$pred.POC.05 <- LPL$POC     
    tile.df.out[-df.na, ]$pred.ROC.05 <- LPL$ROC 
    
    tile.df.out[-df.na, ]$pred.HOC.95 <- UPL$HOC 
    tile.df.out[-df.na, ]$pred.POC.95 <- UPL$POC     
    tile.df.out[-df.na, ]$pred.ROC.95 <- UPL$ROC 
    
    gc()
    
  }
    
    ### Assign the values to a new raster
    pred.HOC <- setValues(tiles[[t]][[1]], tile.df.out$pred.HOC)
    names(pred.HOC) <- paste0("HOC.",dfilenames)
    
    pred.POC <- setValues(tiles[[t]][[1]], tile.df.out$pred.POC)
    names(pred.POC) <- paste0("POC.",dfilenames)
    
    pred.ROC <- setValues(tiles[[t]][[1]], tile.df.out$pred.ROC)
    names(pred.ROC) <- paste0("ROC.",dfilenames)
    
    pred.HOC.05 <- setValues(tiles[[t]][[1]], tile.df.out$pred.HOC.05)
    names(pred.HOC.05) <- paste0("HOC.05p.",dfilenames)
    
    pred.POC.05 <- setValues(tiles[[t]][[1]], tile.df.out$pred.POC.05)
    names(pred.POC.05) <- paste0("POC.05p.",dfilenames)
    
    pred.ROC.05 <- setValues(tiles[[t]][[1]], tile.df.out$pred.ROC.05)
    names(pred.ROC.05) <- paste0("ROC.05p.",dfilenames)
    
    pred.HOC.95 <- setValues(tiles[[t]][[1]], tile.df.out$pred.HOC.95)
    names(pred.HOC.95) <- paste0("HOC.95p.",dfilenames)
    
    pred.POC.95 <- setValues(tiles[[t]][[1]], tile.df.out$pred.POC.95)
    names(pred.POC.95) <- paste0("POC.95p.",dfilenames)
    
    pred.ROC.95 <- setValues(tiles[[t]][[1]], tile.df.out$pred.ROC.95)
    names(pred.ROC.95) <- paste0("ROC.95p.",dfilenames)
    
    
    ### Make levelplot
    
    ras <- stack(pred.HOC.05, pred.HOC, pred.HOC.95,
                 pred.POC.05, pred.POC, pred.POC.95,
                 pred.ROC.05, pred.ROC, pred.ROC.95)
    
    # names <- c("HOC LPL","HOC","HOC UPL",
    #            "POC LPL","POC","POC UPL",
    #            "ROC LPL","ROC","ROC UPL")
    # 
    
    pdf(paste0("SOCfractions.Tile",t,".",dfilenames,".",combi.names[i],".pdf"),width = 14,height = 14)
    print(levelplot(ras, 
              margin=FALSE,
              layout=c(3,3),
              cex=0.8,
        
             # names.attr = names,
              par.strip.text=list(cex=1.2, lines=1, col='black'),# suppress marginal graphics
              colorkey=list(
                space='right',                   # plot legend at the right
                labels=list(at=round(seq(0,100, by=5)), font=4)  # legend ticks and labels 
              ),    
              par.settings=list(
                strip.background=list(col='transparent'),
                axis.line=list(col='transparent') # suppress axes and legend outline
              ),
              scales=list(draw=FALSE),            # suppress axis labels
              col.regions=rev(viridis(100)),      # colour ramp
              at=seq(0, max(na.omit(getValues(ras))), len=100)
    ))
    dev.off()
    
    pdf(paste0("rgb.Tile",t,".",dfilenames,".",combi.names[i],".pdf"),width = 14,height = 14)
    plotRGB(ras, r = 2, g = 5, b = 8, stretch ="lin", scale=100)
    dev.off()
  }
  
}

rm("aes.list", "Backt", "backtf","column.names","dat.train" ,"data","data.depth",
   "df.na","form1","form2","HOC.index","HOCsoildat","i","ilr.properties","j",
   "list_ras","LPL","matrix.names","names","nfolds","nsamp","ntreefinal","obser",
   "orderProps","p1","p2","p3","p4","p5","POC.index","pred.HOC","pred.HOC.05",
   "pred.HOC.95","pred.ilr1","pred.ilr1.sample","pred.ilr2","pred.ilr2.sample",
   "pred.POC","pred.POC.05","pred.POC.95","pred.ROC","pred.ROC.05","pred.ROC.95",
   "preds.ilr","predsTable","properties","ras","rf.ilr1","rf.ilr2","ROC.index",
   "row.names","SOCfr.05","SOCfr.95","sp","t","tab.res","testSet","testTable",
   "thisseed","tile.df","tile.df.out","UPL","z","outDir")


# 3. Make plots for SOC fractions' individual models ----------------------

dir.create(paste0(OutDir,dfilenames,"/IndModels/"))
setwd(paste0(OutDir,dfilenames,"/IndModels/")) 
some.seeds2 <- c(2013, 2016, 2021)  

### Dataset
data = SOCFr
covars = c(covar.names, covar.depth.specific)
properties <- possible.combis[[1]]
  
### Model settings
nfolds = 10
thisseed <- some.seeds2[1]
ntreefinal <- 500

### 1. Prepare the data
### remove rows with NA
data.depth <-  data[complete.cases(data[,c(properties,covars)]), c(properties,covars)]
  
### 2. Perform cross-validation

# create a data.frame to store the prediction of each fold (record) - Five columns, three for the original fractions,
# two for the ilr
testTable  <- data.depth[,c(properties)]
predsTable <- data.frame(matrix(ncol = 3, nrow = nrow(testTable), data = NA))
colnames(predsTable) <- c("pred.HOC","pred.POC","pred.ROC")
testTable <- cbind(testTable,predsTable); rm(predsTable)
  
# make the formulas for the three SOC fractions
HOC.var <-properties[grep(properties,pattern = "HOC.")]
POC.var <-properties[grep(properties,pattern = "POC.")]
ROC.var <-properties[grep(properties,pattern = "ROC.")]

form.HOC <- as.formula(paste(HOC.var, paste(covars, collapse=" + "), sep=" ~ "))
form.POC <- as.formula(paste(POC.var, paste(covars, collapse=" + "), sep=" ~ "))
form.ROC <- as.formula(paste(ROC.var, paste(covars, collapse=" + "), sep=" ~ "))

# samples that will be used for the validation
set.seed(2013)
sp <- sample(1:nfolds, nrow(data.depth), replace = T)
sp <- sample(sp)
data.depth$sp <- sp
  
for(j in 1:nfolds){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  dat.train <- data.depth[data.depth$sp != j,][c(properties, covars)]
  testSet   <- data.depth[data.depth$sp == j,covars]
  
  #################### 
  ## model 
  library(ranger)
  # rf calibration and prediction
  rf.HOC <- ranger(form.HOC, 
                   data = dat.train, 
                   num.trees = 500, 
                   quantreg = TRUE) # model fitting on training set
  testTable$pred.HOC[data.depth$sp == j] <- predict(rf.HOC, data = testSet)$predictions # predict the test set
  
  rf.POC <- ranger(form.POC, 
                   data = dat.train, 
                   num.trees = 500, 
                   quantreg = TRUE) # model fitting on training set
  testTable$pred.POC[data.depth$sp == j] <- predict(rf.POC, data = testSet)$predictions # predict the test set
  
  rf.ROC <- ranger(form.ROC, 
                   data = dat.train, 
                   num.trees = 500, 
                   quantreg = TRUE) # model fitting on training set
  testTable$pred.ROC[data.depth$sp == j] <- predict(rf.ROC, data = testSet)$predictions # predict the test set
  
  }

### Store predictions for Solar diagram
SolarDiagram.list2 <- c(SolarDiagram.list, testTable)
  
##### Validation statistics table
tab.res <- data.frame(matrix(data=NA, nrow = 3, ncol = 8))
tab.res[,3] <- c("HOC","POC","ROC")
tab.res[,2] <- depth
tab.res[,1] <- "Individual models"
colnames(tab.res) <- c("Combination","Depth",'SOC fraction', 'ME', 'RMSE', 'r2', 'R2', 'rhoC')
  
### Here, in case I use this for different depths
HOC.index <- grep(names(testTable),pattern = "HOC.")
POC.index <- grep(names(testTable),pattern = "POC.")
ROC.index <- grep(names(testTable),pattern = "ROC.")
  
tab.res[1,][4:8] <- eval(testTable[,HOC.index], testTable$pred.HOC, obj = 'quant')[1:5]
tab.res[2,][4:8] <- eval(testTable[,POC.index], testTable$pred.POC, obj = 'quant')[1:5]
tab.res[3,][4:8] <- eval(testTable[,ROC.index], testTable$pred.ROC, obj = 'quant')[1:5]
tab.res
  
write.csv(tab.res, file=paste0("./TableCV_",dfilenames,"IndModels.csv"))
  
### List of variables
#aes.list <- paste0(c("HOC.", "POC.", "ROC."), gsub(pattern="_", replacement = ".", x = dfilenames))
aes.list <- names(testTable)[1:3]
  
p1 <- ggplot(testTable, aes_string(aes.list[1], "pred.HOC"))+
  geom_hex(binwidth = c(1,1), aes(colour = ..count..)) +
  scale_fill_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans()) +
  scale_colour_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans()) +
  guides(fill = guide_colourbar(title = "Count")) +
  geom_abline(intercept = 0, slope = 1, color="steelblue3", size=1) +
  xlab("Observed HOC (%)") +
  ylab("Predicted HOC (%)") +
  ggtitle(paste0("HOC (",depth,")")) +
  xlim(0,100) +
  ylim(0,100)
  
p2 <- ggplot(testTable, aes_string(aes.list[2], "pred.POC")) +
  geom_hex(binwidth = c(1,1), aes(colour = ..count..)) +
  scale_fill_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans()) +
  scale_colour_viridis_c(option = "A",direction = 1, trans = pseudo_log_trans()) +
  guides(fill = guide_colourbar(title = "Count")) +
    geom_abline(intercept = 0, slope = 1, color="steelblue3", size=1)+
    xlab("Observed POC (%)")+
    ylab("Predicted POC (%)")+
    ggtitle(paste0("POC (",depth,")"))+
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
    ggtitle(paste0("ROC (",depth,")"))+
    xlim(0,100)+
    ylim(0,100)
  
pdf(paste0("CVplots.",dfilenames,"_IndModels.pdf"),width = 14,height = 4)
print(gridExtra::grid.arrange(p1,p2,p3,nrow=1))
dev.off()
  
  
#### 3. Map the predictions on two tiles

# Fit RF model using all data
set.seed(thisseed+1998)
rf.HOC <- ranger(form.HOC, 
                 data = data.depth,
                 num.trees = ntreefinal, 
                 quantreg = TRUE) # model fitting on training set

set.seed(thisseed+2046)
rf.POC <- ranger(form.POC, 
                 data = data.depth,
                 num.trees = ntreefinal, 
                 quantreg = TRUE) # model fitting on training set

set.seed(thisseed+1975)
rf.ROC <- ranger(form.ROC, 
                 data = data.depth,
                 num.trees = ntreefinal, 
                 quantreg = TRUE) # model fitting on training set

for (t in 1:length(tiles)) {
  
  ### Predictions
  pred.HOC <- raster::predict(object = tiles[[t]], 
                              model = rf.HOC, 
                              fun = function(model, ...) predict(model, ...)$predictions)
  names(pred.HOC) <- paste0("HOC.",dfilenames)
  
  
  pred.POC <- raster::predict(object = tiles[[t]], 
                              model = rf.POC, 
                              fun = function(model, ...) predict(model, ...)$predictions)
  names(pred.POC) <- paste0("POC.",dfilenames)
  
  pred.ROC <- raster::predict(object = tiles[[t]], 
                              model = rf.ROC, 
                              fun = function(model, ...) predict(model, ...)$predictions)
  names(pred.ROC) <- paste0("ROC.",dfilenames)
  
  ### Prediction interval limits
  pred.HOC.95 <- raster::predict(object = tiles[[t]], 
                               model = rf.HOC, 
                               fun = function(model, ...) predict(model,type = 'quantiles', quantiles =c(0.95), ...)$predictions[,1])
  names(pred.HOC.95) <- paste0("HOC.95p.",dfilenames)
  
  pred.HOC.05 <- raster::predict(object = tiles[[t]], 
                               model = rf.HOC, 
                               fun = function(model, ...) predict(model,type = 'quantiles', quantiles =c(0.05), ...)$predictions[,1])
  names(pred.HOC.05) <- paste0("HOC.05p.",dfilenames)
  
  pred.POC.95 <- raster::predict(object = tiles[[t]], 
                                 model = rf.POC, 
                                 fun = function(model, ...) predict(model,type = 'quantiles', quantiles =c(0.95), ...)$predictions[,1])
  names(pred.POC.95) <- paste0("POC.95p.",dfilenames)
  
  pred.POC.05 <- raster::predict(object = tiles[[t]], 
                                 model = rf.POC, 
                                 fun = function(model, ...) predict(model,type = 'quantiles', quantiles =c(0.05), ...)$predictions[,1])
  names(pred.POC.05) <- paste0("POC.05p.",dfilenames)
  
  
  pred.ROC.95 <- raster::predict(object = tiles[[t]], 
                                 model = rf.ROC, 
                                 fun = function(model, ...) predict(model,type = 'quantiles', quantiles =c(0.95), ...)$predictions[,1])
  names(pred.ROC.95) <- paste0("ROC.95p.",dfilenames)
  
  pred.ROC.05 <- raster::predict(object = tiles[[t]], 
                                 model = rf.ROC, 
                                 fun = function(model, ...) predict(model,type = 'quantiles', quantiles =c(0.05), ...)$predictions[,1])
  names(pred.ROC.05) <- paste0("ROC.05p.",dfilenames)
  
  ### Make levelplot
    
  ras <- stack(pred.HOC.05, pred.HOC, pred.HOC.95,
               pred.POC.05, pred.POC, pred.POC.95,
               pred.ROC.05, pred.ROC, pred.ROC.95)
    
    # names <- c("HOC LPL","HOC","HOC UPL",
    #            "POC LPL","POC","POC UPL",
    #            "ROC LPL","ROC","ROC UPL")
    # 
    
    pdf(paste0("SOCfractions.Tile",t,".",dfilenames,".IndModels.pdf"),width = 14,height = 14)
    print(levelplot(ras, 
                    margin=FALSE,
                    layout=c(3,3),
                    cex=0.8,
                    
                    # names.attr = names,
                    par.strip.text=list(cex=1.2, lines=1, col='black'),# suppress marginal graphics
                    colorkey=list(
                      space='right',                   # plot legend at the right
                      labels=list(at=round(seq(0,100, by=5)), font=4)  # legend ticks and labels 
                    ),    
                    par.settings=list(
                      strip.background=list(col='transparent'),
                      axis.line=list(col='transparent') # suppress axes and legend outline
                    ),
                    scales=list(draw=FALSE),            # suppress axis labels
                    col.regions=rev(viridis(100)),      # colour ramp
                    at=seq(0, max(na.omit(getValues(ras))), len=100)
    ))
    dev.off()
    
    pdf(paste0("rgb.Tile",t,".",dfilenames,".IndModels.pdf"),width = 14,height = 14)
    plotRGB(ras, r = 2, g = 5, b = 8, stretch ="lin", scale=100)
    dev.off()
}
  
rm("aes.list", "Backt", "backtf","column.names","dat.train" ,"data","data.depth",
   "df.na","form1","form2","HOC.index","HOCsoildat","i","ilr.properties","j",
   "list_ras","LPL","matrix.names","names","nfolds","nsamp","ntreefinal","obser",
   "orderProps","p1","p2","p3","p4","p5","POC.index","pred.HOC","pred.HOC.05",
   "pred.HOC.95","pred.ilr1","pred.ilr1.sample","pred.ilr2","pred.ilr2.sample",
   "pred.POC","pred.POC.05","pred.POC.95","pred.ROC","pred.ROC.05","pred.ROC.95",
   "preds.ilr","predsTable","properties","ras","rf.ilr1","rf.ilr2","ROC.index",
   "row.names","SOCfr.05","SOCfr.95","sp","t","testSet","testTable",
   "thisseed","tile.df","tile.df.out","UPL","z","outDir", rf.HOC, rf.POC, rf.ROC,
   ROC.var, some.seeds, some.seeds2,HOC.var, POC.var, form.HOC, form.POC, form.ROC, p,e,p2)

save.image("C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/Output_Comparison/15_30/comparison.RData")
load("C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/Output_Comparison/15_30/comparison.RData")


# 4. Compare the seven model possibilities --------------------------------

### Solar diagram

# load function
source('C:/Users/mrom8073/OneDrive - The University of Sydney (Staff)/SOC_saturation/codes/gg_comparison.R')

models = list(SolarDiagram.list2$pred.HOC,
              SolarDiagram.list$HPR$pred.HOC, SolarDiagram.list$HRP$pred.HOC,
              SolarDiagram.list$RPH$pred.HOC, SolarDiagram.list$RHP$pred.HOC,
              SolarDiagram.list$PRH$pred.HOC, SolarDiagram.list$PHR$pred.HOC)

models = list(SolarDiagram.list2$pred.POC,
              SolarDiagram.list$HPR$pred.POC, SolarDiagram.list$HRP$pred.POC,
              SolarDiagram.list$RPH$pred.POC, SolarDiagram.list$RHP$pred.POC,
              SolarDiagram.list$PRH$pred.POC, SolarDiagram.list$PHR$pred.POC)

models = list(SolarDiagram.list2$pred.ROC,
              SolarDiagram.list$HPR$pred.ROC, SolarDiagram.list$HRP$pred.ROC,
              SolarDiagram.list$RPH$pred.ROC, SolarDiagram.list$RHP$pred.ROC,
              SolarDiagram.list$PRH$pred.ROC, SolarDiagram.list$PHR$pred.ROC)

names(models) <- c("Ind Models",combi.names)

obser = SolarDiagram.list2$HOC.15.30
obser = SolarDiagram.list2$POC.15.30
obser = SolarDiagram.list2$ROC.15.30

colorval =c(tab.res$rhoC[1],  rho.list$HPR[1],  rho.list$HRP[1],
            rho.list$RPH[1],  rho.list$RHP[1],  rho.list$PRH[1],  rho.list$PHR[1])
colorval =c(tab.res$rhoC[2],  rho.list$HPR[2],  rho.list$HRP[2],
            rho.list$RPH[2],  rho.list$RHP[2],  rho.list$PRH[2],  rho.list$PHR[2])
colorval =c(tab.res$rhoC[3],  rho.list$HPR[3],  rho.list$HRP[3],
            rho.list$RPH[3],  rho.list$RHP[3],  rho.list$PRH[3],  rho.list$PHR[3])

# make solar diagram
gg_solar(mods = models, 
         obs = obser,
         colorval = colorval,
         colorval.name = 'CCC',
         label = TRUE, 
         x.axis_begin = -1.1,
         x.axis_end = 1.1,
         y.axis_end = 1.9,
         by = 0.1)

# make Taylor diagram
gg_taylor(mods = models, 
          obs = obser, 
          label = TRUE)

### end of the script