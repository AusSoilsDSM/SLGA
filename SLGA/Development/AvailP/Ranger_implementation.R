# Random Forest modeling for digital soil mapping
# Using Ranger within Caret implementation of the original Random Forests algorithm 
# Using Ranger for speed and Caret so that the predict raster function works
# Random hold back of 30% of training data for validation of model
# Model fitting using 10 K-folds with cross validation, final model fitted to 70% of training data
# Uncertainty analysis by Bootstrapping
#
# Version 1.4 - 15/04/2021 - Fixed bug with variable importance when only one depth present
# Version 1.3 - 17/03/2021 - Added concordance and bias stats of model fit for both model calibration and validation
# Version 1.2 - 15/03/2021 - Added variable importance calculation
# Version 1.1 - 28/01/2021 - Fixed bug with scripts ability to handle variable number of depths (not just the standard GSM depths) in the Training data file.
# Version 1.0 - 08/10/2020 - Bug with fitControl parameters (final model created using training data only)
# Version 0.1 - 22/05/2020 - Original script
#
# Required data and format
# Training data in csv file in TrainingData subdirectory with following column label and order: X, Y, ID, VALUE
# Covariate data in GTiff format, mosaic of whole modeling area in Covariate/Mosaics subdirectory
# Covariate tiles in Covariate/Tiles/TitleNumber subdirectory
#
## User inputs
wd <- "D:\\LASER4" #set working directory

## Processing starts here #####
###library(httr)
library(raster)
library(rgdal)
library(sp)
###library(devtools)

## Data prep
# Get training data
td <- paste(wd, "//TrainingData", sep = "")
setwd(td)
data <- read.table("ASRIS.csv", header = TRUE, sep = ",")
depths <- names(data[-(1:3)]) # List of depths in training data

# Get Covariates for whole modelling area
##files <- list.files(path = paste(wd, "//Covariates//Mosaics", sep = ""), pattern = "\\.tif", full.names = TRUE) 
files <- list.files(path = paste(wd, "//Covariates", sep = ""), pattern = "\\.tif", full.names = TRUE) 


# Stack covariate rasters
r1 <- raster(files[1])
for(i in 2:length(files)){
  r1 <- stack(r1, files [i])
}

# Intersect soil points with covariates
data_sp <- data
coordinates(data_sp) <- ~ X + Y
crs(data_sp) <- CRS('+init=EPSG:4326')
DSM_data <- raster::extract(r1, data_sp, sp = 1, method = 'simple')
DSM_data<- as.data.frame(DSM_data)
setwd(wd)
write.csv (DSM_data, file ="Site_Covariate_intersect.csv")

### Modelling
library(caret)
library(ranger)
library(dplyr)
library(e1071)
library(gstat)

dir.create("Bootstrap") # Create Bootstrap models directory
dir.create("Bootstrap/models")
calGOOFDat <- data.frame(Depths = NA, Calib_RMSE = NA, Calib_R2 = NA, Calib_MAE = NA, Calib_MSE = NA, Calib_CC = NA, Calib_Bias = NA) #Calibration data stats summary df
GOOFDat <- data.frame(Depths = NA, RMSE = NA, R2 = NA, MAE = NA, MSE = NA, CC = NA, Bias = NA) # Summary stats df
ovi <- data.frame(Depth = NA) # Variable importance across depths df

#Create concordance Function (M. Webb 2015)
ccc<- function(observed, predicted){
  mx=mean(observed)
  my=mean(predicted)
  s2x=var(observed)
  s2y=var(predicted)
  sxy=mean((observed-mx)*(predicted-my))
  ccc=2*sxy/(s2x+s2y+(mx-my)^2 )
  return(ccc)}

## For each depth fit a model
# Establish calibration and validation data sets
for (d in 1:length(depths)) {
  set.seed(123)
  DSM_data_depth <- DSM_data[,c(1:3,3+d,(4+length(depths)):(3+length(depths)+length(files)))] # Subset mod data to specfic depth
  DSM_data_depth <- DSM_data_depth[complete.cases(DSM_data_depth),] 
  training <- sample(nrow(DSM_data_depth), 0.7 * nrow(DSM_data_depth)) # Vector with training data row numbers
  cDat <- DSM_data_depth[training, 4:(4+length(files))] # Based on number of covariates in ..//mosaic folder 
  vDat <- DSM_data_depth[-training, 4:(4+length(files))] # Based on number of covariates in ..//mosaic folder

  # Set modelling parameters
  nbag <- 50 # Number of bootstraps
  fitControl <- trainControl(method = "cv", number = 10, p = 0.7, returnResamp = "final", verboseIter = FALSE, indexFinal = training) #Ten K-folds
  
  # Uncertainity anaylsis using bootstrapping method
  dir.create(paste("Bootstrap/models/", depths[d], sep = "")) # Create models subdirectory for each depth
  TrainDat <- DSM_data_depth[,4:(4+length(files))]
  names(TrainDat)[1] <- "VALUE"
  
  # Fit Ranger (RF) model for each bootstrap
  for (i in 1:nbag) {
    trainingREP <- sample.int(nrow(cDat), 1.0 * nrow(cDat), replace = TRUE)
    fit_ranger <- train(VALUE ~ ., data = TrainDat, method = "ranger", trControl = fitControl, importance = 'impurity')
    modelFile <- paste(paste(paste(paste(getwd(), "/Bootstrap/models/", depths[d], "/", sep = ""), "bootMod_", sep = ""), i, sep = ""), ".rds", sep = "")
    saveRDS(object = fit_ranger, file = modelFile)
    }

  # List all files indirectory
  r.models <- list.files(path = paste(getwd(), "/Bootstrap/models/", depths[d], "/", sep = ""), pattern = "\\.rds$", full.names = TRUE)

  # Determine overall variable importance
  fit_ranger <- readRDS(r.models[1])
  imp <- varImp(fit_ranger)
  vi <- imp$importance
  for (i in 2:nbag) {
    fit_ranger <- readRDS(r.models[i])
    imp <- varImp(fit_ranger)
    impdf <- imp$importance
    vi <- cbind(vi, impdf)
    }
  vi$Mean <- rowMeans(vi) # Calculate mean across all bootstrap models
  library(matrixStats) # Required for rowSds
  vi$SD <- rowSds(as.matrix(vi[,1:nbag])) # Calculate standard deviation across all bootstrap models
  vi$Rank <- (vi$Mean/vi$SD) # Calculate Rank based on mean/sd
  vi <- vi[order(-vi$Rank),] #Order according to rank
  mean <-  subset(vi, select = c('Mean'))
  ovi <- cbind(ovi,mean)
  
  # Assess goodness of fit
  # Calibration data
  Mat <- matrix(NA, nrow = nbag, ncol = 6)
  for (i in 1:nbag) {
    fit_ranger <- readRDS(r.models[i])
    Dat <- cDat[1]
    Dat <- setNames( Dat, c("obs")) 
    Dat$pred <- predict(fit_ranger, newdata = cDat)
    Mat[i, 1:3] <- as.matrix(defaultSummary(Dat)) # RMSE, R^2, MAE
    Mat[i, 4] <- mean((Dat$obs -Dat$pred)^2) #MSE
    Mat[i, 5] <- ccc(Dat$obs, Dat$pred) # Concordance
    Mat[i, 6] <- sum(Dat$pred - Dat$obs)/length(Dat$obs) # Bias
    }

  calDat <- as.data.frame(Mat)
  names(calDat) <- c("Calib_RMSE", "Calib_R2", "Calib_MAE", "Calib_MSE", "Calib_CC", "Calib_Bias")
  calDatmeans <- colMeans(calDat)
  calDatmeans$Depths <- depths[d]
  calGOOFDat <- rbind(calGOOFDat, calDatmeans)
  
  #Validation data
  Pred.V <- matrix(NA, ncol = nbag, nrow = nrow(vDat))
  cubiMat <- matrix(NA, nrow = nbag, ncol =6)
  for (i in 1:nbag) {
    fit_ranger <- readRDS(r.models[i])
    Pred.V[, i] <- predict(fit_ranger, newdata = vDat)
    Dat <- vDat[1]
    Dat <- setNames( Dat, c("obs")) 
    Dat$pred <- predict(fit_ranger, newdata = vDat)
    cubiMat[i, 1:3] <- as.matrix(defaultSummary(Dat)) # RMSE, R^2, MAE
    cubiMat[i, 4] <- as.matrix(mean((Dat$obs - Dat$pred)^2)) #MSE
    cubiMat[i, 5] <- ccc(Dat$obs, Dat$pred) # Concordance
    cubiMat[i, 6] <- sum(Dat$pred - Dat$obs)/length(Dat$obs) # Bias
  }
  
  Pred.V_mean <- rowMeans(Pred.V)

  valDat <- as.data.frame(cubiMat)
  names(valDat) <- c("RMSE", "R2", "MAE", "MSE", "CC", "Bias")
  valDatmeans <- colMeans(valDat)
  valDatmeans$Depths <- depths[d]
  GOOFDat <- rbind(GOOFDat, valDatmeans) # Validation stats
  
  avgMSE <- mean(valDat[, 4]) #Average validation MSE
  
  # Validate the quantification of uncertainity
  # SD of variance at each validation sample point due to bootstrapping = sqrt(variance + MSE)
  val.sd <- matrix(NA, ncol = 1, nrow = nrow(Pred.V))
  for (i in 1:nrow(Pred.V)) {
    val.sd[i, 1] <- sqrt(var(Pred.V[i, ]) + avgMSE)
    }

  #Percentile of normal distribution
  qp <- qnorm(c(0.995, 0.9875, 0.975, 0.95, 0.9, 0.8, 0.7, 0.6, 0.55, 0.525))
  # zfactor multiplication
  vMat <- matrix(NA, nrow = nrow(Pred.V), ncol = length(qp))
  for (i in 1:length(qp)) {
    vMat[, i] <- val.sd * qp[i]
    }

  # Upper predication limit
  uMat <- matrix(NA, nrow = nrow(Pred.V), ncol = length(qp))
  for (i in 1:length(qp)) {
    uMat[, i] <- Pred.V_mean + vMat[, i]
    }

  # Lower predication limit
  lMat <- matrix(NA, nrow = nrow(Pred.V), ncol = length(qp))
  for (i in 1:length(qp)) {
    lMat[, i] <- Pred.V_mean - vMat[, i]
    }

  #PI coverage probability (PICP)
  # Determine which predictions are within the PI limits
  bMat <- matrix(NA, nrow = nrow(Pred.V), ncol = length(qp))
  for (i in 1:ncol(bMat)) {
    names(vDat)[1] <- "VALUE"
    bMat[, i] <- as.numeric(vDat$VALUE <= uMat[, i] & vDat$VALUE >= lMat[, i])
  }
  # Plot results to file
  jpeg(file = paste(getwd(),"/Bootstrap/", depths[d], "_PICP_plot.jpg", sep = ""))
  cs <- c(99.5, 98.75, 97.5, 95, 90, 80, 70, 60, 55, 52.5)
  plot(cs, ((colSums(bMat)/nrow(bMat)) * 100), ylab = "PICP", xlab = "Confidence level", main = paste("Depth ", depths[d], sep = ""), abline(0,1), xlim = c(0, 100), ylim = c(0, 100))
  dev.off()
}
# Summaries variable importance data
ovi <- ovi[2:(1+length(depths))]
#ovi <- setNames(ovi, c(depths[1],depths[2],depths[3],depths[4],depths[5],depths[6]))
for (n in 1:length(depths)) {names(ovi)[n] <- depths[n]} #rename columns
ovi$Mean <- rowMeans(ovi) # Calculate mean across all depths
ovi$SD <- rowSds(as.matrix(ovi[,1:length(depths)])) # Calculate standard deviation across all bootstrap models
ovi$Rank <- (ovi$Mean/ovi$SD) # Calculate Rank based on mean/sd
ovi <- ovi[order(-ovi$Rank),] # Sort in descending order of importance

# Combine calibration and validation GOOF data
GOOFDat <- cbind(calGOOFDat, GOOFDat[2:7])

# Write Goodness of Fit data to file
setwd(paste(wd, "/Bootstrap/", sep = ""))
write.csv(GOOFDat, "GOOFData.csv", row.names = FALSE)
write.csv(avgMSE, "avgMSE.csv", row.names = FALSE) #Save to file for using in mapping part on HPC
write.csv(ovi, "Overall_variable_importance.csv")

### Mapping
## Predict fitted model tile by tile
# Make a list of tiles from tile folder
tile <- list.dirs(path = paste(wd, "//Covariates//Tiles", sep = ""), full.names = FALSE, recursive = FALSE)
dir.create(paste(wd,"/Bootstrap/map", sep = ""))

# Map tile by tile
for (t in 1:length(tile)) {
  dir.create(paste(wd,"/Bootstrap/map/", tile[t], sep = ""))  
  files <- list.files(path = paste(wd, "/Covariates/Tiles/", tile[t], "/", sep = ""), pattern = ".tif", full.names = TRUE, recursive = TRUE)
  
  # Stack covariates for this tile
  t1 <- raster(files[1])
  for(i in 2:length(files)){
    t1 <- stack(t1, files [i])
    }
  
  # Map each depth of this tile
  for (d in 1:length(depths)) {
    r.models <- list.files(path = paste(getwd(), "/models/", depths[d], "/", sep = ""), pattern = "\\.rds$", full.names = TRUE)
    dir.create(paste(wd,"/Bootstrap/map/", tile[t], "/", depths[d], sep = "")) # output folder
  
    # Variance of each prediction
    for (i in 1:nbag) {
      fit_ranger <- readRDS(r.models[i])
      mapFile <- paste(paste(paste(paste(wd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "bootMap_", sep = ""), i, sep = ""), ".tif", sep = "")
      predict(t1, fit_ranger, filename = mapFile, format = "GTiff", overwrite = T)
      }

    # Determine pixel mean
    # 1. Load pathway to rasters
    files <- list.files(paste(wd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), pattern = "bootMap_", full.names = TRUE)
    # 2. Stack raster
    m1 <- raster(files[1])
    for (i in 2:length(files)) {
      m1 <- stack(m1, files[i])
      }
    # 3. Calculate mean
    meanFile <- paste(paste(paste(wd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "meanPred_", sep = ""), ".tif", sep = "")
    bootMap.mean <- writeRaster(mean(m1), filename = meanFile, format = "GTiff", overwrite = TRUE)

    # Estimate variance at each pixel
    # Squared variance at each pixel
    for (i in 1:length(files)) {
      r2 <- raster(files[i])
      diffFile <- paste(paste(paste(paste(wd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "bootAbsDif_", sep = ""), i, sep = ""), ".tif", sep = "")
      jj <- (r2 - bootMap.mean)^2
      writeRaster(jj, filename = diffFile, format = "GTiff", overwrite = TRUE)
      }

    # Calculate sum of the squared difference
    # 1. Look for files with BootAbsDif in name
    files2 <- list.files(paste(wd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), pattern = "bootAbsDif", full.names = TRUE)
    # 2. Stack rasters
    r3 <- raster(files2[i])
    for (i in 2:length(files2)) {
      r3 <- stack(r3, files2[i])
      }
    # 3. Calculate sum of square differences for each pixel and write to file
    sqDiffFile <- paste(paste(paste(wd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "sqDiffPred_", sep = ""), ".tif", sep = "")
    bootMap.sqDiff <- writeRaster(sum(r3), filename = sqDiffFile, format = "GTiff", overwrite = TRUE)

    # Calculate variance
    varFile <- paste(paste(paste(wd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "varPred_", sep = ""), ".tif", sep = "")
    bootMap.var <- writeRaster(((1/(nbag - 1)) * bootMap.sqDiff), filename = varFile, format = "GTiff", overwrite = TRUE)

    # Calculate overall prediction variance
    varFile2 <- paste(paste(paste(wd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "varPredF_", sep = ""), ".tif", sep = "")
    bootMap.varF <- writeRaster((bootMap.var + avgMSE), filename = varFile2, format = "GTiff", overwrite = TRUE)

    # Determine prediction interval (PI)
    # PI = SD*z(90% probablity interval)
    # SD
    sdFile <- paste(paste(paste(wd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "sdPred_", sep = ""), ".tif", sep = "")
    bootMap.sd <- writeRaster(sqrt(bootMap.varF), filename = sdFile, format = "GTiff", overwrite = TRUE)

    # z - Standard error (se)
    seFile <- paste(paste(paste(wd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "sePred_", sep = ""), ".tif", sep = "")
    bootMap.se <- writeRaster((bootMap.sd * qnorm(0.95)), filename = seFile, format = "GTiff", overwrite = TRUE)

    # Map upper prediction limit
    uplFile <- paste(paste(paste(wd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "uplPred_", sep = ""), ".tif", sep = "")
    bootMap.upl <- writeRaster((bootMap.mean + bootMap.se), filename = uplFile, format = "GTiff", overwrite = TRUE)

    # Map lower prediction limit
    lplFile <- paste(paste(paste(wd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "lplPred_", sep = ""), ".tif", sep = "")
    bootMap.lpl <- writeRaster((bootMap.mean - bootMap.se), filename = lplFile, format = "GTiff", overwrite = TRUE)

    # Map prediction interval range
    pirFile <- paste(paste(paste(wd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "pirPred_", sep = ""), ".tif", sep = "")
    bootMap.pir <- writeRaster((bootMap.upl - bootMap.lpl), filename = pirFile, format = "GTiff", overwrite = TRUE)
    }
  }
### End of script Yah!
