###  Date : Thu Dec 23 08:47:22 2021                      
###  Project : TERN 
###  Purpose : Prepare data for and develop bootstrapped RF models
#################################

library(terra)
library(ranger)
library(hexbin)
library(Cubist)
library(stringr)
library(ggplot2)
library(sf)

terraOptions(progress = 1)

scriptDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/Functions'
#covDir = '/datasets/work/lw-soildatarepo/work/http/Products/TERN/Covariates/Mosaics/90m_PCA'
covDir = '/datasets/work/lw-soildatarepo/work/http/Products/TERN/Covariates/Mosaics/90m'
rootDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/MeasuredTextures/V3/Parsimonious'
workDir <- paste0(rootDir, '/TrainingData')
if(!dir.exists(workDir)){dir.create(workDir, recursive = T)}


source(paste0(scriptDir, '/GeneralUtils.R'))
source(paste0(scriptDir, '/ModelUtils.R'))
source(paste0(scriptDir, '/RandomForestUtils_V2.R'))

numBoots=50


inDF <- readRDS( paste0(rootDir,'/allDrills', '.rds'))
pts <- st_as_sf(inDF, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
plot(st_geometry(pts))
head(inDF)
colnames(inDF)

c1 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/CLY/CLY_000_005_EV_N_P_AU_TRN_N_20210902.tif')
c2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/CLY/CLY_005_015_EV_N_P_AU_TRN_N_20210902.tif')
c3 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/CLY/CLY_015_030_EV_N_P_AU_TRN_N_20210902.tif')
c4 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/CLY/CLY_030_060_EV_N_P_AU_TRN_N_20210902.tif')
c5 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/CLY/CLY_060_100_EV_N_P_AU_TRN_N_20210902.tif')
c6 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/CLY/CLY_100_200_EV_N_P_AU_TRN_N_20210902.tif')

s1 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SND/SND_000_005_EV_N_P_AU_TRN_N_20210902.tif')
s2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SND/SND_005_015_EV_N_P_AU_TRN_N_20210902.tif')
s3 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SND/SND_015_030_EV_N_P_AU_TRN_N_20210902.tif')
s4 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SND/SND_030_060_EV_N_P_AU_TRN_N_20210902.tif')
s5 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SND/SND_060_100_EV_N_P_AU_TRN_N_20210902.tif')
s6 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SND/SND_100_200_EV_N_P_AU_TRN_N_20210902.tif')

soc1 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SOC/SOC_000_005_EV_N_P_AU_NAT_N_20220727.tif')
soc2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SOC/SOC_005_015_EV_N_P_AU_NAT_N_20220727.tif')
soc3 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SOC/SOC_015_030_EV_N_P_AU_NAT_N_20220727.tif')
soc4 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SOC/SOC_030_060_EV_N_P_AU_NAT_N_20220727.tif')
soc5 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SOC/SOC_060_100_EV_N_P_AU_NAT_N_20220727.tif')
soc6 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SOC/SOC_100_200_EV_N_P_AU_NAT_N_20220727.tif')

ph1 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/PHW/PHW_000_005_EV_N_P_AU_TRN_N_20220520.tif')
ph2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/PHW/PHW_005_015_EV_N_P_AU_TRN_N_20220520.tif')
ph3 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/PHW/PHW_015_030_EV_N_P_AU_TRN_N_20220520.tif')
ph4 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/PHW/PHW_030_060_EV_N_P_AU_TRN_N_20220520.tif')
ph5 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/PHW/PHW_060_100_EV_N_P_AU_TRN_N_20220520.tif')
ph6 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/PHW/PHW_100_200_EV_N_P_AU_TRN_N_20220520.tif')

slga <- vector('list', 6)
slga[[1]] <- c(c1, s1, soc1, ph1)
slga[[2]] <- c(c2, s2, soc2, ph2)
slga[[3]] <- c(c3, s3, soc3, ph3)
slga[[4]] <- c(c4, s4, soc4, ph4)
slga[[5]] <- c(c5, s5, soc5, ph5)
slga[[6]] <- c(c6, s6, soc6, ph6)

#covs <- c(c1,c2, c3, c4, c5, c6, s1, s2, s3,s4, s5, s6, soc1, soc2, soc3, soc4, soc5, soc6, ph1, ph2, ph3, ph4, ph5, ph6)

# slgaCovs <- terra::extract(covs, vect(pts))
# head(slgaCovs)

#covs <- data.frame(inDF$DUL_005, inDF$DLL_005, slgaCovs$CLY_000_005_EV_N_P_AU_TRN_N_20210902, slgaCovs$SND_000_005_EV_N_P_AU_TRN_N_20210902, slgaCovs$SOC_000_005_EV_N_P_AU_NAT_N_20120404, slgaCovs$PHW_000_005_EV_N_P_AU_TRN_N_20222005)
#covs <- na.omit(covs)

modelling.samplePercentage = 90

# allcovs <- na.omit(data.frame( covs, inDF[, 19:91]))
# names(inDF)
# nrow(allcovs)

splitSamples <-createTrainingSample(dataframe=inDF, seed=111, percentOfTraining=modelling.samplePercentage)
trainSet <- as.data.frame(splitSamples[1])
validSet <- as.data.frame(splitSamples[2])
colnames(trainSet) <- colnames(inDF)
colnames(validSet) <- colnames(inDF)

write.table(trainSet, paste0(workDir, '/trainingData.csv'), sep=",", row.names=F)
write.table(validSet, paste0(workDir, '/validationData.csv'), sep=",", row.names=F)

trainSet <- read.csv(paste0(workDir, '/trainingData.csv'), stringsAsFactors = F)
validSet <- read.csv(paste0(workDir, '/validationData.csv'), stringsAsFactors = F)




# library(Cubist)
# mod <- cubist(y=as.numeric(allTrainSet[,1]), x=allTrainSet[,-1] , committees = 1)
# summary(mod)
# predsDUL <- predict(mod, allValidSet[,-1])
# 
# fitStats(obsVal=allValidSet[,1], modelVal=predsDUL)
# 
# 
# library(ranger)
# mod <- ranger(allTrainSet[,1] ~ ., data = allTrainSet[,-1], write.forest = T, importance = 'impurity', num.trees = 500)
# mod
# preds <- predict(mod, allValidSet[,-1])
# fitStats(obsVal=allValidSet[,1], modelVal=preds$predictions)
# sort(mod$variable.importance)


####   DLL predictions are best

# awc <- predsDUL-predsDLL
# hist(awc)




atts <- c('DLL', 'DUL')
depths <- c('005','015','030','060','100','200')


for (jj in 2:length(depths)) {
  
  depth=depths[jj]
  
  slgaRs <- slga[[jj]] 
  
  ptsTrain <- st_as_sf(trainSet, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
  slgaTrain <- terra::extract(slgaRs, vect(ptsTrain))

  ptsValid <- st_as_sf(validSet, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
  slgaValid <- terra::extract(slgaRs, vect(ptsValid))
  
 
  
  ##### generate trainset for this attribute and depth with slga depths added in
  col <- paste0('DLL_', depth)
  colNum <- which(!is.na(match(colnames(trainSet), col)))
  col2 <- paste0('DUL_', depth)
  colNum2 <- which(!is.na(match(colnames(trainSet), col2)))

  allTrainSet <- data.frame(trainSet[, 1:3], trainSet[colNum],  trainSet[colNum2], slgaTrain[, -1], trainSet[, 19:91])
  allValidSet <- data.frame(validSet[, 1:3], validSet[colNum],  validSet[colNum2], slgaValid[, -1], validSet[, 19:91])
  
  idxs <- which(!is.na(allTrainSet[4]))
  dSet <- allTrainSet[idxs,  ]
  write.csv(dSet, paste0(workDir, '/All_AWC_TrainingData_', depth, '.csv'), row.names = F)
  
  idxs <- which(!is.na(allValidSet[4]))
  vSet <- allValidSet[idxs,  ]
  write.csv(vSet, paste0(workDir, '/All_AWC_ValidData_', depth, '.csv'), row.names = F)
  
  
  #### generate training and external validation data sets for each bootstrap
  
  bootDir <- paste0(workDir, '/Boots/AWC_', depth)
  if(!dir.exists(bootDir)){dir.create(bootDir, recursive = T)}
  for (i in 1:numBoots) {
    print(i)
    #ccidxs <- which(complete.cases(dSet))
    #set2 <- dSet[ccidxs,]
    set2 <- dSet
    ### add a unique id for selecting random sample
    uid <- seq(1:nrow(set2))
    set3 <- cbind(uid, set2)
    
    sidxs <- sample(nrow(set3),replace=T)
    trainsetx = set3[sidxs,]
    
    tuids <- sort(unique(trainsetx$uid))
    vuids <- sort(which(!set3$uid %in% tuids))
    
    #vidxs <- which(vuids %in% tuids)
    
    bootValid <- set3[set3$uid %in% vuids,]
    write.csv(trainsetx, paste0(bootDir, '/AWC_boot_train_AWC_', depth, '_', i,'.csv'), row.names = F)
    write.csv(bootValid, paste0(bootDir, '/AWC_boot_valid_AWC_', depth, '_', i,'.csv'), row.names = F)
  }
  
  #### Generate the bootstrapped models
  for (i in 1:numBoots) {
    print(i)
    
    t1 <- read.csv(paste0(bootDir, '/AWC_boot_train_AWC_', depth, '_', i,'.csv'), stringsAsFactors = F)
    col <- paste0('DLL_', depth)
    colNumDLL <- which(!is.na(match(colnames(t1), col)))
   # t2 <- t1[, c(7:ncol(t1))]
    trainSetDLL <- t1[complete.cases(t1), ]
    
    v1 <- read.csv(paste0(bootDir, '/AWC_boot_valid_AWC_', depth, '_', i,'.csv'), stringsAsFactors = F)
    #v2 <- v1[, c(7:ncol(v1))]
    validSetDLL <- v1[complete.cases(v1), ]
    
    #which(v1$uid %in% t1$uid)
    
    #### Generate a RF model
    dv <- trainSetDLL[,colNumDLL]
    iv <- trainSetDLL[,7:ncol(trainSetDLL)]
    #colnames(iv)
    # Rmodel <- ranger(dv ~ ., data = iv, write.forest = T, importance = 'impurity', num.trees = 500)
    # modelDir <- paste0(rootDir, '/Models/', att, '/Depth_', depth)
    # if(!dir.exists(modelDir))dir.create(modelDir, recursive = T)
    # RmodelPath = paste0(modelDir, '/RFmodel_', att, '_', depth, '_', i, '.rds')
    # saveRDS(Rmodel,RmodelPath )
    # Rmodel <- readRDS(RmodelPath)
    # summariseRFModel(modelPath = RmodelPath, att)
    
    modDLL <- cubist(y=dv, x=iv, committees = 1)
    summary(modDLL)
    modelDir <- paste0(rootDir, '/Models/', 'DLL', '/Depth_', depth)
    if(!dir.exists(modelDir))dir.create(modelDir, recursive = T)
    RmodelPathDLL = paste0(modelDir, '/Cubistmodel_', 'DLL', '_', depth, '_', i, '.rds')
    saveRDS(modDLL,RmodelPathDLL )
    predsDLL <- predict(modDLL, validSetDLL[,7:ncol(validSetDLL)])

    RmodelFilenameDLL <- paste0(modelDir, '/Cubistmodel_', 'DLL', '_', depth, '_', i)
    print(fitStats(obsVal=validSetDLL[,colNumDLL], modelVal=predsDLL, attName=paste0('DLL',' ', depth, ' - Cubist'), outfilename = paste0(RmodelFilenameDLL, '_ModelStats.txt'), legPos='topleft', verbose = T))
    write.table(data.frame(obs=validSetDLL[,colNumDLL], pred=predsDLL), paste0(RmodelFilenameDLL, '_Obs_vs_Mod.csv'), sep=",", row.names=F)
    
    #####  Make DUL model  #####
    col <- paste0('DUL_', depth)
    colNumDUL <- which(!is.na(match(colnames(t1), col)))
    dv <- trainSetDLL[,colNumDUL]
    iv <- trainSetDLL[,c(colNumDLL, 7:ncol(trainSetDLL))]
    modDUL <- cubist(y=dv, x=iv, committees = 1)
    summary(modDUL)
    modelDir <- paste0(rootDir, '/Models/', 'DUL', '/Depth_', depth)
    if(!dir.exists(modelDir))dir.create(modelDir, recursive = T)
    RmodelPathDUL = paste0(modelDir, '/Cubistmodel_', 'DUL', '_', depth, '_', i, '.rds')
    saveRDS(modDUL,RmodelPathDUL )
    predsDUL <- predict(modDUL, validSetDLL[,c(colNumDLL,7:ncol(validSetDLL))])

    RmodelFilenameDUL <- paste0(modelDir, '/Cubistmodel_', 'DUL', '_', depth, '_', i)
    print(fitStats(obsVal=validSetDLL[,colNumDUL], modelVal=predsDUL, attName=paste0('DUL',' ', depth, ' - Cubist'), outfilename = paste0(RmodelFilenameDUL, '_ModelStats.txt'), legPos='topleft', verbose = T))
    write.table(data.frame(obs=validSetDLL[,colNumDLL], pred=predsDLL), paste0(RmodelFilenameDUL, '_Obs_vs_Mod.csv'), sep=",", row.names=F)
  }
}






