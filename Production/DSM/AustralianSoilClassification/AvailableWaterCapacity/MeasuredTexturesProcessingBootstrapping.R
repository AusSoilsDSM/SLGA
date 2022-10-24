
#################################
###  Author : Ross Searle         
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
rootDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/MeasuredTextures/V2/Parsimonious'
workDir <- paste0(rootDir, '/TrainingData')
if(!dir.exists(workDir)){dir.create(workDir)}


source(paste0(scriptDir, '/GeneralUtils.R'))
source(paste0(scriptDir, '/ModelUtils.R'))
source(paste0(scriptDir, '/RandomForestUtils_V2.R'))

numBoots=50


inDF <- readRDS( paste0(rootDir,'/allDrills', '.rds'))
pts <- st_as_sf(inDF, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
plot(st_geometry(pts))
head(inDF, 1)
colnames(inDF)

# # Replace NAs with mean values - bit of a hack but not many values affected
# for (i in 18:ncol(alldf)) {
#   #print(paste0(colnames(alldf)[i], ' = ', sum(is.na(alldf[,i]))))
#   m <- mean(alldf[i][,1], na.rm=T)
#   idxs<-which(is.na(alldf[,i]))
#   alldf[idxs, i] <- m
# }
# 
# sum(is.na(alldf[, 18:ncol(alldf)]))
# write.csv(alldf, '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/MeasuredTextures/CovariateDrill_NoNAs.csv', row.names = F)


#### check out what the data looks like

# att='DUL'
# att='DLL'
# 
# ds <- c('005','015','030','060','100','200')
# for (i in 1:length(ds)) {
#   c <- inDF[paste0(att, '_', ds[i])][,1]
#   d <- density(na.omit(c))
#   plot(d, main=paste0("Kernel Density of ", att, " - ", ds[i], 'cm'))
#   polygon(d, col="blue", border="blue")
#   print(paste0('number of samples = ', length(c)))
#   print(summary(c))
# }



modelling.samplePercentage = 90


splitSamples <-createTrainingSample(dataframe=inDF, seed=111, percentOfTraining=modelling.samplePercentage)
trainSet <- as.data.frame(splitSamples[1])
validSet <- as.data.frame(splitSamples[2])
colnames(trainSet) <- colnames(inDF)
colnames(validSet) <- colnames(inDF)

write.table(trainSet, paste0(workDir, '/trainingData.csv'), sep=",", row.names=F)
write.table(validSet, paste0(workDir, '/validationData.csv'), sep=",", row.names=F)

allTrainSet <- read.csv(paste0(workDir, '/trainingData.csv'), stringsAsFactors = F)
allValidSet <- read.csv(paste0(workDir, '/validationData.csv'), stringsAsFactors = F)


#########  Start here for new model generation


atts <- c('DLL', 'DUL')
depths <- c('005','015','030','060','100','200')
# #att = 'DLL'
# att = 'DUL'
# 
#   depth = '005'
#   depth = '015'
#   depth = '030'
#   depth = '060'
#   depth = '100'
#   depth = '200'
  
for (ii in 1:length(atts)) {
  att=atts[ii]
    
  for (jj in 1:length(depths)) {
      
        depth=depths[jj]
          
        ##### generate trainset for this attribute and depth
        col <- paste0(att, '_', depth)
        colNum <- which(!is.na(match(colnames(allTrainSet), col)))

        idxs <- which(!is.na(allTrainSet[colNum]))
        colnames(allTrainSet)
        dSet <- allTrainSet[idxs,  ]

        #### generate training and external validation data sets for each bootstrap
        
        #####s  Select required covariates
        # cidxs <- which(!grepl('organism_PCA', colnames(dSet)))
        # set1 <- dSet[,cidxs]
        
        bootDir <- paste0(workDir, '/Boots/', att, '_', depth)
        if(!dir.exists(bootDir)){dir.create(bootDir, recursive = T)}
        for (i in 1:numBoots) {
          print(i)
          ccidxs <- which(complete.cases(dSet))
          set2 <- dSet[ccidxs,]
          ### add a unique id for selecting random sample
          uid <- seq(1:nrow(set2))
          set3 <- cbind(uid, set2)
          
          sidxs <- sample(nrow(set3),replace=T)
          trainsetx = set3[sidxs,]
          
          tuids <- sort(unique(trainsetx$uid))
          vuids <- sort(which(!set3$uid %in% tuids))
          
          #vidxs <- which(vuids %in% tuids)
          
          bootValid <- set3[set3$uid %in% vuids,]
          write.csv(trainsetx, paste0(bootDir, '/AWC_boot_train_', att, '_', depth, '_', i,'.csv'), row.names = F)
          write.csv(bootValid, paste0(bootDir, '/AWC_boot_valid_', att, '_', depth, '_', i,'.csv'), row.names = F)
        }
        
        #### Generate the RF model
        for (i in 1:numBoots) {
          print(i)
        
           t1 <- read.csv(paste0(bootDir, '/AWC_boot_train_', att, '_', depth, '_', i,'.csv'), stringsAsFactors = F)
           col <- paste0(att, '_', depth)
           colNum <- which(!is.na(match(colnames(t1), col)))
           t2 <- t1[, c(1,2, colNum, 20:ncol(t1))]
           trainSet <- t2[complete.cases(t2), ]
           
           v1 <- read.csv(paste0(bootDir, '/AWC_boot_valid_', att, '_', depth, '_', i,'.csv'), stringsAsFactors = F)
           v2 <- v1[, c(1,2, colNum, 20:ncol(v1))]
           validSet <- v2[complete.cases(v2), ]
           
           which(v1$uid %in% t1$uid)
        
            #### Generate a RF model
           dv <- trainSet[,3]
           iv <- trainSet[, 4:ncol(trainSet)]
           colnames(iv)
            Rmodel <- ranger(dv ~ ., data = iv, write.forest = T, importance = 'impurity', num.trees = 500)
            modelDir <- paste0(rootDir, '/Models/', att, '/Depth_', depth)
            if(!dir.exists(modelDir))dir.create(modelDir, recursive = T)
            RmodelPath = paste0(modelDir, '/RFmodel_', att, '_', depth, '_', i, '.rds')
            saveRDS(Rmodel,RmodelPath )
            Rmodel <- readRDS(RmodelPath)
            summariseRFModel(modelPath = RmodelPath, att)
            
            vdata <- validSet[, 3:ncol(validSet)]
            
            preds = predict(Rmodel, data=vdata,  predict.all = F)
            Rtdf <- data.frame(validSet[,3], preds$predictions)
            RmodelFilename <- paste0(rootDir, '/Models/', att, '/Depth_', depth, '/RFmodel_', att, '_', depth, '_', i)
             print(fitStats(obsVal=Rtdf[1],modelVal=Rtdf[2], attName=paste0(att,' ', depth, ' - RF'), outfilename = paste0(RmodelFilename, '_ModelStats.txt'), legPos='topleft', verbose = T))
             write.table(Rtdf, paste0(RmodelFilename, '_Obs_vs_Mod.csv'), sep=",", row.names=F)
            #hexbinplot(Rtdf[,1] ~ Rtdf[,2])
        }
    }
}
