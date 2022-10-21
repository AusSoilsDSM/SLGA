library(raster)
library(ranger)
library(hexbin)
library(Cubist)
library(stringr)
library(ggplot2)

rasterOptions(progress = 'text', chunksize=1e+08,maxmemory=1e+09)

getRasterDecodes <- function(model = model){
  ids <- model$forest$class.values
  cats <-  as.character(model$forest$levels[ids])
  decodes <- data.frame( RID = ids, Category = cats)
  decodesOrd <- decodes[order(decodes$RID),]
  return (decodesOrd)
}

scriptDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/Functions'
covDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'
workDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN'

source(paste0(scriptDir, '/GeneralUtils.R'))
source(paste0(scriptDir, '/ModelUtils.R'))
source(paste0(scriptDir, '/RandomForestUtils_V2.R'))

covs <- read.csv(paste0(workDir, '/covsToUse.csv'), stringsAsFactors = F)
covsToUse <- paste0(covDir, '/', covs[covs$Parsimonious==1, 2], '.tif')
covsNotToUse <-covs[covs$Parsimonious==0, 2]
stk<-stack(covsToUse)
aext <- extent(stk)

inDF <- read.csv(paste0(workDir, '/ASC/Inputs/MergedASCs.csv'), stringsAsFactors = F)
head(inDF)

idxs <- which(!is.na(inDF$Longitude))
pts <- inDF[idxs,]
idxs <- which(pts$Longitude >=aext@xmin & pts$Longitude <= aext@xmax & pts$Latitude >= aext@ymin & pts$Latitude <= aext@ymax) 
pts <- pts[idxs,]
head(pts, 20)
coordinates(pts)<- ~ Longitude + Latitude
crs(pts)<- crs(stk)
plot(pts)
drill <- raster::extract(stk, pts, df=T)

alldf <- cbind(pts@data, pts@coords, drill)
head(alldf)

write.csv(alldf, paste0(workDir, '/ASC/Inputs/ASCdrill.csv'), row.names = F)

alldf <- read.csv(paste0(workDir, '/ASC/Inputs/ASCdrill.csv'), stringsAsFactors = F)

head(alldf)
colnames(alldf)


#next line is a stuff up - we need to keep all the data columns, not a subset as below, for final map validation
indf <- alldf[, c(14, 18:103)]
head(indf)
cc <- complete.cases(indf)
####Remove NAs etc

idxs <- which(is.na(indf$mergedASC))
indf <- indf[-idxs, ]
nrow(indf)

sapply(indf, function(x) sum(is.na(x)))
colnames(indf)
indf <- indf[, -7]
sapply(indf, function(x) sum(is.na(x)))
cc <- which(complete.cases(indf))
length(cc)
indf <- indf[cc,]
nrow(indf)
write.csv(indf, paste0(workDir, '/ASC/Inputs/ASCdrill_NoNAs.csv'), row.names = F)

#alldf <- read.csv(paste0(workDir, '/ASC/Inputs/ASCdrill_NoNAs.csv'), stringsAsFactors = F)


modelling.samplePercentage = 90


splitSamples <-createTrainingSample(alldf, 1, modelling.samplePercentage)
trainSet <- as.data.frame(splitSamples[1])
validSet <- as.data.frame(splitSamples[2])
colnames(trainSet) <- colnames(alldf)
colnames(validSet) <- colnames(alldf)


write.table(trainSet, paste0(workDir, '/ASC/Inputs/trainingData.csv'), sep=",", row.names=F)
write.table(validSet, paste0(workDir, '/ASC/Inputs/validationData.csv'), sep=",", row.names=F)

trainSet <- read.csv(paste0(workDir, '/ASC/Inputs/trainingData.csv'), stringsAsFactors = F)
validSet <- read.csv(paste0(workDir, '/ASC/Inputs/validationData.csv'), stringsAsFactors = F)


### ASC Only
trainSet <- read.csv(paste0(workDir, '/ASC/Inputs/ASCOnlytrainingData.csv'), stringsAsFactors = F)
validSet <- read.csv(paste0(workDir, '/ASC/Inputs/ASCOnlyvalidationData.csv'), stringsAsFactors = F)



#####  Boootstrap the training samples   #############

bootDir <- paste0(workDir, '/ASCOnly/BootSamples')
if(!dir.exists(bootDir)){dir.create(bootDir, recursive = T)}

dSet <- data.frame(RID=seq(1:nrow(trainSet)), trainSet, stringsAsFactors = F)
str(dSet)

numBoots=50

for (i in 1:numBoots) {
  print(i)
  trainsetx = dSet[sample(1:nrow(dSet),replace=T),]
  #idxs <- setdiff( dSet$prof_id , trainsetx$prof_id )
  idxs2 <- which(!dSet$RID %in% trainsetx$RID)
  bootValid <- dSet[idxs2,]
  
  write.csv(trainsetx, paste0(bootDir, '/ASC_boot_train_', i,'.csv'), row.names = F)
  write.csv(bootValid, paste0(bootDir, '/ASC_boot_valid_', i, '.csv'), row.names = F)
}


att <- 'ASCOnly'

for (i in 1:numBoots) {
  print(i)
  trainSet <- read.csv(paste0(bootDir, '/ASC_boot_train_', i,'.csv'), stringsAsFactors = F)
  asc <- trainSet[,2]
  covs <- trainSet[,-c(1, 2)]
  validSet <- read.csv(paste0(bootDir, '/ASC_boot_valid_', i,'.csv'), stringsAsFactors = F)
  vobs <- validSet[,2]
  vcovs <- validSet[,-c(1, 2)]
 
  Rmodel <- ranger(asc ~ ., data = covs, write.forest = T, importance = 'impurity', num.trees = 500)
  RmodelPath = paste0(workDir, '/ASCOnly/BootModels/RFmodel_', att, '_', i, '.rds')
  saveRDS(Rmodel,RmodelPath )
  
  preds = predict(Rmodel, data=vcovs,  predict.all = F)
  Rtdf <- data.frame(vobs, preds$predictions, stringsAsFactors = F)
  RmodelFilename <- paste0(workDir, '/ASCOnly/BootModels/RFmodel_', att, '_', i)
  
  ct <- fitStatsCategorical(obsVal=vobs,modelVal=preds$predictions, paste0(att, ' - RF'), outfilename =  paste0(RmodelFilename, '_ModelStats.txt'), 'topleft', verbose = T)
  write.table(Rtdf, paste0(RmodelFilename, '_Obs_vs_Mod.csv'), sep=",", row.names=F)
  
  rmp <- getRasterDecodes(model)
  write.csv(rmp, paste0(RmodelFilename, '_ASCcodes.csv'), row.names=F)
  
}


