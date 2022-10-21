library(raster)
library(ranger)
library(hexbin)
library(Cubist)
library(stringr)

rasterOptions(progress = 'text', chunksize=1e+08,maxmemory=1e+09)

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
nrow(inDF)
nrow(inDF[!is.na(inDF$GSG) | !is.na(inDF$PPF) | !is.na(inDF$ASCCode),])
nrow(inDF[!is.na(inDF$ASCCode),])


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

indf <- alldf[, c(14, 18:103)]
head(indf)
cc <- complete.cases(indf)
####Remove NAs etc


###### use all estimates of ASC eg ppfs and gsgs
idxs <- which(is.na(indf$mergedASC))

sapply(indf, function(x) sum(is.na(x)))
colnames(indf)
indf <- indf[, -7]
sapply(indf, function(x) sum(is.na(x)))


###### use only estimates of ASC 
indf <- alldf[, c(9, 18:103)]
head(indf)
idxs <- which(is.na(indf$ASCCode))
indf <- indf[-idxs, ]
nrow(indf)



cc <- which(complete.cases(indf))
length(cc)
indf <- indf[cc,]
nrow(indf)
write.csv(indf, paste0(workDir, '/ASC/Inputs/ASCOnlydrill_NoNAs.csv'), row.names = F)

alldf <- read.csv(paste0(workDir, '/ASC/Inputs/ASCOnlydrill_NoNAs.csv'), stringsAsFactors = F)

modelling.samplePercentage = 90


splitSamples <-createTrainingSample(alldf, 1, modelling.samplePercentage)
trainSet <- as.data.frame(splitSamples[1])
validSet <- as.data.frame(splitSamples[2])
colnames(trainSet) <- colnames(alldf)
colnames(validSet) <- colnames(alldf)


write.table(trainSet, paste0(workDir, '/ASC/Inputs/ASCOnlytrainingData.csv'), sep=",", row.names=F)
write.table(validSet, paste0(workDir, '/ASC/Inputs/ASCOnlyvalidationData.csv'), sep=",", row.names=F)

trainSet <- read.csv(paste0(workDir, '/ASC/Inputs/ASCOnlytrainingData.csv'), stringsAsFactors = F)
validSet <- read.csv(paste0(workDir, '/ASC/Inputs/ASCOnlyvalidationData.csv'), stringsAsFactors = F)

asc <- trainSet[,1]
covs <- trainSet[,-1]

#att <- 'ASC'
att <- 'ASCOnly'


#### Generate a RF model
Rmodel <- ranger(asc ~ ., data = covs, write.forest = TRUE, importance = 'impurity', num.trees = 500)
RmodelPath = paste0(workDir, '/ASC/Models/RFmodel_', att, '.rds')
saveRDS(Rmodel,RmodelPath )
Rmodel <- readRDS(RmodelPath)
summariseRFModel( RmodelPath, att)

vobs <- validSet[,1]
vcovs <- validSet[,-1]

preds = predict(Rmodel, data=vcovs,  predict.all = F)
Rtdf <- data.frame(vobs, preds$predictions, stringsAsFactors = F)
RmodelFilename <- paste0(workDir, '/ASC/Models/RFmodel_', att)

ct <- fitStatsCategorical(obsVal=vobs,modelVal=preds$predictions, paste0(att, ' - RF'), outfilename =  paste0(RmodelFilename, '_ModelStats.txt'), 'topleft', verbose = T)
write.table(Rtdf, paste0(RmodelFilename, '_Obs_vs_Mod.csv'), sep=",", row.names=F)

library(C50)
cfive <- C5.0(y=as.factor(asc), x=covs)
summary(cfive)
cfive$dims
preds <- predict(cfive, validSet)
ctc <- fitStatsCategorical(obsVal=validSet$mergedASC,modelVal=preds, paste0(att, ' - RF'), outfilename =  paste0(RmodelFilename, '_ModelStatsC5.txt'), 'topleft', verbose = T)



