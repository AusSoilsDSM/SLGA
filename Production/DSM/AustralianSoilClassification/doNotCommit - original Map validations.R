library(ranger)
library(Metrics)
library(irr)

workDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN/ASC'
scriptDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/Functions'

source(paste0(scriptDir, '/RFUtils.R'))
source(paste0(scriptDir, '/ModelUtils.R'))

numBoots = 50
att <- 'ASC'

vd  <- read.csv(paste0(workDir, '/Inputs/validationData.csv'), stringsAsFactors = F)

valDF <- data.frame(obs=vd[,1])

for (j in 1:numBoots) {
  print(paste0('Model iteration ', j))
  model <- readRDS(paste0(workDir, '/BootModels/RFmodel_ASC_', j, '.rds'))
  prediction = predict(model, vd[,-1],  predict.all = F, num.threads=1)$predictions
  valDF <- cbind(valDF, prediction)
}


mData <- apply(valDF[,-1], 1, getModalValue)
odf <- data.frame(obsVal=obsVals,modelVal=mData)
write.csv(odf, paste0(workDir, '/BootModels_ObsVMod.csv'), row.names = F)
odf <- read.csv(paste0(workDir, '/BootModels_ObsVMod.csv'), stringsAsFactors = F)

obsVals <- valDF$obs
ct <- fitStatsCategorical(obsVal=obsVals,modelVal=mData, paste0(att, ' Bootstrap - RF'), outfilename =  paste0( 'Bootstrap_ModelStats.txt'), 'topleft', verbose = T)

vdf <- read.csv('C:/Projects/TernLandscapes/ASC/BootModels_ObsVMod.csv')
accmat <- table(vdf)

agree(vdf)
kappa2(vdf)

Metrics::accuracy(actual=vdf$obsVal, predicted = vdf$modelVal)

Metrics::ce(actual=vdf$obsVal, predicted = vdf$modelVal)
Metrics::precision(actual=vdf$obsVal, predicted = vdf$modelVal)



####  link to description of stats below https://blogs.fu-berlin.de/reseda/accuracy-statistics-in-r/

UA <- diag(accmat) / rowSums(accmat) * 100
PA <- diag(accmat) / colSums(accmat) * 100
OA <- sum(diag(accmat)) / sum(accmat) * 100

sign <- binom.test(x = sum(diag(accmat)),
                   n = sum(accmat),
                   alternative = c("two.sided"),
                   conf.level = 0.95
)

pvalue <- sign$p.value
pvalue
## [1] 5.30128e-39
