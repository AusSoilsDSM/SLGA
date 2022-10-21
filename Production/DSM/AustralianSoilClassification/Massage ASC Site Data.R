library(sf)
library(stringr)
library(dplyr)


workDir <- 'C:/Projects/TernLandscapes/AWC/ObsDataSDF'

#### RRead in the raw data extracetd from the SDF and merged

## sort out the soil classification data
soilClassRaw <- read.csv( paste0(workDir, '/SDF_All_Classification.csv'), stringsAsFactors = F)[,-1]
nrow(soilClassRaw)
head(soilClassRaw)
t <- as.numeric(soilClassRaw$Longitude)
idxs <- which(is.na(t))
idxs
soilClass <- soilClassRaw[-idxs,]
count(soilClassRaw[!is.na(soilClassRaw$ASC), ], 'Dataset')
count(soilClassRaw[!is.na(soilClassRaw$PPF), ], 'Dataset')
count(soilClassRaw[!is.na(soilClassRaw$GSG), ], 'Dataset')
nrow(soilClassRaw)

#idxs <- which(is.na(soilClass$ASC) & is.na(soilClass$PPF) & is.na(soilClass$GSG))


ascgrps <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/ASCCodes.csv', stringsAsFactors = F)
head(ascgrps)
mdf1 <- merge(soilClassRaw, ascgrps, all.x=T, all.y=F, by=c('ASC'))
nrow(mdf1)
head(mdf1)
colnames(mdf1)
soilgrps <- mdf1[,-1]
head(soilgrps)
nrow(soilgrps)

write.csv(soilgrps, 'c:/temp/test1.csv')

unique(soilgrps$ASCCode)


# ASCmappings <- read.csv('C:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/ASCGroups.csv', stringsAsFactors = F)
# head(ASCmappings)
# nrow(soilgrps)
# mdf1 <- merge(soilgrps, ASCmappings, by.x = 'ASCCode', by.y = 'ASC', all.x=T)
# nrow(mdf1)
# head(mdf1)

mdf1 <- soilgrps

PPFmappings <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/PPFGroups.csv', stringsAsFactors = F)
mdf2 <- merge(mdf1, PPFmappings, by=c('PPF'), all.x=T)
nrow(mdf2)
head(mdf2)
idxs <- which(mdf2$GSG == '')
mdf2$GSG[idxs] <- NA

write.csv(mdf2, 'c:/temp/test2.csv')

gsgmaps <- read.csv('c:/Projects/TernLandscapes/AWC/DataMassage/SoilGrps/GSGGroups.csv', stringsAsFactors = F)

head(gsgmaps)
colnames(gsgmaps)[3] <- 'ASCFromGSG'
mdf3 <- merge(mdf2, gsgmaps, by=c('GSG'), all.x=T)
nrow(mdf3)
head(mdf3)


odf3 <- mdf3
mdf3$mergedASC <- NA
head(mdf3)
idxs <- which(!is.na(odf3$ASCCode) | odf3$ASCCode=='')
length(idxs)
odf3$mergedASC[idxs] <- odf3$ASCCode[idxs]
head(odf3)

idxs <- which(is.na(odf3$mergedASC) & (!is.na(odf3$ASCFromGSG) | odf3$ASCCode==''))
length(idxs)
odf3$mergedASC[idxs] <- odf3$ASCFromGSG[idxs]
head(odf3)

idxs <- which(is.na(odf3$mergedASC) & !is.na(odf3$ASCcodeFomPPF))
length(idxs)
odf3$mergedASC[idxs] <- odf3$ASCcodeFomPPF[idxs]
head(odf3)
nrow(odf3)

write.csv(odf3, 'C:/Projects/TernLandscapes/ASC/MergedASCs.csv', row.names=F)


