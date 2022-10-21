library(raster)
library(sf)

rasterOptions(progress = 'text')


allPts <- sf::st_read('C:/Projects/TernLandscapes/ASC/InputData/mergedASCLocs.shp')

atlas <- raster('C:/Projects/TernLandscapes/ASC/AtlasRaster.tif')
asris <- raster('C:/Projects/TernLandscapes/ASC/AsrisRaster.tif')
Harry <- raster('C:/Projects/TernLandscapes/ASC/ASCOrder_Harry_Resamp.tif')
ascOnlyBoot <- raster('C:/Projects/TernLandscapes/ASC/ASCOnly/ASCOnly_boot.tif')
ascOnlySingle <- raster('C:/Projects/TernLandscapes/ASC/ASCOnly/ASCOnlyRemap.tif')
All <- raster('C:/Projects/TernLandscapes/ASC/MapsSingle/ASC_Single_Remap.tif')
AllBoot <- raster('C:/Projects/TernLandscapes/ASC/ASC_boot.tif')

stk <- stack(c(atlas, asris, Harry,  ascOnlyBoot, ascOnlySingle, All, AllBoot))


drills <- raster::extract(stk, allPts)


RVR <- raster('C:/Projects/TernLandscapes/ASC/RVRRemap.tif')
RVRdrills <- raster::extract(RVR, allPts)

#####  Validation using all the observed data
validData <- cbind(allPts, drills, RVRdrills)
head(validData)


remap <- read.csv('C:/Projects/TernLandscapes/ASC/codeMerging.csv', stringsAsFactors = F)
validDataM <- merge(validData, remap, by.x = 'mergedASC', by.y = 'PolyCode')
head(validDataM)

validDF <- data.frame(obs=validDataM$RasterCode, atlas=validDataM$AtlasRaster, asris=validDataM$AsrisRaster, 
                      RVR=validDataM$RVRdrills, Harry=validDataM$ASCOrder_Harry_Resamp,
                      ascOnlySingle=validDataM$ASCOnlyRemap, ascOnlyBoot=validDataM$ASCOnly_boot,
                      ASC_Single=validDataM$ASC_Single, ASCBoot=validDataM$ASC_boot)
head(validDF)




write.csv(validDF, 'C:/Projects/TernLandscapes/ASC/mapValidationStats.csv', row.names = F )
validDF = read.csv('C:/Projects/TernLandscapes/ASC/mapValidationStats.csv', stringsAsFactors = F)

valDF <- data.frame(ASCProduct=c('Atlas', 'ASRIS','RVR', 'HarryRF', 'ASCOnlySingle', 'ASCOnlyBootStrapped', 'AllASCSingle', 'AllASCBootstrapped' ), InternalValidation=numeric(8), ExternalValidation=numeric(8))

idxs <- which(!is.na(validDF$atlas))
atlasVD <- validDF$obs[idxs] == validDF$atlas[idxs]
valDF[1,2] = sum(atlasVD)/length(validDF$obs[idxs])

idxs <- which(!is.na(validDF$asris))
asrisVD <- validDF$obs[idxs] == validDF$asris[idxs]
valDF[2,2] = sum(asrisVD)/length(validDF$obs[idxs])

idxs <- which(!is.na(validDF$RVR))
rvrVD <- validDF$obs[idxs] == validDF$RVR[idxs]
valDF[3,2] = sum(rvrVD)/length(validDF$obs[idxs])

idxs <- which(!is.na(validDF$Harry))
harryVD <- validDF$obs[idxs] == validDF$Harry[idxs]
valDF[4,2] = sum(harryVD)/length(validDF$obs[idxs])

idxs <- which(!is.na(validDF$ascOnlySingle))
ascOnlySingleVD <- validDF$obs[idxs] == validDF$ascOnlySingle[idxs]
valDF[5,2] = sum(ascOnlySingleVD)/length(validDF$obs[idxs])

idxs <- which(!is.na(validDF$ascOnlyBoot ))
ascOnlyBootVD <- validDF$obs[idxs] == validDF$ascOnlyBoot [idxs]
valDF[6,2] = sum(ascOnlyBootVD)/length(validDF$obs[idxs])

idxs <- which(!is.na(validDF$ASC_Single))
ASC_SingleVD <- validDF$obs[idxs] == validDF$ASC_Single[idxs]
valDF[7,2] = sum(ASC_SingleVD)/length(validDF$obs[idxs])

idxs <- which(!is.na(validDF$ASCBoot ))
ASCBootVD <- validDF$obs[idxs] == validDF$ASCBoot[idxs]
valDF[8,2] = sum(ASCBootVD)/length(validDF$obs[idxs])



#######   Validation using held out validation set

allValidSet <- read.csv('C:/Projects/TernLandscapes/ASC/InputData/validationDataWithExtras.csv', stringsAsFactors = T)
head(allValidSet)

vdf <- data.frame(Longitude=allValidSet$Longitude, Latitude=allValidSet$Latitude, mergedASC=allValidSet$mergedASC, stringsAsFactors=F)
head(vdf)

coordinates(vdf) <- ~ Longitude + Latitude
drills2 <- raster::extract(stk, vdf)
head(drills2)

RVR <- raster('C:/Projects/TernLandscapes/ASC/RVRRemap.tif')
RVRdrillsE <- raster::extract(RVR, vdf)


validData2 <- cbind(allValidSet, drills2,RVRdrillsE)
head(validData2)


# ASCOnlyallValidSet <- read.csv('C:/Projects/TernLandscapes/ASC/InputData/ASCOnlyvalidationDataWithExtras.csv', stringsAsFactors = T)
# idxs2 <- which(is.na(ASCOnlyallValidSet$ASCCode)) 
# ASCOnlyallValidSet <- ASCOnlyallValidSet[-idxs2,]
# head(ASCOnlyallValidSet)
# 
# vdf2 <- data.frame(Longitude=ASCOnlyallValidSet$Longitude, Latitude=ASCOnlyallValidSet$Latitude, ASConly=ASCOnlyallValidSet$ASCCode, stringsAsFactors=F)
# 
# stk <- stack(c(ascOnlyBoot, ascOnlySingle))
# coordinates(vdf2) <- ~ Longitude + Latitude
# drills3 <- raster::extract(stk, vdf2)
# head(drills3)
# 
# validData3 <- cbind(ASCOnlyallValidSet, drills3)
# head(validData3)


remap <- read.csv('C:/Projects/TernLandscapes/ASC/codeMerging.csv', stringsAsFactors = F)
validDataM2 <- merge(validData2, remap, by.x = 'mergedASC', by.y = 'PolyCode')
head(validDataM2)

write.csv(validDataM2, 'C:/Projects/TernLandscapes/ASC/InputData/validationDataWithExtrasAndASCCodes.csv', row.names = F)
validDataM2 <- read.csv('C:/Projects/TernLandscapes/ASC/InputData/validationDataWithExtrasAndASCCodes.csv', stringsAsFactors = F)

idxs <- which(!is.na(validDataM2$AtlasRaster))
atlasVD <- validDataM2$RasterCode[idxs] == validDataM2$AtlasRaster[idxs]
valDF[1,3] = sum(atlasVD)/length(idxs)

idxs <- which(!is.na(validDataM2$AsrisRaster))
asrisVD <- validDataM2$RasterCode[idxs] == validDataM2$AsrisRaster[idxs]
valDF[2,3] = sum(asrisVD)/length(idxs)

idxs <- which(!is.na(validDataM2$RVRdrillsE))
rvrVD <- validDataM2$RasterCode[idxs] == validDataM2$RVRdrillsE[idxs]
valDF[3,3] = sum(rvrVD)/length(idxs)

# idxs <- which(!is.na(validDataM2$ASCOrder_Harry_Resamp))
# harryVD <- validDataM2$RasterCode[idxs] == validDataM2$ASCOrder_Harry_Resamp[idxs]
# valDF[4,3] = sum(harryVD)/length(idxs)

# idxs <- which(!is.na(validDataM3$ASCOnlyRemap))
# asconlySingleVD <- validDataM3$RasterCode[idxs] == validDataM3$ASCOnlyRemap[idxs]
# valDF[5,2] = sum(asconlySingleVD)/length(idxs)
# 
# idxs <- which(!is.na(validDataM3$ASCOnly_boot))
# ASCOnly_bootVD <- validDataM3$RasterCode[idxs] == validDataM3$ASCOnly_boot[idxs]
# valDF[6,2] = sum(ASCOnly_bootVD)/length(idxs)

idxs <- which(!is.na(validDataM2$ASC_Single_Remap))
ASC_Single_RemapVD <- validDataM2$RasterCode[idxs] == validDataM2$ASC_Single_Remap[idxs]
valDF[7,3] = sum(ASC_Single_RemapVD)/length(idxs)

idxs <- which(!is.na(validDataM2$ASC_boot))
ASC_bootVD <- validDataM2$RasterCode[idxs] == validDataM2$ASC_boot[idxs]
valDF[8,3] = sum(ASC_bootVD)/length(idxs)










