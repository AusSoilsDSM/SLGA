##############################################################################################################
###  Author : Ross Searle         
###  Date : Mon Aug 09 12:09:41 2021                      
###  Project : TERN Landscapes
###  Purpose : Generate Validation metrics for the older ASC maps and the new DSM RF modelled map
#############################################################################################################

library(raster)
library(sf)
library(irr)

rasterOptions(progress = 'text')


allPts <- sf::st_read('C:/Projects/TernLandscapes/ASC/InputData/mergedASCLocs.shp')

atlas <- raster('C:/Projects/TernLandscapes/ASC/AtlasRaster.tif')  # Atlas of Australian Soil map
asris <- raster('C:/Projects/TernLandscapes/ASC/AsrisRaster.tif')  # ASRIS Best Scale Polygon map
ascOnlySingle <- raster('C:/Projects/TernLandscapes/ASC/ASCOnly/ASCOnlyRemap.tif')  # single RF model using ASC data only - no GSG or PPF remaps
Harry <- raster('C:/Projects/TernLandscapes/ASC/ASCOrder_Harry_Resamp.tif')  # Random Forest model done by Harry Goodman to prototype this approach
ascOnlyBoot <- raster('C:/Projects/TernLandscapes/ASC/ASCOnly/ASCOnly_boot.tif')  # bootstrapped RF model using ASC data only - no GSG or PPF remaps
All <- raster('C:/Projects/TernLandscapes/ASC/MapsSingle/ASC_Single_Remap.tif')  # single RF model using all ASC data - GSG or PPF remaps
AllBoot <- raster('C:/Projects/TernLandscapes/ASC/ASC_boot.tif') # bootstrapped RF model using all ASC data - GSG or PPF remaps

stk <- stack(c(atlas, asris, Harry,  ascOnlyBoot, ascOnlySingle, All, AllBoot))


drills <- raster::extract(stk, allPts)


RVR <- raster('C:/Projects/TernLandscapes/ASC/RVRRemap.tif')
RVRdrills <- raster::extract(RVR, allPts)

#####  Validation using all the observed data
validData <- cbind(allPts, drills, RVRdrills) # at a different spatial support of 1km
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


###### Training Data (Internal Data) validations  #############

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



#######  External Validation using held out validation set  ##########
####### Only really applies to the new DSM models but included the older maps to check the number were plausible

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


idxs <- which(!is.na(validDataM2$ASC_Single_Remap))
ASC_Single_RemapVD <- validDataM2$RasterCode[idxs] == validDataM2$ASC_Single_Remap[idxs]
valDF[7,3] = sum(ASC_Single_RemapVD)/length(idxs)

idxs <- which(!is.na(validDataM2$ASC_boot))
ASC_bootVD <- validDataM2$RasterCode[idxs] == validDataM2$ASC_boot[idxs]
valDF[8,3] = sum(ASC_bootVD)/length(idxs)


write.csv(valDF, paste0('C:/Projects/TernLandscapes/ASC/Validations/ExternalValidationStats.csv'), row.names = F)


vdf <- na.omit(data.frame(validDataM2$mergedASC, validDataM2$ASC_boot , stringsAsFactors = F))
remap <- read.csv('C:/Projects/TernLandscapes/ASC/codeMerging.csv', stringsAsFactors = F)
vdf2 <- merge(vdf, remap, by.x = 'validDataM2.ASC_boot', by.y = 'RasterCode')
head(vdf2)
vdf3 <- na.omit(data.frame(vdf2$validDataM2.mergedASC, vdf2$PolyCode , stringsAsFactors = F))
kappa2(vdf3)


accmat <- table(vdf3)
accmat

write.csv(accmat, paste0('C:/Projects/TernLandscapes/ASC/Validations/ASC_ConfusionMatrix.csv'), row.names = T)


####  link to description of stats below https://blogs.fu-berlin.de/reseda/accuracy-statistics-in-r/

UserAccuracy <- diag(accmat) / rowSums(accmat) * 100
ProducerAccuracy <- diag(accmat) / colSums(accmat) * 100
OA <- sum(diag(accmat)) / sum(accmat) * 100

upDF <- rbind(UserAccuracy, ProducerAccuracy)
upDF
write.csv(upDF , 'C:/Projects/TernLandscapes/ASC/Validations/UserProducerScores.csv')

sign <- binom.test(x = sum(diag(accmat)),
                   n = sum(accmat),
                   alternative = c("two.sided"),
                   conf.level = 0.95
)


sign

pvalue <- sign$p.value
pvalue




