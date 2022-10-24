library(terra)
library(sf)

terraOptions(progress = 1)

scriptDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/Functions'
#rootDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/MeasuredTextures/V3/Parsimonious'

rootDir <- 'Y:/Ross/TERN/AWC/MeasuredTextures/V3/Parsimonious'
workDir <- paste0(rootDir, '/TrainingData')
validDir <- paste0(rootDir, '/ValidationStats')

if(dir.exists(validDir))(dir.create(validDir))

source(paste0(scriptDir, '/ModelUtils.R'))

allValidSet <- read.csv(paste0(workDir, '/validationData.csv'), stringsAsFactors = F)

depths1 <- c('000', '005','015','030','060','100')
depths2 <- c('005','015','030','060','100','200')

att <- 'DUL'
att <- 'DLL'

d <- paste0('/scratch1/', ident, '/AWCv3/Parsimonious/Mosaics')
#d <- paste0('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/', att)

for (i in 1:length(depths1)) {
  depth <- depths2[i]
  #r <- rast(paste0(d, '/', att, '_', depths1[i], '_', depths2[i], '_EV_N_P_AU_TRN_N_20210614.tif'))
  r <- rast(paste0(d, '/',att, '_', depth, '_Mean.tif'))
  vdf <- na.omit(allValidSet[, c('Latitude', 'Longitude', paste0(att, '_', depths2[i]))])
  head(vdf)
  pts <- st_as_sf(vdf, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
  mv = as.data.frame(terra::extract(r, vect(pts),method="simple"))
  
  odf = data.frame(vdf, mv[2], stringsAsFactors = F)
  colnames(odf) <- c('Latitude', 'Longitude', paste0(att, '_', depths2[i],'_Observed'),  paste0(att, '_', depths2[i],'_Modelled'))
  RmodelFilename <- paste0(validDir, '/', att, '_', depth)
  print(fitStats(obsVal=odf[3],modelVal=odf[4], attName=paste0(att,' ', depth, ' - RF'), outfilename = paste0(RmodelFilename, '_ModelStats.txt'), legPos='topleft', verbose = T, savePlot = T))
}




######  Just plot validation point locations
for (i in 1:length(depths1)) {
  depth <- depths2[i]
  vdf <- na.omit(allValidSet[, c('Latitude', 'Longitude', paste0(att, '_', depths2[i]))])
  head(vdf)
  pts <- st_as_sf(vdf, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
  plot(st_geometry(pts))
}



### Do validation of AWC
for (i in 1:length(depths1)) {
  depth <- depths2[i]
  #r <- rast(paste0(d, '/', att, '_', depths1[i], '_', depths2[i], '_EV_N_P_AU_TRN_N_20210614.tif'))
  rDUL <- rast(paste0(d, '/','DUL', '_', depth, '_Mean.tif'))
  rDLL <- rast(paste0(d, '/','DLL', '_', depth, '_Mean.tif'))
  vdfDUL <- na.omit(allValidSet[, c('Latitude', 'Longitude', paste0('DUL', '_', depths2[i]))])
  vdfDLL <- na.omit(allValidSet[, c('Latitude', 'Longitude', paste0('DLL', '_', depths2[i]))])
  head(vdfDUL)
  ptsDLL <- st_as_sf(vdfDLL, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
  ptsDUL <- st_as_sf(vdfDUL, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
  mvDLL = as.data.frame(terra::extract(rDLL, vect(ptsDLL),method="simple"))
  mvDUL = as.data.frame(terra::extract(rDUL, vect(ptsDUL),method="simple"))
  
  awcMod <- mvDUL[2] - mvDLL[2]
  awcObs <- vdfDUL[3] - vdfDLL[3]
  
  odf = data.frame(awcObs, awcMod, stringsAsFactors = F)
  colnames(odf) <- c(paste0('AWC', '_', depths2[i],'_Observed'),  paste0('AWC', '_', depths2[i],'_Modelled'))
  RmodelFilename <- paste0(validDir, '/', 'AWC', '_', depth)
  print(fitStats(obsVal=odf[1],modelVal=odf[2], attName=paste0('AWC',' ', depth, ''), outfilename = paste0(RmodelFilename, '_ModelStats.txt'), legPos='topleft', verbose = T, savePlot = T))
}
