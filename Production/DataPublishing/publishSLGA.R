
#################################
###  Author : Ross Searle         
###  Date : Fri Apr 01 10:35:13 2022                      
###  Project : TERN Landscapes
###  Purpose : Publish the SLGA datasets as COGs
#################################

startTime <- Sys.time()
print(startTime)
      
library(terra)
library(stringr)

#terraOptions(progress=0, memmax=10)
terraOptions(tempdir = paste0('/scratch2/', ident, '/Rtmp'), progress=1)
#terraOptions( progress=1)



source('/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Production/NationalMaps/DataPublishing/Util_DataPublishing.R')

gdalPath <- '/apps/gdal/3.2.2/bin'
#gdalPath <- 'E:/LocalProgs/QGIS3.2/bin'

# infoDir <- 'Y:/Ross/SLGA/SLGA/Production/NationalMaps/DataPublishing'
# outDir <- 'Q:/work/http/Products/CSIRO/SoilGenericGroups'
# templateR <- rast('M:/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif')


args = commandArgs(trailingOnly=T)
k = as.numeric(args[1])
att = args[2]
dataDir <- args[3]
resamp = as.logical(args[4])
adjustVal <- as.numeric(args[5])
fillHoles <- as.logical(args[6])
sigFigures <- as.numeric(args[7])
makeCOG <- as.logical(args[8])
minVal <- as.numeric(args[9])
isInt <- as.logical(args[10])
doMask <- as.logical(args[11])

print(args)
print(paste0('Attribute = ', att))
print(paste0('dataDir = ', dataDir))
print(paste0('doResample = ', resamp))
print(paste0('adjustVal = ', adjustVal))
print(paste0('fillHoles = ', fillHoles))
print(paste0('Decimal places = ', sigFigures))
print(paste0('makeCOG = ', makeCOG))
print(paste0('minVal = ', minVal))
print(paste0('isInt = ', isInt))
print(paste0('applyMask = ', doMask))

print(paste0('tmpDir= ', tempdir()))

infoDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Production/NationalMaps/DataPublishing'
#dataDir <- '/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/predictions/SLGA_ready/90m'

outDir <- paste0('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/', att)
if(!dir.exists(outDir)){dir.create(outDir)}
#templateR <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif'
templateR <- '/datasets/work/lw-soildatarepo/work/http/Products/TERN/Covariates/Mosaics/30m/Relief_dem1sv1_0.tif'


#fls <- list.files(dataDir, full.names = T, pattern = paste0(att, '_'))
fls <- list.files(dataDir, full.names = T, pattern = '*.tif$')

inRasterPath <- fls[k]


#RATTable <- read.csv(paste0(infoDir, '/RATs/NationalGenericSoilGroupsRAT.csv'), stringsAsFactors = F)
#colnames(RATTable)[1] <- 'Value'
#str(RATTable)


mt <- read.csv(paste0(infoDir, '/MetaData/', att, '_Metadata.csv'), sep = ',', header = F)


op <- publishRaster(inRasterPath=inRasterPath, outDir=outDir, rasterTemplate=templateR, doResample=resamp, isIntegerRaster=isInt, adjustVal=adjustVal, setMin=minVal, fillHoles=fillHoles, sigFigures=sigFigures, applyMask=doMask, metadataTable=mt, RATTable=NULL, MakeCOG=makeCOG)

#op <- publishRaster(inRasterPath=inRasterPath, outDir=outDir, resampleTemplate=NULL, maskTemplate=NULL, metadataTable=mt, RATTable=RATTable, MakeCOG=T)

system(paste0(gdalPath,'/gdalinfo -mdd all ', op))

print(paste0('Is Valid COG = ', isValidCOG(op, quiet=T)))

endTime <- Sys.time()
print(endTime)
difft <- difftime(endTime, startTime, units = "auto")
print(difft)
print('Processing successfully completed')




