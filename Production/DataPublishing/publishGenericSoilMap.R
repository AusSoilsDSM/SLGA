
#################################
###  Author : Ross Searle         
###  Date : Fri Apr 01 10:35:13 2022                      
###  Project : TERN Landscapes
###  Purpose : Publish the Geenric Soil group Map
#################################

startTime <- Sys.time()
print(startTime)
      
library(terra)
library(stringr)

terraOptions(progress=1)

gdalPath <- '/apps/gdal/3.2.2/bin'
#gdalPath <- 'E:/LocalProgs/QGIS3.2/bin'

# infoDir <- 'Y:/Ross/SLGA/SLGA/Production/NationalMaps/DataPublishing'
# dataDir <- '//fs1-cbr.nexus.csiro.au/{lw-slga}/work/Projects/GenericSoilMap/Outputs'
# outDir <- 'Q:/work/http/Products/CSIRO/SoilGenericGroups'
# templateR <- rast('M:/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif')

infoDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Production/NationalMaps/DataPublishing'
dataDir <- '/datasets/work/lw-slga/work/Projects/GenericSoilMap/Outputs'
outDir <- '/datasets/work/lw-soildatarepo/work/http/Products/CSIRO/SoilGenericGroups'
templateR <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif'
inRasterPath <- paste0(dataDir, '/GenericSoilGroups.tif')


#inRasterPath <- paste0('/datasets/work/af-digiscapesm/work/Ross/temp/testing.tif')
#templateR <- '/datasets/work/af-digiscapesm/work/Ross/temp/testing.tif'

RATTable <- read.csv(paste0(infoDir, '/RATs/NationalGenericSoilGroupsRAT.csv'), stringsAsFactors = F)
mt <- read.csv(paste0(infoDir, '/MetaData/GenericSoilGroup_Metadata.csv'), sep = ',')


colnames(RATTable)[1] <- 'Value'
str(RATTable)

op <- publishRaster(inRasterPath=inRasterPath, outDir=outDir, resampleTemplate=templateR, maskTemplate=templateR, metadataTable=mt, RATTable=RATTable, MakeCOG=T)

op <- publishRaster(inRasterPath=inRasterPath, outDir=outDir, resampleTemplate=NULL, maskTemplate=NULL, metadataTable=mt, RATTable=RATTable, MakeCOG=T)


endTime <- Sys.time()
print(endTime)
difft <- difftime(endTime, startTime, units = "auto")
print(difft)
print('Processing successfully completed')



system(paste0('C:/LocalProgs/QGIS3.2/bin/gdalinfo -mdd all ', op))

