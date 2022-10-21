
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
library(reticulate)

terraOptions(progress=1, memmax=10)

source('/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Production/NationalMaps/DataPublishing/Util_DataPublishing.R')

gdalPath <- '/apps/gdal/3.2.2/bin'


att <- "SoilColour"
inRasterPath <- '/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/spatialPredictions/surface_RGB_updated.tif'
inRasterPath <- '/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/spatialPredictions/subsoil_RGB_updated.tif'

infoDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Production/NationalMaps/DataPublishing'

outDir <- paste0('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/SLGAReady')
if(!dir.exists(outDir)){dir.create(outDir)}
templateRp <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif'
outPath <- paste0(outDir, '/', basename(inRasterPath))

mt <- read.csv(paste0(infoDir, '/MetaData/', att, '_Metadata.csv'), sep = ',', header = F)

inRaster<- rast(inRasterPath )
templateR <- rast(templateRp)

rr <- doResample(inRaster=inRaster, resampleTemplate=templateR)
plot(rr)

tmpPath <- paste0('/scratch1/', ident, '/tmp.tif')

writeRaster(rr, tmpPath, overwrite = T)

ds = geo$gdal$Open(tmpPath, geo$gdal$GA_Update)

  print(paste0('Adding Metadata....'))
  for (b in 1:nrow(mt)) {
    itemName <- mt[b,1]
    itemVal <- mt[b,2]
      ds$SetMetadataItem(itemName,itemVal )
}

  ds$FlushCache()
  ds=NULL
  
  print('Generating COG')

  system(paste0(gdalPath, '/gdaladdo --config GDAL_CACHEMAX 5000 -ro ',tmpPath))
  cmd <- paste0(gdalPath, '/gdal_translate ', tmpPath, ' ', outPath ,' --config GDAL_CACHEMAX 5000 -stats -co TILED=YES -co COMPRESS=DEFLATE -co BIGTIFF=YES -co COPY_SRC_OVERVIEWS=YES' ) 
  system(cmd)
  
print(paste0('Is Valid COG = ', isValidCOG(outPath, quiet=T)))

endTime <- Sys.time()
print(endTime)
difft <- difftime(endTime, startTime, units = "auto")
print(difft)
print('Processing successfully completed')




