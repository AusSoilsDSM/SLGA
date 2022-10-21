###############################################################################################################
###  Author : Ross Searle         
###  Date : Mon Aug 09 12:11:31 2021                      
###  Project : TERN Landscapes
###  Purpose : Extract the ASRIS Level 5 polygons from the "Best scale ASRIS compilation map and merge them 
###            into the modelled ASC map - ASRIS L5 values replace Modelled DSM estimates
###############################################################################################################

library(raster)
library(fasterize)
library(sf)
library(reticulate)
library(xml2)
library(htmltidy)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+08,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory


polys <- st_read('C:/Projects/GIS/National/ASRIS/ASRISCombinedGeo.shp')
colnames(polys)


#### subset the level 5 (1:100k) scale data out
L5 <- polys[polys$ALevel==5, ]
st_write(L5, 'c:/projects/ASC/L5.shp')
plot(st_geometry(L5))

r <- raster('C:/Projects/TernLandscapes/ASC/FinalMassage/ASC_boot.tif')

#### Match the L5 codes to the raster ASC numeric codes
L5 <- st_read('c:/projects/TERNLandscapes/ASC/L5.shp')

which(is.null(polys$ASC_ORD))
codes <- unique(L5$ASC_ORD)
write.csv(codes, 'c:/projects/ASC/polyCodes.csv')

idxs <- which(L5$ASC_ORD=='ZZ' | L5$ASC_ORD=='YY' | L5$ASC_ORD=='NR' | is.na(L5$ASC_ORD))
p2 <- L5[-idxs,]
cm <- read.csv('c:/projects/TERNLandscapes/ASC/codeMerging.csv')
cm
p3 <- merge(p2, cm, by.x = 'ASC_ORD', by.y = 'PolyCode' )


colnames(p3)
plot(st_geometry(p3))

#p2$ascNum <- as.numeric(as.factor(p2$ASC_ORD))
st_write(p3, 'c:/projects/TERNLandscapes/ASC/FinalMassage/p3.shp')

p3 <- st_read('c:/projects/TERNLandscapes/ASC/FinalMassage/p3.shp')
head(p3)
rP3 <- fasterize(p3, r, field = "RasterCode", fun="first")
plot(r)
plot(rP2)
writeRaster(rP3, 'c:/projects/TERNLandscapes/ASC/FinalMassage/rp3.tif')

rP3 <- raster('c:/projects/TERNLandscapes/ASC/FinalMassage/rp3.tif')

mr <- merge(rP3, r)
writeRaster(mr, 'c:/projects/TERNLandscapes/ASC/FinalMassage/merged_L5_ASCBoot.tif')

mr <- raster('C:/Projects/TernLandscapes/ASC/FinalMassage/merged_L5_ASCBoot.tif')
rci <- raster('C:/Projects/TernLandscapes/ASC/FinalMassage/ASC_boot_CI.tif')

waterBodyMask <- raster('D:/TERNSoils/CoVariates/Relief/relief_dems_3s_mosaic1.tif')
or <- mask(mr, waterBodyMask, filename='C:/Projects/TernLandscapes/ASC/FinalMassage/merged_L5_ASCBootMasked.tif', datatype='INT1U', overwrite=T, NAflag=0)
dataType(or)
oci <- mask(rci, waterBodyMask, filename='C:/Projects/TernLandscapes/ASC/FinalMassage/merged_L5_ASC_CI_BootMasked.tif')


file.rename(from ='C:/Projects/TernLandscapes/ASC/FinalMassage/merged_L5_ASCBootMasked.tif', to='C:/Projects/TernLandscapes/ASC/FinalMassage/ASC_EV_C_P_AU_TRN_N.tif')
file.rename(from ='C:/Projects/TernLandscapes/ASC/FinalMassage/merged_L5_ASC_CI_BootMasked.tif', to='C:/Projects/TernLandscapes/ASC/FinalMassage/ASC_CI_C_P_AU_TRN_N.tif')



########  Below not done by me - Ash Sommer has scripts to achieve the same
MD <- read.csv('C:/Projects/TernLandscapes/ASC/MetaData/ASC_Metadata.csv', sep = '=', stringsAsFactors = F)
#inRasterPath <- 'C:/Temp/meta/smips_totalbucket_mm_20190105.tif'
inRasterPath <- 'C:/Projects/TernLandscapes/ASC/merged_L5_ASCBootMasked.tif'
outDir <- 'C:/Temp/meta2'
RATTable <- read.csv('C:/Projects/TernLandscapes/ASC/ASC_RAT.csv', stringsAsFactors = F)
colnames(RATTable)[1] <- 'Value'
str(RATTable)
mt <- read.csv('C:/Projects/TernLandscapes/ASC/MetaData/ASC_Metadata.csv', sep = '=')

op <- insertMetadata(inRasterPath=inRasterPath, outDir=outDir, metadataTable=mt, RATTable=RATTable, MakeCOG=T)

op <- insertMetadata(inRasterPath=inRasterPath, outDir=outDir, metadataTable=mt, RATTable=RATTable, MakeCOG=F)


system(paste0('C:/LocalProgs/QGIS3.2/bin/gdalinfo -mdd all ', op))



