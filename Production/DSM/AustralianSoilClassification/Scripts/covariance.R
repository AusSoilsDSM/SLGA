##########################################################################
###       Packages
##########################################################################

library(fasterize)
library(sf)
library(raster)

##########################################################################
###       Inputs
##########################################################################

asris.filename = 'E:/Harry/ASRIS/ASRISCombinedGeo.shp'
atlas.filename = 'E:/Harry/Atlas/soilAtlas2M_ASC.shp'
mosaic.filename = 'E:/Harry/ASC_ORD_mosaic.tif'

##########################################################################
###       Initialise
##########################################################################

asrisPoly <- st_read(asris.filename)
atlasPoly = st_read(atlas.filename)
r <- raster(mosaic.filename)

order.original = readRDS("Z:/Harry/OriginalFactors.rds")

##########################################################################
###       Compute Covariance
##########################################################################

#Rasterisze asris polygon
asrisR <- fasterize(asrisPoly, r, 'ASC_ORD' )
plot(asrisR)

#Reclassify asris values to align with mosaic
reclass.asris = as.matrix(data.frame(1:length(levels(asrisPoly[["ASC_ORD"]])), match(levels(asrisPoly[["ASC_ORD"]]), order.original)))
asrisR.reclass = reclassify(asrisR, reclass.asris)

#Save as reclassifed asris as .tif
writeRaster(asrisR.reclass, 'E:/Harry/Asris_ASC_ORD.tif', format='GTiff', overwrite=T)

#Compute covariance 
cov.asris = crosstab(asrisR.reclass, r)
saveRDS(cov.asris, paste0('Z:/Harry/Output/Covariance', '/Asris', '_Covariance.rds'))




#Rasterisze atlas polygon
atlasR <- fasterize(atlasPoly, r, 'asclut_ASC' )
plot(atlasR)

#Reclassify asris values to align with mosaic
reclass.atlas = as.matrix(data.frame(1:length(levels(atlasPoly[["asclut_ASC"]])), match(levels(atlasPoly[["asclut_ASC"]]), order.original)))
atlasR.reclass = reclassify(atlasR, reclass.atlas)
crs(atlasR.reclass) = crs(r)

#Save as reclassifed atlas as .tif
writeRaster(atlasR.reclass, 'E:/Harry/Atlas_ASC_ORD.tif', format='GTiff', overwrite=T)

#Compute covariance 
cov.atlas = crosstab(atlasR.reclass, r)
saveRDS(cov.atlas, paste0('Z:/Harry/Output/Covariance', '/Atlas', '_Covariance.rds'))


