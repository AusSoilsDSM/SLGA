library(fasterize)
library(raster)

rasterOptions(progress = 'text')


asris = sf::st_read('C:/Projects/GIS/National/ASRIS/ASRISCombinedGeo.shp')
colnames(asris)

templateR <- raster('C:/Projects/TernLandscapes/ASC/ASCOnly/ASCOnlyRemap.tif')

remap <- read.csv('C:/Projects/TernLandscapes/ASC/codeMerging.csv', stringsAsFactors = F)
asrisM <- merge(asris, remap, by.x = 'ASC_ORD', by.y = 'PolyCode')
head(asrisM)

aR <- fasterize(sf = asrisM, raster = templateR, field = 'RasterCode')
writeRaster(aR, 'C:/Projects/TernLandscapes/ASC/AsrisRaster.tif')
 

atlas <-  sf::st_read('C:/Projects/GIS/National/soilAtlas2M/soilAtlas2M_ASC.shp')
atlasM <- merge(atlas, remap, by.x = 'asclut_ASC', by.y = 'PolyCode')
head(atlasM)


atR <- fasterize(sf = atlasM, raster = templateR, field = 'RasterCode')
plot(atR, maxpixels = 100000)
writeRaster(atR, 'C:/Projects/TernLandscapes/ASC/AtlasRaster.tif')

