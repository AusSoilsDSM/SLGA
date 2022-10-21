library(raster)
rasterOptions(progress = 'text')


rootDir <- 'C:/Projects/TernLandscapes/ASC'

rm <- read.csv(paste0(rootDir, '/codeRemaps.csv'))


rcl <- data.frame(is=rm$Remap, becomes=rm$ASCNum, stringsAsFactors = F)

ascR <- raster(paste0(rootDir, '/Maps/ASC.tif'))

Rrm <- reclassify(ascR, rcl, filename=paste0(rootDir, '/Maps/ASCRemap.tif'))



rm <- read.csv(paste0(rootDir, '/ASCOnlyRasterCodes.csv'))
rcl <- data.frame(is=rm$RID, becomes=rm$ASCNum, stringsAsFactors = F)
ascR <- raster(paste0('C:/Projects/TernLandscapes/ASC/ASCOnly/ASCOnly.tif'))
Rrm <- reclassify(ascR, rcl, filename=paste0('C:/Projects/TernLandscapes/ASC/ASCOnly/ASCOnlyRemap.tif'))



rvr <- raster('C:/Projects/TernLandscapes/ASC/RVR/Figure 3d_ASCRF/asc_rf_asc.tif')
rm <- read.csv(paste0(rootDir, '/codeRemaps.csv'))
rcl <- data.frame(is=rm$Remap, becomes=rm$ASCNum, stringsAsFactors = F)
rvrr <- reclassify(rvr, rcl, filename=paste0('C:/Projects/TernLandscapes/ASC/RVRRemap.tif'))
