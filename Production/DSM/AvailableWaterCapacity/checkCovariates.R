library(terra)

covFilter = 'Parsimonious'
covsDF <- read.csv('/datasets/work/af-digiscapesm/work/Ross/TERN/covsToUse90m.csv', stringsAsFactors = F)
covToUse <- covsDF[covsDF[covFilter]==1,]$Covariate
covPaths <- paste0(covDir, '/', covToUse, '.tif')
covPaths

tilesBdy <- st_read(paste0('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/Metadata/90m/allTiles_combined_RASTER.shp'))
tile <- tilesBdy[1174,]
plot(tile)

e <- ext(tile)
terra::window(e)


r <- rast(paste0('/scratch1/', ident, '/AWC/Parsimonious/Tiles/DLL_Parsimonious_005/1174_DLL_005_Mean.tif'))
plot(r)


covRoot <- '/datasets/work/lw-soildatarepo/work/http/Products/TERN/Covariates/Mosaics/90m'

for (i in 1:length(covToUse)) {
  print(i)
  rp <- paste0(covRoot, '/', covToUse[i], '.tif')
  rc <- rast(rp)
  terra::window(e)
  terra::plot(rc, ext=e, main=covToUse[i])
}

