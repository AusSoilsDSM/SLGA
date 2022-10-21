library(raster)
rasterOptions(progress = 'text')

source('c:/PrivateInfo/PrivateInfo.R')

templateR <- raster('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif')

r <- raster('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/predictions/SLGA_ready/90m/PHW_000_005_EV_N_P_AU_TRN_N_20210913.tif')

rs <- raster::resample(r, templateR, filename=paste0('/scratch1/', ident, '/test1.tif'), overwrite=T)


terraOptions(prog=1)
templateR <- terra::rast('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif')
r <- terra::rast('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/predictions/SLGA_ready/90m/PHW_000_005_EV_N_P_AU_TRN_N_20210913.tif')
rs <- terra::resample(r, templateR, filename=paste0('/scratch1/', ident, '/test1.tif'), overwrite=T)


rrs <- raster(paste0('/scratch1/', ident, '/test1.tif'))
r2 <- raster::focal(rrs, w=matrix(1,3,3),  fun=mean, na.rm=T, NAonly=T, filename=paste0('/scratch1/', ident, '/test2.tif'), overwrite=T)
r3 <- raster::mask(r2, templateR)
raster::writeRaster(r3, filename=paste0('/scratch1/', ident, '/test3.tif'), overwrite=T)
