### GRDC soil-water NOW
# raster data for model averaging ANU model
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.3.22
# modified: 29.3.22

# CODE PURPOSE
# load in netcdf file and save rasters 
# 
library(terra)


models.gen<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/models_output/"
base.raster<- raster("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/GIS/covariates/bars/climate_PCA_1.COG.tif")



files<- list.files(path = paste0(models.gen,"ANU/rasters/"),pattern = ".nc$", full.names = T)

cube <- rast(files[1])
cube
names(cube)

r1<- cube[[7]]
r1
terra::writeRaster(x = r1, filename = paste0(models.gen,"ANU/rasters/april2021_bars_anu.tif"), datatype = "FLT4S", overwrite = TRUE)

r2<- cube[[8]]
r2
terra::writeRaster(x = r2, filename = paste0(models.gen,"ANU/rasters/july2021_bars_anu.tif"), datatype = "FLT4S", overwrite = TRUE)


## resample to covariate resolution
files<- list.files(path = paste0(models.gen,"ANU/rasters/"),pattern = ".tif$", full.names = T)
files

s1<- stack(files)
s1
plot(s1[[1]])

# resample
s2<- projectRaster(from = s1, to = base.raster,method = "bilinear")
plot(s2[[1]])

raster::writeRaster(x = s2[[1]], filename = paste0(models.gen,"ANU/rasters/april2021_bars_anu_resampled_90m.tif"),datatype = "FLT4S", overwrite = TRUE)
raster::writeRaster(x = s2[[2]], filename = paste0(models.gen,"ANU/rasters/july2021_bars_anu_resampled_90m.tif"),datatype = "FLT4S", overwrite = TRUE)







