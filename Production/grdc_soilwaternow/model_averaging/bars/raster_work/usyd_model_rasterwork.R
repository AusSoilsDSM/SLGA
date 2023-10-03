### GRDC soil-water NOW
# raster data for model averaging USYD
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.3.22
# modified: 5.4.22

# CODE PURPOSE
# select files and resample to 90m
# 
library(terra);library(raster)


models.gen<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/models_output/"


# DUL and LL rasters
soil.rasters.path<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/GIS/soilwaterlimits/bars/"
soil.rasters<- list.files(path = soil.rasters.path,pattern = "resampled", full.names = T)
soil.rasters



# BARS
base.raster<- raster("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/GIS/covariates/bars/climate_PCA_1.COG.tif")

files<- list.files(path = paste0(models.gen,"USYD/rasters/bars/"),pattern = ".tif$", full.names = T)
files

# April
r1<- raster(files[1], band=1)
r1
plot(r1)

# July
r2<- raster(files[1], band=2)
r2
plot(r2)


# resample
april.rs<- projectRaster(from = r1,to = base.raster,method = "bilinear")
plot(april.rs)
z1.awc<- (april.rs* 0.9) -  raster(soil.rasters[[2]])
plot(z1.awc)


july.rs<- projectRaster(from = r2,to = base.raster,method = "bilinear")
plot(july.rs)
z2.awc<- (july.rs* 0.9) -  raster(soil.rasters[[2]])
plot(z2.awc)


# write rasters to file
fnames1<- paste0(models.gen,"USYD/rasters/bars/rescaled_bars_april_usyd.tif")
fnames1
raster::writeRaster(x = z1.awc,filename = fnames1, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)

fnames2<- paste0(models.gen,"USYD/rasters/bars/rescaled_bars_july_usyd.tif")
fnames2
raster::writeRaster(x = z2.awc,filename = fnames2, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
