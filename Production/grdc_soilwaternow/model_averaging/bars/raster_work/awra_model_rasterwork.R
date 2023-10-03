### GRDC soil-water NOW
# raster data for model averaging AWRA
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.3.22
# modified: 29.3.22

# CODE PURPOSE
# select files and resample to 90m
# 
library(terra);library(raster)


models.gen<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/models_output/"

# BARS
base.raster<- raster("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/GIS/covariates/bars/climate_PCA_1.COG.tif")

files<- list.files(path = paste0(models.gen,"AWRA/rasters/AwraAreas/Areas/BARS/"),pattern = ".tif$", full.names = T)
files

april.raster.awra<- raster(files[91])
april.raster.awra
plot(april.raster.awra)

july.raster.awra<- raster(files[182])
july.raster.awra
plot(july.raster.awra)


# resample
april.rs<- projectRaster(from = april.raster.awra,to = base.raster,method = "bilinear")
plot(april.rs)

july.rs<- projectRaster(from = july.raster.awra,to = base.raster,method = "bilinear")
plot(july.rs)

# write rasters to file
fnames1<- paste0(models.gen,"AWRA/rasters/AwraAreas/Areas/BARS/rescaled_april_awra.tif")
fnames1
raster::writeRaster(x = april.rs,filename = fnames1, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)

fnames2<- paste0(models.gen,"AWRA/rasters/AwraAreas/Areas/BARS/rescaled_july_awra.tif")
fnames2
raster::writeRaster(x = july.rs,filename = fnames2, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
