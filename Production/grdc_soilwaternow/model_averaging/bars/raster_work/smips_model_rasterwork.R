### GRDC soil-water NOW
# raster data for model averaging SMIPS
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

files<- list.files(path = paste0(models.gen,"SMIPS/rasters/SMIPSAreas/Areas/BARS/"),pattern = ".tif$", full.names = T)
files

april.raster.smips<- raster(files[91])
april.raster.smips
plot(april.raster.smips)

july.raster.smips<- raster(files[182])
july.raster.smips
plot(july.raster.smips)


# resample
april.rs<- projectRaster(from = april.raster.smips,to = base.raster,method = "bilinear")
plot(april.rs)

july.rs<- projectRaster(from = july.raster.smips,to = base.raster,method = "bilinear")
plot(july.rs)

# write rasters to file
fnames1<- paste0(models.gen,"SMIPS/rasters/SMIPSAreas/Areas/BARS/rescaled_april_smips.tif")
fnames1
raster::writeRaster(x = april.rs,filename = fnames1, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)

fnames2<- paste0(models.gen,"SMIPS/rasters/SMIPSAreas/Areas/BARS/rescaled_july_smips.tif")
fnames2
raster::writeRaster(x = july.rs,filename = fnames2, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
