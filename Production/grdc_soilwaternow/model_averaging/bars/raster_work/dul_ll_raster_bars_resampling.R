### GRDC soil-water NOW
# Process DUL and LL average raster for bars for rootzone
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.3.22
# modified: 29.3.22

# CODE PURPOSE
# Get the model outputs into a form that it can be combined easily with other model outputs.
# 


library(raster);library(rgdal);library(sp)

# BARS
base.raster<- raster("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/GIS/covariates/bars/climate_PCA_1.COG.tif")


# input rasters
files<- list.files(path = "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/GIS/soilwaterlimits/bars/",full.names = T)
files
s1<- stack(files)

# resample
s2<- projectRaster(from = s1, to = base.raster,method="bilinear")


# layer thicknesses
# 0-5cm = 50mm
# 5-15cm = 100mm
# 15-30cm = 150mm
# 30-60cm = 300mm
# 60-100cm = 400mm

names(s2)

mean.ll<- mean((s2[[1]]/100),(s2[[2]]/100),(s2[[3]]/100),(s2[[4]]/100),(s2[[5]]/100))
mean.ll
mean.dul<- mean((s2[[7]]/100),(s2[[8]]/100),(s2[[9]]/100),(s2[[10]]/100),(s2[[11]]/100))
mean.dul

# mean ll (mm)
mean.ll_mm<- mean.ll * 900



# write rasters to file
raster::writeRaster(x = mean.ll, 
            filename = "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/GIS/soilwaterlimits/bars/bars_slga_ll_resampled.tif",
            format = "GTiff", datatype = "FLT4S", overwrite = TRUE)

raster::writeRaster(x = mean.dul, 
                    filename = "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/GIS/soilwaterlimits/bars/bars_slga_dul_resampled.tif",
                    format = "GTiff", datatype = "FLT4S", overwrite = TRUE)

raster::writeRaster(x = mean.ll_mm, 
                    filename = "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/GIS/soilwaterlimits/bars/bars_slga_ll_mm_resampled.tif",
                    format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
