### GRDC soil-water NOW
# Get dul and ll raster for each of the bounding box areas
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.3.22
# modified: 29.3.22

# CODE PURPOSE
# Bring in estimated SLGA maps of DUL and LL15
# clip to regions of interest
# 

library(raster);library(rgdal);library(sp)




## DUl and LL15 rasters
soilwater.raster<- "/datasets/work/lw-soildatarepo/work/http/Products/Collabs/SoilWaterNow/AWC/"
soilwater.raster<- list.files(path = soilwater.raster, pattern = "Median",full.names = T)
soilwater.raster<- stack(soilwater.raster)
crs(soilwater.raster)