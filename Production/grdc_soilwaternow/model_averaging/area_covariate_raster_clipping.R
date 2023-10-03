### GRDC soil-water NOW
# Clipping rasters of covariates for specified area bounding boxes
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 25.3.22
# modified: 25.3.22

# CODE PURPOSE
# Input bounding boxes and selected covariates, clip to bounding boxes
# 

library(raster);library(sp);library(rgdal)

root.dir<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/GIS/area_bbs/"
out.dir<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/GIS/covariates/"

# raster files
raster.files<- list.files(path = "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/PCS/",recursive = T,full.names = T)
raster.files<- raster.files[c(1:12,33:52)]
raster.files<- raster.files[c(1,5,6,7,13,16,17,18,24,25,26,27)]
raster.files
s1<- stack(raster.files)

# bring in bounding boxes
bars.bb<- readOGR(paste0(root.dir,"bars_bb.shp"))
muttama.bb<- readOGR(paste0(root.dir,"muttama_bb.shp"))
ep.bb<- readOGR(paste0(root.dir,"ep_bb.shp"))

# BARS
# raster clipping
cr <- crop(s1, y = extent(bars.bb), snap = "out")

# rasterise the polygon
fr <- rasterize(x = bars.bb, y = cr)

# clip using mask
cr.clip <- mask(x = cr, mask = fr)  #use the mask 

cr.clip
nlayers(cr.clip)

for (i in 1:nlayers(cr.clip)){
  r1<- cr.clip[[i]]
  fname<- paste0(out.dir,"bars/",names(r1),".tif")
  writeRaster(x = r1,filename = fname, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)}

# EP
# raster clipping
cr <- crop(s1, y = extent(ep.bb), snap = "out")

# rasterise the polygon
fr <- rasterize(x = ep.bb, y = cr)

# clip using mask
cr.clip <- mask(x = cr, mask = fr)  #use the mask 

cr.clip
nlayers(cr.clip)

for (i in 1:nlayers(cr.clip)){
  r1<- cr.clip[[i]]
  fname<- paste0(out.dir,"EP/",names(r1),".tif")
  writeRaster(x = r1,filename = fname, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)}
  

# Muttama
# raster clipping
cr <- crop(s1, y = extent(muttama.bb), snap = "out")

# rasterise the polygon
fr <- rasterize(x = muttama.bb, y = cr)

# clip using mask
cr.clip <- mask(x = cr, mask = fr)  #use the mask 

cr.clip
nlayers(cr.clip)

for (i in 1:nlayers(cr.clip)){
  r1<- cr.clip[[i]]
  fname<- paste0(out.dir,"muttama/",names(r1),".tif")
  writeRaster(x = r1,filename = fname, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)}
  
  
