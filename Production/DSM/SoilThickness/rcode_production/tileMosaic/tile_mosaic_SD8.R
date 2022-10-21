## SOIL DEPTH
# thresh150cm_SD


## Mosaic tiled 
library(raster);library(rgdal);library(sp)

# folder locations
fols<- as.numeric(list.files("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/spatialPrediction/tiles/"))
fols<- sort(fols)
fols
length(fols)


raster_list <- list() # initialise the list of rasters

#for (i in 1:50){
for (i in 1:length(fols)){
  fpath1<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/spatialPrediction/tiles/",fols[i])
  r1<- raster(list.files(fpath1, pattern="thresh150cm_SD.tif", full.names=TRUE))
  raster_list <- append(raster_list, r1)
  print(i)
}

#raster_list
raster_list$filename <- "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/spatialPrediction/thresh150cm_SD.tif"
raster_list$datatype <- "FLT4S"
raster_list$format <- "GTiff"
raster_list$overwrite <- TRUE
raster_list$na.rm <- TRUE

# do the mosaic
mos <- do.call(merge, raster_list)



