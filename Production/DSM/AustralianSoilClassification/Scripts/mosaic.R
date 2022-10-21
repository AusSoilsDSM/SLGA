## Mosaic tiled PC rasters
library(raster)
library(rgdal)
library(sp)

#data.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry/ASC_ORD'
data.directory = '/OSM/CBR/AF_DIGISCAPESM/work/Harry/ASC_ORD'


### organism pca 1
raster_list <- list() # initialise the list of rasters

#Raster file names
rast.list = list.files(data.directory, '.tif$', recursive = F, full.names = T )

#Append rasters to list
for (i in 1:length(rast.list)){
  r1<- raster(rast.list[i])
  raster_list <- append(raster_list, r1)
  print(i)
}

# do the mosaic
mos <- do.call(merge, raster_list)


writeRaster(mos, '/OSM/CBR/AF_DIGISCAPESM/work/Harry/ASC_ORD_mosaic.tif', format='GTiff', overwrite=T)


