### TERN LANDSCAPES 
# Soil pH 4A1 [geoms]
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 13.9.21
# modified: 13.9.21

# CODE PURPOSE
# Moasic prediction geom tiles
##

## libraries
library(sp);library(rgdal);library(raster)

# root directories
root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/predictions/"
root.tiles<- paste0(root, "tiles/")
slurm.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/mosaic/slurm/"
#r.code<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/mosaic/tile_mosaic_4a1_median_d1.R"

# tiles
fols<- as.numeric(list.files(root.tiles))
fols<- sort(fols)
fols
length(fols)



### variables
vart<- "pH_4a1"
vart2<- "geomsimsFP"
vart3<- "alldat"
depth<- "d1"


#file name output
f.name<- paste0(root, depth, "/", vart, "/", vart, "_", vart2, "_",vart3,"_", depth, ".tif")
f.name
#file name input
f.name.in<- paste0(vart, "_", depth, "_", vart2, "_", vart3,"_modelfit.tif")
f.name.in



# initialise the list of rasters
raster_list <- list() 

# cycle through each tile and append to list
for (i in 1:length(fols)){
  fpath1<- paste0(root.tiles,fols[i], "/", depth,"/",vart,"/")
  r1<- raster(list.files(fpath1, pattern = f.name.in, full.names=TRUE))
  raster_list <- append(raster_list, r1)
  print(i)
}

# SLURM output 
slurm.out1<- paste0(slurm.out,f.name.in, "_tilemos_begin.txt")
itOuts<- c(as.character(Sys.time()))
write.table(itOuts, 
            file = slurm.out1,
            row.names = F, col.names = F, sep=",")


#raster_list
raster_list$filename <- f.name
raster_list$datatype <- "FLT4S"
raster_list$format <- "GTiff"
raster_list$overwrite <- TRUE
raster_list$na.rm <- TRUE

# do the mosaic
mos <- do.call(merge, raster_list)

# SLURM output 
slurm.out2<- paste0(slurm.out,f.name.in, "_tilemos_end.txt")
itOuts<- c(as.character(Sys.time()))
write.table(itOuts, 
            file = slurm.out2,
            row.names = F, col.names = F, sep=",")



