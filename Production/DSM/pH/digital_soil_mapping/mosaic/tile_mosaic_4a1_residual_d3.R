### TERN LANDSCAPES 
# Soil pH 4A1
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 26.5.21
# modified: 26.5.21

# CODE PURPOSE
# Moasic prediction tiles
##

## libraries
library(sp);library(rgdal);library(raster)

# root directories
root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/predictions/"
root.tiles<- paste0(root, "tiles/")
slurm.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/mosaic/slurm/"
r.code<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/mosaic/tile_mosaic_4a1_median_d3.R"

# tiles
fols<- as.numeric(list.files(root.tiles))
fols<- sort(fols)
fols
length(fols)



### variables
vart<- "pH_4a1"
vart2<- "residual"
depth<- "d3"


#file name output
f.name<- paste0(root, depth, "/", vart, "/", vart2, "_", depth, ".tif")
f.name
#file name input
f.name.in<- paste0("pred_", vart2, "_", depth)
f.name.in



# initialise the list of rasters
raster_list <- list() 

# cycle through each tile and append to list
for (i in 1:length(fols)){
  fpath1<- paste0(root.tiles,fols[i], "/", depth,"/",vart,"/")
  r1<- raster(list.files(fpath1, pattern = f.name.in, full.names=TRUE))
  raster_list <- append(raster_list, r1)
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



