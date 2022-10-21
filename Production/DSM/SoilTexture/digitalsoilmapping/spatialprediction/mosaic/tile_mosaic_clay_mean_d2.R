## SOIL TEXTURE
# mosaicing
library(raster);library(rgdal);library(sp)

## inputs
n1<- "clay"
n2<- "mean"
n3<- "d2"


#file name
f.name<- paste0("pred_", n1,"_compos_", n2, "_", n3)

# folder locations
root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/predictions/tiles/"
root.short<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/predictions/"
slurm.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/spatialprediction/mosaic/"

fols<- as.numeric(list.files(root))
fols<- sort(fols)
fols
length(fols)



### distance 

raster_list <- list() # initialise the list of rasters

#for (i in 1:50){
for (i in 1:length(fols)){
  fpath1<- paste0(root,fols[i])
  r1<- raster(list.files(fpath1, pattern = f.name, full.names=TRUE))
  raster_list <- append(raster_list, r1)
  print(i)
}

# SLURM output 
slurm.out1<- paste0(slurm.out,f.name, "_tilemos_begin.txt")
itOuts<- c(as.character(Sys.time()))
write.table(itOuts, 
            file = slurm.out1,
            row.names = F, col.names = F, sep=",")

#raster_list
raster_list$filename <- paste0(root.short,f.name, ".tif")
raster_list$datatype <- "FLT4S"
raster_list$format <- "GTiff"
raster_list$overwrite <- TRUE
raster_list$na.rm <- TRUE

# do the mosaic
mos <- do.call(merge, raster_list)

# SLURM output 
slurm.out2<- paste0(slurm.out,f.name, "_tilemos_end.txt")
itOuts<- c(as.character(Sys.time()))
write.table(itOuts, 
            file = slurm.out2,
            row.names = F, col.names = F, sep=",")



