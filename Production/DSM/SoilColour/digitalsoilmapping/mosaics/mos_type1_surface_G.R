## soil color mosaics

# mosaicing
library(raster);library(rgdal);library(sp)


# folder locations
root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/spatialPredictions/tiles/"
root.short<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/spatialPredictions/"
slurm.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/rcode/slurm/spatialprediction/mosaics/"

fols<- as.numeric(list.files(root))
fols<- sort(fols)
fols
length(fols)



### distance 

raster_list <- list() # initialise the list of rasters
f.name<- "type1_surface_G.tif"

#for (i in 1:50){
for (i in 1:length(fols)){
  print(i)
  fpath1<- paste0(root,fols[i])
  rms<- length(list.files(fpath1, pattern = f.name, full.names=TRUE))
  if(rms ==0){next} else {
    r1<- raster(list.files(fpath1, pattern = f.name, full.names=TRUE))
    raster_list <- append(raster_list, r1)}}

# SLURM output 
sl1<- substr(f.name, start = 1,stop = nchar(f.name)-4)
slurm.out1<- paste0(slurm.out,sl1, "_tilemos_begin.txt")
itOuts<- c(as.character(Sys.time()))
write.table(itOuts, 
            file = slurm.out1,
            row.names = F, col.names = F, sep=",")

#raster_list
raster_list$filename <- paste0(root.short,sl1, ".tif")
raster_list$datatype <- "INT1U"
raster_list$format <- "GTiff"
raster_list$overwrite <- TRUE
raster_list$na.rm <- TRUE

# do the mosaic
mos <- do.call(merge, raster_list)

# SLURM output 
slurm.out2<- paste0(slurm.out,sl1, "_tilemos_end.txt")
itOuts<- c(as.character(Sys.time()))
write.table(itOuts, 
            file = slurm.out2,
            row.names = F, col.names = F, sep=",")

