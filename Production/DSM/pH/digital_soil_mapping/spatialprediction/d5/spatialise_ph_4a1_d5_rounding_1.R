### TERN LANDSCAPES 
# Soil pH 4A1
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 13.9.21
# modified: 13.9.21

# CODE PURPOSE
# rounding rasters to 2dp
##

### variables
vart<- "pH_4a1"
vart1<- "LPL"
vart2<- "mean_RK"
vart3<- "UPL"
depth<- "d5"
cpus<- 12

## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster)

# root directories
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
root.tiles<- paste0(gen.root,"predictions/tiles/")
slurm.out<- paste0(gen.root,"rcode/digital_soil_mapping/spatialprediction/slurm/",vart,"/",depth,"/1/")

# tiles
fols<- as.numeric(list.files(root.tiles))
fols<- sort(fols)
fols
length(fols)

###
# begin parallel cluster and register it with foreach
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)

# Apply model to each tile
oper1<- foreach(i=1:2172, .packages = c("raster", "sp", "rgdal")) %dopar% {
  
  #select the folder
  sfol<- fols[i]
  nm1<- paste0(root.tiles,sfol,"/")
  
  fname1<- paste0(nm1,depth,"/",vart, "/pred_",vart,"_",vart2, "_",depth, ".tif")
  pred.file<- raster(fname1)
  pred.file
  rx<- round(pred.file, digits = 2)
  rx
  writeRaster(x = rx, filename = fname1 ,format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  
  fname2<- paste0(nm1,depth,"/",vart, "/pred_",vart,"_",vart1, "_",depth, ".tif")
  fname2
  lpl.file<- raster(fname2)
  lpl.file
  ry<- round(lpl.file, digits = 2)
  ry
  writeRaster(x = ry, filename = fname2 ,format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  
  fname3<- paste0(nm1,depth,"/",vart, "/pred_",vart,"_",vart3, "_",depth, ".tif")
  fname3
  upl.file<- raster(fname3)
  upl.file
  rz<- round(upl.file, digits = 2)
  rz
  writeRaster(x = rz, filename = fname3 ,format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  
  # SLURM output 
  sluit<- paste0(slurm.out,"slurm_out_",i, "_", sfol,".txt")
  itOuts<- c(as.character(Sys.time()))
  write.table(itOuts, 
            file = sluit,
            row.names = F, col.names = F, sep=",")
}




