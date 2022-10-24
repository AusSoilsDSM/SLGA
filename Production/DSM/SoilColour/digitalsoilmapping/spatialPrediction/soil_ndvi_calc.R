## TERN Landscapes
# Soil Color Mapping of Australia 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# Created: 7.4.21
# Modified: 12.4.21

# CODE PURPOSE
# soil NDVI mapping
##

## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster)

# root directories
root.tiles<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/spatialPredictions/tiles/"
root.slurm<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/rcode/slurm/ndvi/"
r.script<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/rcode/digitalsoilmapping/spatialPrediction/soil_ndvi_calc.R"

fols<- as.numeric(list.files(root.tiles, full.names = FALSE))
fols<- fols[order(fols)]
fols

###
# begin parallel cluster and register it with foreach
cpus<- 12
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)


# Apply model to each tile
oper1<- foreach(i=1:length(fols), .packages = c("raster", "sp", "rgdal", "caret")) %dopar% {
  
  #select the folder
  sfol<- fols[i]
  nm1<- paste0(root.tiles,sfol, "/")
  
  ## get the layers
  r.raster<- raster(x = paste0(nm1,"type1_surface_R.tif"))
  r.raster<- r.raster/255
  r.raster
  
  nir.raster<- raster(x = paste0(nm1,"NIR_median.tif") )
  nir.raster
  
  ## NDVI
  ndvi.raster<- (nir.raster - r.raster)/(nir.raster+r.raster)
  
  # write raster to file
  nm5<- paste0(nm1, "soil_ndvi_surface.tif")
  nm5
  writeRaster(x = ndvi.raster, filename = nm5, format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  
  # SLURM CHECK
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0(root.slurm, "ndvi_slurmckeck_", i,"_",sfol, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
}
  
  
