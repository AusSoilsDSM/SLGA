### soil texture TERN
## spatial prediction: step 3: untangle compositional data
## modified: 26/3/20
## Finished: 


### variables
vart<- "clay"
depth<- "d2"
batch<- 1
srt<- 1
fin<- 500


## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster);library(compositions)

# root directories
root.tiles<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/predictions/tiles/"
root.slurm<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/spatialprediction/slurm/"

### Folders where the predictions are
fols<- as.numeric(list.files(root.tiles, full.names = FALSE))
length(fols)

###
# begin parallel cluster and register it with foreach
cpus<- 8
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)


# Apply model to each tile
oper1<- foreach(i=srt:fin, .packages = c("raster", "sp", "rgdal", "compositions")) %dopar% {
  
  # inverse compostional function
  f2<- function(x)(c(ilrInv(x))) 
  
  #select the folder
  sfol<- fols[i]
  nm1<- paste0(root.tiles,sfol)
  nm1
  
  ## MEAN
  ## get the predictions (mean)
  files<- list.files(path = nm1,  pattern= paste0("mean_", depth, ".tif"), full.names=TRUE, recursive = F)
  files
  
  #stack rasters
  s1<- stack()
  for (j in 1:length(files)){
    s1<- stack(s1, raster(files[j]))
  }
  names(s1)
  
  # inverse
  s2 <- calc(s1, fun=f2)
  
  # write files to raster
  names(s2)<- c("clay", "sand", "silt")
  x.name<- c("clay", "sand", "silt")
  for (j in 1:nlayers(s2)){
    out.name<- paste0(nm1, "/", "pred_",x.name[j], "_compos_mean_", depth, ".tif")
    writeRaster(x = s2[[j]], filename = out.name,format = "GTiff", datatype = "FLT4S", overwrite = TRUE )
  }
  
  ## UPPER PI
  ## get the predictions (mean)
  files<- list.files(path = nm1,  pattern= paste0("upPL_", depth, ".tif"), full.names=TRUE, recursive = F)
  files
  
  #stack rasters
  s1<- stack()
  for (j in 1:length(files)){
    s1<- stack(s1, raster(files[j]))
  }
  names(s1)
  
  # inverse
  s2 <- calc(s1, fun=f2)
  
  # write files to raster
  names(s2)<- c("clay", "sand", "silt")
  x.name<- c("clay", "sand", "silt")
  for (j in 1:nlayers(s2)){
    out.name<- paste0(nm1, "/", "pred_",x.name[j], "_compos_upPL_", depth, ".tif")
    writeRaster(x = s2[[j]], filename = out.name,format = "GTiff", datatype = "FLT4S", overwrite = TRUE )
  }
  
  ## LOWER PI
  ## get the predictions (mean)
  files<- list.files(path = nm1,  pattern= paste0("loPL_", depth, ".tif"), full.names=TRUE, recursive = F)
  files
  
  #stack rasters
  s1<- stack()
  for (j in 1:length(files)){
    s1<- stack(s1, raster(files[j]))
  }
  names(s1)
  
  # inverse
  s2 <- calc(s1, fun=f2)
  
  # write files to raster
  names(s2)<- c("clay", "sand", "silt")
  x.name<- c("clay", "sand", "silt")
  for (j in 1:nlayers(s2)){
    out.name<- paste0(nm1, "/", "pred_",x.name[j], "_compos_loPL_", depth, ".tif")
    writeRaster(x = s2[[j]], filename = out.name,format = "GTiff", datatype = "FLT4S", overwrite = TRUE )
  }
  
  # slurm sign of life
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0(root.slurm, vart, "/",depth, "/",batch, "/slurmckeck_", i, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
}

##END

  




