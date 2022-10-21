### soil texture TERN
## spatial prediction: step 2 statistical moments and upper and lower prediction intervals
## modified: 26/3/20
## Finished: 


### variables
vart<- "sand"
depth<- "d1"
batch<- 2
srt<- 501
fin<- 1000
avgmse_sel<- 7

## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster)

# root directories
root.tiles<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/predictions/tiles/"
root.slurm<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/spatialprediction/slurm/"

avgMSE<- readRDS("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/soiltexture_models_avgMSE.rds")
avgMSE<- as.numeric(as.matrix(avgMSE$avgmse[avgmse_sel]))
avgMSE
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
oper1<- foreach(i=srt:fin, .packages = c("raster", "sp", "rgdal")) %dopar% {
  
  #select the folder
  sfol<- fols[i]
  nm1<- paste0(root.tiles,sfol, "/", vart)
  nm1
  ## get the predictions
  files<- list.files(path = nm1,  pattern= paste0(depth, ".tif"), full.names=TRUE, recursive = T)
  files
  
  #stack rasters
  s1<- stack()
  for (j in 1:length(files)){
    s1<- stack(s1, raster(files[j]))
  }
  names(s1)
  
  # STATISTICAL MOMENTS
  # Calculate mean
  meanFile<- paste0(root.tiles,sfol, "/", "pred_",vart,"_mean_",depth, ".tif")
  bootMap.mean<-writeRaster(mean(s1),filename = meanFile, format = "GTiff", overwrite = TRUE)
  #plot(bootMap.mean)
  
  #Calculate variance
  bootMap.var <- calc(s1, var)
  #plot(bootMap.var)
  
  #Overall prediction variance (adding avgGMSE)
  varFile2<- paste0(root.tiles,sfol, "/", "pred_",vart,"_var_",depth, ".tif")
  bootMap.varF<-writeRaster((bootMap.var + avgMSE) ,filename = varFile2, format = "GTiff", overwrite = TRUE)
  plot(bootMap.varF)
  
  #Standard deviation
  sdFile<- paste0(root.tiles,sfol, "/", "pred_",vart,"_stdev_",depth, ".tif")
  bootMap.sd<-writeRaster(sqrt(bootMap.varF) ,filename = sdFile, format = "GTiff", overwrite = TRUE)
  
  #standard error
  seFile<- paste0(root.tiles,sfol, "/", "pred_",vart,"_SEerr_",depth, ".tif")
  bootMap.se<-writeRaster((bootMap.sd * qnorm(0.95)) ,filename = seFile, format = "GTiff", overwrite = TRUE)
  
  #upper prediction limit
  uplFile<- paste0(root.tiles,sfol, "/", "pred_",vart,"_upPL_",depth, ".tif")
  bootMap.upl<-writeRaster((bootMap.mean + bootMap.se) ,filename = uplFile, format = "GTiff", overwrite = TRUE)
  #plot(bootMap.upl) 
  
  #lower prediction limit
  lplFile<- paste0(root.tiles,sfol, "/", "pred_",vart,"_loPL_",depth, ".tif")
  bootMap.lpl<-writeRaster((bootMap.mean - bootMap.se) ,filename = lplFile, format = "GTiff", overwrite = TRUE)
  plot(bootMap.lpl) 
  
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0(root.slurm, vart, "/",depth, "/",batch, "/slurmckeck_", i, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
}

##END






