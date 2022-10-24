### TERN LANDSCAPES 
# Soil pH 4A1
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 25.5.21
# modified: 25.5.21

# CODE PURPOSE
# calculate mean and statistical moments from prediction iterations
##

### variables
vart<- "pH_4a1"
depth<- "d3"
batch<- 1
srt<- 1
fin<- 2172
cpus<- 12

## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster)

# root directories
model.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/models/"
root.tiles<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/predictions/tiles/"
root.slurm<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/spatialprediction/slurm/"
r.code<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/spatialprediction/d3/statistical_summaries/spatialise_ph_ss_4a1_d3_ALL.R"
slurms<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/spatialprediction/slurm/pH_4a1/d3/"


# average model error summaries
avg.mse.dat<- read.csv(file = paste0(model.out, "ranger_VAL_diogs_pH_summary_alldepths.csv"))
avg.mse.dat
avgMSE<- avg.mse.dat$MSE[which(avg.mse.dat$depth == depth)]
avgMSE

### Folders where the predictions are
fols<- as.numeric(list.files(root.tiles, full.names = FALSE))
length(fols)

###
# begin parallel cluster and register it with foreach
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)


# Apply model to each tile
oper1<- foreach(i=srt:fin, .packages = c("raster", "sp", "rgdal")) %dopar% {
  
  #select the folder
  sfol<- fols[i]
  nm1<- paste0(root.tiles,sfol, "/",depth, "/",vart, "/")
  
  ## get the covariates
  files<- list.files(path = nm1,  pattern="pred_pH", full.names=TRUE, recursive = T)
  files
  
  #stack rasters
  s1<- stack()
  for (j in 1:length(files)){
    s1<- stack(s1, raster(files[j]))
  }
  names(s1)
  
  # STATISTICAL MOMENTS
  # Calculate mean
  meanFile<- paste0(nm1, "pred_",vart,"_mean_",depth, ".tif")
  bootMap.mean<-writeRaster(mean(s1),filename = meanFile, format = "GTiff", overwrite = TRUE)
  #plot(bootMap.mean)
  
  # add kriging estimate
  res.raster<- raster(paste0(nm1,"pred_residual_",depth,".tif"))
  plot(res.raster)
  meanFile<- paste0(nm1, "pred_",vart,"_mean_RK_",depth, ".tif")
  bootMap.mean <- bootMap.mean + res.raster
  writeRaster(bootMap.mean,filename = meanFile, format = "GTiff", overwrite = TRUE)
  
  #Calculate variance
  bootMap.var <- calc(s1, var)
  #plot(bootMap.var)
  
  #Overall prediction variance (adding avgGMSE)
  varFile2<- paste0(nm1, "pred_",vart,"_VAR_",depth, ".tif")
  bootMap.varF<-writeRaster((bootMap.var + avgMSE) ,filename = varFile2, format = "GTiff", overwrite = TRUE)
  #plot(bootMap.varF)
  
  #Standard deviation
  sdFile<- paste0(nm1, "pred_",vart,"_STDEV_",depth, ".tif")
  bootMap.sd<-writeRaster(sqrt(bootMap.varF) ,filename = sdFile, format = "GTiff", overwrite = TRUE)
  
  #standard error
  seFile<- paste0(nm1, "pred_",vart,"_STERR_",depth, ".tif")
  bootMap.se<-writeRaster((bootMap.sd * qnorm(0.95)) ,filename = seFile, format = "GTiff", overwrite = TRUE)
  
  #upper prediction limit
  uplFile<- paste0(nm1, "pred_",vart,"_UPL_",depth, ".tif")
  bootMap.upl<-writeRaster((bootMap.mean + bootMap.se) ,filename = uplFile, format = "GTiff", overwrite = TRUE)
  #plot(bootMap.upl) 
  
  #lower prediction limit
  lplFile<- paste0(nm1, "pred_",vart,"_LPL_",depth, ".tif")
  bootMap.lpl<-writeRaster((bootMap.mean - bootMap.se) ,filename = lplFile, format = "GTiff", overwrite = TRUE)
  #plot(bootMap.lpl) 
  
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0(root.slurm, vart, "/",depth, "/",batch, "/slurmckeck_", i, "_",sfol, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
}

##END

  




