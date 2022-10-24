### TERN LANDSCAPES 
# Soil pH 4A1
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 18.5.22
# modified: 18.5.22

# CODE PURPOSE
# calculate mean and statistical moments from prediction iterations
##

### variables
vart<- "pH_4a1"
depth<- "d2"
batch<- 2
# tile counter
i_cnt<- 1


## libraries
library(sp);library(rgdal);library(raster)

# root directories
model.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/models/"
root.tiles<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/predictions/tiles/"
root.slurm<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/spatialprediction/slurm/"
#r.code<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/spatialprediction/d1/statistical_summaries/spatialise_ph_ss_4a1_d1_ALL.R"
#slurms<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/spatialprediction/slurm/pH_4a1/d1/"


# average model error summaries
avg.mse.dat<- read.csv(file = paste0(model.out, "ranger_VAL_diogs_pH_summary_alldepths_90m.csv"))
avg.mse.dat
avgMSE<- avg.mse.dat$MSE[which(avg.mse.dat$depth == depth)]
avgMSE

### Folders where the predictions are
fols<- as.numeric(list.files(root.tiles, full.names = FALSE))
length(fols)


#select the folder
sfol<- fols[i_cnt]
nm1<- paste0(root.tiles,sfol, "/",depth, "/",vart, "/")
  
## get the covariates
files<- list.files(path = nm1,  pattern="pred_iteration", full.names=TRUE, recursive = T)
#files
  
#stack rasters
s1<- stack()
for (j in 1:length(files)){
  s1<- stack(s1, raster(files[j]))}
#names(s1)
  
# STATISTICAL MOMENTS
# Calculate mean
meanFile<- paste0(nm1, "pred_",vart,"_mean_",depth, ".tif")
bootMap.mean<- mean(s1)
bootMap.mean.r<- signif(bootMap.mean, digits = 3)
writeRaster(bootMap.mean.r,filename = meanFile, format = "GTiff", overwrite = TRUE)

# add kriging estimate
res.raster<- raster(paste0(nm1,"pred_residual_",depth,".tif"))
#plot(res.raster)
meanFile<- paste0(nm1, "pred_",vart,"_mean_RK_",depth, ".tif")
bootMap.mean <- bootMap.mean + res.raster
bootMap.mean.r<- signif(bootMap.mean, digits = 3)
writeRaster(bootMap.mean.r,filename = meanFile, format = "GTiff", overwrite = TRUE)
  
#Calculate variance
bootMap.var <- calc(s1, var)
#plot(bootMap.var)
  
#Overall prediction variance (adding avgGMSE)
varFile2<- paste0(nm1, "pred_",vart,"_VAR_",depth, ".tif")
bootMap.varF<- bootMap.var + avgMSE
writeRaster(bootMap.varF ,filename = varFile2, format = "GTiff", overwrite = TRUE)
#plot(bootMap.varF)
  
#Standard deviation
sdFile<- paste0(nm1, "pred_",vart,"_STDEV_",depth, ".tif")
bootMap.sd<- sqrt(bootMap.varF)
writeRaster(bootMap.sd ,filename = sdFile, format = "GTiff", overwrite = TRUE)
  
#standard error
seFile<- paste0(nm1, "pred_",vart,"_STERR_",depth, ".tif")
bootMap.se<- bootMap.sd * qnorm(0.95)
writeRaster(bootMap.se ,filename = seFile, format = "GTiff", overwrite = TRUE)
  
#upper prediction limit
uplFile<- paste0(nm1, "pred_",vart,"_UPL_",depth, ".tif")
bootMap.upl<- bootMap.mean + bootMap.se
bootMap.upl.r<- signif(bootMap.upl, digits = 3)
writeRaster(bootMap.upl.r ,filename = uplFile, format = "GTiff", overwrite = TRUE)
#plot(bootMap.upl) 
  
#lower prediction limit
lplFile<- paste0(nm1, "pred_",vart,"_LPL_",depth, ".tif")
bootMap.lpl<- bootMap.mean - bootMap.se
bootMap.lpl.r<- signif(bootMap.lpl, digits = 3)
writeRaster(bootMap.lpl.r ,filename = lplFile, format = "GTiff", overwrite = TRUE)
#plot(bootMap.lpl) 

# slurm file output
itOuts<- c(i_cnt,as.character(Sys.time()))
nmz<- paste0(root.slurm, vart, "/",depth, "/",batch, "/slurmckeck_", i_cnt, "_",sfol, ".txt")
write.table(itOuts, file = nmz, row.names = F, col.names = F, sep=",")

# unlink iteration files
unlink(files)


##END

  




