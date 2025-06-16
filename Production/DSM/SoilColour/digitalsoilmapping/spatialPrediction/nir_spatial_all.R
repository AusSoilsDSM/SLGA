## SOIL COLOR
## Spatial application of models realted to NIR band

## libraries
library(sp);library(rgdal);library(raster);library(caret);library(ranger)

args = commandArgs(trailingOnly=TRUE)
print(args)
k = as.numeric(args[1])

# root directories
root.models<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/models/model_fitting/NIR/"
root.tiles<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/spatialPredictions/tiles/"
root.slurm<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/rcode/slurm/spatialprediction/NIR/"


### Folders where the covariates are
dpw<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
length(fols)
fols<- fols[order(fols)]
fols

### place where the models are situated
models<- list.files(path = root.models, pattern = "NIR_model_",full.names = TRUE)
models


# START THE BUSINESS
#select the folder
sfol<- fols[k]
nm1<- paste(dpw,sfol, "/PCS",sep= "")
  
  
## get the covariates
files<- list.files(path = nm1,  pattern="tif$", full.names=TRUE, recursive = T)
files
  
#stack rasters
s1<- stack()
for (j in 1:length(files)){
  s1<- stack(s1, raster(files[j]))
  }
names(s1)
  
  
# other covariates (categorical in nature)
nm2<- paste(dpw,sfol,sep= "")
r4<- raster(paste(nm2,"/relief_geomorphons.tif", sep=""))
  
s3<- stack(s1,r4)
names(s3)
  
# APPLY THE MODEL 
# cycle through each of the models
for (j in 1:50){
  #load model
  mod.sel<- readRDS(models[j])
  mod.char<- as.character(models[j])
    
  # check of label consistency
  names(s3)
  names(mod.sel$trainingData)
    
  # file name and place to put prediction
  nm3<- paste0(root.tiles, sfol, "/NIR_its/")
  nm5<- paste0(nm3, "NIR_pred_it_",j,".tif")
  nm5
    
  #prediction
  pred<- predict(object = s3, model = mod.sel)
  crs(pred)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeRaster(x = pred, filename = nm5, format = "GTiff",datatype = "FLT4S", overwrite = TRUE)}

### get the median and 5th and 95th percentile
it.files<- list.files(path = nm3, pattern = ".tif",full.names = T)
sx<- stack(it.files)
  
#calculate quantiles
f2<- function(x)(quantile(x,probs = c(0.05,0.5,0.95),na.rm=TRUE)) 
predQuantile <- calc(sx, fun=f2)
  
## write quantile rasters to file
  
#10th percentile
r1<- predQuantile[[1]]
names(r1)<- "fifthPercentile_SD"
nm6<- paste0(root.tiles, sfol,"/")
ras1<- paste0(nm6, "/NIR_5thPercentile.tif")
writeRaster(r1,filename = ras1, format="GTiff",overwrite=T)
#plot(r1)
  
#50th percentile
r1<- predQuantile[[2]]
names(r1)<- "median_SD"
nm6<- paste0(root.tiles, sfol,"/")
ras1<- paste0(nm6, "/NIR_median.tif")
writeRaster(r1,filename = ras1, format="GTiff",overwrite=T)
#plot(r1)
  
#90th percentile
r1<- predQuantile[[3]]
names(r1)<- "ninetyfithPercentile_SD"
nm6<- paste0(root.tiles, sfol,"/")
ras1<- paste0(nm6, "/NIR_95thPercentile.tif")
writeRaster(r1,filename = ras1, format="GTiff",overwrite=T)
#plot(r1)
    
# SLURM CHECK
itOuts<- c(k,as.character(Sys.time()))
nmz<- paste0(root.slurm, "slurmckeck_", k,"_",sfol, ".txt")
write.table(itOuts, file = nmz,row.names = F, col.names = F, sep=",")

# END
