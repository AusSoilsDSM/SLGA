## SOIL COLOR
## Spatial application of models
# There are 12 models in play

## modified: 20/8/20
## Finished: 20/8/20


## Load models
## load covariates 
## run the spaital prediction model

### variables
mod<- 1 # model number
varb<- "L"
srt<- 1
fin<- 250


## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster);library(caret);library(ranger)

# root directories
root.models<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/models/model_fitting/"
root.tiles<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/spatialPredictions/tiles/"
root.slurm<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/rcode/slurm/spatialprediction/"



### Folders where the coveraiates are
dpw<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
length(fols)
fols<- fols[order(fols)]
fols

### place where the models are situate
models<- list.files(path = paste0(root.models,"model_", mod), pattern = ".rds",full.names = TRUE)
models

###
# begin parallel cluster and register it with foreach
cpus<- 8
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)


# Apply model to each tile
oper1<- foreach(i=srt:fin, .packages = c("raster", "sp", "rgdal", "caret", "ranger")) %dopar% {
  
  #select the folder
  sfol<- fols[i]
  nm1<- paste(dpw,sfol, "/PCS",sep= "")
  
  ## get the covariates
  files<- list.files(path = nm1,  pattern="tif$", full.names=TRUE, recursive = T)
  #files<- files[c(2:4,1,5:18,20,21,19,22:38)]
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
  # Surface
  #load model
  mod.sel<- readRDS(models[2])
  mod.char<- as.character(models[2])
    
  # check of label consistency
  names(s3)
  names(mod.sel$trainingData)
    
  # file name and place to put prediction
  nm3<- paste0(root.tiles, sfol, "/model_", mod, "/")
  nm5<- paste0(nm3, "surface_model_",mod,"_",varb,".tif")
  nm5
    
  #prediction
  pred<- predict(object = s3, model = mod.sel)
  crs(pred)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeRaster(x = pred, filename = nm5, format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  
  # SUBSOIL
  #load model
  mod.sel<- readRDS(models[1])
  mod.char<- as.character(models[1])
  mod.char
  
  # check of label consistency
  names(s3)
  names(mod.sel$trainingData)
  
  # file name and place to put prediction
  nm3<- paste0(root.tiles, sfol, "/model_", mod, "/")
  nm5<- paste0(nm3, "subsoil_model_",mod,"_",varb,".tif")
  nm5
  
  #prediction
  pred<- predict(object = s3, model = mod.sel)
  crs(pred)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeRaster(x = pred, filename = nm5, format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
    
  # SLURM CHECK
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0(root.slurm, "model_", mod,"/slurmckeck_", i,"_",sfol, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
}

# END