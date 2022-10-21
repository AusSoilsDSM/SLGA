### soil depth 2019
## spatial prediction of categorical models (deep soil prediction)
## modified: 26.7.2019



## Load models
## load covariates 
## run the spaital prediction model

## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster);library(caret);library(ranger)




### Folders where the coveraiates are
dpw<- "/OSM/CBR/AF_DIGISCAPESM/work/CoVariates/tiles/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
length(fols)


### place where the models are situated
models<- list.files(path = "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/", pattern = "aus_soilDepth_binary_model_batch_",full.names = TRUE)


###
# begin parallel cluster and register it with foreach
cpus<- 8
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)


# Apply Cubist model to each tile
batch<- 1
oper1<- foreach(i=1:length(fols), .packages = c("raster", "sp", "rgdal", "caret", "ranger")) %dopar% {
  
  #select the folder
  sfol<- fols[i]
  nm1<- paste(dpw,sfol, "/PCS",sep= "")
  
  ## get the covariates
  files<- list.files(path = nm1,  pattern="tif$", full.names=TRUE, recursive = T)
  #files<- files[c(1:10,12,11,13:29)]
  files
  
  s1<- stack()
  for (j in 1:length(files)){
    s1<- stack(s1, raster(files[j]))
  }
  names(s1)
  
  # other covariates (categorical in nature)
  nm2<- paste(dpw,sfol,sep= "")
  r4<- raster(paste(nm2,"/relief_geomorphons.tif", sep=""))
  
  s3<- stack(r4, s1)
  names(s3)
  
  # APPLY THE MODEL (loop through the iterations)
  
  for (z in 1:50){  # this will change
    #load model
    mod.sel<- readRDS(models[z])
    mod.char<- as.character(models[z])
    
    # file name and place to put prediction
    nm3<- paste("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/spatialPrediction/tiles/",sfol, sep="")
    dir.list<-list.dirs(nm3)
    list.dirs(nm3,full.names = F)[z+1]
    nm4<- dir.list[z+1]
    nm5<- paste(nm4, "/", list.dirs(nm3,full.names = F)[z+1], "_soilDepth__cat_pred.tif", sep="")
    
    #prediction
    pred<- predict(object = s3, model = mod.sel, filename = nm5, format = "GTiff",datatype = "INT2S", overwrite = TRUE)
    
    #cature model output
    nm6<- paste(nm4, "/model_cat_string.txt",sep="")
    
    write.table(capture.output(mod.char), file = nm6,sep = "",row.names = F, col.names = F)}
  
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/rcode/slurm/slurmCheck/CAT1/b1/Iteration_counter_", batch, "_", i, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
}
