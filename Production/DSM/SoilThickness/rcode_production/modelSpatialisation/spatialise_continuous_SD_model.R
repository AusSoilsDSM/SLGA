### soil depth 2019
## spatial prediction
## modified: 29.7.2019



## Load models
## load covariates 
## run the spaital prediction model

## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster);library(caret);library(ranger)




### Folders where the coveraiates are
dpw<- "/OSM/CBR/AF_DIGISCAPESM/work/CoVariates/tiles/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
length(fols)


### place where the models are situate
models<- list.files(path = "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/", pattern = "aus_soilDepth_model_batch_",full.names = TRUE)
models



###
# begin parallel cluster and register it with foreach
cpus<- 8
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)


# Apply Cubist model to each tile
batch<- 1
oper1<- foreach(i=1:250, .packages = c("raster", "sp", "rgdal", "caret", "ranger")) %dopar% {
  
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
  #r1<- raster(paste(nm2,"/Clim_agroclim_hutch.tif", sep=""))
  #r2<- raster(paste(nm2,"/Clim_kpnall.tif",sep=""))
  #r3<- raster(paste(nm2,"/Veg_HS_ICESatGLAS.tif", sep=""))
  r4<- raster(paste(nm2,"/relief_geomorphons.tif", sep=""))
  #s2<- stack(r1,r2,r4)

  s3<- stack(r4,s1)
  names(s3)
  
  # APPLY THE MODEL (loop through the iterations)
  
  for (z in 41:50){  # these are changable indices (fin: 1:10, 11:20;21:30;31:40, 41:50)
    #load model
    mod.sel<- readRDS(models[z])
    mod.char<- as.character(models[z])
    
    # file name and place to put prediction
    nm3<- paste("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/spatialPrediction/tiles/",sfol, sep="")
    dir.list<-list.dirs(nm3)
    list.dirs(nm3,full.names = F)[z+1]
    nm4<- dir.list[z+1]
    nm5<- paste(nm4, "/", list.dirs(nm3,full.names = F)[z+1], "_soilDepth_pred.tif", sep="")
    
    #prediction
    pred<- predict(object = s3, model = mod.sel, filename = nm5, format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
    
    #cature model output
    nm6<- paste(nm4, "/model_string.txt",sep="")
    
    write.table(capture.output(mod.char), file = nm6,sep = "",row.names = F, col.names = F)}
  
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/rcode/slurm/slurmCheck/b1/Iteration_counter_", batch, "_", i, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
  }

  




