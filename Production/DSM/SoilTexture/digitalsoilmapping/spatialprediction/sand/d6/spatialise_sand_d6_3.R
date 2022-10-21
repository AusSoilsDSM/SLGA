### soil texture TERN
## spatial prediction
## modified: 18/3/20
## finished: 25/3/20


## Load models
## load covariates 
## run the spaital prediction model

### variables
vart<- "sand"
depth<- "d6"
batch<- 3
srt<- 501
fin<- 750
modsrt<- 41
modfin<- 50

## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster);library(caret);library(ranger)

# root directories
root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/"
root.tiles<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/predictions/tiles/"
root.slurm<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/spatialprediction/slurm/"



### Folders where the coveraiates are
dpw<- "/datasets/work/af-digiscapesm/work/CoVariates/tiles/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
length(fols)


### place where the models are situate
models<- list.files(path = paste0(root, vart, "/", depth), pattern = ".rds",full.names = TRUE)


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
  
  # APPLY THE MODEL (loop through the iterations)
  for (z in modsrt:modfin){ 
    #load model
    mod.sel<- readRDS(models[z])
    mod.char<- as.character(models[z])
    
    # check of label consistency
    names(s3)
    names(mod.sel$trainingData)
    
    # file name and place to put prediction
    nm3<- paste0(root.tiles, sfol, "/", vart, "/")
    it.cnt<-list.dirs(nm3, recursive = F, full.names = F)[z]
    nm5<- paste0(nm3, "/",it.cnt,"/",depth,"/",it.cnt, "_pred_", vart, "_", depth,".tif")
    nm5
    
    #prediction
    pred<- predict(object = s3, model = mod.sel, filename = nm5, format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
    
    #cature model output
    nm6<- paste0(nm3, "/",it.cnt,"/",depth,"/",it.cnt, "_pred_", vart, "_", depth,"_model_string.txt")
    write.table(capture.output(mod.char), file = nm6,sep = "",row.names = F, col.names = F)}
  
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0(root.slurm, vart, "/",depth, "/",batch, "/slurmckeck_", i, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
}

##END

  




