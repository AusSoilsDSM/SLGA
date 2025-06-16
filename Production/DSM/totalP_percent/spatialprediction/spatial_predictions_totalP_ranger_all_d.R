### TERN LANDSCAPES 
# total P
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 22.11.23
# modified: 22.11.23

# CODE PURPOSE
# apply fitted models to covariate stack
# applicable to all depths


library(sf);library(terra);library(ranger)


# fixed parameters
args = commandArgs(trailingOnly = T)
sel.depth<- as.numeric(args[1])
runs<- as.numeric(args[2])
vart<- "totalP"

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_P/"
model.root<- paste0(g.root,"models/ranger_models/model_objects/")
root.tiles<- paste0(g.root, "/predictions/tiles/")
root.slurm<- paste0(g.root,"/rcode/slurm/outs/digital_soil_mapping/spatialprediction/ranger/")

### Folders where the covariates are
dpw<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/90m/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
fols<- fols[order(fols)]
fols
length(fols)

## tiles to actually focus on
#missing.tiles<- readRDS(file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/micell/tile_mopups/d1_missingtile_modelpreds.rds")
#missing.tiles
#fols<- fols[missing.tiles]
#fols
                    
# point to where the models are
pred.models<- list.files(path = model.root, pattern = paste0("aus_",vart, "_fittedmodel"), 
                         full.names = T, recursive = F )
#test.pred.model<- readRDS(pred.models[1])
#summary(test.pred.model)

set.seed(55)
model.levels<- sample.int(100, 50)
model.levels

#select the folder
sfol<- fols[runs]
nm1<- paste(dpw,sfol, "/PCS",sep= "")
  
## get the covariates
files<- list.files(path = nm1,  pattern="tif$", full.names=TRUE, recursive = T)
files<- files[c(1:38)]
#files<- files[c(1:12,27:52)]
files
  
#stack rasters
s1<- terra::rast(files)
  
# other covariates (categorical in nature)
nm2<- paste(dpw,sfol,sep= "")
r4<- terra::rast(paste(nm2,"/Relief_geomorphons.tif", sep=""))
  
# depth raster
r5<- r4
r5[] <- sel.depth
names(r5)<- "depth"
    
# stack the raster set
s3<- c(s1,r4,r5)
names(s3)
    
# APPLY THE MODELS (loop through the iterations)
for (m in 1:50){
  model.sel<- model.levels[m]
  #load prediction model
  selected.model<- readRDS(pred.models[model.sel])
  mod.char<- as.character(pred.models[model.sel])
      
  # check of label consistency
  names(s3)
  names(selected.model$trainingData)
      
  # file name and place to put prediction
  nm3<- paste0(root.tiles, sfol, "/d", sel.depth, "/")
  nm5<- paste0(nm3, "modpred_median_sim_d",sel.depth,"_",m,".tif")
  
  # predict
  rz.pred<- terra::predict(object = s3, model = selected.model, 
                 filename = nm5, datatype = "FLT4S", 
                 na.rm=TRUE, overwrite = TRUE)
  }
  
# stack all the simulations: median
median.rasters<- list.files(path = nm3, pattern = "modpred_median_",full.names = T, recursive = F)
median.ras<- terra::rast(median.rasters)
median.mean <- terra::app(x = median.ras, fun = median)
median.mean
median.mean<- round(median.mean, digits = 2)
nm5<- paste0(nm3, "modpred_median_fin_d",sel.depth,".tif")
terra::writeRaster(x = median.mean,
            filename = nm5, overwrite = TRUE,datatype = "FLT4S")
unlink(median.rasters)

## slurm file output
itOuts<- c(runs,as.character(Sys.time()))
nmz<- paste0(root.slurm,"d",sel.depth,"/outs/",runs, "_", sfol, "_slurmckeck_d",sel.depth, ".txt")
write.table(itOuts, file = nmz, row.names = F, col.names = F, sep=",")


## END  

  
  
                    
