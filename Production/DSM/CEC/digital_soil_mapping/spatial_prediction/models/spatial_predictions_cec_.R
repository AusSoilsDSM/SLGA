### TERN LANDSCAPES 
# Soil CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 17.1.22
# modified: 30.1.22

# CODE PURPOSE
# Apply cubist simulation models to covariate data



library(parallel);library(sp);library(rgdal);library(doParallel);library(raster);library(Cubist)

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/"
model.root<- paste0(g.root,"models/cec/")
root.tiles<- paste0(g.root, "/predictions/90m/tiles/cec/")
root.slurm<- paste0(g.root,"/rcode/digital_soil_mapping/spatial_prediction/models/slurm/")
# other paths
r.code<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/rcode/digital_soil_mapping/spatial_prediction/models/d1/"
slurm.code<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/rcode/digital_soil_mapping/spatial_prediction/models/slurm/d1/"


### Folders where the covariates are
dpw<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/90m/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
length(fols)
                    
# tile run parameters
x1<-ceiling(seq(1,2150,by = 25))
x2<- c(x1[-1] -1,length(fols))
x3<- 1:length(x1)
x3
length(x3)
runs<- 1
sel.depth<- 1

srt<- x1[runs]
fin<- x2[runs]
batch<- x3[runs]


# point to where the models are
pred.models<- list.files(path = paste0(model.root,"models/"), pattern = "cubist_model_fit", 
                         full.names = T, recursive = F )
test.pred.model<- readRDS(pred.models[1])
summary(test.pred.model)

residual.models<- list.files(path = paste0(model.root,"residuals/"), pattern = "residual_model_fit", 
                             full.names = T, recursive = F )

set.seed(55)
model.levels<- sampleInt(100,50)
model.levels

###
# begin parallel cluster and register it with foreach
cpus<- 12
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)


# cycle through each tile
oper1<- foreach(i=srt:fin, .packages = c("raster", "sp", "rgdal", "Cubist")) %dopar% {
  
  #select the folder
  sfol<- fols[i]
  nm1<- paste(dpw,sfol, "/PCS",sep= "")
  
  ## get the covariates
  files<- list.files(path = nm1,  pattern="tif$", full.names=TRUE, recursive = T)
  files<- files[c(1:12,27:52)]
  files
  
  #stack rasters
  s1<- stack()
  for (j in 1:length(files)){
    s1<- stack(s1, raster(files[j]))}
  names(s1)
  
  # other covariates (categorical in nature)
  nm2<- paste(dpw,sfol,sep= "")
  r4<- raster(paste(nm2,"/Relief_geomorphons.tif", sep=""))
  
  # depth raster
  r5<- r4
  r5[] <- sel.depth
  names(r5)<- "depth"
    
  # stack the raster set
  s3<- stack(s1,r4,r5)
  names(s3)
    
  # APPLY THE MODELS (loop through the iterations)
  for (m in 1:50){
    model.sel<- model.levels[m]
    #load prediction model
    selected.model<- readRDS(pred.models[model.sel])
    mod.char<- as.character(pred.models[model.sel])
      
    #load residual model
    res.model<- readRDS(residual.models[model.sel])
    res.char<- as.character(residual.models[model.sel])
      
    # check of label consistency
    names(s3)
    selected.model$vars$all
      
    # file name and place to put prediction
    nm3<- paste0(root.tiles, sfol, "/d", sel.depth, "/")
    nm5<- paste0(nm3, "modpred_sim_d",sel.depth,"_",m,".tif")
    nm5
      
    #prediction
    pred<- predict(object = s3, model = selected.model, filename = nm5, format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
      
    # check of label consistency [residuals]
    names(s3)
    res.model$vars$all
      
    # file name and place to put prediction
    nm7<- paste0(root.tiles, sfol, "/d", sel.depth, "/")
    nm9<- paste0(nm7, "respred_sim_d",sel.depth,"_",m,".tif")
    nm9
      
    #prediction
    respred<- predict(object = s3, model = res.model, filename = nm9, format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
      
    # regression kriging prediction
    regkrig_pred<- pred + respred
    nm11<- paste0(root.tiles, sfol, "/d", sel.depth, "/")
    nm12<- paste0(nm11, "rkpred_sim_d",sel.depth,"_",m,".tif")
    nm12
    writeRaster(x = regkrig_pred, filename = nm12, format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
      
    # remove the uneeded files
    unlink(nm5);unlink(nm9)}
  
  # stack all the simulations
  rk.rasters<- list.files(path = nm11, pattern = "rkpred_sim",full.names = T, recursive = F)
  rk.ras<- stack(rk.rasters)
  
  # calculate the median
  f2<- function(x)(quantile(x,probs = c(0.5),na.rm=TRUE)) 
  predQuantile <- calc(rk.ras, fun=f2)
  writeRaster(x = predQuantile, filename = paste0(nm11,"rkpred_median_d",sel.depth,".tif"),format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  # back transform
  real.format<- exp(predQuantile) - 0.1
  writeRaster(x = real.format, filename = paste0(nm11,"rkpred_median_BT_d",sel.depth,".tif"),format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  # remove the simulations
  unlink(rk.rasters)
  
  ## slurm file output
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0(root.slurm, "checks/d",sel.depth,"/slurmckeck_d",sel.depth,"_", i, "_",sfol, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
  
  }
  
  
                    