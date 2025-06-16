### TERN LANDSCAPES 
# Soil CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 1.3.22
# modified: 1.3.22

# CODE PURPOSE
# Application of coobs models to coveriates
# Depth 1


# libraries
library(sp);library(rgdal);library(raster);library(ranger);library(parallel);library(doParallel)

# fixed parameters
valobs<- c(17313,16946,11701,8283,6316,3847)
depths<- 1
typs<- "alldat"
typs2<- c("geom", "sims80")
vart<-  "cec"
cpus<- 12

### root directories
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/"
root.models<- paste0(gen.root,"models/hyperparameter_fit/geoms/")
root.tiles<- paste0(gen.root,"predictions/90m/tiles/cec/")
root.slurm<- paste0(gen.root,"rcode/digital_soil_mapping/spatial_prediction/coobs/slurm/checks/d",depths,"/")
r.code<- paste0(gen.root, "rcode/digital_soil_mapping/spatial_prediction/coobs/d",depths, "/")
slurms<- paste0(gen.root,"rcode/digital_soil_mapping/spatial_prediction/coobs/slurm/d",depths,"/")


### Folders where the covariates are
dpw<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/90m/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
length(fols)
fols<- fols[order(fols)]
fols

# tile run parameters
x1<-ceiling(seq(1,2150,by = 25))
x2<- c(x1[-1] -1,length(fols))
x3<- 1:length(x1)
x3
length(x3)
runs<- 1
srt<- x1[runs]
fin<- x2[runs]
batch<- x3[runs]


###
# begin parallel cluster and register it with foreach
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)




# Apply model to each tile
oper1<- foreach(i=srt:fin, .packages = c("raster", "sp", "rgdal", "caret", "ranger")) %dopar% {
  
  # models
  ### geom models
  geom.mod<- readRDS(file = paste0(root.models,vart,"_d",depths,"_", typs2[1],"_",typs,"_modelfit.rds"))
  sims.mod<-  readRDS(file = paste0(root.models,vart,"_d",depths,"_", typs2[2],"_",typs,"_modelfit.rds"))
  geom.mod;sims.mod
  
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
    s1<- stack(s1, raster(files[j]))
  }
  names(s1)
  
  # other covariates (categorical in nature)
  nm2<- paste(dpw,sfol,sep= "")
  r4<- raster(paste(nm2,"/Relief_geomorphons.tif", sep=""))
  s3<- stack(s1,r4)
  
  
  # check for name matchings
  names(s3)
  names(sims.mod$trainingData)
  names(geom.mod$trainingData)
  
  
  # predict geom model
  # file name and place to put prediction
  nm3<- paste0(root.tiles, sfol, "/d", depths, "/")
  nm5<- paste0(nm3, vart,"_d",depths,"_", typs2[1],"_",typs,"_modelfit.tif")
  nm5
  pred_geom<- predict(object = s3, model = geom.mod, filename = nm5, format = "GTiff",datatype = "INT1U", overwrite = TRUE)
  
  # predict sims model
  # file name and place to put prediction
  nm3<- paste0(root.tiles, sfol, "/d", depths, "/")
  nm6<- paste0(nm3, vart,"_d",depths,"_", typs2[2],"_",typs,"_modelfit.tif")
  nm6
  pred_sims80<- predict(object = s3, model = sims.mod)
  # need to backtransform?
  pred_sims80x<- pred_sims80^2
  # write file 
  writeRaster(x = pred_sims80x, filename = nm6, format = "GTiff",datatype = "FLT4S", overwrite = TRUE )
  
  # Generate the final geom product
  fprod<- pred_sims80x * pred_geom
  # multiplier for number of observations
  fprod2<- valobs[depths] * fprod
  # round down
  fprod2<- floor(fprod2)
  nm7<- paste0(nm3, vart,"_d",depths,"_geomsimsFP_",typs,"_modelfit.tif")
  nm7
  writeRaster(x = fprod2, filename = nm7, format = "GTiff",datatype = "INT4S", overwrite = TRUE )
  
  # remove not needed rasters
  unlink(nm5)
  unlink(nm6)
  
  ## slurm outputs
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0(root.slurm,"slurmckeck_d",depths,"_", i, "_",sfol, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
}

# END
  
  
  
  
