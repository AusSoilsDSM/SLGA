### TERN LANDSCAPES 
# Soil pH (CaCl2)
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 5.10.22
# modified: 6.10.22

# CODE PURPOSE
# Application of prediction models to covariate tiles
# Depth 3


# libraries
library(sp);library(rgdal);library(raster);library(ranger)

# fixed parameters
args = commandArgs(trailingOnly = T)
sel.depth<- as.numeric(args[1])
runs<- as.numeric(args[2])

### variables
vart<- "ph_4b"

## libraries
library(sp);library(rgdal);library(raster);library(caret);library(ranger)

# root directories
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
root.tiles<- paste0(gen.root,"/predictions/tiles/")
root.models<- paste0(gen.root, "models/",vart,"/d",sel.depth,"/")
root.slurm<- paste0(gen.root,"/rcode/slurm/",vart,"/temp_outs/digital_soil_mapping/spatialprediction/model_its/d",sel.depth,"/")

### Folders where the covariates are
dpw<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/90m/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
length(fols)
fols<- fols[order(fols)]
#fols


# point to where the models are
pred.models<- list.files(path = root.models,pattern = ".rds" ,
                         full.names = T, recursive = F )
#pred.models
#test.pred.model<- readRDS(pred.models[1])
#summary(test.pred.model)

set.seed(55)
model.levels<- raster::sampleInt(100,50)
#model.levels

#select the folder
sfol<- fols[runs]
nm1<- paste(dpw,sfol, "/PCS",sep= "")

## get the covariates
files<- list.files(path = nm1,  pattern="tif$", full.names=TRUE, recursive = T)
#files<- files[c(1:12,27:52)]
files<- files[c(1:38)]  ## "R version 4.1.3 (2022-03-10)"
#files

#stack rasters
s1<- stack()
for (j in 1:length(files)){
  s1<- stack(s1, raster(files[j]))}
#names(s1)

# other covariates (categorical in nature)
nm2<- paste(dpw,sfol,sep= "")
r4<- raster(paste(nm2,"/Relief_geomorphons.tif", sep=""))

# stack the raster set
s3<- stack(s1,r4)
#names(s3)

for (m in 1:50){
  model.sel<- model.levels[m]
  #load prediction model
  selected.model<- readRDS(pred.models[model.sel])
  mod.char<- as.character(pred.models[model.sel])
  
  
  # check of label consistency
  sum.check<- sum(names(s3) == names(selected.model$trainingData)[1:39])
  if(sum.check != 39){stop("error")}
  
  # file name and place to put prediction
  nm3<- paste0(root.tiles, sfol, "/d", sel.depth, "/", vart, "/")
  nm5<- paste0(nm3, "modpred_sim_d",sel.depth,"_",m,".tif")
  #nm5
  
  #prediction
  pred<- predict(object = s3, model = selected.model, filename = nm5, format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  
  }

# summarise outputs and save mean
nmx<- paste0(root.tiles, sfol, "/d", sel.depth, "/", vart, "/")
nmx.files<- list.files(path = nmx, pattern = "modpred",full.names = T,recursive = F)
#nmx.files

#stack rasters
nmx.stack<- stack()
for (k in 1:length(nmx.files)){
  nmx.stack<- stack(nmx.stack, raster(nmx.files[k]))}
#nmx.stack

# STATISTICAL MOMENTS
# Calculate mean
meanFile<- paste0(nmx, "pred_",vart,"_mean_d",sel.depth, ".tif")
bootMap.mean<- mean(nmx.stack)
bootMap.mean.r<- signif(bootMap.mean, digits = 3)
writeRaster(bootMap.mean.r,filename = meanFile, format = "GTiff", overwrite = TRUE)

# unlist the iteration outputs
unlink(nmx.files)



## slurm outputs
itOuts<- c(runs,as.character(Sys.time()))
nmy<- paste0(root.slurm,"slurmckeck_d",sel.depth,"_", runs, "_",sfol, ".txt")
write.table(itOuts, file = nmy, row.names = F, col.names = F, sep=",")


# END
  
  
  
  