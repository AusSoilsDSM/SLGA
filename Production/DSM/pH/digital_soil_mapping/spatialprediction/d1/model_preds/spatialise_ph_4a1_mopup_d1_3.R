### TERN LANDSCAPES 
# Soil pH 4A1
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 14.5.21
# modified: 16.5.22

# CODE PURPOSE
# Extend models to covariate data
##

### variables
vart<- "pH_4a1"
depth<- "d1"
batch<- 1
# tile counter
i_cnt<- 3


## libraries
library(sp);library(rgdal);library(raster);library(caret);library(ranger)

# root directories
root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/models/"
root.tiles<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/predictions/tiles/"
root.slurm<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/spatialprediction/slurm/"
r.code<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/spatialprediction/d1/spatialise_ph_4a1_d1_1.R"
slurms<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/spatialprediction/slurm/pH_4a1/d1/"


### Folders where the coveraiates are
dpw<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/90m/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
fols<- fols[order(fols)]
length(fols)
fols

## tiles to actually focus on
missing.tiles<- readRDS(file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/data/miscells/d1_missingtile_modelpreds.rds")
missing.tiles
fols<- fols[missing.tiles]
fols


### place where the models are situated
models<- list.files(path = paste0(root, depth), pattern = ".rds",full.names = TRUE)


#select the folder
sfol<- fols[i_cnt]
nm1<- paste(dpw,sfol, "/PCS",sep= "")
  
## get the covariates
files<- list.files(path = nm1,  pattern="tif$", full.names=TRUE, recursive = T)
files<- files[c(1:12,27:52)]
  
#stack rasters
s1<- stack()
for (j in 1:length(files)){
  s1<- stack(s1, raster(files[j]))}
  names(s1)
  
# other covariates (categorical in nature)
nm2<- paste(dpw,sfol,sep= "")
r4<- raster(paste(nm2,"/Relief_geomorphons.tif", sep=""))

s3<- stack(s1,r4)
names(s3)
  
# APPLY THE MODEL (loop through the iterations)
for (z in 1:50){ 
  #load model
  mod.sel<- readRDS(models[z])
  mod.char<- as.character(models[z])
    
  # check of label consistency
  names(s3)
  names(mod.sel$trainingData)
    
  # file name and place to put prediction
    nm3<- paste0(root.tiles, sfol, "/", depth, "/", vart, "/")
    nm5<- paste0(nm3, "pred_iteration_",vart,"_",depth,"_",z,".tif")
    nm5
    
    #prediction
    pred<- predict(object = s3, model = mod.sel, filename = nm5, format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
    }

# save slurmoutputs
itOuts<- c(i_cnt,as.character(Sys.time()))
nmz<- paste0(root.slurm, vart, "/",depth, "/",batch, "/slurmckeck_", missing.tiles[i_cnt], "_",sfol, ".txt")
nmz
write.table(itOuts, file = nmz, row.names = F, col.names = F, sep=",")

##END

  




