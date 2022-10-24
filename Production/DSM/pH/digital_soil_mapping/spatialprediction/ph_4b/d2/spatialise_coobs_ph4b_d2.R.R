### TERN LANDSCAPES 
# Soil pH (CaCl2)
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 10.10.22
# modified: 10.10.22

# CODE PURPOSE
# Application of coobs models to covariates
# Depth 1


# libraries
library(sp);library(rgdal);library(raster);library(ranger)

# fixed parameters
#args = commandArgs(trailingOnly = T)
#sel.depth<- as.numeric(args[1])
#runs<- as.numeric(args[2])

#sel.depth=1
#runs=1

# fixed parameters
typs<- "alldat"
typs2<- c("geom", "sims80")
vart<-  "ph_4b"


### root directories
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
root.models<- paste0(gen.root,"models/",vart, "/d", sel.depth,"/geoms/")
root.tiles<- paste0(gen.root,"/predictions/tiles/")
root.slurm<- paste0(gen.root,"/rcode/slurm/",vart,"/temp_outs/digital_soil_mapping/spatialprediction/coobs/d",sel.depth,"/")

### Folders where the covariates are
dpw<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/90m/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
length(fols)
fols<- fols[order(fols)]
## tiles to actually focus on
missing.tiles<- readRDS(file = paste0(gen.root, "data/miscells/tilemopups/",vart,"/tilemopup_coobs_",vart,"_d",sel.depth,".rds"))
missing.tiles
fols<- fols[missing.tiles]
fols
length(fols)


# Apply model to each tile
# models
### geom models
geom.mod<- readRDS(file = paste0(root.models,vart,"_d",sel.depth,"_", typs2[1],"_",typs,"_modelfit.rds"))
sims.mod<-  readRDS(file = paste0(root.models,vart,"_d",sel.depth,"_", typs2[2],"_",typs,"_modelfit.rds"))
geom.mod;sims.mod
  
#select the folder
sfol<- fols[runs]
nm1<- paste(dpw,sfol, "/PCS",sep= "")
  
## get the covariates
files<- list.files(path = nm1,  pattern="tif$", full.names=TRUE, recursive = T)
files<- files[c(1:38)]
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
nm3<- paste0(root.tiles, sfol, "/d", sel.depth, "/", vart, "/")
nm5<- paste0(nm3, vart,"_d",sel.depth,"_", typs2[1],"_",typs,"_modelfit.tif")
nm5
pred_geom<- predict(object = s3, model = geom.mod, filename = nm5, format = "GTiff",datatype = "INT1U", overwrite = TRUE)
  
# predict sims model
# file name and place to put prediction
nm3<- paste0(root.tiles, sfol, "/d", sel.depth, "/", vart, "/")
nm6<- paste0(nm3, vart,"_d",sel.depth,"_", typs2[2],"_",typs,"_modelfit.tif")
nm6
pred_sims80<- predict(object = s3, model = sims.mod)
# need to backtransform as data was transformed via sqrt transformation
pred_sims80x<- pred_sims80^2
# write file 
writeRaster(x = pred_sims80x, filename = nm6, format = "GTiff",datatype = "FLT4S", overwrite = TRUE )
  
# Generate the final geom product
fprod<- pred_sims80x * pred_geom
# multiplier for number of observations
#fprod2<- valobs[depths] * fprod
# round down
#fprod2<- floor(fprod2)
nm7<- paste0(nm3, vart,"_d",sel.depth,"_geomsimsFP_",typs,"_modelfit.tif")
nm7
writeRaster(x = fprod, filename = nm7, format = "GTiff",datatype = "FLT4S", overwrite = TRUE )
  
# remove not needed rasters
unlink(nm5)
unlink(nm6)
  
## slurm outputs [no mopups]
#itOuts<- c(runs,as.character(Sys.time()))
#nmy<- paste0(root.slurm,"slurmckeck_d",sel.depth,"_", runs, "_",sfol, ".txt")
#write.table(itOuts, file = nmy, row.names = F, col.names = F, sep=",")

## slurm outputs [mopups]
itOuts<- c(missing.tiles[runs],as.character(Sys.time()))
nmy<- paste0(root.slurm,"slurmckeck_d",sel.depth,"_", missing.tiles[runs], "_",sfol, ".txt")
write.table(itOuts, file = nmy, row.names = F, col.names = F, sep=",")


# END
  
  
  
  