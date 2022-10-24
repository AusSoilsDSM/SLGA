### TERN LANDSCAPES 
# Soil pH
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 7.10.22
# modified: 7.10.22

# CODE PURPOSE
# Uncertainty analysis steps: PCA
# extend spatially


library(sp);library(rgdal);library(raster)

### variables
args = commandArgs(trailingOnly = T)
sel.depth<- as.numeric(args[1])
runs<- as.numeric(args[2])

### variables
vart<- "ph_4b"
pc<- 33 # number of selected pcs

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
root.tiles<- paste0(g.root,"/predictions/tiles/")
root.slurm<- paste0(g.root,"/rcode/slurm/",vart,"/temp_outs/digital_soil_mapping/spatialprediction/uncert_pca/d",sel.depth,"/")
cluster.out<- paste0(g.root,"models/ph_4b/d",sel.depth,"/uncertainty/cluster_centroids/")

### Folders where the covariates are
dpw<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/90m/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
length(fols)
fols<- fols[order(fols)]

## necessary inputs
# PCA object for converting covariates to PC variables
pr_comp<- readRDS(file = paste0(cluster.out,"PCA_data_oob_ph4b_covariates_d",sel.depth,".rds"))
names(pr_comp$center)


# cluster centers of optimal config
#cluster.centre<- readRDS(file = paste0(cluster.out,"101_cluster_center.rds"))
#cluster.centre

# model errors distribution per cluster
#cluster.errors<- readRDS(file = paste0(cluster.out, "cluster_errors/101_cluster_errors_cluster.rds"))
#cluster.errors

#select the folder
sfol<- fols[runs]
nm1<- paste(dpw,sfol, "/PCS",sep= "")
  
## get the covariates
files<- list.files(path = nm1,  pattern="tif$", full.names=TRUE, recursive = T)
#files<- files[c(1:12,27:52)]
files<- files[c(1:38)]
files
  

#stack rasters
s1<- stack()
for (j in 1:length(files)){
  s1<- stack(s1, raster(files[j]))}
names(s1)
  
# other covariates (categorical in nature)
nm2<- paste(dpw,sfol,sep= "")
r4<- raster(paste(nm2,"/Relief_geomorphons.tif", sep=""))
  
# stack the raster set
s3<- stack(s1,r4)
names(s3)== names(pr_comp$center)
    
# APPLY PCA MODEL
nm3<- paste0(root.tiles, sfol, "/d", sel.depth, "/", vart, "/pca/")
  
#prediction
pred<- predict(object = s3, model = pr_comp, index=1:pc)
#write rasters to file
for (k in 1:nlayers(pred)){
  r1<- pred[[k]]
  names(r1)<- paste0("PC", k)
  nm5<- paste0(nm3,names(r1),".tif")
  writeRaster(r1, filename = nm5, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)}
  
  ## slurm file output
  itOuts<- c(runs,as.character(Sys.time()))
  nmz<- paste0(root.slurm, "slurmckeck_pca_d",sel.depth,"_", runs, "_",sfol, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
  

# END
  
  
                    
