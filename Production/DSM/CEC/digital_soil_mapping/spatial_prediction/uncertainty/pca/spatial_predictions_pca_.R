### TERN LANDSCAPES 
# Soil CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 19.1.22
# modified: 31.1.22

# CODE PURPOSE
# Uncertainty analysis steps: PCA


library(parallel);library(sp);library(rgdal);library(doParallel);library(raster)

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/"
root.tiles<- paste0(g.root, "/predictions/90m/tiles/cec/")
root.slurm<- paste0(g.root,"/rcode/digital_soil_mapping/spatial_prediction/uncertainty/slurm/pca/")
model.root<- paste0(g.root,"models/cec/")
cluster.out<- paste0(g.root,"models/cec/cluster_centroids/")

### Folders where the covariates are
dpw<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/90m/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
length(fols)
fols<- fols[order(fols)]
length(fols)

# tile run parameters
x1<-ceiling(seq(1,2150,by = 50))
x2<- c(x1[-1] -1,length(fols))
x3<- 1:length(x1)
x1;x2;x3
runs<- 1
sel.depth<- 1

srt<- x1[runs]
fin<- x2[runs]
batch<- x3[runs]
srt;fin


## necessary inputs
# PCA object for converting covariates to PC variables
pr_comp<- readRDS(file = paste0(model.root,"PCA_data_oob_cec_modelPreds_all_updated.rds"))
names(pr_comp$center)
pc<- 32

# cluster centers of optimal config
#cluster.centre<- readRDS(file = paste0(cluster.out,"101_cluster_center.rds"))
#cluster.centre

# model errors distribution per cluster
#cluster.errors<- readRDS(file = paste0(cluster.out, "cluster_errors/101_cluster_errors_cluster.rds"))
#cluster.errors

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
  names(s3)== names(pr_comp$center)
    
  # APPLY PCA MODEL
  nm3<- paste0(root.tiles, sfol, "/d", sel.depth, "/")
  
  #prediction
  pred<- predict(object = s3, model = pr_comp, index=1:pc)
  #write rasters to file
  for (k in 1:nlayers(pred)){
    r1<- pred[[k]]
    names(r1)<- paste0("PC", k)
    nm5<- paste0(nm3,names(r1),".tif")
    writeRaster(r1, filename = nm5, format = "GTiff", datatype = "FLT4S", overwrite = TRUE)}
  
  ## slurm file output
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0(root.slurm, "checks/d",sel.depth,"/slurmckeck_pca_d",sel.depth,"_", i, "_",sfol, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
  
  }
  
  
                    
