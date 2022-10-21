### TERN LANDSCAPES 
# Soil CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 25.1.22
# modified: 9.2.22

# CODE PURPOSE
# Uncertainty analysis steps: estimation of cluster memberships from PCA outputs, then estimation of empirical errors, then derivations of upper and lower PIs [90% confidence]


library(parallel);library(sp);library(rgdal);library(doParallel);library(raster)

## fuzzy allocation function
source("/datasets/work/af-tern-mal-deb/work/projects/natsoil_monitoring/rcode/fuzme/fuzall.R")

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/"
root.tiles<- paste0(g.root, "/predictions/90m/tiles/cec/")
root.slurm<- paste0(g.root,"/rcode/digital_soil_mapping/spatial_prediction/uncertainty/slurm/memberships_errors/")
model.root<- paste0(g.root,"models/cec/")
cluster.out<- paste0(g.root,"models/cec/cluster_centroids/")

### Folders where the covariates are
fols<- as.numeric(list.files(root.tiles, full.names = FALSE))
fols<- fols[order(fols)]
length(fols)

# tile run parameters
x1<-ceiling(seq(1,2150,by = 12))
x2<- c(x1[-1] -1,length(fols))
x3<- 1:length(x1)
length(x3)
x1;x2;x3
runs<- 1
sel.depth<- 1

srt<- x1[runs]
fin<- x2[runs]
batch<- x3[runs]
srt;fin


## necessary inputs
# PCA object for converting covariates to PC variables
#pr_comp<- readRDS(file = paste0(model.root,"PCA_data_oob_cec_modelPreds_all.rds"))
#names(pr_comp$center)
#pc<- 38

# cluster centers of optimal config
cluster.centre<- readRDS(file = paste0(cluster.out,"93_cluster_center.rds"))
cluster.centre

# model errors distribution per cluster
cluster.errors<- readRDS(file = paste0(cluster.out, "cluster_errors/93_cluster_errors_cluster.rds"))
cluster.errors

pqants<- c(0.005,0.0125,0.025,0.05,0.1,0.2,0.3,0.4,0.45,0.475,
           0.5250,0.5500,0.6000,0.7000,0.8000,0.9000,0.9500,0.9750,0.9875,0.9950)
length(pqants)
probs.check<- pqants[20:11] - pqants[1:10]
probs.check
# get the errors for the lower 5th and upper 95th quantile
names(cluster.errors)
sel.lower<- cluster.errors[,5]
sel.upper<- cluster.errors[,18]

# pca data covariance matirx
cov.matrix<- readRDS(file = paste0(cluster.out, "PCA_covariance_matrix.rds"))

# fuzzy clustering inputs
phi<- 1.1
W<- cov.matrix
distype<- 3 # mahalanobis
km.centroids<-cluster.centre

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
  nm1<- paste0(root.tiles,sfol, "/d", sel.depth, "/")
  
  ## get the covariates
  files<- list.files(path = nm1,  pattern="PC", full.names=TRUE, recursive = F)
  order.right<- c(1,12,23,27:32,2:11,13:22,24:26)
  files<- files[order.right]
  files
  
  #stack rasters
  s1<- stack(files)
  s1
  names(s1)
  
  # load the prediction raster
  pred.files<- list.files(path = nm1,  pattern=paste0("rkpred_median_d",sel.depth), full.names=TRUE, recursive = F)
  r1<- raster(pred.files)
  
  
  # establish rasters to deposit lower and upper PIs
  lower_PI<- raster(s1[[1]])
  upper_PI<- raster(s1[[1]])
  lower_PI[] <-NA
  upper_PI[]<- NA
  nm3<- paste0(root.tiles, sfol, "/d", sel.depth, "/")
  nm4<- paste0(nm3, "rkpred_lowerPI_d",sel.depth,".tif")
  nm5<- paste0(nm3, "rkpred_upperPI_d",sel.depth,".tif")
  
  lower_PI<- writeStart(lower_PI,filename= nm4, format='GTiff',datatype = "FLT4S", overwrite=TRUE)
  upper_PI<- writeStart(lower_PI,filename= nm5, format='GTiff',datatype = "FLT4S", overwrite=TRUE)
  

  
  ## line by line work
  for(j in 1:dim(lower_PI)[1]){
    cov.Frame<- getValues(s1,j) 
    pred.frame<- getValues(r1,j)
    len.com<- length(which(complete.cases(cov.Frame)))
    if (len.com == 0){ # if a complete line is full of NAs
      a.matrix<<- matrix(NA, nrow=nrow(cov.Frame), ncol= 2)
      # write to raster
      lower_PI<- writeValues(lower_PI,a.matrix[,1],j)
      upper_PI<- writeValues(upper_PI,a.matrix[,2],j)
      next} else {
        complete.indexes<- which(complete.cases(cov.Frame))
        if (len.com == 1){
          sub.frame<- as.data.frame(t(cov.Frame[complete.indexes,]))
          sub.predframe<- pred.frame[complete.indexes] } else {
            sub.frame<- as.data.frame(cov.Frame[complete.indexes,])
            sub.predframe<- pred.frame[complete.indexes]}
        # calculate the fuzzy memberships
        datAll<- fuzall(data = sub.frame,phi = phi,centroid = km.centroids,distype = distype,W = W)
        memberships<- as.data.frame(datAll$membership)
        #max.member.class<- apply(memberships,1,which.max)
        #max.member <- apply(memberships, 1, max)  
        
        # errors
        avgLower_errors<- as.matrix(memberships) %*% sel.lower
        avgUpper_errors<- as.matrix(memberships) %*% sel.upper
        
        # add errors to predictions
        pred_lower<- avgLower_errors + sub.predframe
        pred_upper<- avgUpper_errors + sub.predframe
        
        # prepare matrix
        a.matrix<- matrix(NA, nrow=nrow(cov.Frame), ncol= 2)
        a.matrix[complete.indexes,1]<- pred_lower
        a.matrix[complete.indexes,2]<- pred_upper
        lower_PI<- writeValues(lower_PI,a.matrix[,1],j)
        upper_PI<- writeValues(upper_PI,a.matrix[,2],j)}
    }
  
  lower_PI<- writeStop(lower_PI)
  upper_PI<- writeStop(upper_PI) 
  
  # back transformation of upper and lower predictions
  # back transform
  nm6<- paste0(nm3, "rkpred_lowerPI_BT_d",sel.depth,".tif")
  nm7<- paste0(nm3, "rkpred_upperPI_BT_d",sel.depth,".tif")
  real.format_lower<- exp(lower_PI) - 0.1
  # mask out to prediction raster
  real.format_lower.x<- mask(real.format_lower,r1)
  writeRaster(x = real.format_lower.x, filename = nm6,format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  
  # mask out to prediction raster
  real.format_upper<- exp(upper_PI) - 0.1
  real.format_upper.x<- mask(real.format_upper,r1)
  writeRaster(x = real.format_upper.x, filename = nm7,format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  
  # prediction interval range
  real.format_range<- real.format_upper.x - real.format_lower.x
  nm8<- paste0(nm3, "rkpred_rangePI_BT_d",sel.depth,".tif")
  writeRaster(x = real.format_range, filename = nm8,format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  
  # unlink the pca rasters
  #unlink(files)
  
  ## slurm file output
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0(root.slurm, "checks/d",sel.depth,"/slurmckeck_memberships_d",sel.depth,"_", i, "_",sfol, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
  }
  
  
                    
