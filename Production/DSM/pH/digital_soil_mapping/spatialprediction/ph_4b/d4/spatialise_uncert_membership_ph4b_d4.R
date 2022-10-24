### TERN LANDSCAPES 
# Soil pH
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 7.10.22
# modified: 14.10.22

# CODE PURPOSE
# Uncertainty analysis steps: estimation of cluster memberships from PCA outputs, then estimation of empirical errors, then derivations of upper and lower PIs [90% confidence]


library(sp);library(rgdal);library(raster)

### variables
args = commandArgs(trailingOnly = T)
sel.depth<- as.numeric(args[1])
runs<- as.numeric(args[2])

#sel.depth=2
#runs= 1

### variables
vart<- "ph_4b"

## fuzzy allocation function
source("/datasets/work/af-tern-mal-deb/work/projects/natsoil_monitoring/rcode/fuzme/fuzall.R")

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
root.tiles<- paste0(g.root,"/predictions/tiles/")
root.slurm<- paste0(g.root,"/rcode/slurm/",vart,"/temp_outs/digital_soil_mapping/spatialprediction/uncert_membership/d",sel.depth,"/")
cluster.out<- paste0(g.root,"models/",vart,"/d",sel.depth,"/uncertainty/cluster_centroids/")
data.root<- paste0(g.root,"models/",vart,"/d",sel.depth,"/data_obs_preds/ext/")

### Folders where the covariates are
fols<- as.numeric(list.files(root.tiles, full.names = FALSE))
fols<- fols[order(fols)]
length(fols)
## tiles to actually focus on
missing.tiles<- readRDS(file = paste0(g.root, "data/miscells/tilemopups/",vart,"/tilemopup_uncert_membership_",vart,"_d",sel.depth,".rds"))
missing.tiles
fols<- fols[missing.tiles]
fols
length(fols)

## necessary inputs
# PCA object for converting covariates to PC variables
#pr_comp<- readRDS(file = paste0(model.root,"PCA_data_oob_cec_modelPreds_all.rds"))
#names(pr_comp$center)
#pc<- 38

# cluster files
cluster.files<- list.files(path = cluster.out, pattern = "cluster_center",full.names = T, recursive = F)
cluster.files
cluster.files<- cluster.files[1:250]

# cluster error files
error.files<- list.files(path = paste0(cluster.out, "cluster_errors/"), pattern = "cluster_errors_cluster", full.names = T, recursive = F)
error.files


# select the appropriate settings
cluster.files<- cluster.files[94]
error.files<- error.files[94]

# load cluster and error files
cluster.data<- readRDS(cluster.files)
error.data<- readRDS(error.files)

pqants<- c(0.005,0.0125,0.025,0.05,0.1,0.2,0.3,0.4,0.45,0.475,
           0.5250,0.5500,0.6000,0.7000,0.8000,0.9000,0.9500,0.9750,0.9875,0.9950)
length(pqants)
probs.check<- pqants[20:11] - pqants[1:10]
probs.check

# get the errors for the lower 5th and upper 95th quantile
names(error.data)
sel.lower<- error.data[,5]
sel.upper<- error.data[,18]

# pca data covariance matirx
cov.matrix<- readRDS(file = paste0(data.root, "PCA_covariance_matrix_d",sel.depth,"_uncertanal.rds" ))

# fuzzy clustering inputs
phi<- 1.1
W<- cov.matrix
distype<- 3 # mahalanobis
km.centroids<-cluster.data


## START THE WORK
#select the folder
sfol<- fols[runs]
nm1<- paste0(root.tiles, sfol, "/d", sel.depth, "/", vart, "/pca/")
  
## get the covariates
files<- list.files(path = nm1,  pattern="PC", full.names=TRUE, recursive = F)
files
order.right<- c(1,12,23,28:33,2:11,13:22,24:27)
files<- files[order.right]
files
  
#stack rasters
s1<- stack(files)
#s1
names(s1)
  
# load the prediction raster
nmz<- paste0(root.tiles, sfol, "/d", sel.depth, "/", vart, "/")
pred.files<- list.files(path = nmz,  pattern=paste0("RKpred_", vart, "_mean_d",sel.depth), full.names=TRUE, recursive = F)
r1<- raster(pred.files)
r1

# establish rasters to deposit lower and upper PIs
lower_PI<- raster(s1[[1]])
upper_PI<- raster(s1[[1]])
lower_PI[] <-NA
upper_PI[]<- NA
nm4<- paste0(nmz, "RKpred_", vart, "_lowerPI_d",sel.depth,".tif")
nm5<- paste0(nmz, "RKpred_", vart, "_upperPI_d",sel.depth,".tif")
  
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
      upper_PI<- writeValues(upper_PI,a.matrix[,2],j)
      }
    }

lower_PI<- writeStop(lower_PI)
upper_PI<- writeStop(upper_PI) 
  
# tidy up: lower
# Masking 
real.format_lower<- mask(lower_PI,r1)
# rounding
real.format_lower.round<- round(real.format_lower, digits = 2)
writeRaster(x = real.format_lower.round, filename = nm4,format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  
# tidy up: upper
# Masking 
real.format_upper<- mask(upper_PI,r1)
# rounding
real.format_upper.round<- round(real.format_upper, digits = 2)
writeRaster(x = real.format_upper.round, filename = nm5,format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  
  
# prediction interval range
real.format_range<- real.format_upper.round - real.format_lower.round
real.format_range.round<- round(real.format_range, digits = 2)
nm8<- paste0(nmz, "RKpred_", vart, "_range_d",sel.depth,".tif")
writeRaster(x = real.format_range.round, filename = nm8,format = "GTiff",datatype = "FLT4S", overwrite = TRUE)
  
# unlink the pca rasters
#unlink(files)
  
## slurm outputs [no mopups]
#itOuts<- c(runs,as.character(Sys.time()))
#nmy<- paste0(root.slurm,"slurmckeck_memberships_d",sel.depth,"_", runs, "_",sfol, ".txt")
#write.table(itOuts, file = nmy, row.names = F, col.names = F, sep=",")

## slurm outputs [mopups]
itOuts<- c(missing.tiles[runs],as.character(Sys.time()))
nmy<- paste0(root.slurm,"slurmckeck_memberships_d",sel.depth,"_", missing.tiles[runs], "_",sfol, ".txt")
write.table(itOuts, file = nmy, row.names = F, col.names = F, sep=",")

# END
                    
