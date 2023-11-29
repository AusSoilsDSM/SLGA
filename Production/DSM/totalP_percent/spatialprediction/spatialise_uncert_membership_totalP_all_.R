### TERN LANDSCAPES 
# total_P
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 23.11.23
# modified: 23.11.23

# CODE PURPOSE
# Uncertainty analysis steps: estimation of cluster memberships from covariate outputs, then estimation of empirical errors, then derivations of upper and lower PIs [90% confidence]


library(sf);library(terra)

### variables
args = commandArgs(trailingOnly = T)
sel.depth<- as.numeric(args[1])
runs<- as.numeric(args[2])

### variables
# cluster numbers
clus.nos<- seq(from =1, to = 501,by=25)
clus.nos
sel.clus<- which(clus.nos== 101) # change accordingly
sel.clus
vart<- "totalP"

## fuzzy allocation function
source("/datasets/work/af-tern-mal-deb/work/projects/natsoil_monitoring/rcode/fuzme/fuzall.R")

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_P/"
root.tiles<- paste0(g.root,"/predictions/tiles/")
root.slurm<- paste0(g.root,"rcode/slurm/outs/digital_soil_mapping/spatialprediction/uncert/d",sel.depth,"/")
cluster.out<- paste0(g.root,"models/uncertainty/cluster_centroids/")
data.root<- paste0(g.root,"models/ranger_models/data_obs_preds/ext/")

dpw<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/90m/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
fols<- fols[order(fols)]
fols
length(fols)

## tiles to actually focus on
#missing.tiles<- readRDS(file = paste0(g.root, "data/miscells/tilemopups/",vart,"/tilemopup_uncert_membership_",vart,"_d",sel.depth,".rds"))
#missing.tiles
#fols<- fols[missing.tiles]
#fols
#length(fols)

# cluster files
cluster.files<- list.files(path = cluster.out, pattern = "alldepth",full.names = F, recursive = F)
cluster.files
rd1<- strsplit(cluster.files,"_")
rd1<- as.numeric(sapply(rd1, `[`, 1))
rd1<- order(rd1)
cluster.files<- list.files(path = cluster.out, pattern = "alldepth",full.names = T, recursive = F)
cluster.files<- cluster.files[rd1]
cluster.files

# cluster error files
error.files<- list.files(path = paste0(cluster.out, "cluster_errors/"), pattern = "cluster_errors_cluster", full.names = T, recursive = F)
error.files<- error.files[rd1]
error.files


# select the appropriate settings
cluster.files<- cluster.files[sel.clus]
error.files<- error.files[sel.clus]

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

# data covariance matrix
cov.matrix<- readRDS(file = paste0(g.root,"data/ross/model_ready/tern_",vart,"_covariates_varcov_matrix_CAL.rds"))

# fuzzy clustering inputs
phi<- 1.1
W<- cov.matrix
distype<- 3 # mahalanobis
km.centroids<-cluster.data


## START THE WORK
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

# check names matches
names(as.data.frame(cluster.data)) == names(s3)


# load the prediction raster
nmz<- paste0(root.tiles, sfol, "/d", sel.depth, "/")
pred.files<- list.files(path = nmz,  pattern="modpred_median", full.names=TRUE, recursive = F)
r1<- terra::rast(pred.files)
r1

# establish rasters to deposit lower and upper PIs
lower_PI<- terra::rast(s1[[1]])
upper_PI<- terra::rast(s1[[1]])
lower_PI[] <-NA
upper_PI[]<- NA
nm4<- paste0(nmz, "modpred_clustering_lowerPI_d",sel.depth,".tif")
nm5<- paste0(nmz, "modpred_clustering_upperPI_d",sel.depth,".tif")
  
terra::writeStart(lower_PI,filename= nm4,datatype = "FLT4S", overwrite=TRUE)
terra::writeStart(upper_PI,filename= nm5,datatype = "FLT4S", overwrite=TRUE)
  
## line by line work
for(j in 1:nrow(lower_PI)){
  cov.Frame<- s3[j,]
  pred.frame<- r1[j,]
  len.com<- length(which(complete.cases(cov.Frame)))
  if (len.com == 0){ # if a complete line is full of NAs
    a.matrix<<- matrix(NA, nrow=nrow(cov.Frame), ncol= 2)
    # write to raster
    terra::writeValues(lower_PI,a.matrix[,1],start = j, nrows = 1)
    terra::writeValues(upper_PI,a.matrix[,2],start = j, nrows = 1)
    next} else {
      complete.indexes<- which(complete.cases(cov.Frame))
      if (len.com == 1){
        sub.frame<- cov.Frame[complete.indexes,]
        sub.predframe<- pred.frame[complete.indexes,1] } else {
          sub.frame<- as.data.frame(cov.Frame[complete.indexes,])
          sub.predframe<- pred.frame[complete.indexes,1]
          }
      # calculate the fuzzy memberships
      datAll<- fuzall(data = as.matrix(sub.frame),phi = phi,centroid = km.centroids,distype = distype,W = W)
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
      terra::writeValues(lower_PI,a.matrix[,1],start = j, nrows = 1)
      terra::writeValues(upper_PI,a.matrix[,2],start = j, nrows = 1)
      }
    }

terra::writeStop(lower_PI)
terra::writeStop(upper_PI) 
  
# tidy up: lower
# rounding
real.format_lower.round<- round(lower_PI, digits = 2)
names(real.format_lower.round)<- "lower_PI"
terra::writeRaster(x = real.format_lower.round, filename = nm4,datatype = "FLT4S", overwrite = TRUE)
  
# tidy up: upper
# rounding
real.format_upper.round<- round(upper_PI, digits = 2)
names(real.format_upper.round)<- "upper_PI"
terra::writeRaster(x = real.format_upper.round, filename = nm5,datatype = "FLT4S", overwrite = TRUE)
  
  
# prediction interval range
real.format_range<- real.format_upper.round - real.format_lower.round
real.format_range.round<- round(real.format_range, digits = 2)
nm8<- paste0(nmz, "modpred_clustering_PIrange_d",sel.depth,".tif")
names(real.format_range.round)<- "pred_range"
writeRaster(x = real.format_range.round, filename = nm8,datatype = "FLT4S", overwrite = TRUE)
  
## slurm outputs [no mopups]
itOuts<- c(runs,as.character(Sys.time()))
nmy<- paste0(root.slurm,"slurmckeck_d",sel.depth,"_", runs, "_",sfol, ".txt")
write.table(itOuts, file = nmy, row.names = F, col.names = F, sep=",")

## slurm outputs [mopups]
#itOuts<- c(missing.tiles[runs],as.character(Sys.time()))
#nmy<- paste0(root.slurm,"slurmckeck_d",sel.depth,"_", missing.tiles[runs], "_",sfol, ".txt")
#write.table(itOuts, file = nmy, row.names = F, col.names = F, sep=",")

  
# END
                    
