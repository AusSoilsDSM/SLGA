### TERN LANDSCAPES 
# total N
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 15.11.23
# modified: 15.11.23

# CODE PURPOSE
# Use model geom object to estimate convex hull and coobs metrics for the selected 500K samples


# libraries
library(terra);library(sf)

## HPC stuff
args = commandArgs(trailingOnly = T)
sel.depth<- as.numeric(args[1])
runs<- as.numeric(args[2])


# fixed parameters
vart<- "totalN"
depth<- sel.depth
obs<- 9175

### root directories
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_N/"
root.models<- paste0(g.root,"models/geoms/")
root.slurm<- paste0(g.root,"rcode/slurm/outs/digital_soil_mapping/coobs/d",depth, "/")
func.root<- paste0(g.root, "rcode/miscell/")
data.out<- paste0(g.root, "data/coobs/d",depth,"/")
data.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/covariates/90m_covariates/data/"

# data file
data.files<- list.files(path = data.root, pattern = "block", full.names = T)
data.files


### geom models
all.data.model<- readRDS(file =paste0(root.models, vart, "_d",depth,"_covdat_hull_object_ALLDAT.rds"))
all.data.model$max_dist
hist(all.data.model$max_dist_vector)
min(all.data.model$max_dist_vector);max(all.data.model$max_dist_vector)
max.dist<- all.data.model$max_dist
min.dist<- 0
max.dist<- 585 # hard coded (based on population data)

in.data<- readRDS(file = data.files[runs])
#in.data<- in.data[1:500,]
  
## Add some columns
in.data$alldat_geom<-NA
in.data$alldat_sim99_prop<-NA
in.data$alldat_sim95_prop<-NA
in.data$alldat_sim90_prop<-NA
in.data$alldat_sim80_prob<-NA
  
in.data$alldat_sim99_val<-NA
in.data$alldat_sim95_val<-NA
in.data$alldat_sim90_val<-NA
in.data$alldat_sim80_val<-NA

in.data$rawdist<-NA
  
  
# check for name matchings
names(in.data)[2:40]
names(all.data.model$PCA_object$center)
names(all.data.model$PCA_object$center)==names(in.data)[2:40]
  

####################################################################################################
# PCA object [all data]
pc.object<- all.data.model$PCA_object
## Project foreign spectra onto model spectra space
PCA_projection <- predict(pc.object, in.data[,c(2:40)])
# strip down to the required dimensions
PCA_projection<- PCA_projection[,1:ncol(all.data.model$selected_PCA_scores)]
  
# unique combinations object
combos_obj<- all.data.model$uniq_combos
#all.data.model$convex_hulls
  
### Estimate whether the spectra fit in the polygon objects
empty.mat<- matrix(NA,nrow=nrow(PCA_projection), ncol= nrow(combos_obj))
for (j in 1:length(all.data.model$convex_hulls)){
  # extract out the required objects
  tri_obj<- all.data.model$convex_hulls[[j]][[1]]
  poly_obj<-all.data.model$convex_hulls[[j]][[2]]
  newScores = data.frame(lab= 1,x=PCA_projection[,combos_obj[j,1]],y=PCA_projection[,combos_obj[j,2]])
  newScores_sf<- sf::st_as_sf(x= newScores, coords = c("x", "y"))
    
  # plot the polygon and all points to be checked
  #par(mfrow=c(1,1))
  #plot(newScores[,2:3],xlab= paste0("PCA_",combos_obj[i,1]), ylab= paste0("PCA_",combos_obj[i,2]), 
  #  xlim=c(min(tri_obj[,1:2]), max(tri_obj[,1:2])),
  #  ylim=c(min(tri_obj[,1:2]), max(tri_obj[,1:2])),
  #  col='black', main='Geometric points-in-polygon', cex=0.5)
  #  polygon(tri_obj)
    
  #create check which points fall within the polygon
  specMatch<- sf::st_within(newScores_sf,poly_obj,sparse = F)
  rmss<- which(specMatch == FALSE)
  #points(newScores[rmss,2:3],pch='X',col='red')
  matches<- as.matrix(as.numeric(specMatch))
  empty.mat[,j]<- matches
  #print(j)
  }

empty.mat<- as.data.frame(empty.mat)
empty.mat[is.na(empty.mat)] <- 0
polygon_assess<- rowSums(empty.mat)/ncol(empty.mat)
polygon_assess<- as.numeric(format(round(polygon_assess, 2), nsmall = 2))
in.data$alldat_geom<- polygon_assess
  
### all data COOBS work
  
#covariance matrix
covmat<- as.matrix(cov(all.data.model$selected_PCA_scores))
  
for (k in 1:nrow(in.data)){
  # data distance
  datDist<- mahalanobis(x = all.data.model$selected_PCA_scores, center = as.matrix(PCA_projection[k,]), cov = covmat) 
  mean(datDist)
    
  # normalise
  datNdist<- (datDist - min.dist)/(max.dist - min.dist) # standardise
  datNdist[datNdist > 1] <- 1 #if the datNdist is greater than 1 that means it datDist is greater than maxDist ##HACK
  datNdist <- 1- datNdist  # Higher values mean more similar
  #hist(datNdist)
    
  #count how many obs are above a given threshold
  in.data$rawdist[k]<- mean(datDist)
  in.data$alldat_sim99_prop[k] <- sum(datNdist >= 0.99)/obs
  in.data$alldat_sim95_prop[k] <- sum(datNdist >= 0.95)/obs
  in.data$alldat_sim90_prop[k] <- sum(datNdist >= 0.90)/obs
  in.data$alldat_sim80_prob[k] <- sum(datNdist >= 0.80)/obs
    
  in.data$alldat_sim99_val[k] <- sum(datNdist >= 0.99)
  in.data$alldat_sim95_val[k] <- sum(datNdist >= 0.95)
  in.data$alldat_sim90_val[k] <- sum(datNdist >= 0.90)
  in.data$alldat_sim80_val[k] <- sum(datNdist >= 0.80)}

# save file
saveRDS(object = in.data,file = paste0(data.out, "pca_sites_covariates_90m_stack_wgs_geomwork_",vart,"_d",depth,"_block_",runs,".rds"))

# slurm SoL  
itOuts<- c(runs,as.character(Sys.time()))
  nmz<- paste0(root.slurm, "slurmckeck_block_",runs, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")



# END
  
  
  
  