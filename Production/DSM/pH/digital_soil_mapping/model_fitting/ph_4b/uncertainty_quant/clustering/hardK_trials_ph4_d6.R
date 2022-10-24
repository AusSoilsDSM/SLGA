### TERN LANDSCAPES 
# Soil CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 8.9.22
# modified: 8.9.22

# CODE PURPOSE
# Hard kmeans clustering of the covariates for the later intention of quantifying prediciton uncertainties
# Look to 1 to 250

depth<- "d6"

# roots
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
input.root<- paste0(g.root,"models/ph_4b/",depth, "/data_obs_preds/val/")
data.root<- paste0(g.root, "data/curated_all/")
dists.root<- paste0(g.root, "data/field_2_4B_dists/")
cluster.out<- paste0(g.root,"models/ph_4b/",depth, "/uncertainty/cluster_centroids/")
data.out<- paste0(g.root,"models/ph_4b/",depth, "/uncertainty/cluster_centroids/")

#data
# site data
site.dat<- readRDS(paste0(data.root,"tern_soilpH4B_siteDat_covariates_20223008_CALVALDAT_ARD.rds"))
new.col.names<- substr(x = names(site.dat)[17:54],start = 1, stop = nchar(names(site.dat)[17:54])-4)
new.col.names
names(site.dat)[17:54]<- new.col.names
names(site.dat)[1:10]
lns<- length(which(site.dat[,14] < 0))
lns
if (lns != 0){site.dat<- site.dat[-which(site.dat[,14] < 0),]}
#site.dat$Relief_geomorphons<- as.factor(site.dat$Relief_geomorphons)
site.dat[c(1,10,55,500),1:10]


input.data<- read.table(file = paste0(input.root, "ranger_VAL_preds_average_summaries_pH4b_depth_",depth,".txt"),header = T, sep = ",")
input.data[c(1,10,55,500),1:10]

names(site.dat)
input.data<- cbind(input.data,site.dat[,17:55])


# cluster numbers
clus.nos<- c(1:250)
clus.nos
cluster.matrix<- matrix(NA, nrow = nrow(input.data),ncol = length(clus.nos))
dim(cluster.matrix)

# covariate data
names(input.data)
covs.data<- input.data[,15:53]
# PCA analysis
pr_spectra<-prcomp(covs.data, center = T, scale. = T)
cum_props<- c(summary(pr_spectra)[[6]][3,]) 
props<- which(cum_props>= 0.975)[1]
props ## 32 is the optimal number
# scale the data
scaled.covs.data<-  pr_spectra$x[,1:props] 
dim(scaled.covs.data)
# save the pca object
saveRDS(object = pr_spectra, file = paste0(data.out,"PCA_data_oob_ph4b_covariates_",depth,".rds"))

# loop through and do kmeans with each different cluster number
for (i in 1:length(clus.nos)){
  cluster.dat<- kmeans(x = scaled.covs.data, centers = clus.nos[i], iter.max = 5000, nstart = 25)
  cluster.matrix[,i]<- as.matrix(cluster.dat$cluster)
  cluster.center<- cluster.dat$centers
  # save cluster center
  saveRDS(object = cluster.center, file = paste0(cluster.out,i,"_",depth,"_cluster_center.rds"))
  print(i)}

cluster.matrix<- as.data.frame(cluster.matrix)
summary(as.factor(cluster.matrix[,105]))

## join up with labels
out.dat<- cbind(input.data,cluster.matrix)

# save data
saveRDS(object = out.dat, file = paste0(cluster.out,"cluster_centers_oob_ph4b_modelPreds_all.rds"))
