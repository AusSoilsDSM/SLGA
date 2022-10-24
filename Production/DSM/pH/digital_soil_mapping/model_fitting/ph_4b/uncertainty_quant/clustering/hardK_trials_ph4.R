### TERN LANDSCAPES 
# Soil CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 15.12.21
# modified: 15.12.21

# CODE PURPOSE
# Hard kmeans clustering of the covariates for the later intention of quantifying prediciton uncertainties
# Look to up to 100 classes. And run one with 250, 500, 750 and 1000 classes


# roots
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/"
input.root<- paste0(g.root,"models/cec/goofs/")
cluster.out<- paste0(g.root,"models/cec/cluster_centroids/")
data.out<- paste0(g.root,"models/cec/")

input.data<- readRDS(file = paste0(input.root, "oob_cec_modelPreds_all.rds"))


# cluster numbers
clus.nos<- c(1:100,200,250,500,750,1000)
clus.nos
cluster.matrix<- matrix(NA, nrow = nrow(input.data),ncol = length(clus.nos))


# covariate data
names(input.data)
covs.data<- input.data[,21:60]
# PCA analysis
pr_spectra<-prcomp(covs.data, center = T, scale. = T)
cum_props<- c(summary(pr_spectra)[[6]][3,]) 
props<- which(cum_props>= 0.975)[1]
props ## 32 is the optimal number
# scale the data
scaled.covs.data<-  pr_spectra$x[,1:props] 
dim(scaled.covs.data)
# save the pca object
saveRDS(object = pr_spectra, file = paste0(data.out,"PCA_data_oob_cec_modelPreds_all_updated.rds"))

# loop through and do kmeans with each different cluster number
for (i in 1:length(clus.nos)){
  cluster.dat<- kmeans(x = scaled.covs.data, centers = clus.nos[i], iter.max = 5000, nstart = 25)
  cluster.matrix[,i]<- as.matrix(cluster.dat$cluster)
  cluster.center<- cluster.dat$centers
  # save cluster center
  saveRDS(object = cluster.center, file = paste0(cluster.out,i,"_cluster_center.rds"))
  print(i)}

cluster.matrix<- as.data.frame(cluster.matrix)
summary(as.factor(cluster.matrix[,105]))

## join up with labels
out.dat<- cbind(input.data,cluster.matrix)

# save data
saveRDS(object = out.dat, file = paste0(cluster.out,"summaries/cluster_centers_oob_cec_modelPreds_all.rds"))
