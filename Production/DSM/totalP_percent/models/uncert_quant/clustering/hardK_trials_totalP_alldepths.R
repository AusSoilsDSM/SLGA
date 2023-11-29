### TERN Landscapes 
# total P
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 21.11.23
# modified: 21.11.23

# CODE PURPOSE
# Hard kmeans clustering of the covariates for the later intention of quantifying prediction uncertainties
# Look to 1 to 500 with 25 step interval

options(warn=0)
depth<- "alldepth"

# roots
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_P/"
data.root<- paste0(g.root, "data/ross/model_ready/")
cluster.out<- paste0(g.root,"models/uncertainty/cluster_centroids/")

#data
# site data
site.dat<- readRDS(paste0(data.root,"tern_totalP_labelled_covariates_CAL.rds"))
names(site.dat)



# cluster numbers
clus.nos<- seq(from =1, to = 501,by=25)
clus.nos
cluster.matrix<- matrix(NA, nrow = nrow(site.dat),ncol = length(clus.nos))
dim(cluster.matrix)

# covariate data
names(site.dat)
covs.data<- site.dat[,6:45]
names(covs.data)

# estimate variance covariance matrix
varcov.mat<- cov(covs.data)
# save matrix
saveRDS(object = varcov.mat, file = paste0(data.root,"tern_totalP_covariates_varcov_matrix_CAL.rds"))


# loop through and do kmeans with each different cluster number
for (i in 1:length(clus.nos)){
  cluster.dat<- kmeans(x = covs.data, centers = clus.nos[i], iter.max = 100000, nstart = 10)
  cluster.matrix[,i]<- as.matrix(cluster.dat$cluster)
  cluster.center<- cluster.dat$centers
  # save cluster center
  saveRDS(object = cluster.center, file = paste0(cluster.out,i,"_",depth,"_cluster_center.rds"))
  print(i)}

cluster.matrix<- as.data.frame(cluster.matrix)


## join up with labels
names(site.dat)
label.dat<- site.dat[,1:5]
out.dat<- cbind(label.dat,cluster.matrix)

# save data
saveRDS(object = out.dat, file = paste0(cluster.out,"cluster_centers_totalP_all.rds"))
