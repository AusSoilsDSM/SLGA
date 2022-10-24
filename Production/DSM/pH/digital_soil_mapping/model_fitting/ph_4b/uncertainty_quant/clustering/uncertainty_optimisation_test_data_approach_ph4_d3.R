### TERN LANDSCAPES 
# Soil pH
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 14.9.22
# modified: 14.9.22

# CODE PURPOSE
# Test data uncertainty quantification and optimization

library(MASS)

#fuzzy allocation function
source("/datasets/work/af-tern-mal-deb/work/projects/natsoil_monitoring/rcode/fuzme/fuzall.R")
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/miscell/goof.R")

depth<- "d3"

# roots
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
input.root<- paste0(g.root,"models/ph_4b/",depth,"/data_obs_preds/ext/")
cluster.out<- paste0(g.root,"models/ph_4b/",depth,"/uncertainty/cluster_centroids/")
data.outs<- paste0(g.root,"outs/4B_meth/external_evaluation/")


input.data<- read.table(file = paste0(input.root,"ranger_EXT_preds_pH4b_depth_",depth,"_summary.txt"),header = T, sep = ",")
names(input.data)
ext.covariate.dat<- readRDS(file = paste0(input.root,"ranger_EXT_covariates_pH4b_depth_",depth,".rds"))
names(ext.covariate.dat)
new.col.names<- substr(x = names(ext.covariate.dat)[12:49],start = 1, stop = nchar(names(ext.covariate.dat)[12:49])-4)
names(ext.covariate.dat)[12:49]<- new.col.names

# PCA object (33 comps)
pr_comp<- readRDS(file = paste0(cluster.out,"PCA_data_oob_ph4b_covariates_",depth,".rds"))


# cluster files
cluster.files<- list.files(path = cluster.out, pattern = "cluster_center",full.names = T, recursive = F)
cluster.files
cluster.files<- cluster.files[1:250]

# cluster error files
error.files<- list.files(path = paste0(cluster.out, "cluster_errors/"), pattern = "cluster_errors_cluster", full.names = T, recursive = F)
error.files

# summary output
summary.mat<- matrix(NA, nrow= length(cluster.files), ncol =4)


# cluster data
names(ext.covariate.dat)
cluster.data<- ext.covariate.dat[,12:50]
names(cluster.data)
# scale the data
names(pr_comp$center) == names(cluster.data)
scaled.covs.data<- predict(pr_comp, newdata=cluster.data)[,1:33]
dim(scaled.covs.data)

# error quantiles
pqants<- c(0.005,0.0125,0.025,0.05,0.1,0.2,0.3,0.4,0.45,0.475,
           0.5250,0.5500,0.6000,0.7000,0.8000,0.9000,0.9500,0.9750,0.9875,0.9950)
length(pqants)
probs.check<- pqants[20:11] - pqants[1:10]
probs.check

# cut down input data for testsing
#input.data<- input.data[1:50,]
#scaled.covs.data<- scaled.covs.data[1:50,]

# prepare for fuzzy allocation work
# covariance matrix
cov.matrix<- as.matrix(cov(scaled.covs.data))
phi<- 1.1
W<- cov.matrix
distype<- 3 # mahalanobis


for (i in 1:length(cluster.files)){
  # load the cluster centres and errors
  km.centroids<- readRDS(cluster.files[i])
  sel.errors<- readRDS(error.files[i])
  sel.errors<- sel.errors[,-1]
  summary.mat[i,1]<- as.matrix(dim(km.centroids)[1])
  
  # run fuzzy allocation [this actually takes a long time!]
  fuzzy.covs<- fuzall(data = scaled.covs.data,phi = phi,centroid = km.centroids,distype = distype,W = W)
  # memberships
  fuzzy.members<- fuzzy.covs$membership
  # get class with maximum membersip
  max.member.class<- apply(fuzzy.members,1,which.max)
  
  weighted.outs.mat<- matrix(NA, nrow = nrow(input.data), ncol = length(pqants))
  picp.outs.mat<- matrix(NA, nrow = nrow(input.data), ncol = 10)
  range.outs.mat<- matrix(NA, nrow = nrow(input.data), ncol = 10)
  for (j in 1:nrow(input.data)){
    sel.membership<- fuzzy.members[j,]
    # calculate the weighted errors
    for (k in 1:10){
      sel.lower<- sel.errors[,k]
      sel.upper<- sel.errors[,(20-k+1)]
      
      # multiply
      weighted.lower<- sum(sel.membership * sel.lower)
      weighted.upper<- sum(sel.membership * sel.upper)
      
      weighted.outs.mat[j,k]<- weighted.lower + input.data$pred_avgRK[j]
      weighted.outs.mat[j,(20-k+1)]<- weighted.upper + input.data$pred_avgRK[j]
      
      # prediction range
      range.outs.mat[j,k]<- weighted.outs.mat[j,(20-k+1)] - weighted.outs.mat[j,k]
      
      # picp
      if(input.data$target_avg[j] <= weighted.outs.mat[j,(20-k+1)] & input.data$target_avg[j] >= weighted.outs.mat[j,k]){
        picp.outs.mat[j,k]<- 1} else {
          picp.outs.mat[j,k]<- 0}
    }
    }
  
  # PICP work
  sel.picp<- colSums(picp.outs.mat)/nrow(input.data)
  plot(probs.check,sel.picp)
  goof.out<- goof(observed = probs.check,predicted = sel.picp,plot.it = T)
  summary.mat[i,2]<- as.matrix(goof.out$concordance)
  goof.out.short<- goof(observed = probs.check[1:4],predicted = sel.picp[1:4],plot.it = F)
  summary.mat[i,3]<- as.matrix(goof.out.short$concordance)
  
  # range
  summary.mat[i,4]<- as.matrix(colMeans(range.outs.mat)[4])
  
  print(i)}


summary.mat<- summary.mat[order(summary.mat[,1]),]
summary.mat<- as.data.frame(summary.mat)
names(summary.mat)<- c("cluster_nos", "concord_full","concord_short", "error_range")
write.csv(x = summary.mat, file = paste0(data.outs, "summary_stats_uncertainty_optimisation_",depth,".csv"),row.names = F)
