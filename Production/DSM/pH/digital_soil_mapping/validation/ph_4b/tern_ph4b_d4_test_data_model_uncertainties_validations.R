### TERN LANDSCAPES 
# Soil pH 4b
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 19.9.22
# modified: 19.9.22

# CODE PURPOSE
# Evaluate model prediction uncertainties
# 203 class; 33 PCA variables


depth<- "d4"

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
data.root<- paste0(g.root,"models/ph_4b/",depth,"/data_obs_preds/ext/")
resmod.root<- paste0(g.root,"models/ph_4b/variograms/")
site.root<- paste0(g.root, "models/ph_4b/",depth,"/data_obs_preds/cal/")
fig.root<- paste0(g.root, "outs/4B_meth/external_evaluation/")
cluster.out<- paste0(g.root,"models/ph_4b/",depth,"/uncertainty/cluster_centroids/")



# libraries
library(raster);library(rgdal);library(sp);library(automap);library(gstat);library(MASS)

#fuzzy allocation function
source("/datasets/work/af-tern-mal-deb/work/projects/natsoil_monitoring/rcode/fuzme/fuzall.R")
# goof function 
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/miscell/goof.R")

# load data
ext.dat<- read.table(file = paste0(data.root,"ranger_EXT_preds_pH4b_depth_",depth,"_summary.txt"),header = T, sep = ",")

# covariate data
ext.covariate.dat<- readRDS(file = paste0(data.root,"ranger_EXT_covariates_pH4b_depth_",depth,".rds"))
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


# select the appropriate settings
cluster.files<- cluster.files[117]
error.files<- error.files[117]
cluster.files;error.files

# PCA
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

# prepare for fuzzy allocation work
# covariance matrix
cov.matrix<- as.matrix(cov(scaled.covs.data))
# save matrix
saveRDS(object = cov.matrix, file = paste0(data.root, "PCA_covariance_matrix_",depth,"_uncertanal.rds" ))
phi<- 1.1
W<- cov.matrix
distype<- 3 # mahalanobis

km.centroids<-readRDS(cluster.files)
sel.errors<-readRDS(error.files)
sel.errors<- sel.errors[,-1]

# run fuzzy allocation [this actually takes a long time!]
fuzzy.covs<- fuzall(data = scaled.covs.data,phi = phi,centroid = km.centroids,distype = distype,W = W)
# memberships
fuzzy.members<- fuzzy.covs$membership
# get class with maximum membership
max.member.class<- apply(fuzzy.members,1,which.max)

weighted.outs.mat<- matrix(NA, nrow = nrow(ext.dat), ncol = length(pqants))
picp.outs.mat<- matrix(NA, nrow = nrow(ext.dat), ncol = 10)
range.outs.mat<- matrix(NA, nrow = nrow(ext.dat), ncol = 10)
for (j in 1:nrow(ext.dat)){
  sel.membership<- fuzzy.members[j,]
  # calculate the weighted errors
  for (k in 1:10){
    sel.lower<- sel.errors[,k]
    sel.upper<- sel.errors[,(20-k+1)]
    
    # multiply
    weighted.lower<- sum(sel.membership * sel.lower)
    weighted.upper<- sum(sel.membership * sel.upper)
    
    weighted.outs.mat[j,k]<- weighted.lower + ext.dat$pred_avgRK[j]
    weighted.outs.mat[j,(20-k+1)]<- weighted.upper + ext.dat$pred_avgRK[j]
    
    # prediction range
    range.outs.mat[j,k]<- weighted.outs.mat[j,(20-k+1)] - weighted.outs.mat[j,k]
    
    # picp
    if(ext.dat$target_avg[j] <= weighted.outs.mat[j,(20-k+1)] & ext.dat$target_avg[j] >= weighted.outs.mat[j,k]){
      picp.outs.mat[j,k]<- 1} else {
        picp.outs.mat[j,k]<- 0}
  }
  print(j)
}


# PICP work
sel.picp<- colSums(picp.outs.mat)/nrow(ext.dat)
plot(probs.check,sel.picp)
goof(observed = probs.check,predicted = sel.picp,plot.it = T)
goof(observed = probs.check[1:4],predicted = sel.picp[1:4],plot.it = T)


### save outputs of this process
weighted.outs.mat<- as.data.frame(weighted.outs.mat)
names(weighted.outs.mat)<- pqants
saveRDS(object = weighted.outs.mat, file = paste0(data.root,"ranger_EXT_preds_pH4b_depth_",depth,"_upper_and_lower.rds"))

picp.outs.mat<- as.data.frame(picp.outs.mat)
names(picp.outs.mat)<- probs.check
saveRDS(object = picp.outs.mat, file = paste0(data.root,"ranger_EXT_preds_pH4b_depth_",depth,"_picp.rds"))

range.outs.mat<- as.data.frame(range.outs.mat)
names(range.outs.mat)<- probs.check
saveRDS(object = range.outs.mat, file = paste0(data.root,"ranger_EXT_preds_pH4b_depth_",depth,"_predictionrange.rds"))

# results data
names(ext.dat)
uncert.data<- cbind(weighted.outs.mat$'0.05',weighted.outs.mat$'0.95')
names(uncert.data)<- c("lower_5th_real","upper_95th_real")
output.dat<- cbind(ext.dat, uncert.data)
saveRDS(object = output.dat, file = paste0(data.root,"ranger_EXT_preds_pH4b_depth_",depth,"_summary_w_upper_and_lower.rds"))
write.csv(x =  output.dat, file = paste0(data.root,"ranger_EXT_preds_pH4b_depth_",depth,"_summary_w_upper_and_lower.csv"),row.names=F)


## Do the fancy plotting stuff

# uncertainty
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"external_val_pH4b_PICP_",depth,".tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(probs.check,sel.picp,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (probs.check*100,sel.picp*100,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


### END 

