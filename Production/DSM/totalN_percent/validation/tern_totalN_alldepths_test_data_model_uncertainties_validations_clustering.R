### TERN LANDSCAPES 
# total N
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 18.11.23
# modified: 18.11.23

# CODE PURPOSE
# Evaluate model prediction uncertainties

depth<- 1
depth2<- "all_depths"

# cluster numbers
clus.nos<- seq(from =1, to = 501,by=25)
clus.nos
sel.clus<- which(clus.nos== 101) # change accordingly
sel.clus
vart<- "totalN"

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_N/"
cluster.out<- paste0(g.root,"models/uncertainty/cluster_centroids/")
data.root<- paste0(g.root, "data/ross/model_ready/")
input.root<- paste0(g.root,"models/ranger_models/data_obs_preds/ext/")
data.outs<- paste0(g.root,"outputs/external_evaluation/")

# libraries
library(MASS)

#fuzzy allocation function
source("/datasets/work/af-tern-mal-deb/work/projects/natsoil_monitoring/rcode/fuzme/fuzall.R")
# goof function 
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/miscell/goof.R")


# load data
input.data<- read.csv(file = paste0(input.root,"ranger_EXT_preds_",vart,"_depth_",depth,"_summary.csv"))
names(input.data)
# select depth specific data
#input.data<- input.data[input.data$depth == depth, ]

# load associated covariate data
ext.covariate.dat<- readRDS(paste0(data.root,"tern_",vart, "_labelled_covariates_TEST.rds"))
# select depth specfic data
#ext.covariate.dat<- ext.covariate.dat[ext.covariate.dat$depth == depth, ]
names(ext.covariate.dat)

# cluster files
cluster.files<- list.files(path = cluster.out, pattern = "cluster_center",full.names = F, recursive = F)
cluster.files<- cluster.files[1:21]
cluster.files
rd1<- strsplit(cluster.files,"_")
rd1<- as.numeric(sapply(rd1, `[`, 1))
rd1<- order(rd1)
cluster.files<- list.files(path = cluster.out, pattern = "cluster_center",full.names = T, recursive = F)
cluster.files<- cluster.files[1:21]
cluster.files<- cluster.files[rd1]
cluster.files

test.cluster.file<- as.data.frame(readRDS(cluster.files[1]))
names(test.cluster.file)


# cluster error files
error.files<- list.files(path = paste0(cluster.out, "cluster_errors/"), pattern = "cluster_errors_cluster", full.names = T, recursive = F)
error.files
error.files<- error.files[rd1]
error.files


# cluster data
names(ext.covariate.dat)
cluster.data<- ext.covariate.dat[,6:45]
names(cluster.data)
# check column matches between covariate data and cluster centres
names(test.cluster.file) == names(cluster.data)

# error quantiles
pqants<- c(0.005,0.0125,0.025,0.05,0.1,0.2,0.3,0.4,0.45,0.475,
           0.5250,0.5500,0.6000,0.7000,0.8000,0.9000,0.9500,0.9750,0.9875,0.9950)
length(pqants)
probs.check<- pqants[20:11] - pqants[1:10]
probs.check


# prepare for fuzzy allocation work
# covariance matrix
cov.matrix<- readRDS(file = paste0(g.root,"data/ross/model_ready/tern_totalN_covariates_varcov_matrix_CAL.rds"))

phi<- 1.1
W<- cov.matrix
distype<- 3 # mahalanobis


# select the appropriate settings
cluster.files<- cluster.files[sel.clus]
error.files<- error.files[sel.clus]
km.centroids<-readRDS(cluster.files)
sel.errors<-readRDS(error.files)
sel.errors<- sel.errors[,-1]

# cut down input data for testsing
#input.data<- input.data[1:100,]
#cluster.data<- cluster.data[1:100,]


# run fuzzy allocation [this actually takes a long time!]
fuzzy.covs<- fuzall(data = as.matrix(cluster.data),phi = phi,centroid = km.centroids,distype = distype,W = W)
# memberships
fuzzy.members<- fuzzy.covs$membership
# get class with maximum membership
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
    
    weighted.outs.mat[j,k]<- weighted.lower + input.data$pred_avg[j]
    weighted.outs.mat[j,(20-k+1)]<- weighted.upper + input.data$pred_avg[j]
    
    # prediction range
    range.outs.mat[j,k]<- weighted.outs.mat[j,(20-k+1)] - weighted.outs.mat[j,k]
    
    # picp
    if(input.data$target[j] <= weighted.outs.mat[j,(20-k+1)] & input.data$target[j] >= weighted.outs.mat[j,k]){
      picp.outs.mat[j,k]<- 1} else {
        picp.outs.mat[j,k]<- 0}
  }
  print(j)
}


# PICP work
sel.picp<- colSums(picp.outs.mat)/nrow(input.data)
plot(probs.check,sel.picp)
goof(observed = probs.check,predicted = sel.picp,plot.it = T)
goof(observed = probs.check[1:4],predicted = sel.picp[1:4],plot.it = T)


### save outputs of this process
weighted.outs.mat<- as.data.frame(weighted.outs.mat)
names(weighted.outs.mat)<- pqants
saveRDS(object = weighted.outs.mat, file = paste0(input.root,"clustering_EXT_preds_",vart, "_depth_d",depth2,"_upper_and_lower.rds"))

picp.outs.mat<- as.data.frame(picp.outs.mat)
names(picp.outs.mat)<- probs.check
saveRDS(object = picp.outs.mat, file = paste0(input.root,"clustering_EXT_preds_", vart, "depth_d",depth2,"_picp.rds"))

range.outs.mat<- as.data.frame(range.outs.mat)
names(range.outs.mat)<- probs.check
saveRDS(object = range.outs.mat, file = paste0(input.root,"clustering_EXT_preds_",vart, "_depth_d",depth2,"_predictionrange.rds"))

# results data
names(input.data)
uncert.data<- as.data.frame(cbind(weighted.outs.mat$'0.05',weighted.outs.mat$'0.95'))
names(uncert.data)<- c("lower_5th_real","upper_95th_real")
output.dat<- cbind(input.data, uncert.data)
saveRDS(object = output.dat, file = paste0(input.root,"clustering_EXT_preds_",vart, "_depth_",depth2,"_summary_w_upper_and_lower.rds"))
write.csv(x =  output.dat, file = paste0(input.root,"clustering_EXT_preds_", vart, "_depth_d",depth2,"_summary_w_upper_and_lower.csv"),row.names=F)


## Do the fancy plotting stuff

# uncertainty
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(data.outs,"external_val_", vart, "_PICP_clustering_",depth2,".tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(probs.check,sel.picp,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (probs.check*100,sel.picp*100,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


### END 

