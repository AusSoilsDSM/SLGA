### TERN LANDSCAPES 
# total N
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 17.11.23
# modified: 17.11.23

# CODE PURPOSE
# Evaluate model prediction uncertainties


depth<- 1  # 1,2,3,4,5,6
dseq<- 0  # 0,5,15,30,60,100
vart<-  "totalN"

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_N/"
data.root<- data.root<- paste0(g.root,"models/ranger_models/data_obs_preds/ext/")
site.root<- paste0(g.root, "models/ranger_models/data_obs_preds/cal/")
fig.root<- paste0(g.root, "outputs/external_evaluation/")
model.root<- paste0(g.root, "models/ranger_models/model_objects/")



# libraries
library(ranger);library(MASS)

# goof function 
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/miscell/goof.R")


# load data
ext.dat<- read.csv(file = paste0(data.root,"ranger_EXT_preds_", vart, "_depth_", depth, "_summary.csv"))
# with covariates
ext.datc<- readRDS(paste0(g.root,"data/ross/model_ready/tern_",vart,"_labelled_covariates_TEST.rds"))
ext.datc<- ext.datc[ext.datc$depth == depth,]
names(ext.datc)
ext.covs<- ext.datc[,6:45]


# cluster files
model.files<- list.files(path = model.root, pattern = "aus_totalN_fittedmodel_",full.names = T, recursive = F)
model.files

# error quantiles
pqants<- c(0.005,0.0125,0.025,0.05,0.1,0.2,0.3,0.4,0.45,0.475,
           0.5250,0.5500,0.6000,0.7000,0.8000,0.9000,0.9500,0.9750,0.9875,0.9950)
length(pqants)
probs.check<- pqants[20:11] - pqants[1:10]
probs.check

# set up a list each with a matrix of nrow = number of cases, ncol = number of models
empty_list <- vector(mode = "list", length = length(pqants))
for (i in 1: length(empty_list)){
  empty_list[[i]]<- matrix(NA, nrow= nrow(ext.dat), ncol = length(model.files))}
str(empty_list)


for (j in 1:length(model.files)){  # takes a bit as it loads each model object and then runs the data 
  #load prediction model
  selected.model<- readRDS(model.files[j])
  
  # check of label consistency
  names(ext.covs)
  names(selected.model$variable.importance)
  
  # predict on data
  ranger.grid<- predict(object = selected.model,ext.covs, type = "quantiles",quantiles = pqants )
  ranger.preds<- ranger.grid$predictions
  
  # put predictions into the matrices
  for (k in 1:length(empty_list)){
    empty_list[[k]][,j]<- ranger.preds[,k]}
  
  print(j)}


# Take the row means for each probability level
out.mat<-  matrix(NA, nrow= nrow(ext.dat), ncol = length(empty_list))
for (i in 1:length(empty_list)){
  sub.dat<- empty_list[[i]]
  sub.means<- rowMeans(sub.dat)
  out.mat[,i]<- sub.means}
  

# PICP and prediction range
picp.outs.mat<- matrix(NA, nrow = nrow(ext.dat), ncol = 10)
range.outs.mat<- matrix(NA, nrow = nrow(ext.dat), ncol = 10)
for (j in 1:nrow(ext.dat)){
  for (k in 1:10){
    # prediction range
    range.outs.mat[j,k]<- out.mat[j,(20-k+1)] - out.mat[j,k]
    
    # picp
    if(ext.dat$target_avg[j] <= out.mat[j,(20-k+1)] & ext.dat$target_avg[j] >= out.mat[j,k]){
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
weighted.outs.mat<- as.data.frame(out.mat)
names(weighted.outs.mat)<- pqants
saveRDS(object = weighted.outs.mat, file = paste0(data.root,"ranger_EXT_preds_bd_depth_d",depth,"_upper_and_lower.rds"))

picp.outs.mat<- as.data.frame(picp.outs.mat)
names(picp.outs.mat)<- probs.check
saveRDS(object = picp.outs.mat, file = paste0(data.root,"ranger_EXT_preds_bd_depth_d",depth,"_picp.rds"))

range.outs.mat<- as.data.frame(range.outs.mat)
names(range.outs.mat)<- probs.check
saveRDS(object = range.outs.mat, file = paste0(data.root,"ranger_EXT_preds_bd_depth_d",depth,"_predictionrange.rds"))

# results data
names(ext.dat)
uncert.data<- cbind(weighted.outs.mat$'0.05',weighted.outs.mat$'0.95')
names(uncert.data)<- c("lower_5th_real","upper_95th_real")
output.dat<- cbind(ext.dat, uncert.data)
saveRDS(object = output.dat, file = paste0(data.root,"ranger_EXT_preds_bd_depth_",depth,"_summary_w_upper_and_lower.rds"))
write.csv(x =  output.dat, file = paste0(data.root,"ranger_EXT_preds_bd_depth_d",depth,"_summary_w_upper_and_lower.csv"),row.names=F)


## Do the fancy plotting stuff

# uncertainty
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"external_val_bd_PICP_ranger",depth,".tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(probs.check,sel.picp,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (probs.check*100,sel.picp*100,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


### END 

