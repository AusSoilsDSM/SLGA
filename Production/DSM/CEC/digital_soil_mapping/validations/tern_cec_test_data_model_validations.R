### TERN LANDSCAPES 
# Soil CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 17.12.21
# modified: 28.01.22

# CODE PURPOSE
# Apply models to test data

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/"
data.root<- paste0(g.root,"data/model_outs/")
val.root<- paste0(g.root,"data/digitalsoilmapping/test/")
cal.root<- paste0(g.root,"data/digitalsoilmapping/calibration/")
model.root<- paste0(g.root,"models/cec/")
cluster.out<- paste0(g.root,"models/cec/cluster_centroids/")
data.out<- paste0(g.root,"models/cec/goofs/")

#fuzzy allocation function
source("/datasets/work/af-tern-mal-deb/work/projects/natsoil_monitoring/rcode/fuzme/fuzall.R")
# goof function 
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/miscell/goof.R")


# libraries
library(caret);library(Cubist);library(raster);library(rgdal);library(sp);library(automap);library(gstat);library(MASS)


#data
# site data
test.dat<- readRDS(paste0(val.root,"tern_cec_siteDat_covariates_TEST_updated.rds"))
names(test.dat)

# set up some matrices to put model outputs for each bootstrap iteration
cubmod.mat<- matrix(NA, ncol = 100, nrow = nrow(test.dat))
residual.mat<- matrix(NA, ncol = 100, nrow = nrow(test.dat))
rk.mat<- matrix(NA, ncol = 100, nrow = nrow(test.dat))
residual.actual<- matrix(NA, ncol = 100, nrow = nrow(test.dat))

# oob goof frames
oob.pred.goof.frame<- matrix(NA, nrow = 100, ncol = 5)
oob.rkpred.goof.frame<- matrix(NA, nrow = 100, ncol = 5)

# apply cubist covariate model
# point to where the models are
pred.models<- list.files(path = paste0(model.root,"models/"), pattern = "cubist_model_fit", 
                         full.names = T, recursive = F )
test.pred.model<- readRDS(pred.models[1])
summary(test.pred.model)

residual.models<- list.files(path = paste0(model.root,"residuals/"), pattern = "residual_model_fit", 
                             full.names = T, recursive = F )

residual.models
test.resid.model<- readRDS(residual.models[1])
summary(test.resid.model)

names(test.dat)
# mean of measured data
obs.dat.real<- test.dat[,16:65]
obs.dat.model<- log(obs.dat.real + 0.1)
obs.means.model<- rowMeans(obs.dat.model,na.rm = T)

for (ibag in 1:100) {
  # load the covariates
  TV.covs_oob<- test.dat[,c(66:104,107)]
  
  # load the selected model
  fitted_model<- readRDS(pred.models[ibag])
  
  # predict
  pred.v <- predict(fitted_model, newdata = TV.covs_oob) # calculate prediction on in bag data

  # place predicitons in prediciton frame
  cubmod.mat[,ibag]<- pred.v
  
  # goof
  oob.pred.goof.frame[ibag, ]<- as.matrix(goof(observed = obs.means.model, predicted = pred.v,plot.it = T))
  
  # residual model predictions
  fitted_residual<- readRDS(residual.models[ibag])
  pred.res.v <- predict(fitted_residual, newdata = TV.covs_oob) # calculate prediction on in bag data
  
  # place predictions in residual frame
  residual.mat[,ibag]<- pred.res.v
  
  pred.RK.v<- pred.v + pred.res.v
  oob.rkpred.goof.frame[ibag, ]<- as.matrix(goof(observed = obs.means.model, predicted = pred.RK.v,plot.it = T))
  
  # place predictions in regression kriging frame
  rk.mat[,ibag]<- pred.RK.v
  
  print(ibag)
}



## goof outputs
oob.pred.goof.frame<- as.data.frame(oob.pred.goof.frame)
names(oob.pred.goof.frame)<- c("R2", "concordance", "MSE", "RMSE", "bias")
write.csv(x = oob.pred.goof.frame, file = paste0(model.root,"goofs/goof_model_TEST.csv"), row.names = F)

oob.rkpred.goof.frame<- as.data.frame(oob.rkpred.goof.frame)
names(oob.rkpred.goof.frame)<- c("R2", "concordance", "MSE", "RMSE", "bias")
write.csv(x = oob.rkpred.goof.frame, file = paste0(model.root,"goofs/goof_RKmodel_TEST.csv"), row.names = F)

# output frames
obs.means.real<- rowMeans(obs.dat.real,na.rm = T)

model.format<- obs.means.model
real.format<- exp(obs.means.model) - 0.1

cor(obs.means.real,real.format)


# regression kriging predictions (model format)
rk.mat<- as.data.frame(rk.mat)
rk.mat.means.model<- rowMeans(rk.mat,na.rm = T)
goof(observed = obs.means.model,predicted = rk.mat.means.model, plot.it = T)
rk_goof_oob.model<- as.data.frame(goof(observed = obs.means.model,predicted = rk.mat.means.model, plot.it = T))
rk_goof_oob.model$rmseIQR<- rk_goof_oob.model$RMSE/IQR(obs.means.model)

# regression kriging predictions (real format)
rk.mat.means.real<- exp(rk.mat.means.model) - 0.1
goof(observed = obs.means.real,predicted = rk.mat.means.real, plot.it = T)
rk_goof_oob.real<- as.data.frame(goof(observed = obs.means.real,predicted = rk.mat.means.real, plot.it = T))
rk_goof_oob.real$rmseIQR<- rk_goof_oob.real$RMSE/IQR(obs.means.real)

summary_rk<- rbind(rk_goof_oob.model,rk_goof_oob.real)
row.names(summary_rk)<- c("model_format", "real_format")
write.csv(x = summary_rk, file = paste0(model.root,"goofs/goof_summaries_TEST_rk.csv"), row.names = F)


# regression predictions (model format)
cubmod.mat<- as.data.frame(cubmod.mat)
reg.mat.means.model<- rowMeans(cubmod.mat,na.rm = T)
goof(observed = obs.means.model,predicted = reg.mat.means.model, plot.it = T)
reg_goof_oob.model<- as.data.frame(goof(observed = obs.means.model,predicted = reg.mat.means.model, plot.it = T))
reg_goof_oob.model$rmseIQR<- reg_goof_oob.model$RMSE/IQR(obs.means.model)

# regression kriging predictions (real format)
reg.mat.means.real<- exp(reg.mat.means.model) - 0.1
goof(observed = obs.means.real,predicted = reg.mat.means.real, plot.it = T)
reg_goof_oob.real<- as.data.frame(goof(observed = obs.means.real,predicted = reg.mat.means.real, plot.it = T))
reg_goof_oob.real$rmseIQR<- reg_goof_oob.real$RMSE/IQR(obs.means.real)

summary_reg<- rbind(reg_goof_oob.model,reg_goof_oob.real)
row.names(summary_reg)<- c("model_format", "real_format")
write.csv(x = summary_reg, file = paste0(model.root,"goofs/goof_summaries_TEST_cubistreg.csv"), row.names = F)


names(test.dat)
out.dat<- cbind(test.dat[,1:15],obs.means.real,obs.means.model,
                rk.mat.means.model,rk.mat.means.real,
                reg.mat.means.model,reg.mat.means.real,
                test.dat[,c(66:104,107)])

saveRDS(object = out.dat,file = paste0(model.root,"goofs/TEST_cec_modelPreds_all.rds"))


### Uncertainty quantification
input.data<- readRDS(paste0(model.root,"goofs/TEST_cec_modelPreds_all.rds"))

# cluster centers of optimal config
cluster.centre<- readRDS(file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/models/cec/cluster_centroids//93_cluster_center.rds")
cluster.centre

# model errors distribution per cluster
cluster.errors<- readRDS("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/models/cec/cluster_centroids/cluster_errors/93_cluster_errors_cluster.rds")
cluster.errors

# PCA object for converting covariates to PC variables
pr_comp<- readRDS(file = paste0(model.root,"PCA_data_oob_cec_modelPreds_all_updated.rds"))


# cluster data
names(input.data)
cluster.data<- input.data[,22:61]
names(cluster.data)
# scale the data
names(pr_comp$center) == names(cluster.data)
scaled.covs.data<- predict(pr_comp, newdata=cluster.data)[,1:32]
dim(scaled.covs.data)

# error quantiles
pqants<- c(0.005,0.0125,0.025,0.05,0.1,0.2,0.3,0.4,0.45,0.475,
           0.5250,0.5500,0.6000,0.7000,0.8000,0.9000,0.9500,0.9750,0.9875,0.9950)
length(pqants)
probs.check<- pqants[20:11] - pqants[1:10]

# prepare for fuzzy allocation work
# covariance matrix
cov.matrix<- as.matrix(cov(scaled.covs.data))
# save matrix
saveRDS(object = cov.matrix, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/models/cec/cluster_centroids/PCA_covariance_matrix.rds" )
phi<- 1.1
W<- cov.matrix
distype<- 3 # mahalanobis

km.centroids<-cluster.centre
sel.errors<-cluster.errors
sel.errors<- sel.errors[,-1]

# run fuzzy allocation [this actually takes a long time!]
fuzzy.covs<- fuzall(data = scaled.covs.data,phi = phi,centroid = km.centroids,distype = distype,W = W)
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
    
    weighted.outs.mat[j,k]<- weighted.lower + input.data$rk.mat.means.model[j]
    weighted.outs.mat[j,(20-k+1)]<- weighted.upper + input.data$rk.mat.means.model[j]
    
    # prediction range
    range.outs.mat[j,k]<- weighted.outs.mat[j,(20-k+1)] - weighted.outs.mat[j,k]
    
    # picp
    if(input.data$obs.means.model[j] <= weighted.outs.mat[j,(20-k+1)] & input.data$obs.means.model[j] >= weighted.outs.mat[j,k]){
      picp.outs.mat[j,k]<- 1} else {
        picp.outs.mat[j,k]<- 0}
  }
}

# PICP work
sel.picp<- colSums(picp.outs.mat)/nrow(input.data)
plot(probs.check,sel.picp)
goof(observed = probs.check,predicted = sel.picp,plot.it = T)
goof(observed = probs.check[1:4],predicted = sel.picp[1:4],plot.it = T)


### save outputs of this process
weighted.outs.mat<- as.data.frame(weighted.outs.mat)
names(weighted.outs.mat)<- pqants
saveRDS(object = weighted.outs.mat, file = paste0(data.out, "TEST_cec_upper_lower_preds_all.rds"))

picp.outs.mat<- as.data.frame(picp.outs.mat)
names(picp.outs.mat)<- probs.check
saveRDS(object = picp.outs.mat, file = paste0(data.out, "TEST_cec_PICP_all.rds"))

range.outs.mat<- as.data.frame(range.outs.mat)
names(range.outs.mat)<- probs.check
saveRDS(object = range.outs.mat, file = paste0(data.out, "TEST_cec_predictionrange_all.rds"))

# results data
names(input.data)
uncert.data<- cbind(weighted.outs.mat$'0.05',weighted.outs.mat$'0.95')
exp.uncert<- exp(uncert.data) - 0.1 
uncert.data<- as.data.frame(cbind(uncert.data,exp.uncert))
names(uncert.data)<- c("lower_5th_model","upper_95th_model","lower_5th_real","upper_95th_real")
output.dat<- cbind(input.data[,c(1:7,16:21)], uncert.data)
saveRDS(object = output.dat, file = paste0(data.out, "TEST_cec_prediction_and_observed_data_all.rds"))
write.csv(x =  output.dat, file = paste0(data.out, "TEST_cec_prediction_and_observed_data_all.csv"),row.names=F)


## Do the fancy plotting stuff

# uncertainty
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(data.out,"picp_TEST_cec.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(probs.check,sel.picp,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (probs.check*100,sel.picp*100,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


## predictions
goof(observed = output.dat$obs.means.model,predicted = output.dat$rk.mat.means.model, plot.it = T)
goof(observed = output.dat$obs.means.real,predicted = output.dat$rk.mat.means.real, plot.it = T)
goof(observed = output.dat$obs.means.model,predicted = output.dat$reg.mat.means.model, plot.it = T)
goof(observed = output.dat$obs.means.real,predicted = output.dat$reg.mat.means.real, plot.it = T)

OBS<- output.dat$obs.means.real
PRED<- output.dat$rk.mat.means.real
xlimits= c(0,110)
ylimits= c(0,110) 
tiff(file=paste0(data.out,"external_val_RK_TEST_realunits.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(OBS,PRED,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted CEC", xlab= "observed CEC",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = xlimits[1],to = xlimits[2],by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = xlimits[1],to = xlimits[2],by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (OBS,PRED,pch=1, col="black", cex=0.5)
abline(0, 1, lwd=1.5, col="red")
dev.off()




