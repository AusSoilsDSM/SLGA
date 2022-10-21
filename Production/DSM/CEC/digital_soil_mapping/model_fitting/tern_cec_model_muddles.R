### TERN LANDSCAPES 
# Soil CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 8.12.21
# modified: 28.01.22

# CODE PURPOSE
# CEC model fitting script. Cubist models

# root directory
data.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/model_outs/"
val.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/digitalsoilmapping/test/"
cal.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/digitalsoilmapping/calibration/"
model.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/models/cec/"
funcs.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/miscell/"

# libraries
library(caret);library(Cubist);library(raster);library(rgdal);library(sp);library(automap);library(gstat);library(MASS)
source(paste0(funcs.out,"goof.R"))

#data
# site data
model.dat<- readRDS(paste0(cal.root,"tern_cec_siteDat_covariates_CALVAL_updated.rds"))
names(model.dat)


# set up some matrices to put model outputs for each bootstrap iteration
cubmod.mat<- matrix(NA, ncol = 100, nrow = nrow(model.dat))
residual.mat<- matrix(NA, ncol = 100, nrow = nrow(model.dat))
rk.mat<- matrix(NA, ncol = 100, nrow = nrow(model.dat))
residual.actual<- matrix(NA, ncol = 100, nrow = nrow(model.dat))

# oob goof frames
oob.pred.goof.frame<- matrix(NA, nrow = 100, ncol = 5)
oob.rkpred.goof.frame<- matrix(NA, nrow = 100, ncol = 5)


# modelling 
nbag<- 100
n= nrow(model.dat)
tv.colunms<- c(16:65)


for (ibag in 1:nbag) {
  
  # select a target variable column
  sel.col<- sample(tv.colunms, 1, replace=F)
  
  # sample with replacement
  s <- sample.int(n, replace = TRUE)
  
  inbag.rows<- unique(s)
  inbag.rows<- inbag.rows[order(inbag.rows)]
  all.rows<- c(1:n)
  outbag.rows<- all.rows[!all.rows %in% inbag.rows]
  
  inbag.data<- model.dat[inbag.rows,]
  outbag.data<- model.dat[outbag.rows,]
  
  # model data
  names(inbag.data)
  TV<- log(inbag.data[,sel.col]+0.1)
  TV.covs<- inbag.data[,c(66:104,107)]
  fitted_model<-  cubist(x = TV.covs, y = TV, cubistControl(extrapolation = 10), committees = 1)
  pred.c <- predict(fitted_model, newdata = TV.covs) # calculate prediction on in bag data
  # save model
  saveRDS(object = fitted_model ,file = paste0(model.root,"models/cubist_model_fit_",ibag,".rds"))
  
  # save model information 
  ## capture output
  var_nm1<- paste0(model.root,"models/model_summaries/cubist_model_fit_summary_",ibag, ".txt")
  
  out1<- capture.output(summary(fitted_model))
  out1<- paste0(out1,collapse = "\r\n")
  cat(out1, file = var_nm1, sep=",", append = T)
  
  out2<- capture.output(fitted_model)
  out2<- paste0(out2,collapse = "\r\n")
  cat(out2, file = var_nm1, sep=",", append = T)
  
  #capture variable importance
  varOutput<- varImp(fitted_model, scale=FALSE)
  varOutput
  out3<- capture.output(varOutput)
  out3<- paste0(out3,collapse = "\r\n")
  cat(out3, file = var_nm1, sep=",", append = T)
  
  
  # use model on out of bag data
  TV_oob<- log(outbag.data[,sel.col]+0.1)
  TV.covs_oob<- outbag.data[,c(66:104,107)]
  
  pred.v <- predict(fitted_model, newdata = TV.covs_oob) # calculate prediction on in bag data
  # place predicitons in prediciton frame
  cubmod.mat[outbag.rows,ibag]<- pred.v
  # goof
  oob.pred.goof.frame[ibag, ]<- as.matrix(goof(observed = TV_oob, predicted = pred.v,plot.it = T))
  
  # residual prediction
  model.residual<- TV - pred.c
  residual.actual[inbag.rows,ibag]<- model.residual
  #hist(model.residual)
  names(inbag.data)
  residual.frame<- cbind(model.residual, inbag.data[,c(107,66:104)])
  names(residual.frame)
  
  # fit cubist residual model
  fitted_residual<-  cubist(x = TV.covs, y = residual.frame$model.residual, cubistControl(extrapolation = 10), committees = 1)
  # save model
  saveRDS(object = fitted_residual ,file = paste0(model.root,"residuals/residual_model_fit_",ibag,".rds"))
  #summary(fitted_residual)
  # apply cubist residual model to oob data
  pred.res.v <- predict(fitted_residual, newdata = TV.covs_oob) # calculate prediction on in bag data
  # place predicitons in residual frame
  residual.mat[outbag.rows,ibag]<- pred.res.v
  
  pred.RK.v<- pred.v + pred.res.v
  oob.rkpred.goof.frame[ibag, ]<- as.matrix(goof(observed = TV_oob, predicted = pred.RK.v,plot.it = T))
  # place predicitons in regression kriging frame
  rk.mat[outbag.rows,ibag]<- pred.RK.v
  
  print(ibag)
  }



## process and save outputs

## goof outputs
oob.pred.goof.frame<- as.data.frame(oob.pred.goof.frame)
names(oob.pred.goof.frame)<- c("R2", "concordance", "MSE", "RMSE", "bias")
write.csv(x = oob.pred.goof.frame, file = paste0(model.root,"goofs/goof_model_oob.csv"), row.names = F)

oob.rkpred.goof.frame<- as.data.frame(oob.rkpred.goof.frame)
names(oob.rkpred.goof.frame)<- c("R2", "concordance", "MSE", "RMSE", "bias")
write.csv(x = oob.rkpred.goof.frame, file = paste0(model.root,"goofs/goof_RKmodel_oob.csv"), row.names = F)

# output frames
names(model.dat)
all.TV.means<- rowMeans(model.dat[,16:65])
all.TV.means<- log(all.TV.means + 0.1)

# regression kriging predictions
rk.mat<- as.data.frame(rk.mat)
rk.mat.means<- rowMeans(rk.mat,na.rm = T)
goof(observed = all.TV.means,predicted = rk.mat.means, plot.it = T)
rk_goof_oob<- as.data.frame(goof(observed = all.TV.means,predicted = rk.mat.means, plot.it = T))


# predicted residual predictions
residual.mat<- as.data.frame(residual.mat)
residual.mat.means<- rowMeans(residual.mat,na.rm = T)

# regression predictions
cubmod.mat<- as.data.frame(cubmod.mat)
cubmod.mat.means<- rowMeans(cubmod.mat,na.rm = T)
goof(observed = all.TV.means,predicted = cubmod.mat.means, plot.it = T)
reg_goof_oob<- as.data.frame(goof(observed = all.TV.means,predicted = cubmod.mat.means, plot.it = T))
summary_oob<- rbind(rk_goof_oob,reg_goof_oob)
row.names(summary_oob)<- c("RK_cubist", "cubist")
summary_oob
write.csv(x = summary_oob, file = paste0(model.root,"goofs/goof_suumaries_alldat_oob.csv"), row.names = F)

# model_residuals
residual.actual<- as.data.frame(residual.actual)
residual.actual.means<- rowMeans(residual.actual,na.rm = T)

# mean of measured data
names(model.dat)
obs.dat<- model.dat[,16:65]
obs.dat<- log(obs.dat + 0.1)
obs.means<- rowMeans(obs.dat,na.rm = T)

# save ouputs
names(model.dat)
labels.dat<- model.dat[,1:15]
covs.dat<- model.dat[,c(66:104,107)]

out.dat<- cbind(labels.dat,cubmod.mat.means,residual.mat.means,rk.mat.means,residual.actual.means,obs.means,covs.dat)
saveRDS(object = out.dat,file = paste0(model.root,"goofs/oob_cec_modelPreds_all.rds"))


#out.dat<- readRDS(file = paste0(cluster.out, "summaries/cluster_centers_oob_cec_modelPreds_all.rds"))
#names(out.dat)
#out.dat<- cbind(out.dat[,1:19],obs.means, out.dat[,20:172])
#saveRDS(object = out.dat, file = paste0(cluster.out,"summaries/cluster_centers_oob_cec_modelPreds_all.rds"))



