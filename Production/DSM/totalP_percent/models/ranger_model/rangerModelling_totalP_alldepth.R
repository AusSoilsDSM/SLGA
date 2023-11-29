### TERN LANDSCAPES 
# Total N
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 20.11.23
# modified: 20.11.23

# CODE PURPOSE
# Model is inclusive of all depths
# Use optimally determined hyperparameter values to fit ranger models
# fit 100 models 
# derive external model errors for the uncertainty analysis

# fixed parameters
vart<- "totalP"
its<- 100
depth<- "alldepth"

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_P/"
data.root<- paste0(g.root, "data/ross/model_ready/")
model.out<- paste0(g.root, "models/ranger_models/")
funcs.out<- paste0(g.root, "rcode/miscell/")


# libraries
library(caret);library(ranger);library(MASS);library(automap);library(gstat)
source(paste0(funcs.out,"goof.R"))

#data
# site data
site.dat<- readRDS(paste0(data.root,"tern_totalP_labelled_covariates_CAL.rds"))
#site.dat<- site.dat[1:500,]
# external validation data
ext.dat<- readRDS(paste0(data.root,"tern_totalP_labelled_covariates_TEST.rds"))
#ext.dat<- ext.dat[1:500,]

# putting model diagnostics into tables
cal_diog<- matrix(NA, ncol= 7, nrow=its)
val_diog<- matrix(NA, ncol= 7, nrow=its)
ext_diog<- matrix(NA, ncol= 7, nrow=its)

# set up some matrices to put model outputs for each bootstrap iteration
pred.mat_c<- matrix(NA, ncol = its, nrow = nrow(site.dat))
residual.mat_c<- matrix(NA, ncol = its, nrow = nrow(site.dat))
pred.mat_v<- matrix(NA, ncol = its, nrow = nrow(site.dat))

# model tuning parameters
tgrid <- expand.grid(
  .mtry = 21,
  .splitrule= "variance",
  .min.node.size = 5)


# cycle through its number of realisations
for (zz in 1:its){
  
  ##############################################################################
  ## calibration and validation (bootstrap)
  boots<- sample.int(nrow(site.dat), replace = TRUE)
  uboots<- unique(boots)
  inbag.uboots<- uboots[order(uboots)]
  all.rows<- c(1:nrow(site.dat))
  outbag.uboots<- all.rows[!all.rows %in% inbag.uboots]
  
  # calibration data
  DSM_data_c<- site.dat[inbag.uboots,]
  
  # validation
  DSM_data_v<- site.dat[outbag.uboots,]
  
  ranger.model<-train(x= DSM_data_c[,6:45], y= DSM_data_c[,3],
                      tuneGrid = tgrid, 
                      method = "ranger",
                      trControl =trainControl(method = "oob"), 
                      num.trees = 500, 
                      importance = 'impurity')
  
  
  summary(ranger.model)
  ranger.model
  
  ## capture output
  var_nm1<- paste0(model.out,"model_summaries/", "aus_",vart,"_model_iteration_", zz, ".txt")
  
  out1<- capture.output(summary(ranger.model))
  out1<- paste0(out1,collapse = "\r\n")
  cat(out1, file = var_nm1, sep=",", append = T)
  
  out2<- capture.output(ranger.model)
  out2<- paste0(out2,collapse = "\r\n")
  cat(out2, file = var_nm1, sep=",", append = T)
  
  #capture variable importance
  varOutput<- varImp(ranger.model, scale=FALSE)
  varOutput
  out3<- capture.output(varOutput)
  out3<- paste0(out3,collapse = "\r\n")
  cat(out3, file = var_nm1, sep=",", append = T)
  
  # save model to file
  mod_name<- paste0(model.out,"model_objects/","aus_",vart,"_fittedmodel__iteration_", zz, ".rds")
  saveRDS(ranger.model, file = mod_name)
  
  
  ## VALIDATION OF MODEL
  # predict on calibration data
  ranger.pred_c<-predict(ranger.model, DSM_data_c)
  cal_diog[zz, 1]<- 1
  cal_diog[zz, 2]<- zz
  cal_diog[zz, 3:7]<- as.matrix(goof(observed = DSM_data_c[,3], predicted = ranger.pred_c, plot.it = F))
  
  # predict on validation data
  ranger.pred_v<-predict(ranger.model, DSM_data_v)
  val_diog[zz, 1]<- 1
  val_diog[zz, 2]<- zz
  val_diog[zz, 3:7]<- as.matrix(goof(observed = DSM_data_v[,3], predicted = ranger.pred_v, plot.it = F))
  
  # predict on external data
  ranger.pred_ex<-predict(ranger.model, ext.dat)
  ext_diog[zz, 1]<- 1
  ext_diog[zz, 2]<- zz
  ext_diog[zz, 3:7]<- as.matrix(goof(observed = ext.dat$target, predicted = ranger.pred_ex, plot.it = T))
  
  ## Residual 
  pred_residual_c<- residual<- DSM_data_c[,3] - ranger.pred_c
  
  # put predictions into frames
  # calibration predictions
  pred.mat_c[inbag.uboots,zz]<- ranger.pred_c
  residual.mat_c[inbag.uboots,zz]<- pred_residual_c
  pred.mat_v[outbag.uboots,zz]<- ranger.pred_v
  
  # write outputs to file
  cal_frame<- as.data.frame(cal_diog)
  cal_frame<- cal_frame[complete.cases(cal_frame),]
  names(cal_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
  cal_name<- paste0(model.out,"model_diognostics/ranger_CAL_diogs_",vart,"_",depth, ".txt")
  write.table(cal_frame,file = cal_name, sep = ",",row.names = F, col.names = T) 
  
  val_frame<- as.data.frame(val_diog)
  val_frame<- val_frame[complete.cases(val_frame),]
  names(val_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
  val_name<- paste0(model.out,"model_diognostics/ranger_VAL_diogs_",vart,"_",depth,".txt")
  write.table(val_frame,file = val_name, sep = ",",row.names = F, col.names = T)
  
  ext_frame<- as.data.frame(ext_diog)
  ext_frame<- ext_frame[complete.cases(ext_frame),]
  names(ext_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
  ext_name<- paste0(model.out,"model_diognostics/ranger_EXT_diogs_",vart,"_",depth,".txt")
  write.table(ext_frame,file = ext_name, sep = ",",row.names = F, col.names = T)
  
  
  #output observation and prediction for calibration sites
  names(DSM_data_c)
  calOut_dat<- DSM_data_c[,1:5]
  calOut_dat$prediction<- ranger.pred_c
  cal_name1<- paste0(model.out,"data_obs_preds/cal/","ranger_CAL_preds_", vart, "_depth_", depth,"_iteration_", zz, ".txt")
  write.table(calOut_dat,file = cal_name1, sep = ",",row.names = F, col.names = T) 
  
  #output observation and prediction for validation sites
  valOut_dat<- DSM_data_v[,1:5]
  valOut_dat$prediction<- ranger.pred_v
  val_name1<- paste0(model.out,"data_obs_preds/val/","ranger_VAL_preds_", vart, "_depth_", depth,"_iteration_", zz, ".txt")
  write.table(valOut_dat,file = val_name1, sep = ",",row.names = F, col.names = T)
  
  #output observation and prediction for external
  names(ext.dat)
  extOut_dat<- ext.dat[,c(1:5,45)]
  extOut_dat$prediction<- ranger.pred_ex
  ext_name1<- paste0(model.out,"data_obs_preds/ext/","ranger_EXT_preds_", vart, "_depth_", depth,"_iteration_", zz, ".txt")
  write.table(extOut_dat,file = ext_name1, sep = ",",row.names = F, col.names = T) 
  
}

### save the predictions frames
# calibration
pred.mat_c<- as.data.frame(pred.mat_c)
pred.mat_c_rm<- rowMeans(pred.mat_c,na.rm = T)

residual.mat_c<- as.data.frame(residual.mat_c)
residual.mat_c_rm<- rowMeans(residual.mat_c,na.rm = T)


names(site.dat)
calOut_dat<- site.dat[,c(1:5,45)]
calOut_dat$prediction_avg<- pred.mat_c_rm
calOut_dat$residual_avg<- residual.mat_c_rm
cal_name1<- paste0(model.out,"data_obs_preds/cal/","ranger_CAL_preds_average_summaries_", vart, "_depth_", depth, ".txt")
write.table(calOut_dat,file = cal_name1, sep = ",",row.names = F, col.names = T) 

# validation out
pred.mat_v<- as.data.frame(pred.mat_v)
pred.mat_v_rm<- rowMeans(pred.mat_v,na.rm = T)

valOut_dat<- site.dat[,c(1:5,45)]
valOut_dat$prediction_avg<- pred.mat_v_rm
valOut_dat$residual_avg<- valOut_dat$target - valOut_dat$prediction_avg
val_name1<- paste0(model.out,"/data_obs_preds/val/","ranger_VAL_preds_average_summaries_", vart, "_depth_", depth, ".txt")
write.table(valOut_dat,file = val_name1, sep = ",",row.names = F, col.names = T) 




# END

