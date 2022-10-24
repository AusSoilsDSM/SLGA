### TERN LANDSCAPES 
# Soil pH model model fitting
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 2.9.22
# modified: 6.9.22

# CODE PURPOSE
# # Depth 1 [0-5cm]
# Use optimally determined hyperparameter values to fit ranger models
# fit 100 models 

# fixed parameters
vart<- "pH4b"
depth<- "d1"
colsel<- 9
paramsf<- 1
its<- 100

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
data.root<- paste0(g.root, "data/curated_all/")
dists.root<- paste0(g.root, "data/field_2_4B_dists/")
params.out<- paste0(g.root, "data/ranger_model_hyperparams/")
model.out<- paste0(g.root, "models/ph_4b/")
funcs.out<- paste0(g.root, "rcode/miscell/")
slurm.root<- paste0(g.root, "rcode/slurm/ph_4b/digital_soil_mapping/model_fitting/")
r.code<- paste0(g.root, "rcode/digital_soil_mapping/model_fitting/ph_4b/")

# libraries
library(caret);library(ranger);library(raster);library(rgdal);library(sp);library(MASS);library(automap);library(gstat)
source(paste0(funcs.out,"goof.R"))

#data
# site data
site.dat<- readRDS(paste0(data.root,"tern_soilpH4B_siteDat_covariates_20223008_CALVALDAT_ARD.rds"))
# external validation data
ext.dat<- readRDS(paste0(data.root,"tern_soilpH4B_siteDat_covariates_20223008_EXTERNAL_ARD.rds"))
# hyperparameter values
tuning.params<- read.csv(file = paste0(model.out, "optimal_tuning_params_pH_4b_90m.csv"))

## distribtions
dist_35<- readRDS(file = paste0(dists.root,"dists_35_field_2_4b.rds"))
dist_40<- readRDS(file = paste0(dists.root,"dists_40_field_2_4b.rds"))
dist_45<- readRDS(file = paste0(dists.root,"dists_45_field_2_4b.rds"))
dist_50<- readRDS(file = paste0(dists.root,"dists_50_field_2_4b.rds"))
dist_55<- readRDS(file = paste0(dists.root,"dists_55_field_2_4b.rds"))
dist_60<- readRDS(file = paste0(dists.root,"dists_60_field_2_4b.rds"))
dist_65<- readRDS(file = paste0(dists.root,"dists_65_field_2_4b.rds"))
dist_70<- readRDS(file = paste0(dists.root,"dists_70_field_2_4b.rds"))
dist_75<- readRDS(file = paste0(dists.root,"dists_75_field_2_4b.rds"))
dist_80<- readRDS(file = paste0(dists.root,"dists_80_field_2_4b.rds"))
dist_85<- readRDS(file = paste0(dists.root,"dists_85_field_2_4b.rds"))
dist_90<- readRDS(file = paste0(dists.root,"dists_90_field_2_4b.rds"))
dist_95<- readRDS(file = paste0(dists.root,"dists_95_field_2_4b.rds"))
dist_100<- readRDS(file = paste0(dists.root,"dists_100_field_2_4b.rds"))
dist_105<- readRDS(file = paste0(dists.root,"dists_105_field_2_4b.rds"))


# site data (what sort of data frame are we working with)
names(site.dat)
site.dat<- site.dat[,c(1:8,colsel,16,17:55)] # change column selection for target variable
site.dat<- site.dat[complete.cases(site.dat[,c(9,11:49)]),]
site.dat$target<- site.dat[,9]


##########################
# convert field to lab
names(site.dat)
vect<- c(3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5)
cnt<- 9 # target variable

for (j in 1:length(vect)){
  len<- length(which(site.dat$type == "F" & site.dat[,cnt] == vect[j]))
  if(len != 0){
    chg<- which(site.dat$type == "F" & site.dat[,cnt] == vect[j])
    
    # take a sample from a required distribution
    if(vect[j] == 3.5){
      sel.samp<- sample(dist_35,size = len, replace = T)}
    if(vect[j] == 4){
      sel.samp<- sample(dist_40,size = len, replace = T)}
    if(vect[j] == 4.5){
      sel.samp<- sample(dist_45,size = len, replace = T)}
    if(vect[j] == 5){
      sel.samp<- sample(dist_50,size = len, replace = T)}
    if(vect[j] == 5.5){
      sel.samp<- sample(dist_55,size = len, replace = T)}
    if(vect[j] == 6){
      sel.samp<- sample(dist_60,size = len, replace = T)}
    if(vect[j] == 6.5){
      sel.samp<- sample(dist_65,size = len, replace = T)}
    if(vect[j] == 7){
      sel.samp<- sample(dist_70,size = len, replace = T)}
    if(vect[j] == 7.5){
      sel.samp<- sample(dist_75,size = len, replace = T)}
    if(vect[j] == 8){
      sel.samp<- sample(dist_80,size = len, replace = T)}
    if(vect[j] == 8.5){
      sel.samp<- sample(dist_85,size = len, replace = T)}
    if(vect[j] == 9){
      sel.samp<- sample(dist_90,size = len, replace = T)}
    if(vect[j] == 9.5){
      sel.samp<- sample(dist_95,size = len, replace = T)}
    if(vect[j] == 10){
      sel.samp<- sample(dist_100,size = len, replace = T)}
    if(vect[j] == 10.5){
      sel.samp<- sample(dist_105,size = len, replace = T)}
    
    # change the values
    site.dat[chg,50]<- sel.samp}}
######################

# clean up and re-appoint data
names(site.dat)
site.dat<- site.dat[,c(1:9,50,10:49)]
lns<- length(which(site.dat$target < 0))
lns
if (lns != 0){site.dat<- site.dat[-which(site.dat$target < 0),]}
hist(site.dat$target)
summary(site.dat$target)
site.dat$Relief_geomorphons<- as.factor(site.dat$Relief_geomorphons)



# EXTERNAL DATA
# lab dat
names(ext.dat)
ext.dat<- ext.dat[,c(1:8,colsel,16,17:55)] # change column selection for target variable
ext.dat<- ext.dat[complete.cases(ext.dat[,c(9,11:49)]),]
ext.dat$target<- ext.dat[,9]


##########################
# convert field to lab
names(ext.dat)
vect<- c(3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5)
cnt<- 9 # target variable

for (j in 1:length(vect)){
  len<- length(which(ext.dat$type == "F" & ext.dat[,cnt] == vect[j]))
  if(len != 0){
    chg<- which(ext.dat$type == "F" & ext.dat[,cnt] == vect[j])
    
    # take a sample from a required distribution
    if(vect[j] == 3.5){
      sel.samp<- sample(dist_35,size = len, replace = T)}
    if(vect[j] == 4){
      sel.samp<- sample(dist_40,size = len, replace = T)}
    if(vect[j] == 4.5){
      sel.samp<- sample(dist_45,size = len, replace = T)}
    if(vect[j] == 5){
      sel.samp<- sample(dist_50,size = len, replace = T)}
    if(vect[j] == 5.5){
      sel.samp<- sample(dist_55,size = len, replace = T)}
    if(vect[j] == 6){
      sel.samp<- sample(dist_60,size = len, replace = T)}
    if(vect[j] == 6.5){
      sel.samp<- sample(dist_65,size = len, replace = T)}
    if(vect[j] == 7){
      sel.samp<- sample(dist_70,size = len, replace = T)}
    if(vect[j] == 7.5){
      sel.samp<- sample(dist_75,size = len, replace = T)}
    if(vect[j] == 8){
      sel.samp<- sample(dist_80,size = len, replace = T)}
    if(vect[j] == 8.5){
      sel.samp<- sample(dist_85,size = len, replace = T)}
    if(vect[j] == 9){
      sel.samp<- sample(dist_90,size = len, replace = T)}
    if(vect[j] == 9.5){
      sel.samp<- sample(dist_95,size = len, replace = T)}
    if(vect[j] == 10){
      sel.samp<- sample(dist_100,size = len, replace = T)}
    if(vect[j] == 10.5){
      sel.samp<- sample(dist_105,size = len, replace = T)}
    
    # change the values
    ext.dat[chg,50]<- sel.samp}}
######################

# clean up and re-appoint data
names(ext.dat)
ext.dat<- ext.dat[,c(1:9,50,10:49)]
lns<- length(which(ext.dat$target < 0))
lns
if (lns != 0){ext.dat<- ext.dat[-which(ext.dat$target < 0),]}
hist(ext.dat$target)
ext.dat$Relief_geomorphons<- as.factor(ext.dat$Relief_geomorphons)



# putting model diagnostics into tables
cal_diog<- matrix(NA, ncol= 7, nrow=its)
val_diog<- matrix(NA, ncol= 7, nrow=its)
ext_diog<- matrix(NA, ncol= 7, nrow=its)

# set up some matrices to put model outputs for each bootstrap iteration
target.mat<- matrix(NA, ncol = its, nrow = nrow(site.dat))
target.mat_ext<- matrix(NA, ncol = its, nrow = nrow(ext.dat))
pred.mat_c<- matrix(NA, ncol = its, nrow = nrow(site.dat))
residual.mat_c<- matrix(NA, ncol = its, nrow = nrow(site.dat))
pred.mat_v<- matrix(NA, ncol = its, nrow = nrow(site.dat))

# cycle throoug its number of realisations
for (zz in 1:its){
  # site data
  site.dat<- readRDS(paste0(data.root,"tern_soilpH4B_siteDat_covariates_20223008_CALVALDAT_ARD.rds"))
  new.col.names<- substr(x = names(site.dat)[17:54],start = 1, stop = nchar(names(site.dat)[17:54])-4)
  names(site.dat)[17:54]<- new.col.names
  # external validation data
  ext.dat<- readRDS(paste0(data.root,"tern_soilpH4B_siteDat_covariates_20223008_EXTERNAL_ARD.rds"))
  names(ext.dat)[17:54]<- new.col.names
  
  ###############################################
  ### get both datasets model ready
  # CAL VAL DATA
  # lab dat
  names(site.dat)
  site.dat<- site.dat[,c(1:8,colsel,16,17:55)] # change column selection for target variable
  site.dat<- site.dat[complete.cases(site.dat[,c(9,11:49)]),]
  site.dat$target<- site.dat[,9]
  
  
  ##########################
  # convert field to lab
  names(site.dat)
  vect<- c(3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5)
  cnt<- 9 # target variable
  
  for (j in 1:length(vect)){
    len<- length(which(site.dat$type == "F" & site.dat[,cnt] == vect[j]))
    if(len != 0){
      chg<- which(site.dat$type == "F" & site.dat[,cnt] == vect[j])
      
      # take a sample from a required distribution
      if(vect[j] == 3.5){
        sel.samp<- sample(dist_35,size = len, replace = T)}
      if(vect[j] == 4){
        sel.samp<- sample(dist_40,size = len, replace = T)}
      if(vect[j] == 4.5){
        sel.samp<- sample(dist_45,size = len, replace = T)}
      if(vect[j] == 5){
        sel.samp<- sample(dist_50,size = len, replace = T)}
      if(vect[j] == 5.5){
        sel.samp<- sample(dist_55,size = len, replace = T)}
      if(vect[j] == 6){
        sel.samp<- sample(dist_60,size = len, replace = T)}
      if(vect[j] == 6.5){
        sel.samp<- sample(dist_65,size = len, replace = T)}
      if(vect[j] == 7){
        sel.samp<- sample(dist_70,size = len, replace = T)}
      if(vect[j] == 7.5){
        sel.samp<- sample(dist_75,size = len, replace = T)}
      if(vect[j] == 8){
        sel.samp<- sample(dist_80,size = len, replace = T)}
      if(vect[j] == 8.5){
        sel.samp<- sample(dist_85,size = len, replace = T)}
      if(vect[j] == 9){
        sel.samp<- sample(dist_90,size = len, replace = T)}
      if(vect[j] == 9.5){
        sel.samp<- sample(dist_95,size = len, replace = T)}
      if(vect[j] == 10){
        sel.samp<- sample(dist_100,size = len, replace = T)}
      if(vect[j] == 10.5){
        sel.samp<- sample(dist_105,size = len, replace = T)}
      
      # change the values
      site.dat[chg,50]<- sel.samp}}
  ######################
  
  # clean up and re-appoint data
  names(site.dat)
  site.dat<- site.dat[,c(1:9,50,10:49)]
  lns<- length(which(site.dat$target < 0))
  lns
  if (lns != 0){site.dat<- site.dat[-which(site.dat$target < 0),]}
  hist(site.dat$target)
  summary(site.dat$target)
  site.dat$Relief_geomorphons<- as.factor(site.dat$Relief_geomorphons)
  
  # target variable data into matrix
  target.mat[,zz]<- site.dat$target
  
  
  # EXTERNAL DATA
  # lab dat
  names(ext.dat)
  ext.dat<- ext.dat[,c(1:8,colsel,16,17:55)] # change column selection for target variable
  ext.dat<- ext.dat[complete.cases(ext.dat[,c(9,11:49)]),]
  ext.dat$target<- ext.dat[,9]
  
  
  ##########################
  # convert field to lab
  names(ext.dat)
  vect<- c(3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5)
  cnt<- 9 # target variable
  
  for (j in 1:length(vect)){
    len<- length(which(ext.dat$type == "F" & ext.dat[,cnt] == vect[j]))
    if(len != 0){
      chg<- which(ext.dat$type == "F" & ext.dat[,cnt] == vect[j])
      
      # take a sample from a required distribution
      if(vect[j] == 3.5){
        sel.samp<- sample(dist_35,size = len, replace = T)}
      if(vect[j] == 4){
        sel.samp<- sample(dist_40,size = len, replace = T)}
      if(vect[j] == 4.5){
        sel.samp<- sample(dist_45,size = len, replace = T)}
      if(vect[j] == 5){
        sel.samp<- sample(dist_50,size = len, replace = T)}
      if(vect[j] == 5.5){
        sel.samp<- sample(dist_55,size = len, replace = T)}
      if(vect[j] == 6){
        sel.samp<- sample(dist_60,size = len, replace = T)}
      if(vect[j] == 6.5){
        sel.samp<- sample(dist_65,size = len, replace = T)}
      if(vect[j] == 7){
        sel.samp<- sample(dist_70,size = len, replace = T)}
      if(vect[j] == 7.5){
        sel.samp<- sample(dist_75,size = len, replace = T)}
      if(vect[j] == 8){
        sel.samp<- sample(dist_80,size = len, replace = T)}
      if(vect[j] == 8.5){
        sel.samp<- sample(dist_85,size = len, replace = T)}
      if(vect[j] == 9){
        sel.samp<- sample(dist_90,size = len, replace = T)}
      if(vect[j] == 9.5){
        sel.samp<- sample(dist_95,size = len, replace = T)}
      if(vect[j] == 10){
        sel.samp<- sample(dist_100,size = len, replace = T)}
      if(vect[j] == 10.5){
        sel.samp<- sample(dist_105,size = len, replace = T)}
      
      # change the values
      ext.dat[chg,50]<- sel.samp}}
  ######################
  
  # clean up and re-appoint data
  names(ext.dat)
  ext.dat<- ext.dat[,c(1:9,50,10:49)]
  lns<- length(which(ext.dat$target < 0))
  lns
  if (lns != 0){ext.dat<- ext.dat[-which(ext.dat$target < 0),]}
  hist(ext.dat$target)
  ext.dat$Relief_geomorphons<- as.factor(ext.dat$Relief_geomorphons)
  
  # target variable data for external data
  target.mat_ext[,zz]<- ext.dat$target
  
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
  
  
  # model tuning parameters
  tgrid <- expand.grid(
    .mtry = tuning.params$mtry[paramsf],
    .splitrule= as.character(tuning.params$splitrule[paramsf]),
    .min.node.size = tuning.params$nodesize[paramsf])
  
  names(DSM_data_c)
  str(DSM_data_c)
  ranger.model<-train(x= DSM_data_c[,12:50], y= DSM_data_c$target,
                      tuneGrid = tgrid, 
                      method = "ranger",
                      trControl =trainControl(method = "oob"), 
                      num.trees = 500, 
                      importance = 'impurity')
  
  
  summary(ranger.model)
  ranger.model
  
  ## capture output
  var_nm1<- paste0(model.out,"/",depth,"/", "aus_",vart,"_model_",depth, "_iteration_", zz, ".txt")
  
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
  mod_name<- paste0(model.out,"/",depth,"/","aus_",vart,"_fittedmodel_",depth,"_iteration_", zz, ".rds")
  saveRDS(ranger.model, file = mod_name)
  
  
  ## VALIDATION OF MODEL
  # predict on calibration data
  ranger.pred_c<-predict(ranger.model, DSM_data_c)
  cal_diog[zz, 1]<- 1
  cal_diog[zz, 2]<- zz
  cal_diog[zz, 3:7]<- as.matrix(goof(observed = DSM_data_c$target, predicted = ranger.pred_c, plot.it = T))
  
  # predict on validation data
  ranger.pred_v<-predict(ranger.model, DSM_data_v)
  val_diog[zz, 1]<- 1
  val_diog[zz, 2]<- zz
  val_diog[zz, 3:7]<- as.matrix(goof(observed = DSM_data_v$target, predicted = ranger.pred_v, plot.it = T))
  
  # predict on external data
  ranger.pred_ex<-predict(ranger.model, ext.dat)
  ext_diog[zz, 1]<- 1
  ext_diog[zz, 2]<- zz
  ext_diog[zz, 3:7]<- as.matrix(goof(observed = ext.dat$target, predicted = ranger.pred_ex, plot.it = T))
  
  ## Residual modeling using variogram
  pred_residual_c<- residual<- DSM_data_c$target - ranger.pred_c
  
  # put predictions into frames
  # calibration predictions
  pred.mat_c[inbag.uboots,zz]<- ranger.pred_c
  residual.mat_c[inbag.uboots,zz]<- pred_residual_c
  pred.mat_v[outbag.uboots,zz]<- ranger.pred_v
  
  # write outputs to file
  cal_frame<- as.data.frame(cal_diog)
  cal_frame<- cal_frame[complete.cases(cal_frame),]
  names(cal_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
  cal_name<- paste0(model.out,"ranger_CAL_diogs_",vart,"_",depth, ".txt")
  write.table(cal_frame,file = cal_name, sep = ",",row.names = F, col.names = T) 
  
  val_frame<- as.data.frame(val_diog)
  val_frame<- val_frame[complete.cases(val_frame),]
  names(val_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
  val_name<- paste0(model.out,"ranger_VAL_diogs_",vart,"_",depth,".txt")
  write.table(val_frame,file = val_name, sep = ",",row.names = F, col.names = T)
  
  ext_frame<- as.data.frame(ext_diog)
  ext_frame<- ext_frame[complete.cases(ext_frame),]
  names(ext_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
  ext_name<- paste0(model.out,"ranger_EXT_diogs_",vart,"_",depth,".txt")
  write.table(ext_frame,file = ext_name, sep = ",",row.names = F, col.names = T)
  
  
  #output observation and prediction for calibration sites
  names(DSM_data_c)
  calOut_dat<- DSM_data_c[,c(1:11)]
  calOut_dat$prediction<- ranger.pred_c
  cal_name1<- paste0(model.out,"/",depth,"/data_obs_preds/cal/","ranger_CAL_preds_", vart, "_depth_", depth,"_iteration_", zz, ".txt")
  write.table(calOut_dat,file = cal_name1, sep = ",",row.names = F, col.names = T) 
  
  #output observation and prediction for validation sites
  valOut_dat<- DSM_data_v[,c(1:11)]
  valOut_dat$prediction<- ranger.pred_v
  val_name1<- paste0(model.out,"/",depth,"/data_obs_preds/val/","ranger_VAL_preds_", vart, "_depth_", depth,"_iteration_", zz, ".txt")
  write.table(valOut_dat,file = val_name1, sep = ",",row.names = F, col.names = T)
  
  #output observation and prediction for external
  names(ext.dat)
  extOut_dat<- ext.dat[,c(1:11)]
  extOut_dat$prediction<- ranger.pred_ex
  ext_name1<- paste0(model.out,"/",depth,"/data_obs_preds/ext/","ranger_EXT_preds_", vart, "_depth_", depth,"_iteration_", zz, ".txt")
  write.table(extOut_dat,file = ext_name1, sep = ",",row.names = F, col.names = T) 
  
}

### save the predictions frames
# calibration
pred.mat_c<- as.data.frame(pred.mat_c)
pred.mat_c_rm<- rowMeans(pred.mat_c,na.rm = T)

residual.mat_c<- as.data.frame(residual.mat_c)
residual.mat_c_rm<- rowMeans(residual.mat_c,na.rm = T)


calOut_dat<- site.dat[,c(1:11)]
calOut_dat$prediction_avg<- pred.mat_c_rm
calOut_dat$residual_avg<- residual.mat_c_rm
cal_name1<- paste0(model.out,"/",depth,"/data_obs_preds/cal/","ranger_CAL_preds_average_summaries_", vart, "_depth_", depth, ".txt")
write.table(calOut_dat,file = cal_name1, sep = ",",row.names = F, col.names = T) 

# validation/ out
pred.mat_v<- as.data.frame(pred.mat_v)
pred.mat_v_rm<- rowMeans(pred.mat_v,na.rm = T)

pred.mat_v_tv<- as.data.frame(target.mat)
pred.mat_v_tv_rm<- rowMeans(pred.mat_v_tv,na.rm = T)

valOut_dat<- site.dat[,c(1:11)]
valOut_dat$prediction_avg<- pred.mat_v_rm
valOut_dat$target_avg<- pred.mat_v_tv_rm
valOut_dat$residual_avg<- pred.mat_v_tv_rm - pred.mat_v_rm
val_name1<- paste0(model.out,"/",depth,"/data_obs_preds/val/","ranger_VAL_preds_average_summaries_", vart, "_depth_", depth, ".txt")
write.table(valOut_dat,file = val_name1, sep = ",",row.names = F, col.names = T) 




# END

