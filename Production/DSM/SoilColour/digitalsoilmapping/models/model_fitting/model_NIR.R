## Soil color model fitting
# Ranger models NIR surface


library(ranger);library(caret);library(ithir)
root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/processed_1/"
capture.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/models/model_fitting/NIR/"

NIR.data<- readRDS(paste0(root,"tern_NIR_siteDat_covariates_20201007.rds"))
names(NIR.data)[2:3]
NIR.data<- NIR.data[complete.cases(NIR.data[,18:56]),]

#external validation
## calibration and validation
boots<- sample(1:nrow(NIR.data), 500, replace = F)
saveRDS(boots, file = paste0(capture.root,"external_sample_rows.rds"))

ext.data<- NIR.data[boots,]
NIR.data<- NIR.data[-boots,]


#cluster coordinates
c_dat<- scale(NIR.data[,2:3])
cluster_dat<- kmeans(c_dat, centers=13)
summary(as.factor(cluster_dat$cluster))
NIR.data$cluster<- cluster_dat$cluster

# putting model diagnostics into tables
cal_diog<- matrix(NA, ncol= 7, nrow=100)
val_diog<- matrix(NA, ncol= 7, nrow=100)
ext_diog<- matrix(NA, ncol= 7, nrow=100)


for (i in 1:100){
  
  # calibration and validation data
  samps<- sample(1:13,11,replace = F)
  samps
  mod.dat<- NIR.data[which(NIR.data$cluster %in% samps),]
  val.dat<- NIR.data[-which(NIR.data$cluster %in% samps),]
  
  # model hyperparameters
  tgrid <- expand.grid(
    .mtry = 39,
    .splitrule= "extratrees",
    .min.node.size = 5)
  
  names(mod.dat)
  ranger.model<-train(x= mod.dat[,c(18:56)], y= mod.dat$pred_NIR,
                      tuneGrid = tgrid,
                      method = "ranger",
                      trControl =trainControl(method = "oob"), 
                      num.trees = 500, 
                      importance = 'impurity')  
  #save file
  mod.out.sur<- paste0(capture.root,"NIR_model_",i,".rds")
  saveRDS(ranger.model, file = mod.out.sur )
  
  summary(ranger.model)
  ranger.model
  varImp(ranger.model, scale=FALSE)
  
  ## capture output
  var_nm1<- paste0(capture.root,"rangerModel_params_surface_model_",i,".txt")
  out1<- capture.output(summary(ranger.model))
  out1<- paste0(out1,collapse = "\r\n")
  cat(out1, file = var_nm1, sep=",", append = T)
  
  out2<- capture.output(ranger.model)
  out2<- paste0(out2,collapse = "\r\n")
  cat(out2, file = var_nm1, sep=",", append = T)
  
  out3<- capture.output(varImp(ranger.model, scale=FALSE))
  out3<- paste0(out3,collapse = "\r\n")
  cat(out3, file = var_nm1, sep=",", append = T)
  
  out4<- capture.output(samps)
  out4<- paste0(out4,collapse = "\r\n")
  cat(out4, file = var_nm1, sep=",", append = T)
  
  
  # cal/val stats
  # predict on calibration data
  ranger.pred_c<-predict(ranger.model, mod.dat)
  cal_diog[i, 1]<- i
  cal_diog[i, 2]<- i
  cal_diog[i, 3:7]<- as.matrix(goof(observed = mod.dat$pred_NIR, predicted = ranger.pred_c, plot.it = F))
  
  # predict on validation data
  ranger.pred_v<-predict(ranger.model, val.dat)
  val_diog[i, 1]<- i
  val_diog[i, 2]<- i
  val_diog[i, 3:7]<- as.matrix(goof(observed = val.dat$pred_NIR, predicted = ranger.pred_v, plot.it = F))
  
  # predict on external data
  ranger.pred_e<-predict(ranger.model, ext.data)
  ext_diog[i, 1]<- i
  ext_diog[i, 2]<- i
  ext_diog[i, 3:7]<- as.matrix(goof(observed = ext.data$pred_NIR, predicted = ranger.pred_e, plot.it = F))
  
  # write outputs to file
  cal_frame<- as.data.frame(cal_diog)
  cal_frame<- cal_frame[complete.cases(cal_frame),]
  names(cal_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
  cal_name<- paste0(capture.root,"ranger_CAL_diogs_NIR.txt")
  write.csv(cal_frame,file = cal_name) 
  
  val_frame<- as.data.frame(val_diog)
  val_frame<- val_frame[complete.cases(val_frame),]
  names(val_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
  val_name<- paste0(capture.root,"ranger_VAL_diogs_NIR.txt")
  write.csv(val_frame,file = val_name)
  
  ext_frame<- as.data.frame(ext_diog)
  ext_frame<- ext_frame[complete.cases(ext_frame),]
  names(ext_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
  ext_name<- paste0(capture.root,"ranger_EXT_diogs_NIR.txt")
  write.csv(ext_frame,file = ext_name)
  
  print(i)}


#END


