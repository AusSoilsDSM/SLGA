### Soil Depth
### Model Fitting 
## each job fits 5 models
## Modelling soil thickness binary value
## deep soils (>2m) vs. shallower soils
### modified 22/07/19
cnt=1 ## sort of a job identifier

## Ranger models

# libraries
library(caret);library(ranger);library(raster);library(rgdal);library(sp)

# source addtional functions
source("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/rcode/goof_cat.R")

#data
# observations
obs_dat<- readRDS("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/sd_siteDat_covariates_winnowed_20192207.rds")
str(obs_dat)

## remove the bore hole rock outcrop predictions
obs_dat<- obs_dat[obs_dat$type != "bore_rock",]


summary(obs_dat$depth_bin)


# putting model diagnostics into tables
cal_diog<- matrix(NA, ncol= 11, nrow=5)
val_diog<- matrix(NA, ncol= 11, nrow=5)

## specify factor variables
obs_dat$Clim_agroclim_hutch<- as.factor(obs_dat$Clim_agroclim_hutch)
obs_dat$Clim_kpnall<- as.factor(obs_dat$Clim_kpnall)
obs_dat$relief_geomorphons<- as.factor(obs_dat$relief_geomorphons)


## 50 models for job
for (i in 1:50){  # colum 5 is where the target variable info begins
  
  
  ## Training and calibration datasets
  training <- sample(nrow(obs_dat), 0.80 * nrow(obs_dat))
  
  # calibration data
  DSM_data_c<- obs_dat[training,]
    
  # validation
  DSM_data_v<- obs_dat[-training,]
    
  
  ## Model fitting 
  ## ranger model
  names(obs_dat)
  
  # hyper-parameter grid (as determined by cross-validation script)
  tgrid <- expand.grid(
    .mtry = 17,
    .splitrule= c("gini"),
    .min.node.size = c(5))
  
  #fit model
  ranger.model<-train(x= DSM_data_c[,c(6, 8:42)], y= DSM_data_c$depth_bin,
                       tuneGrid = tgrid, 
                       method = "ranger",
                       trControl =trainControl(method = "oob"), 
                       num.trees = 500, 
                       importance = 'impurity')
  
  ## capture output
  var_nm1<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/","aus_soilDepth_binary_model_variableImportance_batch_", cnt, "_iteration_", i, ".txt")
    
  out1<- capture.output(summary(ranger.model))
  cat(out1, file = var_nm1, sep=",", append = T)
  out2<- capture.output(ranger.model)
  cat(out2, file = var_nm1, sep=",", append = T)
  #capture variable importance
  varOutput<- varImp(ranger.model, scale=FALSE)
  out3<- capture.output(varOutput)
  cat(out3, file = var_nm1, sep=",", append = T)
    
  # save model to file
  mod_name<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/","aus_soilDepth_binary_model_batch_", cnt, "_iteration_", i, ".rds")
  saveRDS(ranger.model, file = mod_name)
    
    
  ## VALIDATION OF MODEL
  # predict on calibration data
  ranger.pred_c<-predict(ranger.model, DSM_data_c)
  
    
  cal_diog[i, 1]<- cnt
  cal_diog[i, 2]<- i
  
  # goof_cat function
  gc<- goof_cat(observed = DSM_data_c$depth_bin, predicted = ranger.pred_c)
  cal_diog[i, 3]<- gc$overall_accuracy
  cal_diog[i, 5:6]<- gc$producers_accuracy
  cal_diog[i, 8:9]<- gc$users_accuracy
  cal_diog[i, 11]<- gc$kappa
    
  
  # predict on validation data
  ranger.pred_v<-predict(ranger.model, DSM_data_v)
  
  val_diog[i, 1]<- cnt
  val_diog[i, 2]<- i
  gv<- goof_cat(observed = DSM_data_v$depth_bin, predicted = ranger.pred_v)
  val_diog[i, 3]<- gv$overall_accuracy
  val_diog[i, 5:6]<- gv$producers_accuracy
  val_diog[i, 8:9]<- gv$users_accuracy
  val_diog[i, 11]<- gv$kappa
    
  # write outputs to file
  cal_frame<- as.data.frame(cal_diog)
  cal_frame<- cal_frame[complete.cases(cal_frame[,1]),]
  names(cal_frame)<- c("col", "iter", "OA", "","PRA_0", "PRA_1", "","USA_0", "USA_1", "", "kappa")
  cal_name<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/","binaryModel_CAL_diogs_model_batch_", cnt, ".txt")
  write.table(cal_frame,file = cal_name, sep = ",",row.names = F, col.names = T) 
    
  val_frame<- as.data.frame(val_diog)
  val_frame<- val_frame[complete.cases(val_frame[,1]),]
  names(val_frame)<-  c("col", "iter", "OA", "","PRA_0", "PRA_1", "","USA_0", "USA_1", "", "kappa")
  val_name<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/","binaryModel_VAL_diogs_model_batch_", cnt, ".txt")
  write.table(val_frame,file = val_name, sep = ",",row.names = F, col.names = T)
    
    
  #output observation and prediction for calibration sites
  calOut_dat<- DSM_data_c[,c(1,2,3,7,43:45)]
  calOut_dat$prediction<- ranger.pred_c
  cal_name1<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/","C5_CAL_preds_batch_", cnt, "_iteration_", i, ".txt")
  write.table(calOut_dat,file = cal_name1, sep = ",",row.names = F, col.names = T) 
    
  #output observation and prediction for validation sites
  valOut_dat<- DSM_data_v[,c(1,2,3,7,43:45)]
  valOut_dat$prediction<- ranger.pred_v
  val_name1<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/","C5_VAL_preds_batch_", cnt, "_iteration_", i, ".txt")
  write.table(valOut_dat,file = val_name1, sep = ",",row.names = F, col.names = T)
  }



## END

    
    
    
    
    
  
    
    

