### Soil Depth
### Model Fitting 
## each job fits 5 models
### modified 28/3/19
cnt=1 ## sort of a job identifier

## Ranger models

# libraries
library(caret);library(ranger);library(raster);library(rgdal);library(sp)

# source addtional functions
source("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/rcode/goof.R")

#data
# observations
obs_dat<- readRDS("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/sd_siteDat_covariates_lessthan2_20192207.rds")
str(obs_dat)

## remove the bore hole rock outcrop predictions
obs_dat<- obs_dat[obs_dat$type != "bore_rock",]


#hist(obs_dat$FromDepth)




# putting model diagnostics into tables
cal_diog<- matrix(NA, ncol= 7, nrow=5)
val_diog<- matrix(NA, ncol= 7, nrow=5)

## specify factor variables
obs_dat$Clim_agroclim_hutch<- as.factor(obs_dat$Clim_agroclim_hutch)
obs_dat$Clim_kpnall<- as.factor(obs_dat$Clim_kpnall)
obs_dat$relief_geomorphons<- as.factor(obs_dat$relief_geomorphons)



## 50 models for job
for (i in 1:50){  # colum 5 is where the target variable info begins
  
  ### data simulation
  obs_dat$simDat<- NA
  
  # simulate data for the zero depth observations
  #len_boreRock<- nrow(obs_dat[obs_dat$type == "bore_rock",])
  len_gaRock<- nrow(obs_dat[obs_dat$type == "ga_rocks",])
  
  #obs_dat[which(obs_dat$type == "bore_rock"), "simDat"]<- runif(len_boreRock, min = 0, max = 0.15)
  obs_dat[which(obs_dat$type == "ga_rocks"), "simDat"]<- runif(len_gaRock, min = 0, max = 0.10)
  
  # simulate data for censored observations
  len_censoredDat<- nrow(obs_dat[obs_dat$type == "ternsoil_censored",])
  brC<- c(rbeta(len_censoredDat, shape1 = 2, shape2 =5.5))
  censored_dat<- obs_dat[obs_dat$type == "ternsoil_censored", "FromDepth"]
  sim_censoreddat<- censored_dat + (brC * censored_dat)
  obs_dat[which(obs_dat$type == "ternsoil_censored"), "simDat"]<- sim_censoreddat
  
  # the other sites
  obs_dat[which(obs_dat$type == "bore_soil"), "simDat"]<- obs_dat[which(obs_dat$type == "bore_soil"), "FromDepth"]
  obs_dat[which(obs_dat$type == "ternSoil_actual"), "simDat"]<- obs_dat[which(obs_dat$type == "ternSoil_actual"), "FromDepth"]
  
  
  hist(obs_dat$FromDepth)
  hist(sqrt(obs_dat$simDat))
  #plot(obs_dat$FromDepth,obs_dat$simDat)
  
  # data transform
  obs_dat$simDat_t<- sqrt(obs_dat$simDat)
  hist(obs_dat$simDat_t)
  
  
  
  
  ## Training and calibration datasets
  training <- sample(nrow(obs_dat), 0.80 * nrow(obs_dat))
  
  # calibration data
  DSM_data_c<- obs_dat[training,]
    
  # validation
  DSM_data_v<- obs_dat[-training,]
    
  
  ## Model fitting 
  ## ranger model
  # hyper-parameter grid (as determined by cross-validation script)
  tgrid <- expand.grid(
    .mtry = 17,
    .splitrule= c("variance"),
    .min.node.size = c(5))
    
  #fit model
  names(obs_dat)
  ranger.model<-train(x= DSM_data_c[,c(6, 8:42)], y= DSM_data_c$simDat_t,
                        tuneGrid = tgrid, 
                        method = "ranger",
                        trControl =trainControl(method = "oob"), 
                        num.trees = 500, 
                        importance = 'impurity')  
    
    
  ## capture output
  var_nm1<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/","aus_soilDepth_model_variableImportance_batch_", cnt, "_iteration_", i, ".txt")
    
  out1<- capture.output(summary(ranger.model))
  cat(out1, file = var_nm1, sep=",", append = T)
  out2<- capture.output(ranger.model)
  cat(out2, file = var_nm1, sep=",", append = T)
  #capture variable importance
  varOutput<- varImp(ranger.model, scale=FALSE)
  out3<- capture.output(varOutput)
  cat(out3, file = var_nm1, sep=",", append = T)
    
  # save model to file
  mod_name<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/","aus_soilDepth_model_batch_", cnt, "_iteration_", i, ".rds")
  saveRDS(ranger.model, file = mod_name)
    
    
  ## VALIDATION OF MODEL
  # predict on calibration data
  ranger.pred_c<-predict(ranger.model, DSM_data_c)
  ranger.pred_c<- ranger.pred_c^2 # back-transform
    
  cal_diog[i, 1]<- cnt
  cal_diog[i, 2]<- i
  cal_diog[i, 3:7]<- as.matrix(goof(observed = DSM_data_c$simDat, predicted = ranger.pred_c, plot.it = F))
    
  # predict on validation data
  ranger.pred_v<-predict(ranger.model, DSM_data_v)
  ranger.pred_v<- ranger.pred_v^2
  val_diog[i, 1]<- cnt
  val_diog[i, 2]<- i
  val_diog[i, 3:7]<- as.matrix(goof(observed = DSM_data_v$simDat, predicted = ranger.pred_v, plot.it = F))
    
  # write outputs to file
  cal_frame<- as.data.frame(cal_diog)
  cal_frame<- cal_frame[complete.cases(cal_frame),]
  names(cal_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
  cal_name<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/","ranger_CAL_diogs_model_batch_", cnt, ".txt")
  write.table(cal_frame,file = cal_name, sep = ",",row.names = F, col.names = T) 
    
  val_frame<- as.data.frame(val_diog)
  val_frame<- val_frame[complete.cases(val_frame),]
  names(val_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
  val_name<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/","ranger_VAL_diogs_model_", cnt, ".txt")
  write.table(val_frame,file = val_name, sep = ",",row.names = F, col.names = T)
    
    
  #output observation and prediction for calibration sites
  calOut_dat<- DSM_data_c[,c(1,2,3,43:47)]
  calOut_dat$prediction<- ranger.pred_c
  cal_name1<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/","ranger_CAL_preds_batch_", cnt, "_iteration_", i, ".txt")
  write.table(calOut_dat,file = cal_name1, sep = ",",row.names = F, col.names = T) 
    
  #output observation and prediction for validation sites
  valOut_dat<- DSM_data_v[,c(1,2,3,43:47)]
  valOut_dat$prediction<- ranger.pred_v
  val_name1<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models/","ranger_VAL_preds_batch_", cnt, "_iteration_", i, ".txt")
  write.table(valOut_dat,file = val_name1, sep = ",",row.names = F, col.names = T)
  }


# END

    
    
    
    
  
    
    

