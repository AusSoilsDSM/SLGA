### Soil texture
### Model building
# variable: sand content
# depth: 4
# iteration: 4
cnt=4 ## BATCH NUMBER

# root
vart<- "sand"
root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/sand/d4/"
depth<- "d4"

# tuning grid optimisations
optimparams<- read.csv("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/optimal_tuning_params.csv")
optimparams<- optimparams[10,] # need to change for variable and depth

## External validation
ex.samps<- readRDS("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d4.clay.10per.sampled1.clay.10per.sample.rds")
ex.samps.dat<- readRDS("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d4_clay_external_val.rds")
names(ex.samps.dat)
ex.samps.dat$target<- ex.samps.dat[,7]
hist(ex.samps.dat$target)

## Ranger models
# rscript:  source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/modelfitting/clay/d1/
#                   rangerModelling_texture_clay_d1_2.R")
# slurm:    source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/clay/d1/slurm")


# libraries
library(caret);library(ranger)
# source addtional functions
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/goof.R")


#data
# covariate observations
cov.dat<- readRDS("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/tern_soiltexture_siteDat_covariates_202020305.rds")
str(cov.dat)

# morphological observations
morph.files<- list.files(path = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/", pattern = "morph_psa_data_splined_dsm_extract_prepared", full.names = T)
morph.files

# lab data
lab.dat<- readRDS("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/lab_psa_data_splined_dsm_extract_prepared.rds")

# putting model diagnostics into tables
cal_diog<- matrix(NA, ncol= 7, nrow=50)
val_diog<- matrix(NA, ncol= 7, nrow=50)
ext_diog<- matrix(NA, ncol= 7, nrow=50)



# 10 realisations
for (i in 31:40){


# load morphological data
morph.dat<- readRDS(morph.files[i])
# lab dat

names(lab.dat)
sub.lab.dat<- lab.dat[,c(1,2,4,5,6,12,20)]
test1<- cbind(sub.lab.dat, cov.dat[match(sub.lab.dat$num_obs_ID,cov.dat$num_obs_ID),])
test1<- test1[complete.cases(test1),]

# morphological data
names(morph.dat)
sub.morph.dat<- morph.dat[,c(1:5,11,19)]
test2<- cbind(sub.morph.dat, cov.dat[match(sub.morph.dat$num_obs_ID,cov.dat$num_obs_ID),])
test2<- test2[complete.cases(test2),]
names(test2)[4:5]<- c("Longitude", "Latitude")


test3<- rbind(test1,test2)
#hist(test3$`clay_5-15 cm`)

# clay model 
test4<- test3[-which(test3$`sand_30-60 cm`< -50),]
hist(test4$`sand_30-60 cm`)
test4$relief_geomorphons<- as.factor(test4$relief_geomorphons)
test4$target<- test4$`sand_30-60 cm`

## Ranger model with cross validation
## remove external dataset
test4<- test4[-ex.samps,]

## calibration and validation
boots<- sample.int(nrow(test4), replace = TRUE)
uboots<- unique(boots)

# calibration data
DSM_data_c<- test4[uboots,]

# validation
DSM_data_v<- test4[-uboots,]


# model tuning parameters
tgrid <- expand.grid(
  .mtry = optimparams$mtry,
  .splitrule= "extratrees",
  .min.node.size = optimparams$nodesize)

ranger.model<-train(x= DSM_data_c[,14:52], y= DSM_data_c$target,
                    tuneGrid = tgrid, 
                    method = "ranger",
                    trControl =trainControl(method = "oob"), 
                    num.trees = 500, 
                    importance = 'impurity')
             

summary(ranger.model)
ranger.model

## capture output
var_nm1<- paste0(root,"aus_",vart,"_model_",depth,"_variableImportance_batch_", cnt, "_iteration_", i, ".txt")

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
mod_name<- paste0(root,"aus_",vart,"_fittedmodel_",depth,"_batch_", cnt, "_iteration_", i, ".rds")
saveRDS(ranger.model, file = mod_name)


## VALIDATION OF MODEL
# predict on calibration data
ranger.pred_c<-predict(ranger.model, DSM_data_c)
cal_diog[i, 1]<- cnt
cal_diog[i, 2]<- i
cal_diog[i, 3:7]<- as.matrix(goof(observed = DSM_data_c$target, predicted = ranger.pred_c, plot.it = F))

# predict on validation data
ranger.pred_v<-predict(ranger.model, DSM_data_v)
val_diog[i, 1]<- cnt
val_diog[i, 2]<- i
val_diog[i, 3:7]<- as.matrix(goof(observed = DSM_data_v$target, predicted = ranger.pred_v, plot.it = F))

# predict on external data
ranger.pred_ex<-predict(ranger.model, ex.samps.dat)
ext_diog[i, 1]<- cnt
ext_diog[i, 2]<- i
ext_diog[i, 3:7]<- as.matrix(goof(observed = ex.samps.dat$target, predicted = ranger.pred_ex, plot.it = F))

# predict on all data
ranger.pred_all<-predict(ranger.model, test4)


# write outputs to file
cal_frame<- as.data.frame(cal_diog)
cal_frame<- cal_frame[complete.cases(cal_frame),]
names(cal_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
cal_name<- paste0(root,"ranger_CAL_diogs_",vart,"_",depth,"_model_batch_", cnt, ".txt")
write.table(cal_frame,file = cal_name, sep = ",",row.names = F, col.names = T) 

val_frame<- as.data.frame(val_diog)
val_frame<- val_frame[complete.cases(val_frame),]
names(val_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
val_name<- paste0(root,"ranger_VAL_diogs_",vart,"_",depth,"_model_batch_", cnt, ".txt")
write.table(val_frame,file = val_name, sep = ",",row.names = F, col.names = T)

ext_frame<- as.data.frame(ext_diog)
ext_frame<- ext_frame[complete.cases(ext_frame),]
names(ext_frame)<- c("col", "iter", "R2", "CCC","MSE", "RMSE", "bias")
ext_name<- paste0(root,"ranger_EXT_diogs_",vart,"_",depth,"_model_batch_", cnt, ".txt")
write.table(ext_frame,file = ext_name, sep = ",",row.names = F, col.names = T)


#output observation and prediction for calibration sites
names(DSM_data_c)
calOut_dat<- DSM_data_c[,c(1:5,53)]
calOut_dat$prediction<- ranger.pred_c
cal_name1<- paste0(root,"ranger_CAL_preds_", vart, "_", depth,"_batch_", cnt, "_iteration_", i, ".txt")
write.table(calOut_dat,file = cal_name1, sep = ",",row.names = F, col.names = T) 

#output observation and prediction for validation sites
valOut_dat<- DSM_data_v[,c(1:5,53)]
valOut_dat$prediction<- ranger.pred_v
val_name1<- paste0(root,"ranger_VAL_preds_", vart, "_", depth,"_batch_", cnt, "_iteration_", i, ".txt")
write.table(valOut_dat,file = val_name1, sep = ",",row.names = F, col.names = T)

#output observation and prediction for external
names(ex.samps.dat)
extOut_dat<- ex.samps.dat[,c(1:5,53)]
extOut_dat$prediction<- ranger.pred_ex
ext_name1<- paste0(root,"ranger_EXT_preds_", vart, "_", depth,"_batch_", cnt, "_iteration_", i, ".txt")
write.table(extOut_dat,file = ext_name1, sep = ",",row.names = F, col.names = T) 

#output observation and prediction all sites (to be used later for kriging)
allOut_dat<- test4[,c(1:5,53)]
allOut_dat$prediction<- ranger.pred_all
all_name1<- paste0(root,"ranger_ALL_preds_", vart, "_", depth,"_batch_", cnt, "_iteration_", i, ".txt")
write.table(allOut_dat,file = all_name1, sep = ",",row.names = F, col.names = T)
}


# END

