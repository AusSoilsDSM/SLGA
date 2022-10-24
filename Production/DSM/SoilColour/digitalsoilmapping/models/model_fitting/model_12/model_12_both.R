## Soil color model fitting
# Ranger models of L a b for 2 depths
# Predictand: L
# Covariates: Environs

library(ranger);library(caret)
root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/processed_1/"
capture.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/models/model_fitting/model_12/"
var_nm1<- paste0(capture.root,"rangerModel_params_surface_model_12.txt")
var_nm2<- paste0(capture.root,"rangerModel_params_subsoil_model_12.txt")
mod.out.sur<- paste0(capture.root,"surface_model_12.rds")
mod.out.sub<- paste0(capture.root,"subsoil_model_12.rds")
  

top.data<- readRDS(paste0(root,"tern_soilcolor_siteDat_covariates_surface_calset.rds"))
#### SURFACE MODELS
# B [continuous models]
# model tuning parameters
tgrid <- expand.grid(
  .mtry = 41,
  .splitrule= "variance",
  .min.node.size = 5)

names(top.data)
ranger.model1<-train(x= top.data[,c(9,10,12:50)], y= top.data$B, 
                     tuneGrid = tgrid,
                     method = "ranger",
                     trControl =trainControl(method = "oob"), 
                     num.trees = 500, 
                     importance = 'impurity')  
#save file
saveRDS(ranger.model1, file = mod.out.sur )

summary(ranger.model1)
ranger.model1
varImp(ranger.model1, scale=FALSE)

## capture output
out1<- capture.output(summary(ranger.model1))
out1<- paste0(out1,collapse = "\r\n")
cat(out1, file = var_nm1, sep=",", append = T)

out2<- capture.output(ranger.model1)
out2<- paste0(out2,collapse = "\r\n")
cat(out2, file = var_nm1, sep=",", append = T)

out3<- capture.output(varImp(ranger.model1, scale=FALSE))
out3<- paste0(out3,collapse = "\r\n")
cat(out3, file = var_nm1, sep=",", append = T)


sub.data<- readRDS(paste0(root,"tern_soilcolor_siteDat_covariates_subsoil_calset.rds"))
#### SUBSOIL MODELS
# B [continuous variable]
# model tuning parameters
tgrid <- expand.grid(
  .mtry = 41,
  .splitrule= "variance",
  .min.node.size = 5)

names(sub.data)
ranger.model1<-train(x= sub.data[,c(9,10,12:50)], y= sub.data$B, 
                     tuneGrid = tgrid,
                     method = "ranger",
                     trControl =trainControl(method = "oob"), 
                     num.trees = 500, 
                     importance = 'impurity')  

#save file
saveRDS(ranger.model1, file = mod.out.sub)


summary(ranger.model1)
ranger.model1
varImp(ranger.model1, scale=FALSE)

## capture output
out1<- capture.output(summary(ranger.model1))
out1<- paste0(out1,collapse = "\r\n")
cat(out1, file = var_nm2, sep=",", append = T)

out2<- capture.output(ranger.model1)
out2<- paste0(out2,collapse = "\r\n")
cat(out2, file = var_nm2, sep=",", append = T)

out3<- capture.output(varImp(ranger.model1, scale=FALSE))
out3<- paste0(out3,collapse = "\r\n")
cat(out3, file = var_nm2, sep=",", append = T)

#END


