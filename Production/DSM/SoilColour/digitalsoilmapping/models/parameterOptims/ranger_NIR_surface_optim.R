## Soil color parameterisations
# Ranger models of NIR

library(ranger);library(caret)
root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/processed_1/"
capture.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/models/parameter_selection/"

NIR.data<- readRDS(file = paste0(root,"tern_NIR_siteDat_covariates_20201007.rds"))
names(NIR.data)
NIR.data<- NIR.data[complete.cases(NIR.data[,18:56]),]

## optimal parameters for each model

#### SURFACE MODELS
#NIR predcition
ranger.model<-train(x= NIR.data[,c(18:56)], y= NIR.data$pred_NIR, 
                     method = "ranger",
                     trControl =trainControl(method = "oob"), 
                     num.trees = 500, 
                     importance = 'impurity')  


summary(ranger.model)
ranger.model
varImp(ranger.model, scale=FALSE)

## capture output
var_nm1<- paste0(capture.root,"rangerModel_params_surface_NIR.txt")

out1<- capture.output(summary(ranger.model))
out1<- paste0(out1,collapse = "\r\n")
cat(out1, file = var_nm1, sep=",", append = T)

out2<- capture.output(ranger.model)
out2<- paste0(out2,collapse = "\r\n")
cat(out2, file = var_nm1, sep=",", append = T)

out3<- capture.output(varImp(ranger.model, scale=FALSE))
out3<- paste0(out3,collapse = "\r\n")
cat(out3, file = var_nm1, sep=",", append = T)

#END


