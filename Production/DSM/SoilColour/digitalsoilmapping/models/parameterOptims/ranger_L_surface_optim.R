## Soil color parameterisations
# Ranger models of L a b for 2 depths

library(ranger);library(caret)
root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/processed_1/"
capture.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/models/parameter_selection/"

top.data<- readRDS(file = paste0(root,"tern_soilcolor_siteDat_covariates_surface_calset.rds"))
sub.data<- readRDS(file = paste0(root,"tern_soilcolor_siteDat_covariates_subsoil_calset.rds"))


## optimal parameters for each model

#### SURFACE MODELS
# L [categorical model]
names(top.data)
ranger.model1<-train(x= top.data[,c(12:50)], y= top.data$L, 
                     method = "ranger",
                     trControl =trainControl(method = "oob"), 
                     num.trees = 500, 
                     importance = 'impurity')  


summary(ranger.model1)
ranger.model1
varImp(ranger.model1, scale=FALSE)

## capture output
var_nm1<- paste0(capture.root,"rangerModel_params_surface_L.txt")

out1<- capture.output(summary(ranger.model1))
out1<- paste0(out1,collapse = "\r\n")
cat(out1, file = var_nm1, sep=",", append = T)

out2<- capture.output(ranger.model1)
out2<- paste0(out2,collapse = "\r\n")
cat(out2, file = var_nm1, sep=",", append = T)

out3<- capture.output(varImp(ranger.model1, scale=FALSE))
out3<- paste0(out3,collapse = "\r\n")
cat(out3, file = var_nm1, sep=",", append = T)

#END


