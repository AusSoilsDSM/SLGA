### Soil texture
### Model Fitting: Parameter optimisation
# variable: sand content
# depth: 2

## Ranger models

# libraries
library(caret);library(ranger);library(raster);library(rgdal);library(sp)


#data
# covariate observations
cov.dat<- readRDS("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/tern_soiltexture_siteDat_covariates_202020305.rds")
str(cov.dat)

# morphological observations
morph.dat<- readRDS("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/morph_psa_data_splined_dsm_extract_prepared_sim_1.rds")

# lab data
lab.dat<- readRDS("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/lab_psa_data_splined_dsm_extract_prepared.rds")


# select variable
# lab dat
names(lab.dat)
sub.lab.dat<- lab.dat[,c(1,2,4,5,6,10,18)]
test1<- cbind(sub.lab.dat, cov.dat[match(sub.lab.dat$num_obs_ID,cov.dat$num_obs_ID),])
test1<- test1[complete.cases(test1),]

# morphological data
names(morph.dat)
sub.morph.dat<- morph.dat[,c(1:5,9,17)]
test2<- cbind(sub.morph.dat, cov.dat[match(sub.morph.dat$num_obs_ID,cov.dat$num_obs_ID),])
test2<- test2[complete.cases(test2),]
names(test2)[4:5]<- c("Longitude", "Latitude")


test3<- rbind(test1,test2)
hist(test3$`sand_5-15 cm`)

# clay model 
test4<- test3[-which(test3$`sand_5-15 cm`== -9999),]
hist(test4$`sand_5-15 cm`)
test4$relief_geomorphons<- as.factor(test4$relief_geomorphons)
test4$target<- test4$`sand_5-15 cm`

## Ranger model with cross validation
names(test4)

#tgrid <- expand.grid(
#  .mtry = 7:17,
#  .splitrule= c("variance", "extratrees"),
#  .min.node.size = c(1:10)
#)

ranger.model<-train(x= test4[,14:52], y= test4$target,method = "ranger",trControl = trainControl(method = "oob"), num.trees =  500)  

summary(ranger.model)
ranger.model

## capture output
var_nm1<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/parameterOptimisation/rangerModel_params_sand_d2.txt"

out1<- capture.output(summary(ranger.model))
out1<- paste0(out1,collapse = "\r\n")
cat(out1, file = var_nm1, sep=",", append = T)

out2<- capture.output(ranger.model)
out2<- paste0(out2,collapse = "\r\n")
cat(out2, file = var_nm1, sep=",", append = T)


saveRDS(ranger.model, "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/parameterOptimisation/rangerModel_params_sand_d2.rds")





