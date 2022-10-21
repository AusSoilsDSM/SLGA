### TERN LANDSCAPES 
# Soil CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 2.12.21
# modified: 2.12.21

# CODE PURPOSE
# Figure out the hyperparameters to use for random forst model of CEC prediction

# root directory
data.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/model_outs/"
params.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/ranger_model_hyperparams/"
model.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/models/hyperparameter_fit/"
slurm.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/rcode/digital_soil_mapping/model_fitting/slurm/"
r.code<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/rcode/digital_soil_mapping/model_fitting/"


# libraries
library(caret);library(ranger);library(raster);library(rgdal);library(sp)


#data
# site data
site.dat<- readRDS(paste0(data.root,"tern_cec_siteDat_covariates_2021125.rds"))
names(site.dat)

site.dat$avgCEC<- rowMeans(site.dat[,16:65])
site.dat$log_avgCEC<- log(site.dat$avgCEC + 0.1)
as.factor(site.dat$upper)
site.dat$depth<- NA
site.dat$depth[which(site.dat$upper == 0)]<- 1
site.dat$depth[which(site.dat$upper == 5)]<- 2
site.dat$depth[which(site.dat$upper == 15)]<- 3
site.dat$depth[which(site.dat$upper == 30)]<- 4
site.dat$depth[which(site.dat$upper == 60)]<- 5
site.dat$depth[which(site.dat$upper == 100)]<- 6

# pull model dataframe together
names(site.dat)
summary(site.dat)
model.frame<- site.dat[,c(114,115,66:112)]

## Ranger model with cross validation
names(model.frame)


ranger.model<-train(x= model.frame[,2:49], y= model.frame$log_avgCEC,method = "ranger",trControl = trainControl(method = "oob"), num.trees =  500)  
summary(ranger.model)
ranger.model

## capture output
var_nm1<- paste0(params.out,"rangerModel_params_CEC.txt")

out1<- capture.output(summary(ranger.model))
out1<- paste0(out1,collapse = "\r\n")
cat(out1, file = var_nm1, sep=",", append = T)

out2<- capture.output(ranger.model)
out2<- paste0(out2,collapse = "\r\n")
cat(out2, file = var_nm1, sep=",", append = T)


saveRDS(ranger.model, paste0(model.out,"rangerModel_params_CEC.rds"))

