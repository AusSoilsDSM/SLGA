### TERN LANDSCAPES 
# Soil pH model hyperparam optimisation model extrapolation parameters
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 2.9.21
# modified: 2.9.21

# CODE PURPOSE
# Figure out the hyperparameters to use 
# Depth 3
# lab data geometry


# fixed parameters
vart<- "pH_4a1"
depth<- "d3"


# general root directory
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
# root directory
data.root<- paste0(gen.root, "data/geoms/")
params.out<- paste0(gen.root,"data/ranger_model_hyperparams/geoms/")
model.out<- paste0(gen.root,"models/geoms/")
slurm.root<- paste0(gen.root, "rcode/digital_soil_mapping/model_fitting/geoms/slurm/")
r.code<- paste0(gen.root, "rcode/digital_soil_mapping/model_fitting/geoms/")


# libraries
library(caret);library(ranger);library(raster);library(rgdal);library(sp)


#data
site.dat<- readRDS(paste0(data.root,"pca_sites_covariates_90m_stack_wgs_geomwork_",vart,"_",depth,"_","allblocks.rds"))
names(site.dat)

# select variable
names(site.dat)[2:40]
names(site.dat)[c(41:53)]
site.dat$labdat_geom_bin<- NA
site.dat[which(site.dat$labdat_geom != 1),"labdat_geom_bin"] <- 0
site.dat[which(site.dat$labdat_geom == 1),"labdat_geom_bin"] <- 1
site.dat$labdat_geom_bin<- as.factor(site.dat$labdat_geom_bin)
summary(site.dat$labdat_geom_bin)

sub.site.dat<- site.dat[,c(53,2:40)]
sub.site.dat<- sub.site.dat[complete.cases(sub.site.dat),]
sub.site.dat$Relief_geomorphons<- as.factor(sub.site.dat$Relief_geomorphons)
#sub.site.dat<- sub.site.dat[1:500,]


## Ranger model with cross validation
names(sub.site.dat)

#tgrid <- expand.grid(
#  .mtry = 7:17,
#  .splitrule= c("variance", "extratrees"),
#  .min.node.size = c(1:10)
#)

ranger.model<-train(x= sub.site.dat[,2:40], y= sub.site.dat$labdat_geom_bin,method = "ranger",trControl = trainControl(method = "oob"), num.trees =  500)  
summary(ranger.model)
ranger.model

## capture output
var_nm1<- paste0(params.out,"rangerModel_labdat_geom_params_",vart,"_",depth,".txt")

out1<- capture.output(summary(ranger.model))
out1<- paste0(out1,collapse = "\r\n")
cat(out1, file = var_nm1, sep=",", append = T)

out2<- capture.output(ranger.model)
out2<- paste0(out2,collapse = "\r\n")
cat(out2, file = var_nm1, sep=",", append = T)


# END



