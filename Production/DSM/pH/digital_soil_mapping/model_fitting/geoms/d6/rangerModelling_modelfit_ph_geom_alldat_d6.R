### TERN LANDSCAPES 
# Soil pH model extrapolation parameters [model fitting] 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 6.9.21
# modified: 6.9.21

# CODE PURPOSE
# Fit model
# Depth 1 [0-5cm]
# All data geom


# fixed parameters
hypx<- 6
depth<- "d6"
type1<- "geom"
type2<- "alldat"
vart<- "pH_4a1"

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

# model hyperparameters
hyperparams<- read.csv(file = paste0(params.out,"geoms_all_hyperparams.csv"))


#data
site.dat<- readRDS(paste0(data.root,"pca_sites_covariates_90m_stack_wgs_geomwork_",vart,"_",depth,"_","allblocks.rds"))
names(site.dat)

# select variable
names(site.dat)[2:40]
names(site.dat)[c(41:53)]
site.dat$alldat_geom_bin<- NA
site.dat[which(site.dat$alldat_geom != 1),"alldat_geom_bin"] <- 0
site.dat[which(site.dat$alldat_geom == 1),"alldat_geom_bin"] <- 1
site.dat$alldat_geom_bin<- as.factor(site.dat$alldat_geom_bin)
summary(site.dat$alldat_geom_bin)

sub.site.dat<- site.dat[,c(53,2:40)]
sub.site.dat<- sub.site.dat[complete.cases(sub.site.dat),]
sub.site.dat$Relief_geomorphons<- as.factor(sub.site.dat$Relief_geomorphons)
#sub.site.dat<- sub.site.dat[1:500,]


## Ranger model 
names(sub.site.dat)

# model tuning parameters
tgrid <- expand.grid(
  .mtry = hyperparams$mtry[hypx],
  .splitrule= as.character(hyperparams$splitrule[hypx]),
  .min.node.size = hyperparams$min_node_size[hypx])
tgrid


ranger.model<-train(x= sub.site.dat[,2:40], y= sub.site.dat$alldat_geom_bin,
                    tuneGrid = tgrid, 
                    method = "ranger",
                    trControl =trainControl(method = "oob"), 
                    num.trees = 500, 
                    importance = 'impurity')


summary(ranger.model)
ranger.model


saveRDS(object = ranger.model, file = paste0(model.out,vart, "_", depth,"_",type1,"_",type2, "_modelfit.rds"))

## END



