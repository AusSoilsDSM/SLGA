### TERN LANDSCAPES 
# pH modelling [coobs]
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 21.9.22
# modified: 21.9.22

# CODE PURPOSE
# Fit model using pre-figured hyperparameters
# Depth 4
# sims80


# fixed parameters
vart<- "ph_4b"
depth<- 4
type1<- "sims80"
type2<- "alldat"


# general root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
# root directory
data.root<- paste0(g.root, "outs/4B_meth/geoms/d",depth,"/")
params.out<- paste0(g.root,"models/",vart,"/d",depth, "/geoms/")
slurm.root<- paste0(g.root, "rcode/slurm/ph_4b/temp_outs/digital_soil_mapping/model_space/model_fitting/")


# libraries
library(caret);library(ranger);library(raster);library(rgdal);library(sp)


#data
site.dat<- readRDS(paste0(data.root,"pca_sites_covariates_90m_stack_wgs_geomwork_",vart,"_",depth,"_","allblocks.rds"))
names(site.dat)

# select variable
names(site.dat)[2:40]
names(site.dat)[c(41:51)]
site.dat$alldat_geom_bin<- NA
site.dat[which(site.dat$alldat_geom != 1),"alldat_geom_bin"] <- 0
site.dat[which(site.dat$alldat_geom == 1),"alldat_geom_bin"] <- 1
site.dat$alldat_geom_bin<- as.factor(site.dat$alldat_geom_bin)
summary(site.dat$alldat_geom_bin)

# remove the 0 values
site.dat.x<- site.dat[site.dat$alldat_geom_bin == 1, ]

names(site.dat.x)
sub.site.dat<- site.dat.x[,c(47,2:40)]
sub.site.dat<- sub.site.dat[complete.cases(sub.site.dat),]
sub.site.dat$Relief_geomorphons<- as.factor(sub.site.dat$Relief_geomorphons)
#sub.site.dat<- sub.site.dat[1:500,]


## Ranger model with cross validation
names(sub.site.dat)[1]<- "target"
#hist(sub.site.dat$target)
#hist(sqrt(sub.site.dat$target))
sub.site.dat$transformed<- sqrt(sub.site.dat$target)
# backtransformation
#bt<- (sub.site.dat$transformed)^2
#bt==sub.site.dat$target
#plot(bt,sub.site.dat$target)
#summary(sub.site.dat$transformed)
#names(sub.site.dat)

# model tuning parameters
tgrid <- expand.grid(
  .mtry = 20,
  .splitrule= "variance",
  .min.node.size = 5)
tgrid


ranger.model<-train(x= sub.site.dat[,2:40], y= sub.site.dat$transformed,
                    tuneGrid = tgrid, 
                    method = "ranger",
                    trControl =trainControl(method = "oob"), 
                    num.trees = 500, 
                    importance = 'impurity')

summary(ranger.model)
ranger.model


# save model
saveRDS(object = ranger.model, file = paste0(params.out,vart, "_", depth,"_",type1,"_",type2, "_modelfit.rds"))

## END



