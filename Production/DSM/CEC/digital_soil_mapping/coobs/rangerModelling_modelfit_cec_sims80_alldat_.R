### TERN LANDSCAPES 
# Soil cec model coobs parameters [model fitting] 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 16.2.22
# modified: 28.2.22

# CODE PURPOSE
# Fit model
# Depth 1 
# All data sims80 [sqrt(sims80)]


# fixed parameters
depth<- "d1"
type1<- "sims80"
type2<- "alldat"
vart<- "cec"

# general root directory
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/"
# root directory
data.root<- paste0(gen.root, "data/geoms/")
params.out<- paste0(gen.root,"data/ranger_model_hyperparams/geoms/")
model.out<- paste0(gen.root,"models/hyperparameter_fit/geoms/")
slurm.root<- paste0(gen.root, "rcode/digital_soil_mapping/coobs/slurm/")
r.code<- paste0(gen.root, "rcode/digital_soil_mapping/coobs/")


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
hist(sub.site.dat$target)
hist(sqrt(sub.site.dat$target))
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
saveRDS(object = ranger.model, file = paste0(model.out,vart, "_", depth,"_",type1,"_",type2, "_modelfit.rds"))

## END



