### TERN LANDSCAPES 
# Total N
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 15.11.23
# modified: 15.11.23

# CODE PURPOSE
# Model fitting using the best set of hyperparameters
# Depth 4
# All data coobs sims90


# fixed parameters
vart<- "totalN"
depth<- 4
type1<- "sims90"
type2<- "alldat"


# general root directory
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_N/"
# root directory
data.root<- paste0(gen.root, "data/coobs/d",depth,"/")
params.out<- paste0(gen.root, "data/coobs/d",depth,"/outs")
model.out<- paste0(gen.root,"models/geoms/")



# libraries
library(caret);library(ranger);library(terra);library(sf)


#data
site.dat<- readRDS(paste0(data.root,"pca_sites_covariates_90m_stack_wgs_geomwork_",vart,"_",depth,"_","allblocks.rds"))
names(site.dat)

# select variable
names(site.dat)[2:40]
names(site.dat)[c(41:52)]
site.dat$alldat_geom_bin<- NA
site.dat[which(site.dat$alldat_geom != 1),"alldat_geom_bin"] <- 0
site.dat[which(site.dat$alldat_geom == 1),"alldat_geom_bin"] <- 1
site.dat$alldat_geom_bin<- as.factor(site.dat$alldat_geom_bin)
summary(site.dat$alldat_geom_bin)

# remove the 0 values
site.dat.x<- site.dat[site.dat$alldat_geom_bin == 1, ]

names(site.dat.x)
sub.site.dat<- site.dat.x[,c(50,2:40)]
sub.site.dat<- sub.site.dat[complete.cases(sub.site.dat),]
sub.site.dat$Relief_geomorphons<- as.factor(sub.site.dat$Relief_geomorphons)
#sub.site.dat<- sub.site.dat[1:500,]

# target variable transformation
names(sub.site.dat)[1]<- "target"
hist(sub.site.dat$target)


# model tuning parameters
tgrid <- expand.grid(
  .mtry = 20,
  .splitrule= "variance",
  .min.node.size = 5)
tgrid


ranger.model<-train(x= sub.site.dat[,2:40], y= sub.site.dat$target,
                    tuneGrid = tgrid, 
                    method = "ranger",
                    trControl =trainControl(method = "oob"), 
                    num.trees = 500, 
                    importance = 'impurity')

summary(ranger.model)
ranger.model


# save model
saveRDS(object = ranger.model, file = paste0(model.out,vart, "_d", depth,"_",type1,"_",type2, "_modelfit.rds"))

## END



# END


