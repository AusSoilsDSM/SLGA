### TERN Landscapes 
# Cation Exchange Capacity
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 17.11.21
# modified: 22.11.21

# CODE PURPOSE
# pedotransfer models of CEC both soil only and soil and environmental 
# Extension of fitted models to full dataset of texture and soc data where no cec recorded
##

#libraries
library(rgdal);library(sp);library(Cubist);library(raster);library(ithir);library(MASS)


# roots and functions
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/data_curation/"
model.outs<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/models/pedotransfer/"
data.outs<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/model_outs/pedotransfer/"
cov.root<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics_PCA_30m/"
aux.root<- "/datasets/work/af-tern-mal-deb/work/projects/openSpecs_2019/spectroscopyPlatform/github_site/spectra_scripts/rcodes/auxillary/"
# load model function for cubist bagging models
source(file = paste0(aux.root,"bagging_functions_cubist.R"))

# load pedotransfer function models
spatial_cubist_model<- readRDS(file = paste0(model.outs,"cubist_model_object_spatial.rds")) 
aspatial_cubist_model<- readRDS(file = paste0(model.outs,"cubist_model_object_aspatial.rds")) 


# covariate data
pnt.data.df<- readRDS(file = paste0(g.root,"pedotransferWork_4_allsoildata_4_cecExtension_covariateextraction.rds"))
names(pnt.data.df)

#Load model and apply
#make prediction on test data
names(pnt.data.df)
test.covdata<- pnt.data.df[, c(6,8:10,13:57)]
names(test.covdata)

soil_c.bag_predict<-predict_bag_CUBIST(model.bpls = spatial_cubist_model$model.bpls,
                                       newspec = test.covdata,
                                       nbag = 50,
                                       transform = 3,
                                       fudge = 0.1,
                                       PI = c(0.05,0.95),
                                       MSE = (spatial_cubist_model$oob_rmse_m)^2,
                                       out.dir = paste0(model.outs,"spatial/"))



# generate a number of model runs
model.bpls = spatial_cubist_model$model.bpls
newspec = test.covdata
nbag = 50
transform = 3
fudge = 0.1
PI = c(0.05,0.95)
MSE = (spatial_cubist_model$oob_rmse_m)^2
out.dir = paste0(model.outs,"spatial/")

n<-nrow(newspec)
pred.v <- matrix(0,nrow=n,ncol=nbag)

if(is.null(out.dir) == TRUE){
  for (ibag in 1:nbag) {
    pred.v[,ibag] <- predict(model.bpls[[ibag]], newdata = newspec)}}

if(is.null(out.dir) == FALSE){
  # list files
  model.files<- list.files(path = out.dir, pattern = ".rds", full.names = T)
  for (ibag in 1:length(model.files)) {
    spec.model<- readRDS(model.files[ibag])
    pred.v[,ibag] <- predict(spec.model, newdata = newspec)
    rm(spec.model)}}

# empirical mean and variance
pred.ave<-apply(pred.v, 1, mean)
pred.var<-apply(pred.v, 1, var)

# add systematic variance
pred.sd<- sqrt(pred.var + MSE)

# data.frame up
df<- data.frame(pred.ave,pred.sd)
df<- cbind(1000,df)
sims <- t(apply(df, 1, function(x) rnorm(x[1], mean=x[2], sd=x[3])))

if (transform == 1){sims<- sims - fudge}
if (transform == 2){sims<- (sims)^2 - fudge}
if (transform == 3){sims<- exp(sims) - fudge}

# select a subset of runs
sel.runs<- sample(ncol(sims), 50, replace = FALSE)
sel.runs
pred.quants<- sims[,sel.runs]
out.dat<- as.data.frame(pred.quants)
names(out.dat)<- paste0("sim_",1:50)

# link up with labels
names(pnt.data.df)
out.dat<- cbind(pnt.data.df[,1:13],out.dat)

# write to table
saveRDS(object = out.dat, file = paste0(data.outs,"spatial/pedotransferWork_4_allsoildata_4_cecExtension_modelsimruns__spatial_output.rds"))





# ASPATIAL pedotransfer modelling
names(pnt.data.df)

#make prediction on covariate data
test.covdata<- pnt.data.df[, c(6,8:10,13)]
names(test.covdata)

soil_c.bag_predict<-predict_bag_CUBIST(model.bpls = aspatial_cubist_model$model.bpls,
                                       newspec = test.covdata,
                                       nbag = 50,
                                       transform = 3,
                                       fudge = 0.1,
                                       PI = c(0.05,0.95),
                                       MSE = (aspatial_cubist_model$oob_rmse_m)^2,
                                       out.dir = paste0(model.outs,"aspatial/"))



# generate a number of model runs
model.bpls = aspatial_cubist_model$model.bpls
newspec = test.covdata
nbag = 50
transform = 3
fudge = 0.1
PI = c(0.05,0.95)
MSE = (aspatial_cubist_model$oob_rmse_m)^2
out.dir = paste0(model.outs,"aspatial/")

n<-nrow(newspec)
pred.v <- matrix(0,nrow=n,ncol=nbag)

if(is.null(out.dir) == TRUE){
  for (ibag in 1:nbag) {
    pred.v[,ibag] <- predict(model.bpls[[ibag]], newdata = newspec)}}

if(is.null(out.dir) == FALSE){
  # list files
  model.files<- list.files(path = out.dir, pattern = ".rds", full.names = T)
  for (ibag in 1:length(model.files)) {
    spec.model<- readRDS(model.files[ibag])
    pred.v[,ibag] <- predict(spec.model, newdata = newspec)
    rm(spec.model)}}

# empirical mean and variance
pred.ave<-apply(pred.v, 1, mean)
pred.var<-apply(pred.v, 1, var)

# add systematic variance
pred.sd<- sqrt(pred.var + MSE)

# data.frame up
df<- data.frame(pred.ave,pred.sd)
df<- cbind(1000,df)
sims <- t(apply(df, 1, function(x) rnorm(x[1], mean=x[2], sd=x[3])))

if (transform == 1){sims<- sims - fudge}
if (transform == 2){sims<- (sims)^2 - fudge}
if (transform == 3){sims<- exp(sims) - fudge}

# select a subset of runs
sel.runs<- sample(ncol(sims), 50, replace = FALSE)
sel.runs
pred.quants<- sims[,sel.runs]
out.dat<- as.data.frame(pred.quants)
names(out.dat)<- paste0("sim_",1:50)

# link up with labels
names(pnt.data.df)
out.dat<- cbind(pnt.data.df[,1:13],out.dat)

# write to table
saveRDS(object = out.dat, file = paste0(data.outs,"aspatial/pedotransferWork_4_allsoildata_4_cecExtension_modelsimruns__aspatial_output.rds"))


## END



