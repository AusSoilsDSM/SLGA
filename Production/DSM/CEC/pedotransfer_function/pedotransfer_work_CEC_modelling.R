### TERN Landscapes 
# Cation Exchange Capacity
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 17.11.21
# modified: 3.12.21

# CODE PURPOSE
# pedotransfer models of CEC both soil only and soil and environmental 
# Cubist modelling with bootstrap
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

# Analysis data
analysis.data<- readRDS(file = paste0(g.root,"pedotransferWork_3_allsoildata_pred_and_extension.rds"))
analysis.data.ext<- analysis.data[which(analysis.data$cec == -5555),]
analysis.data.mod<- analysis.data[which(analysis.data$cec != -5555),]


# any cleaning up
analysis.data.mod<- analysis.data.mod[-which(analysis.data.mod$cec < 0),]
hist(log(analysis.data.mod$cec +0.1))
analysis.data.mod$logcec<- log(analysis.data.mod$cec + 0.1)
hist(analysis.data.mod$clay)
hist(analysis.data.mod$sand)
hist(analysis.data.mod$silt)
hist(analysis.data.mod$soc)
analysis.data.mod<- analysis.data.mod[-which(analysis.data.mod$soc < 0),]
hist(log(analysis.data.mod$cec+0.1))
analysis.data.mod$logsoc<- log(analysis.data.mod$soc+0.1)
analysis.data.mod$upper<- as.factor(analysis.data.mod$upper)

# linear model
lm.model<- lm( logcec ~ clay + sand + silt + logsoc, data = analysis.data.mod)
summary(lm.model)

names(analysis.data.mod)
names(analysis.data.ext)
analysis.data.ext$logcec<- -5555
analysis.data.ext<- analysis.data.ext[-which(analysis.data.ext$soc < 0),]
analysis.data.ext$logsoc<- log(analysis.data.ext$soc+0.1)
analysis.data.ext$upper<- as.factor(analysis.data.ext$upper)

analysis.data<- rbind(analysis.data.mod,analysis.data.ext)

# bring in covariates
cov.files<- list.files(path = cov.root, full.names = T, recursive = T, pattern = ".tif")
cov.files<- cov.files[c(1:25,37:55)]
s1<- stack(cov.files)
s1
names(s1)

## intersect rasters with data
names(analysis.data)
coordinates(analysis.data)<- ~ Longitude + Latitude
pnt.data<- raster::extract(x = s1, y = analysis.data, sp=TRUE)
pnt.data.df<- as.data.frame(pnt.data)
names(pnt.data.df)
saveRDS(object = pnt.data.df, file = paste0(g.root,"pedotransferWork_4_allsoildata_4_covariateextraction.rds"))

#### CUBIST MODEL FITTING 
model.sims<- 50
# load data
pnt.data.df<- readRDS(file = paste0(g.root,"pedotransferWork_4_allsoildata_4_covariateextraction.rds"))

# split out the extension and modelling bits
pnt.data.df.ext<- pnt.data.df[which(pnt.data.df$cec == -5555),]
pnt.data.df.mod<- pnt.data.df[which(pnt.data.df$cec != -5555),]


# make a test data subset of size 1000
test.set <- sample(nrow(pnt.data.df.mod), 1000, replace = FALSE)
testset.data<- pnt.data.df.mod[test.set,]
# update data
pnt.data.df.mod<- pnt.data.df.mod[-test.set,]
length(unique(pnt.data.df.mod$uniq_loc))

# fit the model [with covariates]
names(pnt.data.df.mod)
cubist.model.covs <- cubist(x = pnt.data.df.mod[, c(6,8:10,15:59)], y = pnt.data.df.mod$logcec, cubistControl(extrapolation = 10), committees = 10)
summary(cubist.model.covs)

### bagging Cubist
soil.target<- pnt.data.df.mod$cec
hist(soil.target)
cov.data<- pnt.data.df.mod[, c(6,8:10,15:59)]
names(cov.data)
# fit model
bag_cubist<-fit_bag_CUBIST(spectra = cov.data, 
                           TV = soil.target,
                           nbag = model.sims,
                           transform=3, # 1 [none], 2 [sqrt], 3[log]
                           fudge=0.1,
                           rulz = 100,
                           extrap_sel = 10,
                           committee_sel = 10,
                           out.dir = paste0(model.outs,"spatial/"),
                           export.models = TRUE)
# save Model object
saveRDS(object = bag_cubist, file = paste0(model.outs,"cubist_model_object_spatial.rds"))

# Goodness of fit
# OOB
bag_cubist$oob_rmse_m
bag_cubist$oob_ccc_m
bag_cubist$oob_rmse_nu
bag_cubist$oob_ccc_nu

# in the bag
bag_cubist$cal_rmse_m
bag_cubist$cal_ccc_m
bag_cubist$cal_rmse_nu
bag_cubist$cal_ccc_nu

#make prediction on test data
test.covdata<- testset.data[, c(6,8:10,15:59)]
names(test.covdata)

soil_c.bag_predict<-predict_bag_CUBIST(model.bpls = bag_cubist$model.bpls,
                                       newspec = test.covdata,
                                       nbag = model.sims,
                                       transform = 3,
                                       fudge = 0.1,
                                       PI = c(0.05,0.95),
                                       MSE = (bag_cubist$oob_rmse_m)^2,
                                       out.dir = paste0(model.outs,"spatial/"))

# goof
goof(observed = testset.data$cec, predicted = soil_c.bag_predict$pred_50,type = "DSM",plot.it =T)
out.goof<- as.data.frame(goof(observed = testset.data$cec, predicted = soil_c.bag_predict$pred_50,type = "DSM",plot.it =T))
write.csv(x = out.goof, file = paste0(model.outs,"spatial/diognostics/spatial_pedotransfer_goof.csv"))

# make a plot of observed vs. predicted
tiff(file = paste0(model.outs,"spatial/diognostics/spatial_pedotransfer_goof.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
eqscplot(x = testset.data$cec, 
         y = soil_c.bag_predict$pred_50,
         xlab="observed",
         ylab="predicted",
         main="spatial_pedotransfer_goof_xyplot")
abline(a = 0, b = 1, col = "brown4")
dev.off()


# Derive simulation outcomes for all available data
cov.data<- pnt.data.df[, c(6,8:10,15:59)]
names(cov.data)
# generate a number of model runs
model.bpls = bag_cubist$model.bpls
newspec = cov.data
nbag = model.sims
transform = 3
fudge = 0.1
PI = c(0.05,0.95)
MSE = (bag_cubist$oob_rmse_m)^2
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
out.dat<- cbind(pnt.data.df[,1:15],out.dat)

# adjust the simulated values for the observed data
for (i in 1:nrow(out.dat)){
  sel.dat<- out.dat[i,]
  if (sel.dat$cec == -5555){
    next} else {
      sel.dat[,16:65]<- sel.dat$cec
      out.dat[i,]<- sel.dat}
  print(i)}


# write to table
saveRDS(object = out.dat, file = paste0(data.outs,"spatial/pedotransferWork_4_allsoildata_4_modelsimruns__spatial_output.rds"))




# fit the model [without covariates]
names(pnt.data.df.mod)
cubist.model <- cubist(x = pnt.data.df.mod[, c(6,8:10,15)], y = pnt.data.df.mod$logcec, cubistControl(extrapolation = 10), committees = 10)
summary(cubist.model)

### bagging Cubist
soil.target<- pnt.data.df.mod$cec
hist(soil.target)
cov.data<- pnt.data.df.mod[, c(6,8:10,15)]
names(cov.data)
# fit model
bag_cubist<-fit_bag_CUBIST(spectra = cov.data, 
                           TV = soil.target,
                           nbag = model.sims,
                           transform=3, # 1 [none], 2 [sqrt], 3[log]
                           fudge=0.1,
                           rulz = 100,
                           extrap_sel = 10,
                           committee_sel = 10,
                           out.dir = paste0(model.outs,"aspatial/"),
                           export.models = TRUE)

# save Model object
saveRDS(object = bag_cubist, file = paste0(model.outs,"cubist_model_object_aspatial.rds"))


# Goodness of fit
# OOB
bag_cubist$oob_rmse_m
bag_cubist$oob_ccc_m
bag_cubist$oob_rmse_nu
bag_cubist$oob_ccc_nu

# in the bag
bag_cubist$cal_rmse_m
bag_cubist$cal_ccc_m
bag_cubist$cal_rmse_nu
bag_cubist$cal_ccc_nu

#make prediction on test data
test.covdata<- testset.data[, c(6,8:10,15)]
names(test.covdata)

soil_c.bag_predict<-predict_bag_CUBIST(model.bpls = bag_cubist$model.bpls,
                                       newspec = test.covdata,
                                       nbag = model.sims,
                                       transform = 3,
                                       fudge = 0.1,
                                       PI = c(0.05,0.95),
                                       MSE = (bag_cubist$oob_rmse_m)^2,
                                       out.dir = paste0(model.outs,"aspatial/"))

# goof
goof(observed = testset.data$cec, predicted = soil_c.bag_predict$pred_50,type = "DSM",plot.it =T)
out.goof<- as.data.frame(goof(observed = testset.data$cec, predicted = soil_c.bag_predict$pred_50,type = "DSM",plot.it =T))
write.csv(x = out.goof, file = paste0(model.outs,"aspatial/diognostics/aspatial_pedotransfer_goof.csv"))

# make a plot of observed vs. predicted
tiff(file = paste0(model.outs,"aspatial/diognostics/aspatial_pedotransfer_goof.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
eqscplot(x = testset.data$cec, 
         y = soil_c.bag_predict$pred_50,
         xlab="observed",
         ylab="predicted",
         main="sspatial_pedotransfer_goof_xyplot")
abline(a = 0, b = 1, col = "brown4")
dev.off()

# generate a number of model runs
cov.data<- pnt.data.df[, c(6,8:10,15)]
names(cov.data)
# generate a number of model runs
model.bpls = bag_cubist$model.bpls
newspec = cov.data
nbag = model.sims
transform = 3
fudge = 0.1
PI = c(0.05,0.95)
MSE = (bag_cubist$oob_rmse_m)^2
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
out.dat<- cbind(pnt.data.df[,1:15],out.dat)

# adjust the simulated values for the observed data
for (i in 1:nrow(out.dat)){
  sel.dat<- out.dat[i,]
  if (sel.dat$cec == -5555){
    next} else {
      sel.dat[,16:65]<- sel.dat$cec
      out.dat[i,]<- sel.dat}
  print(i)}

# write to table
saveRDS(object = out.dat, file = paste0(data.outs,"aspatial/pedotransferWork_4_allsoildata_modelsimruns__aspatial_output.rds"))


## END



