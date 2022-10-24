### TERN LANDSCAPES 
# Soil pH model model fitting
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 18.5.21
# modified: 18.5.21

# CODE PURPOSE
# # Apply model fits to all available data [excluding external data.
# need to estimate model residuals

# fixed parameters
vart<- "pH"
depth<- "d6"

# root directory
data.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/data/curated_all/"
model.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/models/"

# libraries
library(caret);library(ranger);library(raster);library(rgdal);library(sp);library(gstat);library(automap);library(matrixStats);library(ithir)


#data
# site data
site.dat<- readRDS(paste0(data.root,"tern_soilpH4a1_siteDat_covariates_CALVALDAT_SimulationAverages_d6.rds"))

### place where the models are situate
models<- list.files(path = paste0(model.out, depth), pattern = ".rds",full.names = TRUE)
models


empt.mat<- matrix(NA, nrow=nrow(site.dat),ncol=50)
for (i in 1:length(models)){
  
  pred.mod<- readRDS(models[i])
  
  # predict on calibration data
  ranger.pred_c<-predict(pred.mod, site.dat)
  print(goof(observed = site.dat$targetAVG, predicted = ranger.pred_c, plot.it = F))
  empt.mat[,i]<- ranger.pred_c}

predAVG<- rowMeans(empt.mat)

site.dat$predAVG<- predAVG
names(site.dat)
site.dat$predResidual<- site.dat$targetAVG - site.dat$predAVG
site.dat<- site.dat[,c(1:10,51,52,11:50)]
hist(site.dat$predResidual)
saveRDS(object = site.dat, file = paste0(data.root,"tern_soilpH4a1_siteDat_covariates_CALVALDAT_SimulationResiduals_d6.rds"))


# fit variogram
site.dat<- as.data.frame(site.dat)
names(site.dat)[4:5]<- c("x", "y")
coordinates(site.dat)<- ~ x + y
# set coordinate reference system
crs(site.dat)<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# transform coordinates to projected
site.dat<- spTransform(site.dat,CRSobj = "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
site.dat<- as.data.frame(site.dat)
names(site.dat)

coordinates(site.dat)<- ~ x + y
vgm1<- variogram(predResidual~1, data = site.dat,width= 1000, cutoff=1500000)
afit<- autofitVariogram(predResidual~1, site.dat)
plot(afit)
afit
#afit # variogram parameters
plot(vgm1, model=afit$var_model)

### save the variogram point data and the variogram model
saveRDS(site.dat, paste0(data.root,"variogram_dat/4a1/tern_soilpH4a1_siteDat_covariates_CALVALDAT_SimulationResiduals_d6_ARD.rds"))
saveRDS(afit, paste0(model.out,"variogram_models/4a1/residuals_variogram_4a1_d6.rds"))
