### Soil Depth
### Process for kriging model residuals
### Load all fitted ranger models, apply all models to the data 
### Use only the censored data in this exercise 
### The script is essentially variogram fitting 

### modified 24/7/19
out.dir<- "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/spatialPrediction/tiles/"

## Ranger models

# libraries
library(caret);library(ranger);library(raster);library(rgdal);library(sp);library(gstat);library(automap);library(matrixStats)

# source addtional functions
source("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/rcode/goof.R")

#data
# observations
obs_dat<- readRDS("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/sd_siteDat_covariates_lessthan2_20192207.rds")
str(obs_dat)

## remove the bore hole rock outcrop predictions
obs_dat<- obs_dat[obs_dat$type != "bore_rock",]


### Get the required data
##
sel.dat<- obs_dat[which(obs_dat$type == "bore_soil" | obs_dat$type == "ternSoil_actual" ),]
summary(sel.dat$type)
# make factors of the categorical data
sel.dat$Clim_agroclim_hutch<- as.factor(sel.dat$Clim_agroclim_hutch)
sel.dat$Clim_kpnall<- as.factor(sel.dat$Clim_kpnall)
sel.dat$relief_geomorphons<- as.factor(sel.dat$relief_geomorphons)


# load the ranger models
mods<- list.files(path = "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/models", pattern = "aus_soilDepth_model_batch_", full.names = T)
mods  


# place to place model predictions
pred_mat<- matrix(NA, nrow = nrow(sel.dat), ncol = length(mods))
dim(pred_mat)


for (i in 1:length(mods)){
  
  # load the model
  smod<- readRDS(mods[i])
  names(smod$trainingData)
  
  # predict model (remember we are working in square root units)
  ranger.pred_c<-predict(smod, sel.dat)
  #ranger.pred_c<- ranger.pred_c^2 # back-transform
  
  # put prediction into matrix
  pred_mat[,i]<- ranger.pred_c}
  

# row medians
sel.dat$pred_median<- rowMedians(pred_mat)


# begin process for geostatiscal analysis
names(sel.dat)
sel.dat.sub<- sel.dat[,c(1,43,44,3,46)]
sel.dat.sub$sqrtObs<- sqrt(sel.dat.sub$FromDepth)


# coordinates
coordinates(sel.dat.sub)<- ~ Longitude + Latitude


## remove spatially duplicated points
sel.dat.sub<- remove.duplicates(sel.dat.sub)

# set coordinate reference system
crs(sel.dat.sub)<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# transform coordinates to projected
sel.dat.sub<- spTransform(sel.dat.sub,CRSobj = "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# make data frame
sel.dat.sub<- as.data.frame(sel.dat.sub)
names(sel.dat.sub)
sel.dat.sub$residual<- sel.dat.sub$sqrtObs - sel.dat.sub$pred_median
hist(sel.dat.sub$residual)

# select a subset of the data to be used for geostatisical analysis
training <- sample(nrow(sel.dat.sub), 0.80 * nrow(sel.dat.sub))

# data frame for variogram
varioDat<- sel.dat.sub[training,]
names(varioDat)
varioDat<- varioDat[,c(1,5:7)]
names(varioDat)[2:3]<- c("x", "y")

# save the other bit of the data
valdat<- sel.dat.sub[-training,]
saveRDS(valdat, "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/ranger_residuals_validation.rds")


# fit variogram
coordinates(varioDat)<- ~ x + y
vgm1<- variogram(residual~1, data = varioDat,width= 1000, cutoff=1500000)
afit<- autofitVariogram(residual~1, varioDat)
plot(afit)
afit
#afit # variogram parameters
plot(vgm1, model=afit$var_model)

### save the variogram point data and the variogram model
saveRDS(varioDat, "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/ranger_residuals_calibration.rds")
saveRDS(afit, "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/ranger_residuals_variogram.rds")



#END





















  



    
    
  
    
    

