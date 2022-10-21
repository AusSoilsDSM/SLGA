### Soil Depth
### Model Fitting 

## Ranger models

# libraries
library(caret);library(ranger);library(raster);library(rgdal);library(sp)


#data
# observations
obs_dat<- read.table("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/dbo_Horzons_loc_sub_step3_Out4.txt",header = T, sep=",")
str(obs_dat)

#covariates
cov_dat<- readRDS("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/siteDat_covariates.rds")
#str(cov_dat)
#plot(cov_dat)
#writeOGR(cov_dat, "Z:/projects/soilDepth_2019/soilDepth/data", "temp", driver="ESRI Shapefile")

cov_dat<- as.data.frame(cov_dat)
str(cov_dat)


### datframe for modelling
target<- obs_dat[,5] # select a column
  
# combine with covariate data
covs<- cov_dat[,3:37]
datFrame<- cbind(target,covs) 
  
# remove any missing values
which(!complete.cases(datFrame))
datFrame<- datFrame[complete.cases(datFrame),]
  
# take note of categorical variables
datFrame$Clim_agroclim_hutch<- as.factor(datFrame$Clim_agroclim_hutch)
datFrame$Clim_kpnall<- as.factor(datFrame$Clim_kpnall)
datFrame$Veg_HS_ICESatGLAS<- as.factor(datFrame$Veg_HS_ICESatGLAS)
datFrame$relief_geomorphons<- as.factor(datFrame$relief_geomorphons)
  
str(datFrame)
  
  
### train a model
training <- sample(nrow(datFrame), 10000)
DSM_data_c<- datFrame[-training,]
DSM_data_v<- datFrame[training,]
str(DSM_data_c)

## Ranger model with defaults
# does 25 bootstrapped samples
ranger.model<-train(x= DSM_data_c[,2:34], y= DSM_data_c$target,method = "ranger", num.trees = 1000)  
summary(ranger.model)
ranger.model
saveRDS(ranger.model, "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/rangerModel_params.rds")

# predict on calibration data
ranger.pred_c<-predict(ranger.model, DSM_data_c)
goof(observed = DSM_data_c$target, predicted = ranger.pred_c, plot.it = T)
 
# predict on calibration data
ranger.pred_v<-predict(ranger.model, DSM_data_v)
goof(observed = DSM_data_v$target, predicted = ranger.pred_v, plot.it = T)



# hyper-parameter grid (as determined by cross-validation script)
#tgrid <- expand.grid(
#  .mtry = 17,
#  .splitrule= c("variance"),
#  .min.node.size = c(5))



