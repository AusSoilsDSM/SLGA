### GRDC soil-water NOW
# testing of raster model averaging
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.3.22
# modified: 5.4.22

# CODE PURPOSE
# apply raster average models
# 
library(terra);library(raster);library(rgdal);library(sp)

boundary<- readOGR("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/data/bars_shapes/BARS_boundary.shp")

gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/"
output.root<- paste0(gen.root,"analysis/bars/rasters/")

# Intercept,smips,anu,usyd,awra
params.april<- c(-33.351401,0.41068882,0.286817837,0.549727738,-0.332873243)
params.july<- c(86.82247885,0.746184025,-1.044285013,0.400696629,-0.882349701)

# input rasters [april 1]
smips.raster<- raster(paste0(gen.root,"data/models_output/SMIPS/rasters/SMIPSAreas/Areas/BARS/rescaled_april_smips.tif"))
plot(smips.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

anu.raster<- raster(paste0(gen.root,"data/models_output/ANU/rasters/april2021_anu_bars_rescaled_awc_mm.tif"))
plot(anu.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

usyd.raster<- raster(paste0(gen.root,"data/models_output/USYD/rasters/bars/rescaled_bars_april_usyd.tif")) 
plot(usyd.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

awra.raster<- raster(paste0(gen.root,"data/models_output/AWRA/rasters/AwraAreas/Areas/BARS/april2021_awra_bars_rescaled_awc_mm.tif")) 
plot(awra.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)


# perform model averaging equation
ma_april<- params.april[1] + (params.april[2]*smips.raster) + (params.april[3]*anu.raster) + (params.april[4]*usyd.raster) + (params.april[5]*awra.raster)
plot(ma_april)
plot(ma_april, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)
raster::writeRaster(x = ma_april,filename = paste0(output.root,"bars_bb_april2021_MAavgd_test.tif"), format = "GTiff", datatype = "FLT4S", overwrite = TRUE)



# input rasters [july 1]
smips.raster<- raster(paste0(gen.root,"data/models_output/SMIPS/rasters/SMIPSAreas/Areas/BARS/rescaled_july_smips.tif"))
plot(smips.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

anu.raster<- raster(paste0(gen.root,"data/models_output/ANU/rasters/july2021_anu_bars_rescaled_awc_mm.tif"))
plot(anu.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

usyd.raster<- raster(paste0(gen.root,"data/models_output/USYD/rasters/bars/rescaled_bars_july_usyd.tif")) ### need to change
plot(usyd.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

awra.raster<- raster(paste0(gen.root,"data/models_output/AWRA/rasters/AwraAreas/Areas/BARS/july2021_awra_bars_rescaled_awc_mm.tif")) ### need to change
plot(awra.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)


# perform model averaging equation
ma_july<- params.july[1] + (params.july[2]*smips.raster) + (params.july[3]*anu.raster) + (params.july[4]*usyd.raster) + (params.july[5]*awra.raster)
plot(ma_july, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)
raster::writeRaster(x = ma_july,filename = paste0(output.root,"bars_bb_july2021_MAavgd_test.tif"), format = "GTiff", datatype = "FLT4S", overwrite = TRUE)




# model averaging with covariates
covariate.files<- list.files(path = "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/GIS/covariates/bars/",pattern = ".tif", full.names = T)
covariate.files
cov.rasters<- stack(covariate.files)
cov.rasters
names(cov.rasters)

params<- c("Intercept","smips",	"anu",	"usyd",
           "awra",	"climate_PCA_1.COG", "climate_PCA_2.COG",	"climate_PCA_3.COG", "climate_PCA_4.COG",
           "parentmaterial_PCA_1.COG",	"parentmaterial_PCA_2.COG",	"parentmaterial_PCA_3.COG",	"parentmaterial_PCA_4.COG",
           "relief_PCA_1.COG",	"relief_PCA_2.COG",	"relief_PCA_3.COG",	"relief_PCA_4.COG")		



# Intercept,smips,anu,usyd,awra
params.april<- c(-114.567589, -0.779205955, 2.527912449, 0.316915435, 0.836474744, 239.278762, -102.7330198, -314.5879682, -1407.933634,
                 -14.39188531, 72.88281159, 21.18379771,	8.301031642,	61.58185823,	-13.85937609,	130.9730471,	-7.809339682)
length(params.april)

params.july<- c(-1701.353685, -0.841157711, 1.911140667, 0.521635239, 0.040671978, 1020.386133, -265.3965606, -389.0137045, -3196.999372,
                -73.14949917,	55.34964368,	34.35576902,	5.635530152,	17.42785121,	-2.576156484,	60.93911621,	24.59997513)


# perform model averaging equation
# input rasters [april 1]
smips.raster<- raster(paste0(gen.root,"data/models_output/SMIPS/rasters/SMIPSAreas/Areas/BARS/rescaled_april_smips.tif"))
plot(smips.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

anu.raster<- raster(paste0(gen.root,"data/models_output/ANU/rasters/april2021_anu_bars_rescaled_awc_mm.tif"))
plot(anu.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

usyd.raster<- raster(paste0(gen.root,"data/models_output/USYD/rasters/bars/rescaled_bars_april_usyd.tif")) 
plot(usyd.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

awra.raster<- raster(paste0(gen.root,"data/models_output/AWRA/rasters/AwraAreas/Areas/BARS/april2021_awra_bars_rescaled_awc_mm.tif")) 
plot(awra.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

ma_april.covs<- params.april[1] + (params.april[2]*smips.raster) + (params.april[3]*anu.raster) + (params.april[4]*usyd.raster) + (params.april[5]*awra.raster) + (params.april[6]*cov.rasters[[1]]) +
  (params.april[7]*cov.rasters[[2]]) + (params.april[8]*cov.rasters[[3]]) + (params.april[9]*cov.rasters[[4]]) + (params.april[10]*cov.rasters[[5]]) + (params.april[11]*cov.rasters[[6]]) + 
  (params.april[12]*cov.rasters[[7]]) + (params.april[13]*cov.rasters[[8]]) + (params.april[14]*cov.rasters[[9]]) + (params.april[15]*cov.rasters[[10]]) + (params.april[16]*cov.rasters[[11]]) + (params.april[17]*cov.rasters[[12]])
plot(ma_april.covs)
plot(ma_april.covs, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)


raster::writeRaster(x = ma_april.covs,filename = paste0(output.root,"bars_bb_april2021_MAavgd_test_covs.tif"), format = "GTiff", datatype = "FLT4S", overwrite = TRUE)

# JULY
# perform model averaging equation
# input rasters [july 1]
smips.raster<- raster(paste0(gen.root,"data/models_output/SMIPS/rasters/SMIPSAreas/Areas/BARS/rescaled_july_smips.tif"))
plot(smips.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

anu.raster<- raster(paste0(gen.root,"data/models_output/ANU/rasters/july2021_anu_bars_rescaled_awc_mm.tif"))
plot(anu.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

usyd.raster<- raster(paste0(gen.root,"data/models_output/USYD/rasters/bars/rescaled_bars_july_usyd.tif")) ### need to change
plot(usyd.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

awra.raster<- raster(paste0(gen.root,"data/models_output/AWRA/rasters/AwraAreas/Areas/BARS/july2021_awra_bars_rescaled_awc_mm.tif")) ### need to change
plot(awra.raster, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)

ma_july.covs<- params.july[1] + (params.july[2]*smips.raster) + (params.july[3]*anu.raster) + (params.july[4]*usyd.raster) + (params.july[5]*awra.raster) + (params.july[6]*cov.rasters[[1]]) +
  (params.july[7]*cov.rasters[[2]]) + (params.july[8]*cov.rasters[[3]]) + (params.july[9]*cov.rasters[[4]]) + (params.july[10]*cov.rasters[[5]]) + (params.july[11]*cov.rasters[[6]]) + 
  (params.july[12]*cov.rasters[[7]]) + (params.july[13]*cov.rasters[[8]]) + (params.july[14]*cov.rasters[[9]]) + (params.july[15]*cov.rasters[[10]]) + (params.july[16]*cov.rasters[[11]]) + (params.july[17]*cov.rasters[[12]])
plot(ma_july.covs)
plot(ma_july.covs, col=rev(terrain.colors(255)),zlim=c(20,140))
plot(boundary,add=T)


raster::writeRaster(x = ma_july.covs,filename = paste0(output.root,"bars_bb_july2021_MAavgd_test_covs.tif"), format = "GTiff", datatype = "FLT4S", overwrite = TRUE)


