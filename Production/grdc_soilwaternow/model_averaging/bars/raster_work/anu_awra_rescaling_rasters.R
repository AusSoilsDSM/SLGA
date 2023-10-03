### GRDC soil-water NOW
# scaling AWRA and ANU rasters
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.3.22
# modified: 29.3.22

# CODE PURPOSE
# select files and resample to 90m
# 
library(terra);library(raster);library(rgdal);library(sp)

# rescaling function
# x is the raster to rescale
#theta_fc:  pedotransfer upper limit [sanji]
#theta_wp: pedotransfer lower limit [sanji]
#obs_fc: observed field capacity [smips]
#obs_min: observed lower limit [smips]
rescale.2p.lo<- function(x){(x[[2]]-x[[3]])/(x[[4]]-x[[5]]) * (x[[1]] - x[[4]]) + x[[2]]}

gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/models_output/"

# DUL and LL rasters
soil.rasters.path<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/GIS/soilwaterlimits/bars/"
soil.rasters<- list.files(path = soil.rasters.path,pattern = "resampled", full.names = T)
dul.raster<- raster(soil.rasters[[1]]) 
ll.raster<- raster(soil.rasters[[3]])

dul.obs<- dul.raster
dul.obs
dul.obs[]<- 1
dul.obs

ll.obs<- ll.raster
ll.obs
ll.obs[]<- 0
ll.obs



# AWRA rasters
awra.root<- paste0(gen.root,"AWRA/rasters/AwraAreas/Areas/BARS/")

awra.rasters<- list.files(path = awra.root,pattern = "rescaled", full.names = T)
awra.raster.april<- raster(awra.rasters[1])
awra.raster.july<- raster(awra.rasters[2])

# rescale raster [april]
rs.april<- stack(awra.raster.april,dul.raster, ll.raster, dul.obs, ll.obs)
rs.april
z1<- calc(rs.april,rescale.2p.lo)
plot(raster(soil.rasters[[2]]))
plot(z1*900)
z1.awc<- (z1* 900) -  raster(soil.rasters[[2]])
plot(z1.awc)
raster::writeRaster(x = z1.awc, filename = paste0(awra.root,"april2021_awra_bars_rescaled_awc_mm.tif"), format = "GTiff", datatype = "FLT4S", overwrite = TRUE)


# rescale raster [july]
rs.july<- stack(awra.raster.july,dul.raster, ll.raster, dul.obs, ll.obs)
rs.july
z2<- calc(rs.july,rescale.2p.lo)
plot(raster(soil.rasters[[2]]))
plot(z2*900)
z2.awc<- (z2* 900) -  raster(soil.rasters[[2]])
plot(z2.awc)
raster::writeRaster(x = z2.awc, filename = paste0(awra.root,"july2021_awra_bars_rescaled_awc_mm.tif"), format = "GTiff", datatype = "FLT4S", overwrite = TRUE)


# ANU rasters
anu.root<- paste0(gen.root,"ANU/rasters/")

anu.rasters<- list.files(path = anu.root,pattern = "resampled_90m", full.names = T)
anu.rasters
anu.raster.april<- raster(anu.rasters[1])
anu.raster.july<- raster(anu.rasters[2])

# rescale raster [april]
rs.april<- stack(anu.raster.april,dul.raster, ll.raster, dul.obs, ll.obs)
rs.april
z1<- calc(rs.april,rescale.2p.lo)
plot(raster(soil.rasters[[2]]))
plot(z1*900)
z1.awc<- (z1* 900) -  raster(soil.rasters[[2]])
plot(z1.awc)
raster::writeRaster(x = z1.awc, filename = paste0(anu.root,"april2021_anu_bars_rescaled_awc_mm.tif"), format = "GTiff", datatype = "FLT4S", overwrite = TRUE)


# rescale raster [july]
rs.july<- stack(anu.raster.july,dul.raster, ll.raster, dul.obs, ll.obs)
rs.july
z2<- calc(rs.july,rescale.2p.lo)
plot(raster(soil.rasters[[2]]))
plot(z2*900)
z2.awc<- (z2* 900) -  raster(soil.rasters[[2]])
plot(z2.awc)
raster::writeRaster(x = z2.awc, filename = paste0(anu.root,"july2021_anu_bars_rescaled_awc_mm.tif"), format = "GTiff", datatype = "FLT4S", overwrite = TRUE)



