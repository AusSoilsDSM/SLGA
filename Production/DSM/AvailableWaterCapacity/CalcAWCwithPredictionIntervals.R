### TERN LANDSCAPES 
# AWC 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.8.22
# modified: 29.8.22

# CODE PURPOSE
# Calculate AWC from subtracting LL15 from DUL
# test code in order to take account of prediction variance 
# apply to small area and for single depth

library(raster)

data.root<- "/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/"
base.root<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/90m/1000/"

# dul rasters
dul.rasters<- list.files(path = paste0(data.root, "DUL/"), pattern = "000_005",full.names = T)
# ll raster
ll15.rasters<- list.files(path = paste0(data.root, "L15/"), pattern = "000_005",full.names = T)
# allrasters
all.raster<- c(dul.rasters,ll15.rasters)
all.raster

all.raster<- stack(all.raster)
all.raster
names(all.raster)

### work on a small area the demo workflow
base.raster<- raster(paste0(base.root, "Clim_ADI.tif"))
base.raster

# crop raster
all.raster.crop<- projectRaster(from = all.raster, to = base.raster)
all.raster.crop
names(all.raster)

#### calculate AWC... DUL - LL15
awc.raster<- all.raster.crop[[3]] - all.raster.crop[[6]]
awc.raster
plot(awc.raster)

### calculate uncertainty of AWC
# variance of DUL
dul.var<- ((all.raster.crop[[2]]-all.raster.crop[[1]])/(2*1.64))^2
dul.var
plot(dul.var)

# variance of LL15
ll.var<- ((all.raster.crop[[5]]-all.raster.crop[[4]])/(2*qnorm(0.95)))^2
ll.var
plot(ll.var)

# combined variance [assuming no correlation]
comb.var<- dul.var + ll.var
comb.var

### derive awc prediciton intervals
# standard deviation
comb.sd <- sqrt(comb.var)
# standard error
comb.se <- comb.sd * qnorm(0.95)
# upper prediction limit
awc.upl<- awc.raster + comb.se
awc.upl
plot(awc.upl)
# lower prediction limit
awc.lpl<- awc.raster - comb.se
awc.lpl
# change all <0 to 0
awc.lpl[awc.lpl < 0] <- 0

# awc maps
plot(awc.raster)
plot(awc.lpl)
plot(awc.upl)

#dul maps
names(all.raster.crop)
plot(all.raster.crop[[3]])
plot(all.raster.crop[[2]])
plot(all.raster.crop[[1]])

#ll maps
plot(all.raster.crop[[6]])
plot(all.raster.crop[[5]])
plot(all.raster.crop[[4]])

