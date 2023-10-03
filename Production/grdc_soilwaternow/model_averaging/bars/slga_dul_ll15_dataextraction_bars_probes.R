### GRDC soil-water NOW
# Unit rescaling of ANU and AWRA outputs
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 7.3.22
# modified: 7.3.22

# CODE PURPOSE
# Bring in estimated SLGA maps of DUL and LL15
# Intersect with probe locations
# 

library(raster);library(rgdal);library(sp)

# bars soil probe locations
probe.locs<- read.csv("/datasets/work/af-tern-mal-deb/work/projects/boorowa_2019/soilMoistureSensing/data/bars_sm_scalingtable_feb2022.csv")
names(probe.locs)
coordinates(probe.locs)<- ~ Longitude + Latitude
proj4string(probe.locs) <- CRS("+proj=longlat +datum=WGS84 +no_defs")


## DUl and LL15 rasters
soilwater.raster<- "/datasets/work/lw-soildatarepo/work/http/Products/Collabs/SoilWaterNow/AWC/"
soilwater.raster<- list.files(path = soilwater.raster, pattern = "Median",full.names = T)
soilwater.raster<- stack(soilwater.raster)
crs(soilwater.raster)

# extract upper and lower limits at probe sites
DSM_data <- extract(x = soilwater.raster, y = probe.locs, sp = 1, method = "simple")
names(DSM_data)
DSM_data<- as.data.frame(DSM_data)
write.csv(x = DSM_data,file = "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/data/bars_probelocs_SLGA_dul_ll.csv",row.names = F)

