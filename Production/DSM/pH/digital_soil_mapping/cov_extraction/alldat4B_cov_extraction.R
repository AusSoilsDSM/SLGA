### TERN LANDSCAPES 
# Soil pH 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 23.8.22
# modified: 23.8.22

# CODE PURPOSE
# Intersect all data with available covariates


# root directory
data.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/data/curated_all/"


library(raster);library(sp);library(rgdal)

# site data
site.dat<- readRDS(paste0(data.root,"lab_4B_field_splined_dsm_ARD.rds"))

# quick filter of the data
# Australia Bounding Box
bbox<- c(112.5,-44.1,153.9,-9.9)
nas<- which(site.dat$Longitude <= bbox[1] | site.dat$Longitude >= bbox[3] | site.dat$Latitude <= bbox[2] | site.dat$Latitude >= bbox[4])
nas
site.dat<- site.dat[-nas,]


## rasters
## covariates
# PCs
list.files("/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/PCS/",  pattern="tif$", full.names=FALSE, recursive = T)
files<- list.files("/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/PCS/",  pattern="tif$", full.names=TRUE, recursive = T)
files
# remove the no artefacts rasters
files<- files[c(1:12,27:52)]
files

s1<- stack()
for (i in 1:length(files)){
  s1<- stack(s1, raster(files[i]))
}
s1

r4<- raster("/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_geomorphons.tif")

# Because s1 and r4 are slightly different the extraction process has to be done seperately

coordinates(site.dat)<- ~ Longitude  + Latitude
crs(site.dat)
proj4string(site.dat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
writeOGR(site.dat, data.root, "tern_soilpH_4B_sites_all", "ESRI Shapefile")


#extract raster values
sr <- raster::extract(x = s1, y = site.dat, method="simple", sp=1)  # VERY RATE LIMITING

sr <- raster::extract(r4, sr, sp= 1, method = "simple")  # VERY RATE LIMITING

sr<- as.data.frame(sr)
sz<- sr[complete.cases(sr),]
plot(sz$Longitude,sz$Latitude)

#save object
saveRDS(sz, file = paste0(data.root,"tern_soilpH4B_siteDat_covariates_20220822.rds"))
#sr<- readRDS(file = "Z:/projects/soilDepth_2019/soilDepth/data//siteDat_covariates.rds")




