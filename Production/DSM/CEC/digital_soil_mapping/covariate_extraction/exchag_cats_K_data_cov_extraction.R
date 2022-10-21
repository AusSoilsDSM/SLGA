### TERN LANDSCAPES 
# Soil CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 2.12.21
# modified: 27.01.22

# CODE PURPOSE
# Intersect all data with available covariates
# data here is individual cations : K

target_variable<- "K"

# root directory
data.root<- paste0("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/data_curation/Cations/",target_variable, "/splineOuts/")


library(raster);library(sp);library(rgdal)

# site data
site.dat<- readRDS(paste0(data.root,"lab_excats_",target_variable,"_data_splined_labelled.rds"))
rms<- unique(c(which(is.na(site.dat$Longitude)),which(is.na(site.dat$Latitude))))
rms
site.dat<- site.dat[-rms,]

# quick filter of the data
# Australia Bounding Box
bbox<- c(112.5,-44.1,153.9,-9.9)
nas<- which(site.dat$Longitude <= bbox[1] | site.dat$Longitude >= bbox[3] | site.dat$Latitude <= bbox[2] | site.dat$Latitude >= bbox[4])
nas
site.dat<- site.dat[-nas,]


## 
## covariates
# PCs
list.files("/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/PCS/",  pattern="tif$", full.names=FALSE, recursive = T)
files<- list.files("/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/PCS/",  pattern="tif$", full.names=TRUE, recursive = T)
files<- files[c(1:12,27:52)]
files

s1<- stack()
for (i in 1:length(files)){
  s1<- stack(s1, raster(files[i]))
}
s1
names(s1)

r4<- raster("/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_geomorphons.tif")

# Because s1 and r4 are slightly different the extraction process has to be done seperately

coordinates(site.dat)<- ~ Longitude  + Latitude
crs(site.dat)
proj4string(site.dat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
writeOGR(site.dat, data.root, "tern_exch_cats_K_sites_all_updated", "ESRI Shapefile")


#extract raster values
sr <- raster::extract(x = s1, y = site.dat, method="simple", sp=1)  # VERY RATE LIMITING

sr <- raster::extract(r4, sr, sp= 1, method = "simple")  # VERY RATE LIMITING

sr<- as.data.frame(sr)
names(sr)
sz<- sr[complete.cases(sr[,17:55]),]
plot(sz$Longitude,sz$Latitude)

#save object
saveRDS(sz, file = paste0(data.root,"tern_exch_cats_",target_variable,"_siteDat_covariates_20220127.rds"))



