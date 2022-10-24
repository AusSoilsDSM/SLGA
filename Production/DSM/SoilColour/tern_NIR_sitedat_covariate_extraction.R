### TERN soil color covariate data extraction
library(raster);library(sp);library(rgdal)

# lab data
# lab data
comb.dat<- readRDS("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/processed_1/soilcolor_visnir_obs.rds")
names(comb.dat)[2:3]<- c("Latitude", "Longitude")
comb.dat[,2]<- as.numeric(comb.dat[,2])
comb.dat[,3]<- as.numeric(comb.dat[,3])



# quick filter of the data
# Australia Bounding Box
bbox<- c(112.5,-44.1,153.9,-9.9)
nas<- which(comb.dat$Longitude <= bbox[1] | comb.dat$Longitude >= bbox[3] | comb.dat$Latitude <= bbox[2] | comb.dat$Latitude >= bbox[4])
nas


## rasters
## covariates
# PCs
list.files("/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/PCS/",  pattern="tif$", full.names=FALSE, recursive = T)
files<- list.files("/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/PCS/",  pattern="tif$", full.names=TRUE, recursive = T)
files

s1<- stack()
for (i in 1:length(files)){
  s1<- stack(s1, raster(files[i]))
}
s1

r4<- raster("/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/relief_geomorphons.tif")

# Because s1 and r4 are slightly different the extraction process has to be done seperately

coordinates(comb.dat)<- ~ Longitude  + Latitude
crs(comb.dat)
proj4string(comb.dat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
writeOGR(comb.dat, "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/processed_1/", "tern_NIR_sites_all", "ESRI Shapefile")
plot(s1[[1]])
plot(comb.dat, add=T)

#extract raster values
sr <- raster::extract(x = s1, y = comb.dat, method="simple", sp=1)  # VERY RATE LIMITING

sr <- raster::extract(r4, sr, sp= 1, method = "simple")  # VERY RATE LIMITING

sr<- as.data.frame(sr)
#sz<- sr[complete.cases(sr),]
#plot(sz$Longitude,sz$Latitude)

#save object
saveRDS(sr, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/processed_1/tern_NIR_siteDat_covariates_20201007.rds")
#sr<- readRDS(file = "Z:/projects/soilDepth_2019/soilDepth/data//siteDat_covariates.rds")




