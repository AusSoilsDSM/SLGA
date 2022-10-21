### TERN LANDSCAPES 
# Soil CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 24.11.21
# modified: 27.01.22

# CODE PURPOSE
# Intersect all data with available covariates
# data here with measured CEC and pedotransfer CEC [spatial approach]


# root directory
data.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/model_outs/pedotransfer/"
data.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/model_outs/"


library(raster);library(sp);library(rgdal)

# site data
site.dat<- readRDS(paste0(data.root,"spatial/pedotransferWork_4_allsoildata_4_modelsimruns__spatial_output.rds"))

# quick filter of the data
# Australia Bounding Box
bbox<- c(112.5,-44.1,153.9,-9.9)
nas<- which(site.dat$Longitude <= bbox[1] | site.dat$Longitude >= bbox[3] | site.dat$Latitude <= bbox[2] | site.dat$Latitude >= bbox[4])
nas
#site.dat<- site.dat[-nas,]

# Map and data out of locations with measurement and pedotransfer function
measure.dat<- site.dat[site.dat$cec != -5555,]
pedot.dat<- site.dat[site.dat$cec == -5555,]

measure.dat<- measure.dat[measure.dat$upper == 0,]
pedot.dat<- pedot.dat[pedot.dat$upper == 0,]
plot(measure.dat$Longitude,measure.dat$Latitude)
plot(pedot.dat$Longitude,pedot.dat$Latitude)
coordinates(measure.dat)<- ~ Longitude + Latitude
writeOGR(obj = measure.dat, data.out, "cec_measurement_1", "ESRI Shapefile")
coordinates(pedot.dat)<- ~ Longitude + Latitude
writeOGR(obj = pedot.dat, data.out, "cec_pedotransfer_1", "ESRI Shapefile")

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
writeOGR(site.dat, data.out, "tern_CEC_sites_all_1", "ESRI Shapefile")


#extract raster values
sr <- raster::extract(x = s1, y = site.dat, method="simple", sp=1)  # VERY RATE LIMITING

sr <- raster::extract(r4, sr, sp= 1, method = "simple")  # VERY RATE LIMITING

sr<- as.data.frame(sr)
names(sr)
sz<- sr[complete.cases(sr[,16:104]),]
plot(sz$Longitude,sz$Latitude)

#save object
saveRDS(sz, file = paste0(data.out,"tern_cec_siteDat_covariates_20220127.rds"))



