### TERN soil color covariate data extraction
library(raster);library(sp);library(rgdal)

# lab data
top.dat<- readRDS("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/processed_1/combined_soil_color_out_process1_topsoils_2ndround_matched.rds")
sub.dat<- readRDS("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/processed_1/combined_soil_color_out_process1_subsoils_2ndround_matched.rds")
top.dat$level<- "top"
sub.dat$level<- "sub"
comb.dat<- rbind(top.dat,sub.dat)



# quick filter of the data
# Australia Bounding Box
bbox<- c(112.5,-44.1,153.9,-9.9)
nas<- which(comb.dat$Longitude <= bbox[1] | comb.dat$Longitude >= bbox[3] | comb.dat$Latitude <= bbox[2] | comb.dat$Latitude >= bbox[4])
nas
comb.dat<- comb.dat[-nas,]

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
writeOGR(comb.dat, "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/processed_1/", "tern_soilcolor_sites_all_2nd_round", "ESRI Shapefile")


#extract raster values
sr <- raster::extract(x = s1, y = comb.dat, method="simple", sp=1)  # VERY RATE LIMITING

sr <- raster::extract(r4, sr, sp= 1, method = "simple")  # VERY RATE LIMITING

sr<- as.data.frame(sr)
#sz<- sr[complete.cases(sr),]
#plot(sz$Longitude,sz$Latitude)

#save object
saveRDS(sr, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/processed_1/tern_soilcolor_siteDat_2ndround_covariates_20200908.rds")
#sr<- readRDS(file = "Z:/projects/soilDepth_2019/soilDepth/data//siteDat_covariates.rds")




