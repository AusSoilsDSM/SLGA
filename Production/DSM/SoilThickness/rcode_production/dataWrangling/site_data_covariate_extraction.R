## soil depth prediction
## covariate data intersection
## modified: 17.07.2019

library(raster);library(rgdal);library(sp);library(rgeos)



# site data
sd_dat<- readRDS("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/sd_siteDat_covariates_winnowed_20191907.rds")
str(sd_dat)
sd_dat<- sd_dat[,c(1:3,33:38)]


# coordinates
coordinates(sd_dat)<- ~ Longitude + Latitude
crs(sd_dat)<- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

# re-project
sd_dat<- spTransform(sd_dat, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


## covariates
# PCs
list.files("/OSM/CBR/AF_DIGISCAPESM/work/CoVariates/PCS/",  pattern="tif$", full.names=FALSE, recursive = T)
files<- list.files("/OSM/CBR/AF_DIGISCAPESM/work/CoVariates/PCS/",  pattern="tif$", full.names=TRUE, recursive = T)
files

s1<- stack()
for (i in 1:length(files)){
  s1<- stack(s1, raster(files[i]))
}
s1

# other covariates (categorical in nature)
r1<- raster("Y:/CoVariates/Climate/Clim_agroclim_hutch.tif")
r2<- raster("Y:/CoVariates/Climate/Clim_kpnall.tif")
#r3<- raster("Y:/CoVariates/Organisms/Veg_HS_ICESatGLAS.tif")
r4<- raster("Y:/CoVariates/Relief/relief_geomorphons.tif")
s2<- stack(r1,r2,r4)
s2

# Because s1 and s2 are slightly different the extraction process has to be done seperately

#extract raster values
sr <- raster::extract(x = s1, y = sd_dat, method="simple", sp=1)  # VERY RATE LIMITING

sr <- raster::extract(s2, sr, sp= 1, method = "simple")  # VERY RATE LIMITING

#save object
saveRDS(sr, file = "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/sd_siteDat_covariates_20192207.rds")
#sr<- readRDS(file = "Z:/projects/soilDepth_2019/soilDepth/data//siteDat_covariates.rds")


