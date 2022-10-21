### Soil Depth
### Making shapelfile of soil thickness observations



# libraries
library(raster);library(rgdal);library(sp)


#data
# observations
obs_dat<- readRDS("Z:/projects/soilDepth_2019/soilDepth/data/sd_siteDat_covariates_fin.rds")
str(obs_dat)

## remove the bore hole rock outcrop predictions
obs_dat<- obs_dat[obs_dat$type != "bore_rock",]


#GIS stuff
coordinates(obs_dat)<- ~ Longitude + Latitude
str(obs_dat@data)
obs_dat<- obs_dat[,1:3]
str(obs_dat)

proj4string(obs_dat) <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
## Write point data to shapefile
writeOGR(obs_dat, "Z:/projects/soilDepth_2019/soilDepth/data/", "obsDat_SD", "ESRI Shapefile")



