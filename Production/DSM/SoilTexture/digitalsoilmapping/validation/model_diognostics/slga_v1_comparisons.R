### instersect external validation data with version 1 SLGA clay data
library(raster);library(sp);library(rgdal)

root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/"
out.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/SLGA_V1_ext_val/"

## raster files of SLGA version 1 [CLAY]
clay.files<- list.files(path = "/OSM/CBR/CLW_TERN_Soil/work/Products/Australia-wide_3D_digital_soil_property_maps/Clay/",pattern = ".tif$", full.names = T, recursive = F)
clay.files

## raster files of SLGA version 1 [SAND]
sand.files<- list.files(path = "/OSM/CBR/CLW_TERN_Soil/work/Products/Australia-wide_3D_digital_soil_property_maps/Sand/",pattern = ".tif$", full.names = T, recursive = F)
sand.files

## raster files of SLGA version 1 [SILT]
silt.files<- list.files(path = "/OSM/CBR/CLW_TERN_Soil/work/Products/Australia-wide_3D_digital_soil_property_maps/Silt/",pattern = ".tif$", full.names = T, recursive = F)
silt.files

all.files<- c(clay.files,sand.files,silt.files)

s1<- stack()
for (i in 1:length(all.files)){
  s1<- stack(s1,raster(all.files[i]))
}
s1



### DEPTH 1
vart<- "clay"
depth<- "d1"

dat<- readRDS(file = paste0(root,depth, "_", vart, "_external_val.rds"))
names(dat)
dat<- dat[,1:13]
coordinates(dat)<- ~ Longitude  + Latitude
crs(dat)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#extract raster values
sr <- raster::extract(x = s1, y = dat, method="simple", sp=1)  # VERY RATE LIMITING

# Save result
sr<- as.data.frame(sr)
#sz<- sr[complete.cases(sr),]
saveRDS(sr, file = paste0(out.root, "slga_v1_data_",depth, "_.rds"))



### DEPTH 2
vart<- "clay"
depth<- "d2"
        
dat<- readRDS(file = paste0(root,depth, "_", vart, "_external_val.rds"))
names(dat)
dat<- dat[,1:13]
coordinates(dat)<- ~ Longitude  + Latitude
crs(dat)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        
#extract raster values
sr <- raster::extract(x = s1, y = dat, method="simple", sp=1)  # VERY RATE LIMITING
        
# Save result
sr<- as.data.frame(sr)
#sz<- sr[complete.cases(sr),]
saveRDS(sr, file = paste0(out.root, "slga_v1_data_",depth, "_.rds"))

### DEPTH 3
vart<- "clay"
depth<- "d3"

dat<- readRDS(file = paste0(root,depth, "_", vart, "_external_val.rds"))
names(dat)
dat<- dat[,1:13]
coordinates(dat)<- ~ Longitude  + Latitude
crs(dat)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#extract raster values
sr <- raster::extract(x = s1, y = dat, method="simple", sp=1)  # VERY RATE LIMITING

# Save result
sr<- as.data.frame(sr)
#sz<- sr[complete.cases(sr),]
saveRDS(sr, file = paste0(out.root, "slga_v1_data_",depth, "_.rds"))


### DEPTH 4
vart<- "clay"
depth<- "d4"

dat<- readRDS(file = paste0(root,depth, "_", vart, "_external_val.rds"))
names(dat)
dat<- dat[,1:13]
coordinates(dat)<- ~ Longitude  + Latitude
crs(dat)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#extract raster values
sr <- raster::extract(x = s1, y = dat, method="simple", sp=1)  # VERY RATE LIMITING

# Save result
sr<- as.data.frame(sr)
#sz<- sr[complete.cases(sr),]
saveRDS(sr, file = paste0(out.root, "slga_v1_data_",depth, "_.rds"))


### DEPTH 5
vart<- "clay"
depth<- "d5"

dat<- readRDS(file = paste0(root,depth, "_", vart, "_external_val.rds"))
names(dat)
dat<- dat[,1:13]
coordinates(dat)<- ~ Longitude  + Latitude
crs(dat)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#extract raster values
sr <- raster::extract(x = s1, y = dat, method="simple", sp=1)  # VERY RATE LIMITING

# Save result
sr<- as.data.frame(sr)
#sz<- sr[complete.cases(sr),]
saveRDS(sr, file = paste0(out.root, "slga_v1_data_",depth, "_.rds"))

### DEPTH 6
vart<- "clay"
depth<- "d6"

dat<- readRDS(file = paste0(root,depth, "_", vart, "_external_val.rds"))
names(dat)
dat<- dat[,1:13]
coordinates(dat)<- ~ Longitude  + Latitude
crs(dat)
proj4string(dat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#extract raster values
sr <- raster::extract(x = s1, y = dat, method="simple", sp=1)  # VERY RATE LIMITING

# Save result
sr<- as.data.frame(sr)
#sz<- sr[complete.cases(sr),]
saveRDS(sr, file = paste0(out.root, "slga_v1_data_",depth, "_.rds"))

# END
