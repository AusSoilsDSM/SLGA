### TERN Landscapes 
# Total N
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 20.11.23
# modified: 20.11.23

# CODE PURPOSE
# Covariate data extraction at data points 
##


library(terra);library(sf)

# root
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_P/"
cov.root<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/PCS/"


# soil P data
all.data <- read.csv(paste0(g.root,"data/ross/TotalP-Splines.csv"))
names(all.data)
all.data$Longitude_copy<- all.data$Longitude
all.data$Latitude_copy<- all.data$Latitude

# covariate stack
cov.files<- list.files(path = cov.root, pattern = ".tif", full.names = T, recursive = T)
cov.files
cov.files<- cov.files[c(1:12,27:52)]
#cov.files<- cov.files[c(1:38)]
cov.files

cov.stack<- terra::rast(cov.files)
cov.stack

# prep for data intersection
all.data.sf <- sf::st_as_sf(x = all.data,coords = c("Longitude", "Latitude"))
st_crs(all.data.sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
st_write(all.data.sf, paste0(g.root,"data/ross/tern_total_P_sites.shp"))

#extract raster values
DSM_data<- terra::extract(x = cov.stack, y = all.data.sf, bind = T, method = "simple")

r1<-terra::rast("/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_geomorphons.tif")

#extract raster values
DSM_data2<- terra::extract(x = r1, y = DSM_data, bind = T, method = "simple")
DSM_data3<- as.data.frame(DSM_data2)


# fix up column manes with cog endings
names(DSM_data3)
new.names<- names(DSM_data3)[11:48]
new.names<- substr(x = new.names,start = 1, stop = nchar(new.names)-4)
new.names
names(DSM_data3)[11:48]<- new.names
names(DSM_data3)


# combine with other data
DSM_data4<- DSM_data3[complete.cases(DSM_data3[,c(11:49)]),]
DSM_data4$depth<- NA
DSM_data5<- DSM_data4
DSM_data5<- DSM_data5[,c(1,2,3,9:50)]
names(DSM_data5)[3]<- "target"
DSM_data5<- DSM_data5[0,]

cnt<- 1
for (i in 3:8){
  new.dat<- DSM_data4[,c(1,2,i,9:50)]
  names(new.dat)[3]<- "target"
  new.dat$depth<- cnt
  new.dat<- new.dat[complete.cases(new.dat[,3]),]
  DSM_data5<- rbind(DSM_data5,new.dat)
  cnt<- cnt + 1}

summary(as.factor(DSM_data5$depth))
DSM_data6<- DSM_data5[order(DSM_data5$SID, DSM_data5$depth),]


#save object
saveRDS(DSM_data6, file = paste0(g.root,"data/ross/model_ready/tern_totalP_labelled_covariates_ALL.rds"))
DSM_data6<- readRDS(file = paste0(g.root,"data/ross/model_ready/tern_totalP_labelled_covariates_ALL.rds"))

# creation of calibration and test data sets
training <- sample(nrow(DSM_data6), 8000)
test.dat<-  DSM_data6[training, ]
cal.dat<- DSM_data6[-training, ]
summary(as.factor(DSM_data6$depth))/nrow(DSM_data6)
summary(as.factor(cal.dat$depth))/nrow(cal.dat)
summary(as.factor(test.dat$depth))/nrow(test.dat)


#save object
saveRDS(cal.dat, file = paste0(g.root,"data/ross/model_ready/tern_totalP_labelled_covariates_CAL.rds"))
saveRDS(test.dat, file = paste0(g.root,"data/ross/model_ready/tern_totalP_labelled_covariates_TEST.rds"))






