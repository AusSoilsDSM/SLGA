library(matrixStats);library(automap);library(gstat);library(sp);library(rgdal)
## Model residuals

root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/"


### CLAY
## D1
vari<- "sand"
depth<- "d3"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# model predictions across the full dataset
files<- list.files(path = new.root,pattern = "ranger_ALL_preds_",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
dat$residual<- dat$target - dat$prediction
names(dat)
dat<- dat[,c(1:6)]

for (i in 1:length(files)){
  tdat<- read.table(file = files[i],sep = ",",header = T)
  tdat$residual<- tdat$target - tdat$prediction
  dat<- cbind(dat, tdat$residual)
  print(i)}

dat$residMedian<- rowMedians(as.matrix(dat[,7:56]))
hist(dat$residMedian)

# variogram work
names(dat)
# coordinates
coordinates(dat)<- ~ Longitude + Latitude


## remove spatially duplicated points
#dat<- remove.duplicates(dat)

# set coordinate reference system
crs(dat)<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# transform coordinates to projected
dat<- spTransform(dat,CRSobj = "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


# fit variogram
names(dat)
#vgm1<- variogram(residMedian~1, data = dat,width= 1000, cutoff=1500000)
afit<- autofitVariogram(residMedian~1, dat)
plot(afit)
afit

# save variogram to file
nm1<- paste0(root, "residuals/variogram_",vari,"_", depth, ".rds" )
nm1
saveRDS(afit, file = nm1)



