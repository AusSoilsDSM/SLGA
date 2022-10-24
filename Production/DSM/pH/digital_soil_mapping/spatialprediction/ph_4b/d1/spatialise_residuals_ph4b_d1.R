### TERN LANDSCAPES 
# Soil pH 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 5.10.22
# modified: 5.10.22

# CODE PURPOSE
# # Residual kriging. Load in variogram model

### variables
args = commandArgs(trailingOnly = T)
sel.depth<- as.numeric(args[1])
runs<- as.numeric(args[2])

### variables
vart<- "ph_4b"

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
data.root<- paste0(g.root, "models/ph_4b/d",sel.depth, "/data_obs_preds/cal/")
model.root<- paste0(g.root, "models/ph_4b/variograms/")
funcs.out<- paste0(g.root, "rcode/miscell/")
root.tiles<- paste0(g.root,"/predictions/tiles/")
root.slurm<- paste0(g.root,"/rcode/slurm/",vart,"/temp_outs/digital_soil_mapping/spatialprediction/model_resids/d",sel.depth,"/")

### Folders where the covariates are
dpw<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/90m/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
length(fols)
fols<- fols[order(fols)]


# libraries
library(raster);library(rgdal);library(sp);library(gstat);library(automap);library(rgeos)


site.dat<- read.table(file = paste0(data.root, "ranger_CAL_preds_average_summaries_pH4b_depth_d",sel.depth,".txt"),header = T, sep = ",")
names(site.dat)
names(site.dat)[4:5]<- c("x", "y")
coordinates(site.dat)<- ~ x + y
# set coordinate reference system
crs(site.dat)<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# transform coordinates to projected
site.dat<- spTransform(site.dat,CRSobj = "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")



# residual variogram
resmod<- readRDS(file = paste0(model.root, "residuals_variogram_pH4b_d",sel.depth,".rds"))
resmod
#plot(resmod)

# select folder
sfol<- fols[runs]
  
# path to base raster
fpath<- paste0(dpw, sfol,"/PCS/Climate/")
  
#raster
r1<- raster(paste(fpath,"climate_PCA_1.tif", sep=""))
  
# project raster to the same crs as data points (metre scale)
r2<- projectRaster(r1, res=90, crs="+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
#plot(r2)
  
#refine grid to remove large areas of NAs
tempD <- data.frame(cellNos = seq(1:ncell(r2)))
vals <- as.data.frame(getValues(r2))
tempD<- cbind(tempD, vals)
tempD <- tempD[complete.cases(tempD), ]
cellNos <- c(tempD$cellNos)
gXY <- data.frame(xyFromCell(r2, cellNos, spatial = FALSE))
tempD<- cbind(gXY, tempD)
#str(tempD)
  
r7<- rasterFromXYZ(tempD[,c(1:3)]) # the raster to interpolate onto
crs(r7)<- crs(r2)
  
# get centroid point of the raster and outside midpoints
e<- extent(r7)
#e
p<- as(e, "SpatialPolygons")
#centroid
c1<- gCentroid(p)
#plot(r7)
#plot(c1,add=T)
  
##Distances of data points to centroid point
#str(dat2)
mat1<- as.data.frame(site.dat)
names(mat1)
#str(mat1)
mat2<- as.matrix(mat1[,12:13]) # matrix of data coordinates
  
as.matrix(as.data.frame(c1)) # matrix of centroid coordinate
  
dist1<- spDistsN1(mat2, as.matrix(as.data.frame(c1)),longlat = FALSE) #distance
#summary(dist1)
mat1<- cbind(mat1,dist1)
mat3<- mat1[order(mat1$dist1),]
mat3<- mat3[1:2000,] # get the nearest 2000 points
  
#kriging data frame
coordinates(mat3)<- ~ x + y
crs(mat3)<- crs(r7)
#mat3
  
#plot(mat3)
#plot(r7,add=T)
  
# kriging model
gRK <- gstat(NULL, "prediction", residual_avg~1, mat3, model=resmod$var_model)
gRK
  
  
## kriging
outPath<- paste0(root.tiles, sfol, "/d", sel.depth, "/", vart, "/pred_residual_d",sel.depth,".tif")
outPath
  
map.RK2<- interpolate(r7, gRK, xyOnly=TRUE, index = 1, na.rm=TRUE)
#plot(map.RK2)

# re-project to original resolution
r8<- projectRaster(map.RK2, r1,filename = outPath, 
                     format="GTiff", 
                     overwrite=T, 
                     datatype="FLT4S")
#r8
#plot(r8)

# load in the prediction raster
modpred.file<- paste0(root.tiles, sfol, "/d", sel.depth, "/", vart, "/pred_",vart,"_mean_d",sel.depth,".tif")
modpred<- raster(modpred.file)

# derive regression kriging prediction
rk.modpred.file<- paste0(root.tiles, sfol, "/d", sel.depth, "/", vart, "/RKpred_",vart,"_mean_d",sel.depth,".tif")
rk.modpred<- modpred + r8
rk.modpred.r<- signif(rk.modpred, digits = 3)
plot(rk.modpred.r)
rk.modpred.r
writeRaster(rk.modpred.r,filename = rk.modpred.file, format = "GTiff", overwrite = TRUE)
  

## SLURM OUTPUT CHECKS
itOuts<- c(runs,as.character(Sys.time()))
nmz<- paste0(root.slurm ,"slurmckeck_d",sel.depth,"_", runs, "_",sfol, ".txt")
write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")


#END





















  


