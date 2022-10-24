### TERN LANDSCAPES 
# Soil pH model model fitting
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 8.9.21
# modified: 8.9.21

# CODE PURPOSE
# Model residuals have been calculated and then aggregated 
# Derive variogram for use in kriging

# fixed parameters
vart<- "pH4b"
depth<- "d2"


# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
data.root<- paste0(g.root, "models/ph_4b/",depth, "/data_obs_preds/cal/")
out.root<- paste0(g.root, "models/ph_4b/variograms/")
funcs.out<- paste0(g.root, "rcode/miscell/")

# libraries
library(rgdal);library(sp);library(gstat);library(automap);library(matrixStats)
source(paste0(funcs.out,"goof.R"))


#data
files<- list.files(path = data.root, pattern = "summaries", full.names = T)
files
# site data
site.dat<- read.table(file = files, sep = ",", header = T)


# fit variogram
names(site.dat)
names(site.dat)[4:5]<- c("x", "y")
coordinates(site.dat)<- ~ x + y
# set coordinate reference system
crs(site.dat)<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# transform coordinates to projected
site.dat<- spTransform(site.dat,CRSobj = "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
site.dat<- as.data.frame(site.dat)
names(site.dat)

coordinates(site.dat)<- ~ x + y
vgm1<- variogram(residual_avg~1, data = site.dat,width= 1000, cutoff=1500000)
afit<- autofitVariogram(residual_avg~1, site.dat)
plot(afit)
afit
#afit # variogram parameters
plot(vgm1, model=afit$var_model)

### save the variogram point data and the variogram model
saveRDS(afit, paste0(out.root,"residuals_variogram_",vart,"_",depth,".rds"))
