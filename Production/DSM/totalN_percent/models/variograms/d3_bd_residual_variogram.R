### TERN LANDSCAPES 
# Bulk Density
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 28.11.22
# modified: 28.11.22

# CODE PURPOSE
# Model residuals have been calculated and then aggregated 
# Derive variogram for use in kriging


# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/bulk_density/"
data.root<- paste0(g.root, "models/ranger_models/data_obs_preds/cal/")
out.root<- paste0(g.root, "models/variograms/")
funcs.out<- paste0(g.root, "rcode/miscell/")

# libraries
library(rgdal);library(sp);library(gstat);library(automap);library(matrixStats)
source(paste0(funcs.out,"goof.R"))


# depths
depths<- c(0,5,15,30,60,100)
sel.depth<- depths[3]

depth<- 3
vart<- "bd"


#data
files<- list.files(path = data.root, pattern = "summaries", full.names = T)
files
# site data
site.dat<- read.table(file = files, sep = ",", header = T)

# subset the data per depth
site.dat<- site.dat[site.dat$upper == sel.depth, ]


# fit variogram
names(site.dat)
names(site.dat)[3:4]<- c("x", "y")
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
