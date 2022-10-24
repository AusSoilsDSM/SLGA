### TERN LANDSCAPES 
# Soil pH model model fitting
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 18.5.21
# modified: 18.5.21

# CODE PURPOSE
# # Residual kriging. Load in variogram model

### variables
vart<- "pH_4a1"
depth<- "d6"
batch<- 6
srt<- 1251
fin<- 1500
cpus<- 12

# root directory
data.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/data/curated_all/variogram_dat/4a1/"
model.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/models/variogram_models/4a1/"
root.tiles<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/predictions/tiles/"
cov.tiles<- "/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/90m/"
slurm.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/spatialprediction/slurm/pH_4a1_residuals/d6/"
r.code<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/spatialprediction/d6/residuals/residual_modelling_rangermodels_pearcey_spatialise_d6_1.R"

# libraries
library(raster);library(rgdal);library(sp);library(gstat);library(automap);library(rgeos);library(parallel);library(doParallel)


# residual data
# site data
site.dat<- readRDS(paste0(data.root,"tern_soilpH4a1_siteDat_covariates_CALVALDAT_SimulationResiduals_d6_ARD.rds"))
crs(site.dat)<- "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# variogram
afit<- readRDS(paste0(model.out,"residuals_variogram_4a1_d6.rds"))
plot(afit)


### Folders where the coveraiates are
fols<- as.numeric(list.files(cov.tiles, full.names = FALSE))
length(fols)

###
# begin parallel cluster and register it with foreach
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)


#kriging to raster data
# get raster (this will change from tile to tile)
oper1<- foreach(i=srt:fin, .packages = c("raster", "sp", "rgdal", "gstat", "automap", "rgeos")) %dopar% {

  # select folder
  sfol<- fols[i]
  
  # path to base raster
  fpath<- paste0(cov.tiles, sfol,"/PCS/Climate/")
  
  #raster
  r1<- raster(paste(fpath,"climate_PCA_1.tif", sep=""))
  
  # project raster to the same crs as data points (metre scale)
  r2<- projectRaster(r1, res=90, crs="+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  plot(r2)
  
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
  plot(r7)
  plot(c1,add=T)
  
  ##Distances of data points to centroid point
  #str(dat2)
  mat1<- as.data.frame(site.dat)
  names(mat1)
  #str(mat1)
  mat2<- as.matrix(mat1[,51:52]) # matrix of data coordinates
  
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
  
  plot(mat3)
  plot(r7,add=T)
  
  # kriging model
  gRK <- gstat(NULL, "prediction", predResidual~1, mat3, model=afit$var_model)
  gRK
  
  
  ## kriging
  outPath<- paste0(root.tiles, sfol, "/", depth, "/", vart, "/pred_residual_",depth,".tif")
  outPath
  
  map.RK2<- interpolate(r7, gRK, xyOnly=TRUE, index = 1, na.rm=TRUE)
  
  # re-project to original resolution
  r8<- projectRaster(map.RK2, r1,filename = outPath, 
                     format="GTiff", 
                     overwrite=T, 
                     datatype="FLT4S")
  #r8
  #plot(r8)
  
  ## SLURM OUTPUT CHECKS
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0(slurm.root ,batch, "/slurmckeck_", i, "_",sfol, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
}

#END





















  


