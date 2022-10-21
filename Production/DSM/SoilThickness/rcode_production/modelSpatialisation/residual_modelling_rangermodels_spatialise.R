### Soil Depth
### Process for kriging modle residuals
### See: /OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/residual_modelling_rangermodels.R
### For the processes involved for getting the data prepared for this analysis
### modified 24/7/19


# directory of where the kriged outputs will go
out.dir<- "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/spatialPrediction/tiles/"

# libraries
library(raster);library(rgdal);library(sp);library(gstat);library(automap);library(parallel);library(doParallel);library(rgeos)

#data
# variogram
afit<- readRDS("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/ranger_residuals_variogram.rds")
plot(afit)

# point data
variodat<- readRDS("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/ranger_residuals_calibration.rds")
variodat
crs(variodat)<- "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#tile folders
fols<- as.numeric(list.files("/OSM/CBR/AF_DIGISCAPESM/work/CoVariates/tiles/", full.names = FALSE))
length(fols)


###
# begin parallel cluster and register it with foreach
cpus<- 12
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)


#kriging to raster data
# get raster (this will change from tile to tile)
oper1<- foreach(i=1:length(fols), .packages = c("raster", "sp", "rgdal", "gstat", "automap", "rgeos")) %dopar% {
  
  # select folder
  sfol<- fols[i]
  
  # path to base raster
  fpath<- paste0("/OSM/CBR/AF_DIGISCAPESM/work/CoVariates/tiles/", sfol,"/PCS/Climate/" ,sep="")
  
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
  plot(r7)
  plot(c1,add=T)
  
  ##Distances of data points to centroid point
  #str(dat2)
  mat1<- as.data.frame(variodat)
  #str(mat1)
  mat2<- as.matrix(mat1[,2:3]) # matrix of data coordinates
  
  as.matrix(as.data.frame(c1)) # matrix of centroid coordinate
  
  dist1<- spDistsN1(mat2, as.matrix(as.data.frame(c1)),longlat = FALSE) #distance
  #summary(dist1)
  mat1<- cbind(mat1,dist1)
  mat3<- mat1[order(mat1$dist1),]
  mat3<- mat3[1:2500,] # get the nearest 1000 points
  
  #kriging data frame
  coordinates(mat3)<- ~ x + y
  crs(mat3)<- crs(r7)
  #mat3
  
  #plot(mat3)
  #plot(r7,add=T)
  
  # kriging model
  gRK <- gstat(NULL, "prediction", residual~1, mat3, model=afit$var_model)
  gRK
  
  
  ## kriging
  #crs(r7)
  #crs(mat3)
  outPath<- paste0(out.dir, sfol, "/residual_krigOut.tif", sep="")
  map.RK2<- interpolate(r7, gRK, xyOnly=TRUE, index = 1, na.rm=TRUE)
  #plot(map.RK2)
  #map.RK2
  
  
  # re-project to original resolution
  r8<- projectRaster(map.RK2, r1,filename = outPath, 
                     format="GTiff", 
                     overwrite=T, 
                     datatype="FLT4S")
  #r8
  #plot(r8)
  
  ## SLURM OUTPUT CHECKS
  itOuts<- c(i,as.character(Sys.time()))
  nmz<- paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/rcode/slurm/slurmCheck/Iteration_counter_RK_", i, ".txt")
  write.table(itOuts, 
              file = nmz,
              row.names = F, col.names = F, sep=",")
}

#END





















  


