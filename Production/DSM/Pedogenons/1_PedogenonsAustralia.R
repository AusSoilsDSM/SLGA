### PEDOGENONS FOR AUSTRALIA
### Date: 02/07/2022
### Author: Mercedes Roman Dobarco
### Email: mercedes.romandobarco@sydney.edu.au
### Secondary email: mercetadzio@gmail.com
### Affiliation: The University of Sydney

### Objectives:
### 1. Prepare covariates
### 2. Regular sampling
### Distance metric: Euclidean / Mahalanobis
### Inverse Cholesky transformation  in case I use Mahalanobis distance
### Clustering algorithm
### 3.1 k-means clustering ----> with Python and GADI - parallelised kmeans
###     Run 50 models in Artemis and keep that with the lower error

### Number of classes: 
### 4. ~ 4000 and 1370 for comparison with Brendan's and Senani's map
### Guo et al. (2003) 15.2540 x (A ^ 0.6315)
# A <-  7692
# famK <- 15.2540 * (A^0.6315)
### ~ 4300

### Desired extent: Australia
### Resolution: 90 m

### Load packages
library(rgdal)
library(sp)
library(sf)
library(raster)
library(gdalUtils)
library(nngeo)
library(gstat)
library(dplyr)
library(tidyverse)
library(foreach)
library(parallel)
library(doParallel)
library(snow)
library(doSNOW)
library(lattice)
library(ggplot2)
library(rasterVis)
library(viridis)
library(scales)
library(gridExtra)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(shiny)   # for web applications
library(RColorBrewer)
library(MASS)
#library(ClusterR)
#library(fclust)
#library(e1071)

# 1. Select CLORPT variables and pre-process if needed ----------------------------------------------

###### Load all covariates
#### Function to load rasters (very old function, I know there must be a better way to write this...)
load_raster <- function (x) {
  maps <- list()
  for (rast in 1:length(x)) {
    maps[[rast]] <- raster(x[rast])
  }
  return(maps)
}

### FOR REPRODUCIBILITY:
### Here indicate your directory with environmental covariates, proxies of soil-forming factors
### I had to do some resampling and re-alignment with gdalwarp to make the covariate stack
### Now this is commented because the step may not always be needed


## Include soil texture data - stable soil properties
#InputDir <- "Y:/PRJ-SoilBiodSOC2019/SoilProperties/soil_texture/"
#OutDir <- "Y:/PRJ-SoilBiodSOC2019/Covariates/Australia90m/"
#desired.extent <- extent(raster(paste0(OutDir,"relief_dems_3s_mosaic1.tif")))
# s <- raster(paste0(InputDir,"pred_clay_compos_mean_d1.tif"))
# r <- raster(paste0(OutDir,"relief_dems_3s_mosaic1_sc.tif"))

# setwd(InputDir)
# list.files()
# texture.vars <- c("pred_clay_compos_mean_d4.tif","pred_clay_compos_mean_d5.tif","pred_clay_compos_mean_d6.tif",
#                   "pred_sand_compos_mean_d4.tif","pred_sand_compos_mean_d5.tif" ,"pred_sand_compos_mean_d6.tif")
# # #all.files <- list.files(pattern=".tif")
# cl <- makeCluster(6)   ### Create cluster
# registerDoParallel(cl)
# system.time(covariates.out <- foreach(i=1:length(texture.vars), .packages=c("raster", "sf"),
#                                            .export = c("texture.vars", "desired.extent")) %dopar% {
#                                              setwd(InputDir) # Set wd
#                                              r <- raster(texture.vars[[i]]) ### load raster
#                                              s <- extend(r, desired.extent)
#                                              setwd(OutDir) # change wd
#                                              writeRaster(s, filename = gsub(".tif","_a.tif", texture.vars[[i]]),
#                                                          format = "GTiff",na.rm=T, inf.rm=T, overwrite = T) # Write to file
#                                              s # Return raster
#                                            })
# stopCluster(cl)


#### Selected covariates (Table 1 in attached report)

### Load raster files
covariates.selection <- c("clim_PTA_a.tif","clim_PTS1_a.tif","clim_PTS2_a.tif", # Precipitation
                          "clim_RSM_a.tif", # Short wave solar radiation
                          "clim_TNM_a.tif", "clim_TXM_a.tif","clim_TRA_a.tif", # Temperature
                          "clim_TRX_a.tif", "clim_TRI_a.tif",
                          "clim_ADM_a.tif",# Aridity
                          "PM_radmap_v4_2019_filtered_dose_GAPFilled_a.tif", # Gamma radiometrics 2019
                          "PM_radmap_v4_2019_filtered_pctk_GAPFilled_a.tif",
                          "PM_radmap_v4_2019_filtered_ppmt_GAPFilled_a.tif",
                          "PM_radmap_v4_2019_ratio_tk_GAPFilled_a.tif",
                          "PM_Gravity.tif",
                          "PM_Weathering_Index.tif", # Weathering index
                          "pred_clay_compos_mean_d4_a.tif","pred_clay_compos_mean_d5_a.tif","pred_clay_compos_mean_d6_a.tif",
                          "pred_sand_compos_mean_d4_a.tif","pred_sand_compos_mean_d5_a.tif" ,"pred_sand_compos_mean_d6_a.tif",
                          "relief_dems_3s_mosaic1.tif", # Relief
                          "relief_mrrtf_3s.tif",
                          "relief_mrvbf_3s_mosaic.tif",
                          "relief_slope_perc.tif",
                          "relief_twi_3s.tif")
### 10 climate, 6 parent material, 6 soil, 5 relief.

InputDir <- "R:/PRJ-SoilBiodSOC2019/Covariates/Australia90m/"
#R:\PRJ-SoilBiodSOC2019\Covariates\Australia90m
setwd(InputDir)
#list.tif <- gsub(".tif","_sc.tif", covariates.selection)
covariates <- load_raster(covariates.selection)

### Some covariates did not have CRS assigned, I do it manually
crs(covariates[[11]]) <-"+proj=longlat +datum=WGS84 +no_defs"
crs(covariates[[12]]) <-"+proj=longlat +datum=WGS84 +no_defs"
crs(covariates[[13]]) <-"+proj=longlat +datum=WGS84 +no_defs"
crs(covariates[[14]]) <-"+proj=longlat +datum=WGS84 +no_defs"
plot(covariates[[17]])

### Create stack
covariates <- stack(covariates)

### Change the names to something shorter
names(covariates) <- c("clim_PTA","clim_PTS1","clim_PTS2","clim_RSM",
                       "clim_TNM","clim_TXM","clim_TRA",
                       "clim_TRX","clim_TRI","clim_ADM",
                       "PM_radmap_dose",
                       "PM_radmap_pctk","PM_radmap_ppmt",
                       "PM_radmap_tk","PM_Gravity","PM_Weathering_Index",
                       "clay_d4", "clay_d5", "clay_d6",
                       "sand_d4", "sand_d5", "sand_d6",
                       "relief_dem","relief_mrrtf","relief_mrvbf",
                       "relief_slope_perc","relief_twi")
plot(covariates)


# 2. Regular sample of all covariates --------------------------------------

# #Set the random number generator to reproduce the results
set.seed(74645)
# Regular sampling N = 5000000
system.time(sampleCLORPT<- sampleRegular(covariates, size = 20000000, xy=TRUE, na.rm = TRUE, sp = TRUE))

### Transform into a dataframe
CLORPT.df <- as.data.frame(sampleCLORPT)
### select only complete cases
CLORPT.df <-CLORPT.df[complete.cases(CLORPT.df),]


# 2.2 Apply Cholesky decomposition to sample data --------------------------

# The basic Euclidean distance treats each variable as equally important in calculating the distance.
# An alternative approach is to scale the contribution of individual variables to the distance value according
# to the variability of each variable. This approach is illustrated by the Mahalanobis distance, 
# which is a measure of the distance between each observation in a multidimensional cloud of points and
# the centroid of the cloud.
# 
# ### Calculate the Mahalanobis distance, as Euclidean distance after applying the Cholesky decomposition
# ## Take out the coordinates
CLORPT.27.df.coords.vl <- CLORPT.df[,1:2] ### Coordinates
CLORPT.27.df.vl <- CLORPT.df[,names(covariates)] ### Covariates

### Change wd to that were I want to store the output
setwd("C:/Users/mrom8073/Desktop/USydney/Postdoc/Projects/Genosoils/PedogenonsAustralia")

CLORPT.27.df.vl[1,]
summary(CLORPT.27.df.vl)

### Calculate the cholesky decomposition of the variance-covariance matrix of the environmental covariates
C.27 <- chol(var(as.matrix(CLORPT.27.df.vl)))

### Multiply the original covariates with the triangular matrix to decorrelate the data
CLORPT.rs.27.vl <- as.matrix(CLORPT.27.df.vl) %*% solve(C.27)
CLORPT.rs.27.vl <- as.data.frame(CLORPT.rs.27.vl)

### Save as feather format, readable in python
install.packages("feather")
library(feather)
write_feather(CLORPT.rs.27.vl, 
              "C:/Users/mrom8073/Desktop/USydney/Postdoc/Projects/Genosoils/PedogenonsAustraliadata.feather")

### Save
save(covariates, covariates.selection,
     CLORPT.rs.27.vl, C.27, CLORPT.27.df.vl, CLORPT.27.df.coords.vl,  
     file="CLORPT.27_vl.RData")


### To double check that indeed the Euclidean distance of the scaled data corresponds
### to the Mahalanobis distance of the original data I do a test

### Euclidean distance of the first 10 in the re-scaled dataframe
b <- dist(CLORPT.rs.27.vl[1:10,], method = "euclidean")

### Mahalanobis distance in the original dataframe
library(biotools)
a <- D2.dist(CLORPT.27.df.vl[1:10,], cov=var(CLORPT.27.df.vl))
b;(sqrt(a)) ### distances are the same
### Clean
rm(a, b)

# ### What is the geographical distance between points?
a <- pointDistance(CLORPT.27.df.coords.vl[300000:301000,], 
                   CLORPT.27.df.coords.vl[300000:301000,], lonlat=TRUE, allpairs=TRUE)
a[1:10,1:10]
rm(a)

### End of this script