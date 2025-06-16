# HPC Version - Mapper
# Version 2.1 - 18/08/2022 - Incorporated 'tiling directly from the mosaics' by Ross Searle
# Version 2.0 - 25/07/2022 - Adapted to CSIRO HPC environment
# Version 1.1 - 08/10/2020 - avgMSE now taken from GOOFData$MSE; Bug with directory names
# Version 1.0 - 14/06/2020 - P.R.Zund
#
# Script maps fitted models using the raster::predict function. 
# Prediction done tile by tile using standard 2170 SLGA tiles and six GSM depths.
# Script designed to be launched from HPC Job Control script written by Ross Searle.
#
# Script requires the following input data on Petrichor scratch directory (/scratch1/ident/modelname)
#   /scratch1/ident/modelname/Bootstrap/models/depths/bootMod_X.rds
#   /scratch1/ident/modelname/Bootstrap/GOOFData.csv
#   /scratch1/ident/modelname/Covariates/Tiles/covariate.tif
#
# User setup instructions
# 1. Run TileSpliterV2.R - This script copies bootstrap models and covariates to /scratch1/ident from PMap Bowen drive.
# 2. Check directories and files as per format above.
# 3. Set user inputs below (same number of bootstraps as used during model fitting).
# 7. Run Mapper
#
## User inputs
modelrun <- "TERN3" # Name of model run directory
nbag <- 50 # number of bootstraps (this must be same as the model fitting script setting)
source("C:/Data/PrivateData.R") #Load private data 

#wd <- paste0("//scratch2/", ident) #On HPC interactive ./RStudio
wd <- "\\fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work/Peter/PMap" #On local drive

#Script starts from here #####
#args = commandArgs(trailingOnly=T) #HPC only - loading arguments
#t = as.numeric(args[1]) #HPC only - Argument for tiles
#d = as.numeric(args[2]) #HPC Only - Argument for depth

library(raster)
library(sf)
library(stringr)

# Input data preparation

bd <- paste(wd, "/", modelrun, sep = "")
tile <- list.dirs(path = '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/90m', full.names = FALSE, recursive = FALSE) # Make a list of tiles from tile folder
print(paste0('Tile ', tile[t]))
setwd(paste(wd, "/", modelrun, sep=""))
dir.create("Bootstrap/map", recursive = TRUE)
depths <- list.dirs(path = paste(wd, "/", modelrun, "/Bootstrap/models/", sep = ""), full.names = FALSE, recursive = FALSE)
GOOFData <- read.csv(paste(wd, "/", modelrun, "/Bootstrap/GOOFData.csv", sep = ""))
avgMSE <- GOOFData$MSE #select MSE data only
avgMSE <- avgMSE[2:7]
covFilter<- "with covariatess in PMap folder used to fit model"
covDir = '/datasets/work/af-digiscapesm/work/Peter/PMap/Covariates'
tileBty <- st_read(paste0('/datasets/work/af-tern-mal-deb', '/work/datasets/national/covariates/tiles/Metadata/90m/allTiles_combined_RASTER.shp'))
tiles=tileBty[t,]
print('Input data preperation complete')

## Covariate clipping on the fly function (Ross Searle, 2022)
clipCovsDRaster <- function(tilePoly, covDir, covFilter){
  
  covToUse <- read.csv('/datasets/work/af-digiscapesm/work/Ross/covariates.csv', stringsAsFactors = F)
  covPaths <- paste0(covDir, '/', covToUse$x)
  
  for (b in 1:length(covPaths)) {
    print(covPaths[b])
    if(file.exists(covPaths[b])){
      r <- raster::raster(covPaths[b])
      rc <- raster::crop(r, tilePoly)
      if(b==1){
        stk<-stack(rc)
      }else{
        stk <- addLayer(stk, rc)
      }
    }else{
      print(paste0('File does not exist - ', covPaths[b]))
    }
  }
  return(stk)
}

## Predict fitted model tile by tile
for (t in 1:length(tile)) {
#  dir.create(paste(bd,"/Bootstrap/map/", tile[t], sep = ""))  
 files <- list.files(path = paste(bd, "/Covariates/Tiles/", tile[t], sep = ""), pattern = ".tif", full.names = TRUE, recursive = TRUE)
  
#  t1 <- clipCovsDRaster(tilePoly = tiles, covDir = covDir, covFilter=covFilter) 
  t1 <- clipCovsDRaster(tilePoly = t, covDir = covDir, covFilter=covFilter) 
 
  #### These line below just deal with R automatically changing covariate names   
  repdot <- str_replace_all(names(t1), '-', '.')
  repdot2 <- str_replace_all(repdot, '[+]', '.')
  names(t1) <- repdot2
  
  print('Rasters stacked')
    
  # Map each depth of this tile
  for (d in 1:length(depths)) {
    r.models <- list.files(path = paste(bd, "/Bootstrap/models/", depths[d], sep = ""), pattern = "\\.rds$", full.names = TRUE)
    dir.create(paste(bd,"/Bootstrap/map/", tile[t], "/", depths[d], sep = "")) # output folder
    print(paste0('Mapping for depth ', depths[d], ' begun.'))
    
    # Variance of each prediction
    for (i in 1:nbag) {
      fit_ranger <- readRDS(r.models[i])
      mapFile <- paste(paste(paste(paste(bd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "bootMap_", sep = ""), i, sep = ""), ".tif", sep = "")
      raster::predict(t1, fit_ranger, filename = mapFile, format = "GTiff", overwrite = T)
      print(paste0('Variance of bootstrap ', i, ' completed'))
    }
    
    # Determine pixel mean
    # 1. Load pathway to rasters
    files <- list.files(paste(bd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), pattern = "bootMap_", full.names = TRUE)
    # 2. Stack raster
    m1 <- raster(files[1])
    for (i in 2:length(files)) {
      m1 <- stack(m1, files[i])
    }
    # 3. Calculate mean
    meanFile <- paste(paste(paste(bd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "meanPred_", sep = ""), ".tif", sep = "")
    bootMap.mean <- raster::writeRaster(mean(m1), filename = meanFile, format = "GTiff", overwrite = TRUE)
    
    # Estimate variance at each pixel
    # Squared variance at each pixel
    for (i in 1:length(files)) {
      r2 <- raster(files[i])
      diffFile <- paste(paste(paste(paste(bd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "bootAbsDif_", sep = ""), i, sep = ""), ".tif", sep = "")
      jj <- (r2 - bootMap.mean)^2
      raster::writeRaster(jj, filename = diffFile, format = "GTiff", overwrite = TRUE)
    }
    
    # Calculate sum of the squared difference
    # 1. Look for files with BootAbsDif in name
    files2 <- list.files(paste(bd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), pattern = "bootAbsDif", full.names = TRUE)
    # 2. Stack rasters
    r3 <- raster(files2[1])
    for (i in 2:length(files2)) {
      r3 <- stack(r3, files2[i])
    }
    # 3. Calculate sum of square differences for each pixel and write to file
    sqDiffFile <- paste(paste(paste(bd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "sqDiffPred_", sep = ""), ".tif", sep = "")
    bootMap.sqDiff <- raster::writeRaster(sum(r3), filename = sqDiffFile, format = "GTiff", overwrite = TRUE)
    
    # Calculate variance
    varFile <- paste(paste(paste(bd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "varPred_", sep = ""), ".tif", sep = "")
    bootMap.var <- raster::writeRaster(((1/(nbag - 1)) * bootMap.sqDiff), filename = varFile, format = "GTiff", overwrite = TRUE)
    
    # Calculate overall prediction variance
    varFile2 <- paste(paste(paste(bd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "varPredF_", sep = ""), ".tif", sep = "")
    bootMap.varF <- raster::writeRaster((bootMap.var + avgMSE[d]), filename = varFile2, format = "GTiff", overwrite = TRUE)
    
    # Determine prediction interval (PI)
    # PI = SD*z(90% probability interval)
    # SD
    sdFile <- paste(paste(paste(bd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "sdPred_", sep = ""), ".tif", sep = "")
    bootMap.sd <- raster::writeRaster(sqrt(bootMap.varF), filename = sdFile, format = "GTiff", overwrite = TRUE)
    
    # z - Standard error (se)
    seFile <- paste(paste(paste(bd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "sePred_", sep = ""), ".tif", sep = "")
    bootMap.se <- raster::writeRaster((bootMap.sd * qnorm(0.95)), filename = seFile, format = "GTiff", overwrite = TRUE)
    
    # Map upper prediction limit
    uplFile <- paste(paste(paste(bd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "uplPred_", sep = ""), ".tif", sep = "")
    bootMap.upl <- raster::writeRaster((bootMap.mean + bootMap.se), filename = uplFile, format = "GTiff", overwrite = TRUE)
    
    # Map lower prediction limit
    lplFile <- paste(paste(paste(bd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "lplPred_", sep = ""), ".tif", sep = "")
    bootMap.lpl <- raster::writeRaster((bootMap.mean - bootMap.se), filename = lplFile, format = "GTiff", overwrite = TRUE)
    
    # Map prediction interval range
    pirFile <- paste(paste(paste(bd, "/Bootstrap/map/", tile[t], "/", depths[d], "/", sep = ""), "pirPred_", sep = ""), ".tif", sep = "")
    bootMap.pir <- raster::writeRaster((bootMap.upl - bootMap.lpl), filename = pirFile, format = "GTiff", overwrite = TRUE)
    print(paste0(depths[d], ' depth complete'))
  }
}
#Remove excess files
for(f in 1:length(files)){file.remove(files[f])}
for(f2 in 1:length(files2)){file.remove(files2[f2])}

print('Job Successfully Finished')
### End of script Yah!
