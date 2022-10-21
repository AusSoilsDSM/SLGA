
#################################
###  Author : Ross Searle         
###  Date : Tue Oct  5 11:12:18 2021                      
###  Project : SoilWaterNow
###  Purpose : Drill the covariate tiles with the HPC
#################################

startTime <- Sys.time()
print(startTime)

library(sf)
library(stringr)
library(terra)

terraOptions(memmax=1)


clipCovsD <- function(tilePoly, covDir){
  
  covs <- read.csv('/datasets/work/af-digiscapesm/work/Ross/TERN/DepthToB/covsToUse90m.csv', stringsAsFactors = F)
  covToUse <- covs[covs$UseAll==1,]$Covariate
  covPaths <- paste0(covDir, '/', covToUse, '.tif')

  for (b in 1:length(covPaths)) {
    print(b)
    if(file.exists(covPaths[b])){
      r <- terra::rast(covPaths[b])
      rc <- terra::crop(r, tilePoly)
      if(b==1){
        stk<-rc
      }else{
        add(stk) <- rc
      }
      
    }else{
      print(paste0('File does not exist - ', covPaths[b]))
    }
  }
  
  return(stk)
}

getCovStackRaw <- function(covDir, filter){
  
  print(paste0("Filtering covariates on method = ", filter))
  covs <- read.csv('/datasets/work/af-digiscapesm/work/Ross/TERN/covsToUse90m.csv', stringsAsFactors = F)
  covToUse <- covs[covs[filter]==1,]$Covariate
  covPaths <- paste0(covDir, '/', covToUse, '.tif')
  
  for (b in 1:length(covPaths)) {
    if(file.exists(covPaths[b])){
      r <- terra::rast(covPaths[b])
      #rc <- terra::crop(r, tilePoly)
      if(b==1){
        stk<-r
      }else{
        add(stk) <- r
      }
    }else{
      print(paste0('File does not exist - ', covPaths[b]))
    }
  }
  return(stk)
}



getCovsStackPCA <- function(covDir){
  
  covPaths <- list.files(covDir, pattern='.tif$', full.names = T)
  
  for (b in 1:length(covPaths)) {
    if(file.exists(covPaths[b])){
      r <- terra::rast(covPaths[b])
      #rc <- terra::crop(r, tilePoly)
      if(b==1){
        stk<-r
      }else{
        add(stk) <- r
      }
    }else{
      print(paste0('File does not exist - ', covPaths[b]))
    }
  }
  return(stk)
}

#HPC indexing
args = commandArgs(trailingOnly=T)
print(args)
k <- as.numeric(args[1])
method = args[2]
print(paste0('Iteration number = ', k))
print(paste0('Method = ', method))

grpSize=10

#### Set up Paths
rootDir = '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/MeasuredTextures/V2/Parsimonious'
covDir = '/datasets/work/lw-soildatarepo/work/http/Products/TERN/Covariates/Mosaics/90m'
outDir = paste0(rootDir,'/Drills/')
if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}


#### read in tile boundaries
tilesBdy <- st_read(paste0('/datasets/work/af-tern-mal-deb', '/work/datasets/national/covariates/tiles/Metadata/90m/allTiles_combined_RASTER.shp'))
print(paste0('Number of rows in Tile shapefile = ', nrow(tilesBdy)))
      
####  Read in depth splined data
inDF <- read.csv('/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/MeasuredTextures/SDF_All_Props_Clean_With_PTFS_Splined.csv')
print(paste0('Number of rows in the input data set = ', nrow(inDF)))
#head(inDF, 2)
pts <- st_as_sf(inDF, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
#plot(st_geometry(pts))

#### iterate through the tiles and extract covariate data

startK = (k-1) * grpSize

allOdf <- data.frame()

#covariates = getCovsStackPCA( covDir=covDir)
covariates = getCovStackRaw( covDir=covDir, filter = method )
print(names(covariates))

for (i in 1:grpSize) {
  kit <- startK + i
  print(paste0('Kit = ', kit))
  if(kit <= nrow(tilesBdy)){

    tile <- tilesBdy[kit,]
    tPts <- pts[tile,]

      if (nrow(tPts) != 0) { 
        covariate.training = as.data.frame(terra::extract(covariates, vect(tPts),method="simple"))
        odf = data.frame(st_drop_geometry(tPts), covariate.training, stringsAsFactors = F)
        allOdf <- rbind(allOdf,odf)
      }
  }  
  paste0("Fininished tile: k = ", kit)  
}

saveRDS(allOdf, paste0(outDir, '/TrainingData_', k, '.rds'))


endTime <- Sys.time()
print(endTime)
difft <- difftime(endTime, startTime, units = "auto")
print(difft)
print('')
print('Processing successfully completed')



