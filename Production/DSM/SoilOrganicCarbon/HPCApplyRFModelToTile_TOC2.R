
#################################
###  Author : Ross Searle         
###  Date : Wed Sept  9 09:06:22 2022                      
###  Project : TERN Landscapes Project
###  Purpose : Map the RF models for tiles on the HPC
###  Notes : 
#################################

startTime <- Sys.time()
print(startTime)
print(paste0('R Version = ', version[13]))

library(terra)
library(matrixStats)
library(sf)
library(stringr)
library(ranger)

source('PrivateInfo.R')

terraOptions(maxmem=20)


#####  This function clips the full national mosaic rasters to the supplied tile boundary
clipCovsD <- function(tilePoly, covDir){
  
  covPaths <- list.files(covDir, pattern='.tif$', full.names = T)
  print(paste0('Number of covariates = ', length(covPaths)))

  for (b in 1:length(covPaths)) {
    #print(covPaths[b])
    cat(b, ' ', sep = "")
    if(file.exists(covPaths[b])){
      r <- terra::rast(covPaths[b])
      rc <- terra::crop(r, tilePoly)
      if(b==1){
        stk<-rc
      }else{
        terra::add(stk) <- rc
      }
    }else{
      print(paste0('File does not exist - ', covPaths[b]))
    }
  }
  return(stk)
}

args = commandArgs(trailingOnly=T)
print(args)
k = as.numeric(args[1])
grpSize=as.numeric(args[2])

machineName <- as.character(Sys.info()['nodename'])
print(paste0('Node=', machineName))
print(paste0('k=',  k, ' groupSize=', grpSize))


modelDir = '/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilCarbon_CNN/alexandre_work/models'
#covDir = '/datasets/work/af-tern-mal-deb/work/datasets/national/alexandre/processed_30m'
covDir = paste0('/scratch2/', ident, '/processed_30m')
outDir = paste0('/scratch1/', ident, '/TOC30m')

if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}

tilesBdy <- st_read(paste0('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/allTiles_30m.shp'))
#tilesBdy <- st_read(paste0('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/Metadata/90m/allTiles_combined_RASTER.shp'))

### We use groups of tasks to get the total number of jobs under the slurm limit of 5000
startK = (k-1) * grpSize

for (i in 1:grpSize) {
  kit <- startK + i
  print(kit)
  if(kit <= nrow(tilesBdy)){
    
    print('')
    print(paste0('Processing tile No. ', kit))
    print('')
    
    ### Get the tile boundary for this iteration
    tile <- tilesBdy[kit,]
    
          print('Loading covariate tile data .....')
          covsR <- clipCovsD(tilePoly = tile, covDir = covDir)
          
          #### fix up some naming issues
           repdot <- str_replace_all(names(covsR), '-', '.')
           repdot2 <- str_replace_all(repdot, '[+]', '.')
           names(covsR) <- repdot2
           print(mem_info(covsR))
          
          #depths <- c("15_30", "30_60", "60_100", "100_200")
          depths <- c("0_5", "5_15")
          
          #### iterate through the depths
          for (d in 1:length(depths)){ # for all depths
            
            # which depth
            depth.TOC <- depths[d]
            od <- paste0(outDir, '/', depth.TOC)
            if(!dir.exists(od)){dir.create(od)}
            
            
            #### load model
            x <- load(paste0(modelDir, '/TOC_',depth.TOC, '_cm_model_evaluationCV_30m.Rdata'))
            model <- get(x)[[1]] ; rm(x)
            
            #### do the model predictions
            
            of1 <- paste0(outDir, '/', depth.TOC, '/mean_', kit, '.tif')
            if(!file.exists(of1)){
              print(paste0('Doing mean predictions for depth ', depth.TOC))
              pred <- terra::predict(object = covsR, model = model, na.rm=T, fun = function(model, ...) predict(model, ...)$predictions)
              writeRaster(pred, file = of1, overwrite=T)
            }else{
              print(paste0('File exists : ', of1))
            }
            
            of2 <- paste0(outDir, '/', depth.TOC, '/predvar05_', kit, '.tif')
            if(!file.exists(of2)){
              print(paste0('Doing 05 predictions for depth ', depth.TOC))
              predvar05 <- raster::predict(object = covsR, model = model, na.rm=T, fun = function(model, ...) predict(model,type = 'quantiles', quantiles =c(0.05), ...)$predictions[,1])
              writeRaster(predvar05, file = of2, overwrite=T)
            }else{
              print(paste0('File exists : ', of2))
            }
            
            of3 <- paste0(outDir, '/', depth.TOC, '/predvar95_', kit, '.tif')
            if(!file.exists(of3)){
              print(paste0('Doing 95 predictions for depth ', depth.TOC))
              predvar95 <- raster::predict(object = covsR, model = model, na.rm=T, fun = function(model, ...) predict(model,type = 'quantiles', quantiles =c(0.95), ...)$predictions[,1])
              writeRaster(predvar95, file = of3, overwrite=T)
            }else{
              print(paste0('File exists : ', of3))
            }
           
          }
    
              print('')
              print('Finished doing model predictions .....')
              print('')
  }
}


endTime <- Sys.time()
print(endTime)
difft <- difftime(endTime, startTime, units = "auto")
print(difft)
####  Keep the line below as it is used to find uncompleted jobs in the R HPC framework
print('Processing successfully completed')
