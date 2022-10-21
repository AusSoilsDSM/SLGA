
#################################
###  Author : Ross Searle         
###  Date : Wed Oct  6 09:06:22 2021                      
###  Project : GRDC SoilWaterNow Project
###  Purpose : Map the RF models for tiles on the HPC
###  Notes : Couldn't get raster predict function to work so went back to the tried and tested approach
#################################

startTime <- Sys.time()
print(startTime)
print(paste0('R Version = ', version[13]))

library(terra)
#library(ranger)
library(matrixStats)
library(sf)
library(stringr)
library(Cubist)
terraOptions(maxmem=20)

clipCovsD <- function(tilePoly, covDir, covFilter, depth){
  
  print(paste0("Filtering covariates on method = ", covFilter))
  covsDF <- read.csv('/datasets/work/af-digiscapesm/work/Ross/TERN/covsToUse90m.csv', stringsAsFactors = F)
  covToUse <- covsDF[covsDF[covFilter]==1,]$Covariate
  covPaths <- paste0(covDir, '/', covToUse, '.tif')
  
  #covPaths <- list.files(covDir, pattern='.tif$', full.names = T)
  
  for (b in 1:length(covPaths)) {
    print(covPaths[b])
    if(file.exists(covPaths[b])){
      r <- terra::rast(covPaths[b])
      rc <- terra::crop(r, tilePoly)
      #m<-as.numeric(global(rc, mean, na.rm = T))
      #idxs <- which(is.na(rc[]))
      #rc[idxs] <- m
      #plot(rc)
      if(b==1){
        stk<-rc
      }else{
        terra::add(stk) <- rc
      }
    }else{
      print(paste0('File does not exist - ', covPaths[b]))
    }
  }
  
  ######  add in the slga rasters  #####
  print('Adding SLGA rasters')
  depths1 <- c('000', '005','015','030','060','100')
  depths2 <- c('005','015','030','060','100','200')
  depthsDF <- data.frame(d1=depths1, d2=depths2)
  d1 <- depthsDF[depthsDF$d2==depth,1]
  cr <- rast(paste0('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/CLY/CLY_', d1, '_', depth, '_EV_N_P_AU_TRN_N_20210902.tif' ))
  sr <- rast(paste0('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SND/SND_', d1, '_', depth, '_EV_N_P_AU_TRN_N_20210902.tif' ))
  socr <- rast(paste0('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SOC/SOC_', d1, '_', depth, '_EV_N_P_AU_NAT_N_20220727.tif' ))
  phr <- rast(paste0('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/PHW/PHW_', d1, '_', depth, '_EV_N_P_AU_TRN_N_20220520.tif' ))
  
  crc <- terra::crop(cr, tilePoly)
  src <- terra::crop(sr, tilePoly)
  socc <- terra::crop(socr, tilePoly)
  phcc <- terra::crop(phr, tilePoly)
  
  terra::add(stk) <- crc
  terra::add(stk) <- src
  terra::add(stk) <- socc
  terra::add(stk) <- phcc
  
  return(stk)
}

args = commandArgs(trailingOnly=T)

k = as.numeric(args[1])
att = args[2]
prod = args[3]
depth = args[4]
covFilter = args[5]
grpSize=as.numeric(args[6])

machineName <- as.character(Sys.info()['nodename'])
print(paste0('Node=', machineName))
print(paste0('k=', k, ' att=', att, ' prod=', prod, ' depth=', depth, ' groupSize=', grpSize))

numBoots=50

combo=paste0(att, '_', covFilter, '_', depth)

rootDir = '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/MeasuredTextures/V3/Parsimonious'
processDir = paste0('/scratch2/', ident, '/', prod, '/processingLogs/', combo)
covDir = '/datasets/work/lw-soildatarepo/work/http/Products/TERN/Covariates/Mosaics/90m'
outDir = paste0('/scratch1/', ident, '/', prod, '/', covFilter, '/Tiles/', combo)

if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}
if(!dir.exists(processDir)){dir.create(processDir, recursive = T)}

tilesBdy <- st_read(paste0('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/tiles/Metadata/90m/allTiles_combined_RASTER.shp'))

startK = (k-1) * grpSize

for (i in 1:grpSize) {
  kit <- startK + i
  print(kit)
  if(kit <= nrow(tilesBdy)){
    
    print('')
    print(paste0('Processing tile No. ', kit))
    print('')
    
    tile <- tilesBdy[kit,]
    
    outMeanRp <- paste0(outDir, '/', kit, '_',att, '_', depth, '_Mean.tif' )
    
    if(!file.exists(outMeanRp)){
    
          print('Loading covariate tile data .....')
          covsR <- clipCovsD(tilePoly = tile, covDir = covDir, covFilter=covFilter, depth=depth)
          repdot <- str_replace_all(names(covsR), '-', '.')
          repdot2 <- str_replace_all(repdot, '[+]', '.')
          names(covsR) <- repdot2
          
          templateR <- covsR[[1]]
          
          ncells = nrow(templateR)*ncol(templateR)
          theSeq = seq(ncells)
          covs = data.frame(theSeq)
           
          for (i in 1:length(names(covsR))) {
            cat(i, ' ')
            r <- covsR[[i]]
            covs[names(r)] <- r[]
          }

          idxs <- which(complete.cases(covs))
          if(length(idxs) > 0){
              print(paste0('Percentage Not NoData = ', sprintf((length(idxs) / (nrow(templateR) * ncol(templateR))) * 100, fmt = '%#.2f')  ))
              y <- covs[idxs,]
              theSeq2 <- seq(1:length(idxs))
              pstk = data.frame(theSeq2)
             
              
              td <- paste0(processDir,'/', kit)
              if(!dir.exists(td)){dir.create(td)}
              
              tmpfls <- list.files(td, pattern = paste0('_', att, '.rds'))
              numtmpfls <- length(tmpfls)
              
              #########  Predict DLL Tiles ########## 
              print(paste0('Doing ', att, ' model predictions .....'))
              
              for (a in 1:numBoots) {
              #for (a in 1:5) {
                #tmpF <- paste0(td,'/', att, '_',a, '.rds' )
                tmpF <- paste0(td,'/', a, '_', att, '.rds' )
                if(file.exists(tmpF) & a < numtmpfls & numtmpfls < numBoots)
                {
                  print(paste0('File exists - ', tmpF))
                  mp <- readRDS(tmpF)
                  pstk[paste0('r_',a)] <- mp
                }
                else
                {
                  cat(a, ' ')
                  if(att=='DUL'){
                    dllR <- rast(paste0('/scratch1/', ident, '/AWCv3/Parsimonious/Tiles/DLL_Parsimonious_', depth, '/',kit, '_', 'DLL', '_', depth,'_Mean.tif' ))
                    vals <- dllR[]
                    idxs <- which(!is.na(vals))
                    dllName <- paste0('DLL_', depth)
                    y[dllName] <- vals[idxs]
                  }
                  
                  model <- readRDS(paste0(rootDir, '/Models/', att, '/Depth_', depth, '/Cubistmodel_', att,'_', depth, '_',a, '.rds' ))
                  mp <- predict(model, y)
                  saveRDS(mp, tmpF)
                  #pstk[paste0('r_',a)] <- mp$predictions
                  pstk[paste0('r_',a)] <- mp
                }
              }
              
              print('')
              print(paste0('Finished doing  ', att, ' model predictions .....'))
              print('')
              
              print(paste0('Doing prediction summaries for ', att ))
              mat <- as.matrix(pstk[,-1])
              qts <- rowQuantiles(mat, probs = c(0.05,0.50,0.95))
              mns <- rowMeans(mat)
              print(paste0('Finished doing  prediction summaries for ', att, '.....'))
              print('')
              
              print(paste0('Writing output tiles for ', att, ' to ', outDir))
             
              templateR[] <- NA
                
              out05Rp <- paste0(outDir, '/',kit, '_', att, '_', depth,'_05.tif' )
              R05 <- rast(templateR)
              R05[idxs] <- qts[,1]
              writeRaster(R05, out05Rp, overwrite=T)
              
              outMedRp <- paste0(outDir, '/',kit, '_', att, '_', depth,'_Median.tif' )
              Rmedian <- rast(templateR)
              Rmedian[idxs] <- qts[,2]
              writeRaster(Rmedian, outMedRp, overwrite=T)
              
              out95Rp <- paste0(outDir, '/',kit, '_', att, '_', depth,'_95.tif' )
              R95 <- rast(templateR)
              R95[idxs] <- qts[,3]
              writeRaster(R95, out95Rp, overwrite=T)
              
              RMean <- rast(templateR)
              RMean[idxs] <- mns
              writeRaster(RMean, outMeanRp, overwrite=T)
              
             
             
              
              
              print('Finished writing output tiles.....')
              print('')
              
              print(paste0('Removing temporary processing files from ', td))
              print('')
              tfls <- list.files(td, full.names = T, recursive = F)
              file.remove(tfls)
              unlink(td, recursive = TRUE)
              
              print('Finished removing temporary processing files')
              print('')
              
          }else{
              print(paste0('No raster data under this tile - ', kit))
          }
    }else{
      print(paste0('File exists - ', outMeanRp))
    }
  }
}


endTime <- Sys.time()
print(endTime)
difft <- difftime(endTime, startTime, units = "auto")
print(difft)
print('Processing successfully completed')



