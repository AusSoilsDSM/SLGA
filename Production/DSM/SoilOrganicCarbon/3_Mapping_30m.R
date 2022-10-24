library(raster)
library(rgdal)
library(ranger)
library(parallel)

# path to the covariates raster
setwd('/media/alex/VERBATIM HD/covariates')

# list all the raster files available 
list_ras <- list.files('/media/alex/VERBATIM HD/covariates/processed_30m/', pattern="tif$")

# load all the rasters
covariates.stack <- stack(paste0('/media/alex/VERBATIM HD/covariates/processed_30m/', list_ras))

# make blocks
bs <- blockSize(covariates.stack,chunksize = 5e7 , minblocks = 50)
bs

####################
#################### TOC
####################

# AW: no need for the depths 0-5 and 5-15 cm (already done)
depths <- c("15_30", "30_60", "60_100", "100_200")

for (d in 1:length(depths)){ # for all depths
  
  # which depth
  depth.TOC <- depths[d]
  
  # load model
  x <- load(paste0('./data/TOC_',depth.TOC, '_cm_model_evaluationCV_30m.Rdata'))
  model <- get(x)[[1]] ; rm(x)
  
  FUN <- function(i){
    #tile_temp <- readAll(crop(covariates.stack, tiles.pol[i]))
    tile_temp <- readAll(crop(covariates.stack, extent(covariates.stack, bs$row[[i]], bs$row[[i]]+bs$nrows[[i]], 1, ncol(covariates.stack))))
    
    #tile_temp2 <- as.data.frame(tile_temp, na.)
    #predict(object = model, data = tile_temp2)
    pred <- raster::predict(object = tile_temp, 
                            model = model, 
                            fun = function(model, ...) predict(model, ...)$predictions)
    #tile_temp2 <- as.data.frame(tile_temp2, )
    predvar05 <- raster::predict(object = tile_temp, 
                               model = model, 
                               fun = function(model, ...) predict(model,type = 'quantiles', quantiles =c(0.05), ...)$predictions[,1])
    predvar95 <- raster::predict(object = tile_temp, 
                               model = model, 
                               fun = function(model, ...) predict(model,type = 'quantiles', quantiles =c(0.95), ...)$predictions[,1])
    
    # link to path where the files are saves
    writeRaster(pred, file = paste0('/media/alex/VERBATIM HD/covariates/tiles/TOC/', depth.TOC, '/pred/', i, '.tif'))
    writeRaster(predvar05, file = paste0('/media/alex/VERBATIM HD/covariates/tiles/TOC/', depth.TOC, '/predvar05/', i, '.tif'))
    writeRaster(predvar95, file = paste0('/media/alex/VERBATIM HD/covariates/tiles/TOC/', depth.TOC, '/predvar95/', i, '.tif'))
 
    removeTmpFiles(h=1)
    }
  
  # create files in the path
  dir.create(paste0('/media/alex/VERBATIM HD/covariates/tiles/TOC/', depth.TOC))
  
  dir.create(paste0('/media/alex/VERBATIM HD/covariates/tiles/TOC/', depth.TOC, '/pred'))
  dir.create(paste0('/media/alex/VERBATIM HD/covariates/tiles/TOC/', depth.TOC, '/predvar05'))
  dir.create(paste0('/media/alex/VERBATIM HD/covariates/tiles/TOC/', depth.TOC, '/predvar95'))
  
  ## not in parallel 
  #llply(seq_along(tiles.pol)[1:100], FUN)
  
  cl <- makeCluster(detectCores())
  clusterExport(cl=cl, 
                varlist=c("bs", "covariates.stack", 'readAll', 'model', 'depth.TOC'))
  clusterEvalQ(cl, {library(ranger); library(raster)})
  parLapply(cl,
            seq_along(1:bs$n),
            FUN)
  stopCluster(cl)
  
  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####
  ##### MERGE PREDICTED MAPS
  # list all files for predicted maps

  n.subtile <- 20
  
  rasters1 <- list.files(paste0('/media/alex/VERBATIM HD/covariates/tiles/TOC/', depth.TOC, '/pred'), pattern="tif$")
  
  sp.t <- split(rasters1, ceiling(seq_along(rasters1)/(length(rasters1)/n.subtile)))
  
  for(i in 1:length(sp.t)){
    
    ras.sub1 <- sp.t[[i]]
    removeTmpFiles(h=0)
    
    # read all rasters and merge
    rast.list <- list()
    for(ii in 1:length(ras.sub1)) { 
      tile_temp <- raster(paste0('/media/alex/VERBATIM HD/covariates/tiles/TOC/', depth.TOC, '/pred/', ras.sub1[ii]))
      rast.list[ii] <- tile_temp
      print(ii)
    }
    
    # And then use do.call on the list of raster objects
    names(rast.list)[1:2] <- c('x', 'y')
    rast.list$fun <- mean
    rast.list$na.rm <- TRUE
    rast.list$tolerance <- 0.1
    
    y_pred <- do.call(mosaic, rast.list)
    
    plot(y_pred)
    
    writeRaster(y_pred, filename =paste0("/media/alex/VERBATIM HD/covariates/maps/TOC_", depth.TOC, "_pred_30m_",i ,".tif"), overwrite = TRUE)
    
    print(i)
  }
  
  rast.list <- list()
  for (i in 1:n.subtile){
  tile_temp <- raster(paste0('/media/alex/VERBATIM HD/covariates/maps/TOC_', depth.TOC, "_pred_30m_",i ,".tif"))
  rast.list[i] <- tile_temp
  
  
  }
  # And then use do.call on the list of raster objects
  names(rast.list)[1:2] <- c('x', 'y')
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  rast.list$tolerance = 0.1
  y_pred <- do.call(mosaic, rast.list)
  plot(y_pred)
  
  writeRaster(y_pred, filename =paste0("/media/alex/VERBATIM HD/covariates/maps/TOC_", depth.TOC, "_pred_30m.tif"))
  
  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####
  ##### MERGE PREDICTED MAPS
  # list all files for predicted maps
  
  n.subtile <- 20
  
  rasters1 <- list.files(paste0('/media/alex/VERBATIM HD/covariates/tiles/TOC/', depth.TOC, '/predvar05'), pattern="tif$")
  
  sp.t <- split(rasters1, ceiling(seq_along(rasters1)/(length(rasters1)/n.subtile)))
  
  for(i in 1:length(sp.t)){
    
    ras.sub1 <- sp.t[[i]]
    removeTmpFiles(h=0)
    
    # read all rasters and merge
    rast.list <- list()
    for(ii in 1:length(ras.sub1)) { 
      tile_temp <- raster(paste0('/media/alex/VERBATIM HD/covariates/tiles/TOC/', depth.TOC, '/predvar05/', ras.sub1[ii]))
      rast.list[ii] <- tile_temp
      print(ii)
    }
    
    # And then use do.call on the list of raster objects
    names(rast.list)[1:2] <- c('x', 'y')
    rast.list$fun <- mean
    rast.list$na.rm <- TRUE
    rast.list$tolerance = 0.1
    
    y_predvar <- do.call(mosaic, rast.list)
    
    plot(y_predvar)
    
    writeRaster(y_predvar, filename =paste0("/media/alex/VERBATIM HD/covariates/maps/TOC_", depth.TOC, "_predvar05_30m_",i ,".tif"), overwrite = TRUE)
    
    print(i)
  }
  
  rast.list <- list()
  for (i in 1:n.subtile){
    tile_temp <- raster(paste0('/media/alex/VERBATIM HD/covariates/maps/TOC_', depth.TOC, "_predvar05_30m_",i ,".tif"), pattern="tif$")
    rast.list[i] <- tile_temp
    
    
  }
  # And then use do.call on the list of raster objects
  names(rast.list)[1:2] <- c('x', 'y')
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  rast.list$tolerance = 0.1
  
  y_predvar <- do.call(mosaic, rast.list)
  plot(y_predvar)
  
  writeRaster(y_predvar, filename =paste0("/media/alex/VERBATIM HD/covariates/maps/TOC_", depth.TOC, "_predvar05p_30m.tif"))
  
  
  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####  #####
  ##### MERGE PREDICTED MAPS
  # list all files for predicted maps
  
  n.subtile <- 20
  
  rasters1 <- list.files(paste0('/media/alex/VERBATIM HD/covariates/tiles/TOC/', depth.TOC, '/predvar95'), pattern="tif$")
  
  sp.t <- split(rasters1, ceiling(seq_along(rasters1)/(length(rasters1)/n.subtile)))
  
  for(i in 1:length(sp.t)){
    
    ras.sub1 <- sp.t[[i]]
    removeTmpFiles(h=0)
    
    # read all rasters and merge
    rast.list <- list()
    for(ii in 1:length(ras.sub1)) { 
      tile_temp <- raster(paste0('/media/alex/VERBATIM HD/covariates/tiles/TOC/', depth.TOC, '/predvar95/', ras.sub1[ii]))
      rast.list[ii] <- tile_temp
      print(ii)
    }
    
    # And then use do.call on the list of raster objects
    names(rast.list)[1:2] <- c('x', 'y')
    rast.list$fun <- mean
    rast.list$na.rm <- TRUE
    rast.list$tolerance = 0.1
    
    y_predvar <- do.call(mosaic, rast.list)
    
    plot(y_predvar)
    
    writeRaster(y_predvar, filename =paste0("/media/alex/VERBATIM HD/covariates/maps/TOC_", depth.TOC, "_predvar95_30m_",i ,".tif"), overwrite = TRUE)
    
    print(i)
  }
  
  rast.list <- list()
  for (i in 1:n.subtile){
    tile_temp <- raster(paste0('/media/alex/VERBATIM HD/covariates/maps/TOC_', depth.TOC, "_predvar95_30m_",i ,".tif"))
    rast.list[i] <- tile_temp
    
    
  }
  # And then use do.call on the list of raster objects
  names(rast.list)[1:2] <- c('x', 'y')
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  rast.list$tolerance = 0.1
  
  y_predvar <- do.call(mosaic, rast.list)
  plot(y_predvar)
  
  writeRaster(y_predvar, filename =paste0("/media/alex/VERBATIM HD/covariates/maps/TOC_", depth.TOC, "_predvar95p_30m.tif"))
  
  
}
