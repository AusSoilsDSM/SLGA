#################################################################################
# Objective: SOC fraction maps at 90m resolution 
# Date: 17/11/2021
# Author: Mercedes Roman
# Project: SOC fraction maps for TERN
# Depth 5-15 cm
# Script for running each tile in Artemis

### Note: this is an example script. You need to adapt it for a different supercomputer infrastructure facility
### this is adapted for Artemis (the University of Sydney HPC)
### Also, here I use 5-15 cm depth, you should change it for any other depth interval

### Load packages
library(sp)
library(raster)
library(rgdal)
library(ranger)
library(compositions)
library(parallel)
library(doParallel)
library(foreach)

### Set the parameters and bring the data
### Bring argument (tile number) from the PBS script
args <- commandArgs(trailingOnly = T)
i <- as.numeric(args[1])
print(paste0("row tile ",i))

### Depth we are predicting for
depth <-  "5_15"
orderProps <- c("HOC","POC","ROC")
combi.name <- "HPR"

### Job array
job_array <- "j2"

### Number of samples to estimate the prediction interval
nsamp <- 100

# path to the covariates raster
InputDir    <-"/project/RDS-FSC-SoilBiodSOC2019-RW/SOCfractions/Input/"
OutDirTiles <-paste0("/project/RDS-FSC-SoilBiodSOC2019-RW/SOCfractions/Output/",combi.name,"/",depth,"/",job_array,"/")
print(OutDirTiles)

# list all the raster files available
setwd(InputDir)
list_ras <- list.files(pattern=".tif$")

# load all the rasters
covariates.stack <- stack(paste0(InputDir,list_ras))
print(covariates.stack)

# dimensions of the stack: 
# nrow = 40800, 
# ncol = 49200, 
# ncell = 2007360000, 
# make blocks
bs <- blockSize(covariates.stack, minblocks = 100, minrows = 4) ### 4 rows per tile, or 10200 tiles

### Make the same for columns manually, although we leave the last one just to make it fit with the last column number of the raster stack
### 1480 cells is ok. --> 1480/4=370
### We try 2005 cells (5 rows X 400 cols)
bc <- list()
bc$col <- seq(from=1, to=ncol(covariates.stack), by=400)
bc$ncol <- c(rep(400,length(bc$col)-1),(ncol(covariates.stack)-((length(bc$col)-1)*400))-1)
bc$n <- length(bc$col)

### Load models
load(paste0('ilr_models.',combi.name,'.',depth,'.RData'))
model.ilr1 <- rf.ilr1.5_15
model.ilr2 <- rf.ilr2.5_15 
rm(rf.ilr1.5_15,rf.ilr2.5_15)

#############################################################################################################

### Start the mapping

### We are going to do a foreach to divide the block into tiles (bc)
cl <- makeCluster(10)   ### Create cluster
registerDoParallel(cl)
getDoParWorkers()

OutTiles <-foreach(j=1:length(bc$col), .packages=c("compositions","raster","ranger"),
                    .export = c("i","bs","bc","covariates.stack")) %dopar% {
                  
### Crop the tile in question 
### Test if the script works with small tile
#tile <- crop(covariates.stack, extent(covariates.stack,bs$row[[i]],bs$row[[i]]+ bs$nrows[[i]],200,300))

### When we crop just by rows, dividing Australia horizontally
#tile <- crop(covariates.stack, extent(covariates.stack, bs$row[[i]], bs$row[[i]]+bs$nrows[[i]], 1, ncol(covariates.stack)))
          
### In this case, we also split it vertically
tile <- crop(covariates.stack, extent(covariates.stack,
                                      bs$row[[i]],
                                      bs$row[[i]]+ bs$nrows[[i]],
                                      bc$col[[j]],
                                      bc$col[[j]]+ bc$ncol[[j]]))
tile 
### Transform into a dataframe
tile.df <- as.data.frame(tile, row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE, long=FALSE)
    
## Copy dataframe for outputs
tile.df.out <- tile.df
tile.df.out$pred.HOC <- NA
tile.df.out$pred.POC <- NA
tile.df.out$pred.ROC <- NA
tile.df.out$pred.HOC.05 <- NA
tile.df.out$pred.POC.05 <- NA
tile.df.out$pred.ROC.05 <- NA
tile.df.out$pred.HOC.95 <- NA
tile.df.out$pred.POC.95 <- NA
tile.df.out$pred.ROC.95 <- NA
    
### Extract the index of the dataframe rows that are na/nan/Inf
df.na <- which(apply(tile.df, MARGIN = 1, FUN = function(x) {any(is.na(x))}))

### If there is no missing data
if(length(df.na)==0){
print("no missing data in this tile")
  
  ### Predict mean SOC fractions (back-transformed ilr predictions)
  pred.ilr1 <- predict(object=model.ilr1,
                       data = tile.df,
                       type = "response")$predictions
  
  pred.ilr2 <- predict(object=model.ilr2,
                       data = tile.df,
                       type = "response")$predictions
  
  preds.ilr <- data.frame(pred.ilr1=pred.ilr1, pred.ilr2=pred.ilr2)
  
  ### Back-transform to HOC, POC, ROC (%)
  Backt <- compositions::ilrInv(preds.ilr)
  Backt <- as.data.frame(Backt)
  colnames(Backt) <- orderProps  #c("HOC", "POC","ROC")
  tile.df.out$pred.HOC <- Backt$HOC * 100
  tile.df.out$pred.POC <- Backt$POC * 100
  tile.df.out$pred.ROC <- Backt$ROC * 100
  
  ### Sample 100 values of ilr1 and ilr2 from the quantile distribution to estimate the prediction intervals
  pred.ilr1.sample <- predict(object=model.ilr1,
                              data = tile.df, 
                              type = "quantiles",
                              what = function(x) sample(x, nsamp, replace = TRUE))$predictions
  
  pred.ilr2.sample <- predict(object=model.ilr2,
                              data = tile.df, 
                              type = "quantiles",
                              what = function(x) sample(x, nsamp, replace = TRUE))$predictions
  
  ### Create array with both matrices
  ### Dimnames
  column.names <- paste0("Samp", c(1:nsamp))
  row.names <- paste0("row", c(1:nrow(tile.df)))
  matrix.names <- c("ilr1","ilr2")
  
  z <- array(c(pred.ilr1.sample, pred.ilr2.sample),
             dim = c(nrow(tile.df), nsamp, 2),
             dimnames = list(row.names, column.names, matrix.names)) 
  
  ### Back-transform to HOC, POC, ROC (%) with apply over the array
  backtf <- function(x){Backt <- compositions::ilrInv(x); return(Backt)}
  Backt <- apply(X = z, MARGIN = c(1,2) , FUN = backtf)
  dimnames(Backt)[[1]] <- orderProps #c("HOC", "POC","ROC")
  
  ### Calculate percentiles
  SOCfr.95 <- apply(X = Backt, MARGIN = c(1,2) , FUN = function(x) quantile(x, c(.95)))
  SOCfr.05 <- apply(X = Backt, MARGIN = c(1,2) , FUN = function(x) quantile(x, c(.05)))
  
  ### Transpose and transform into %
  UPL <-as.data.frame(t(SOCfr.95)); UPL <- UPL*100
  LPL <-as.data.frame(t(SOCfr.05)); LPL <- LPL*100
  
  ### Assign to dataframe with all rows
  tile.df.out$pred.HOC.05 <- LPL$HOC 
  tile.df.out$pred.POC.05 <- LPL$POC 
  tile.df.out$pred.ROC.05 <- LPL$ROC 
      
  tile.df.out$pred.HOC.95 <- UPL$HOC 
  tile.df.out$pred.POC.95 <- UPL$POC     
  tile.df.out$pred.ROC.95 <- UPL$ROC 
      
  ### Assign the values to a new raster
  pred.HOC <- setValues(tile[[1]], tile.df.out$pred.HOC)
  names(pred.HOC) <- paste0("HOC.",depth)
      
  pred.POC <- setValues(tile[[1]], tile.df.out$pred.POC)
  names(pred.POC) <- paste0("POC.",depth)
      
  pred.ROC <- setValues(tile[[1]], tile.df.out$pred.ROC)
  names(pred.ROC) <- paste0("ROC.",depth)
  
  pred.HOC.05 <- setValues(tile[[1]], tile.df.out$pred.HOC.05)
  names(pred.HOC.05) <- paste0("HOC.05p.",depth)
      
  pred.POC.05 <- setValues(tile[[1]], tile.df.out$pred.POC.05)
  names(pred.POC.05) <- paste0("POC.05p.",depth)
      
  pred.ROC.05 <- setValues(tile[[1]], tile.df.out$pred.ROC.05)
  names(pred.ROC.05) <- paste0("ROC.05p.",depth)
      
  pred.HOC.95 <- setValues(tile[[1]], tile.df.out$pred.HOC.95)
  names(pred.HOC.95) <- paste0("HOC.95p.",depth)
      
  pred.POC.95 <- setValues(tile[[1]], tile.df.out$pred.POC.95)
  names(pred.POC.95) <- paste0("POC.95p.",depth)
      
  pred.ROC.95 <- setValues(tile[[1]], tile.df.out$pred.ROC.95)
  names(pred.ROC.95) <- paste0("ROC.95p.",depth)
      
  gc()
  
  } else if (nrow(tile.df) - length(df.na) > 1) {
print("some missing data in tile")
    
    ### Predict mean SOC fractions (back-transformed ilr predictions)
    pred.ilr1 <- predict(object=model.ilr1,
                         data = tile.df[-df.na, ],
                         type = "response")$predictions
    
    pred.ilr2 <- predict(object=model.ilr2,
                         data = tile.df[-df.na, ],
                         type = "response")$predictions
    
    preds.ilr <- data.frame(pred.ilr1=pred.ilr1, pred.ilr2=pred.ilr2)
    
    ### Back-transform to HOC, POC, ROC (%)
    Backt <- compositions::ilrInv(preds.ilr)
    Backt <- as.data.frame(Backt)
    colnames(Backt) <- orderProps #c("HOC", "POC","ROC")
    tile.df.out[-df.na, ]$pred.HOC <- Backt$HOC * 100
    tile.df.out[-df.na, ]$pred.POC <- Backt$POC * 100
    tile.df.out[-df.na, ]$pred.ROC <- Backt$ROC * 100
    
    
    ### Sample 100 values of ilr1 and ilr2 from the quantile distribution to estimate the prediction intervals
    pred.ilr1.sample <- predict(object=model.ilr1,
                                data = tile.df[-df.na, ], 
                                type = "quantiles",
                                what = function(x) sample(x, nsamp, replace = TRUE))$predictions
    
    pred.ilr2.sample <- predict(object=model.ilr2,
                                data = tile.df[-df.na, ], 
                                type = "quantiles",
                                what = function(x) sample(x, nsamp, replace = TRUE))$predictions
    
    ### Create array with both matrices
    ### Dimnames
    column.names <- paste0("Samp", c(1:nsamp))
    row.names <- paste0("row", c(1:nrow(tile.df[-df.na,])))
    matrix.names <- c("ilr1","ilr2")
    
    z <- array(c(pred.ilr1.sample, pred.ilr2.sample), 
               dim = c(nrow(tile.df[-df.na, ]), nsamp, 2), 
               dimnames = list(row.names, column.names, matrix.names)) 
    
    ### Back-transform to HOC, POC, ROC (%) with apply over the array
    backtf <- function(x){Backt <- compositions::ilrInv(x); return(Backt)}
    Backt <- apply(X = z, MARGIN = c(1,2) , FUN = backtf)
    dimnames(Backt)[[1]] <- orderProps #c("HOC", "POC","ROC")
    
    ### Calculate percentiles
    SOCfr.95 <- apply(X = Backt, MARGIN = c(1,2) , FUN = function(x) quantile(x, c(.95)))
    SOCfr.05 <- apply(X = Backt, MARGIN = c(1,2) , FUN = function(x) quantile(x, c(.05)))
  
    ### Transpose and transform into %
    UPL <-as.data.frame(t(SOCfr.95)); UPL <- UPL*100
    LPL <-as.data.frame(t(SOCfr.05)); LPL <- LPL*100
    
    ### Assign to dataframe with all rows
    tile.df.out[-df.na, ]$pred.HOC.05 <- LPL$HOC 
    tile.df.out[-df.na, ]$pred.POC.05 <- LPL$POC     
    tile.df.out[-df.na, ]$pred.ROC.05 <- LPL$ROC 
    
    tile.df.out[-df.na, ]$pred.HOC.95 <- UPL$HOC 
    tile.df.out[-df.na, ]$pred.POC.95 <- UPL$POC     
    tile.df.out[-df.na, ]$pred.ROC.95 <- UPL$ROC 
    
    ### Assign the values to a new raster
    pred.HOC <- setValues(tile[[1]], tile.df.out$pred.HOC)
    names(pred.HOC) <- paste0("HOC.",depth)
    
    pred.POC <- setValues(tile[[1]], tile.df.out$pred.POC)
    names(pred.POC) <- paste0("POC.",depth)
    
    pred.ROC <- setValues(tile[[1]], tile.df.out$pred.ROC)
    names(pred.ROC) <- paste0("ROC.",depth)
    
    pred.HOC.05 <- setValues(tile[[1]], tile.df.out$pred.HOC.05)
    names(pred.HOC.05) <- paste0("HOC.05p.",depth)
    
    pred.POC.05 <- setValues(tile[[1]], tile.df.out$pred.POC.05)
    names(pred.POC.05) <- paste0("POC.05p.",depth)
    
    pred.ROC.05 <- setValues(tile[[1]], tile.df.out$pred.ROC.05)
    names(pred.ROC.05) <- paste0("ROC.05p.",depth)
    
    pred.HOC.95 <- setValues(tile[[1]], tile.df.out$pred.HOC.95)
    names(pred.HOC.95) <- paste0("HOC.95p.",depth)
    
    pred.POC.95 <- setValues(tile[[1]], tile.df.out$pred.POC.95)
    names(pred.POC.95) <- paste0("POC.95p.",depth)
    
    pred.ROC.95 <- setValues(tile[[1]], tile.df.out$pred.ROC.95)
    names(pred.ROC.95) <- paste0("ROC.95p.",depth)
    
    gc()
    
   } else if (nrow(tile.df) - length(df.na) == 1) {
print("only one observation")
     
     ### Predict mean SOC fractions (back-transformed ilr predictions)
     pred.ilr1 <- predict(object=model.ilr1,
                          data = tile.df[-df.na, ],
                          type = "response")$predictions
     
     pred.ilr2 <- predict(object=model.ilr2,
                          data = tile.df[-df.na, ],
                          type = "response")$predictions
     
     preds.ilr <- data.frame(pred.ilr1=pred.ilr1, pred.ilr2=pred.ilr2)
     
     ### Back-transform to HOC, POC, ROC (%)
     Backt <- compositions::ilrInv(preds.ilr)
     names(Backt) <- orderProps #c("HOC", "POC","ROC")
     tile.df.out[-df.na, ]$pred.HOC <- Backt[["HOC"]] * 100
     tile.df.out[-df.na, ]$pred.POC <- Backt[["POC"]] * 100
     tile.df.out[-df.na, ]$pred.ROC <- Backt[["ROC"]] * 100
     
     
     ### Sample 100 values of ilr1 and ilr2 from the quantile distribution to estimate the prediction intervals

     pred.ilr1.sample <- predict(object=model.ilr1,
                                 data = tile.df[-df.na, ], 
                                 type = "quantiles",
                                 what = function(x) sample(x, nsamp, replace = TRUE))$predictions
     
     pred.ilr2.sample <- predict(object=model.ilr2,
                                 data = tile.df[-df.na, ], 
                                 type = "quantiles",
                                 what = function(x) sample(x, nsamp, replace = TRUE))$predictions
     
     ### Create array with both matrices
     ### Dimnames
     z <- data.frame(ilr1=t(pred.ilr1.sample), ilr2=t(pred.ilr2.sample))
     rownames(z) <- paste0("Samp", c(1:nsamp))
   
     ### Back-transform to HOC, POC, ROC (%) with apply over the array
     backtf <- function(x){Backt <- compositions::ilrInv(x); return(Backt)}
     Backt <- backtf(z)
     names(Backt) <- orderProps #c("HOC", "POC","ROC")
     
     ### Calculate percentiles
     SOCfr.95 <- apply(X = Backt, MARGIN = 2, FUN = function(x) quantile(x, c(.95)))
     SOCfr.05 <- apply(X = Backt, MARGIN = 2, FUN = function(x) quantile(x, c(.05)))
     
     ### Transpose and transform into %
     UPL <- as.data.frame(t(SOCfr.95)); UPL <- UPL*100
     LPL <- as.data.frame(t(SOCfr.05)); LPL <- LPL*100
     
     ### Assign to dataframe with all rows
     tile.df.out[-df.na, ]$pred.HOC.05 <- LPL$HOC 
     tile.df.out[-df.na, ]$pred.POC.05 <- LPL$POC     
     tile.df.out[-df.na, ]$pred.ROC.05 <- LPL$ROC 
     
     tile.df.out[-df.na, ]$pred.HOC.95 <- UPL$HOC 
     tile.df.out[-df.na, ]$pred.POC.95 <- UPL$POC     
     tile.df.out[-df.na, ]$pred.ROC.95 <- UPL$ROC 
     
     ### Assign the values to a new raster
     pred.HOC <- setValues(tile[[1]], tile.df.out$pred.HOC)
     names(pred.HOC) <- paste0("HOC.",depth)
     
     pred.POC <- setValues(tile[[1]], tile.df.out$pred.POC)
     names(pred.POC) <- paste0("POC.",depth)
     
     pred.ROC <- setValues(tile[[1]], tile.df.out$pred.ROC)
     names(pred.ROC) <- paste0("ROC.",depth)
     
     pred.HOC.05 <- setValues(tile[[1]], tile.df.out$pred.HOC.05)
     names(pred.HOC.05) <- paste0("HOC.05p.",depth)
     
     pred.POC.05 <- setValues(tile[[1]], tile.df.out$pred.POC.05)
     names(pred.POC.05) <- paste0("POC.05p.",depth)
     
     pred.ROC.05 <- setValues(tile[[1]], tile.df.out$pred.ROC.05)
     names(pred.ROC.05) <- paste0("ROC.05p.",depth)
     
     pred.HOC.95 <- setValues(tile[[1]], tile.df.out$pred.HOC.95)
     names(pred.HOC.95) <- paste0("HOC.95p.",depth)
     
     pred.POC.95 <- setValues(tile[[1]], tile.df.out$pred.POC.95)
     names(pred.POC.95) <- paste0("POC.95p.",depth)
     
     pred.ROC.95 <- setValues(tile[[1]], tile.df.out$pred.ROC.95)
     names(pred.ROC.95) <- paste0("ROC.95p.",depth)
     
     gc()
     
     } else {
print("Empty tile")
       
       ### Assign the values to a new raster
       pred.HOC <- setValues(tile[[1]], tile.df.out$pred.HOC)
       names(pred.HOC) <- paste0("HOC.",depth)
       
       pred.POC <- setValues(tile[[1]], tile.df.out$pred.POC)
       names(pred.POC) <- paste0("POC.",depth)
       
       pred.ROC <- setValues(tile[[1]], tile.df.out$pred.ROC)
       names(pred.ROC) <- paste0("ROC.",depth)
       
       pred.HOC.05 <- setValues(tile[[1]], tile.df.out$pred.HOC.05)
       names(pred.HOC.05) <- paste0("HOC.05p.",depth)
       
       pred.POC.05 <- setValues(tile[[1]], tile.df.out$pred.POC.05)
       names(pred.POC.05) <- paste0("POC.05p.",depth)
       
       pred.ROC.05 <- setValues(tile[[1]], tile.df.out$pred.ROC.05)
       names(pred.ROC.05) <- paste0("ROC.05p.",depth)
       
       pred.HOC.95 <- setValues(tile[[1]], tile.df.out$pred.HOC.95)
       names(pred.HOC.95) <- paste0("HOC.95p.",depth)
       
       pred.POC.95 <- setValues(tile[[1]], tile.df.out$pred.POC.95)
       names(pred.POC.95) <- paste0("POC.95p.",depth)
       
       pred.ROC.95 <- setValues(tile[[1]], tile.df.out$pred.ROC.95)
       names(pred.ROC.95) <- paste0("ROC.95p.",depth)

       gc()
      
     }

### Create a raster stack, and resturn it
stack.out <- stack(pred.HOC,pred.POC,pred.ROC,pred.HOC.05,pred.POC.05,pred.ROC.05,pred.HOC.95,pred.POC.95,pred.ROC.95)
stack.out
}

stopCluster(cl)
names.write <- c("pred.HOC.m.","pred.POC.m.","pred.ROC.m.",
                 "pred.HOC.05p.","pred.POC.05p.","pred.ROC.05p.",
                 "pred.HOC.95p.","pred.POC.95p.","pred.ROC.95p." )

#### Mosaic each variable into just the horizontal tile and save into RDS file
for(var in 1:9){
  rast.list <- list()
  for(j in 1:length(bc$col)){
    rast.list[[j]] <- OutTiles[[j]][[var]]
  }
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  pred <- do.call(mosaic, rast.list)
  names(pred) <- names(OutTiles[[j]][[var]])
  print(pred)
  print(paste0(OutDirTiles,names.write[[var]],i,".",depth,".tif"))
  writeRaster(pred, filename= paste0(OutDirTiles,names.write[[var]],i,".",depth,".tif"),
              na.rm=T,inf.rm=T, format="GTiff", overwrite=TRUE )
  }

### End of the script for this tile :)
