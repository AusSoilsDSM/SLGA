library(raster)
library(rgdal)
library(ranger)
library(stringr)
library(tictoc)


rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+09,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory



##monitor memory

#####################################################################
#########                                                  ##########
#########  Functions above. Actual processing starts here  ##########
#########                                                  ##########
#####################################################################

args = commandArgs(trailingOnly=T)
tic()

# k=1000
# patt<-'ASC'

# chunkLines <- 20
# isnumeric <- F

k = as.numeric(args[1])
patt = args[2]

chunkLines = as.numeric(args[3])
isnumeric = as.logical(args[4])
#   source('/datasets/work/lw-slga/work/Projects/Roper/Scripts/RFUtils.R')
# }


Allstart_time <- Sys.time()
print(paste0('Chunk processing started at ', Allstart_time))

print(paste0('args : k=', k, ' patt=', patt,  ' chunklines=', chunkLines, ' isnumeric=', isnumeric))

scriptDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/Functions'
covDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'
#workDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN/ASC'
workDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN/ASCOnly'
source(paste0(scriptDir, '/RFUtils.R'))


templateR <- raster(paste0(covDir, '/Relief_dems_3s_mosaic1.tif'))


cat(paste0('Iteration No. = ', k, '\n'))

outChkDir <- paste0(workDir, '/BootMaps/', patt, '/Chunks')
if(!dir.exists(outChkDir)){dir.create(outChkDir, recursive = T)}

outfile <- paste0(workDir, '/BootMaps/', patt, '/Chunks/AllCellVals_', k, '.rds')

print(outfile)
if ( !file.exists(outfile)){
  
  cat("\nLoading Data.....\n")
  cat("-------------------------\n")
 
  #templateR <- raster(paste0(rootDir, '/Templates/template.tif'))
  cat('Template\n')
  cat('------------------\n')
  print(templateR)
  
  cat("\nTemplate successfully loaded\n")
  
  cat('Covariates\n------------\n')
  # #cnames <- model$forest$independent.variable.names
  # fls1 <- paste0(covDir, '/', cnames)
  # fls2 <- str_replace_all(fls1, '[.]', '-')
  # fls <- paste0(fls2, '.tif')
  # cat(paste0(cnames,  '\n'))
  # theStack <- stack(fls)
  
  covs <- read.csv(paste0('/datasets/work/af-digiscapesm/work/Ross/TERN/covsToUse.csv'), stringsAsFactors = F)
  covsToUse <- paste0(covDir, '/', covs[covs$Parsimonious==1, 2], '.tif')
  
  theStack <- stack(covsToUse)
  cat(paste0('\nNumber of covariates being used = ', nlayers(theStack), '\n'))
  names(theStack)
  
  cat('\n')
  cat('Loading covariate data ...\n')
  
  #chk <- blockSize(theStack)
  chk <-	getChunkInfo(chunkLines, nrow(templateR))
  
  ncells = chk$nlines[k] * ncol(templateR)
  
  theSeq = seq(ncells)
  covs = data.frame(theSeq)
  
  start_time <- Sys.time()
  print(start_time)
  
  for (i in 1:nlayers(theStack))
  {
    print(i)
    rl = raster(theStack, layer=i)
    v <- as.numeric(getValues(rl, row=chk$starts[k], nrows=chk$nlines[k]))
    
    #### this is a hack to deal with NA needs to be sorted
    if(i > 1){
      badInds <- which(is.na(v))
      if(length(badInds) > 0)
      {
        v[badInds] <- 0
      }
    }
    covs[names(rl)] <- v
  }
  
  cat('Covariates size = ', as.numeric(object.size(covs)),' bytes\n')
  
  valsi <- which(!is.na(covs[,2]))
  valsnd <- which(is.na(covs[,2]))
  
  tt <- Sys.time() - start_time
  
  cat('Covariate data successfully loaded in ', tt, ' sec\n\n')
  cat('Running model predictions on raster chunk .....\n')
  cat("--------------------------------------------------\n")
  cat('Data will be written to ', outfile, '\n')
  
  outDF <- data.frame(valsi)
  
  numBoots = 50
#  numBoots = 4
  
  if(length(valsi) > 0){
    
    for (j in 1:numBoots) {
      
      print(paste0('Model iteration ', j))
      print(paste0('Reading - ', paste0(workDir, '/BootModels/RFmodel_', patt, '_',  j, '.rds')))
      model <- readRDS(paste0(workDir, '/BootModels/RFmodel_', patt, '_', j, '.rds'))
      prediction = predict(model, covs[valsi,-1],  predict.all = F, num.threads=1)$predictions
    
          if(model$forest$treetype == 'Classification')
          {
            if(isnumeric)
            {
              m <- matrix(prediction,nrow=nrow(prediction))
              outM <- apply(m,2,convertToNumeric)
            }else{
              outM <- prediction
            }
          }else{
            outM <- prediction
          }
          outDF <- cbind(outDF, outM)
          #print('Predictions size = ', object.size(outDF))
          cat('Predictions size = ', as.numeric(object.size(outDF)),' bytes\n')
    }
          
    mData <- apply(outDF[,-1], 1, getModalValue)
    ciData <- apply(outDF[,-1], 1, getConfusionIndex)
    odf <- data.frame(valsi=outDF[,1], mode=mData, ci=ciData)
    saveRDS(odf, outfile)
    
    alltt <- Sys.time() - Allstart_time
    print(paste0('Total processing time was ', alltt, ' ',  units(alltt)))
    

    cat(paste0('Iteration Finished Successfully'))
  }
}else{
  
  cat('\nFile exists - ', paste0('cellSummaries_', k, '.rds\n\n'))
  cat(paste0('Iteration Finished Successfully'))
}

