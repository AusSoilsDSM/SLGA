library(raster)
library(rgdal)
library(ranger)
library(stringr)
library(tictoc)

rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+09,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory
#utils:::format.object_size(ncol(templateR)*4*50 * 50, "auto")


##monitor memory

#####################################################################
#########                                                  ##########
#########  Functions above. Actual processing starts here  ##########
#########                                                  ##########
#####################################################################

args = commandArgs(trailingOnly=T)
tic()

# k=1000
# patt<-'ASCOnly'
# modelFileName <- 'RFmodel_ASC.rds'
# modelFileName <- 'RFmodel_ASCOnly.rds'
# chunkLines <- 20
# isnumeric <- F

k = as.numeric(args[1])
patt = args[2]
modelFileName = args[3]
chunkLines = as.numeric(args[4])
isnumeric = as.logical(args[5])
#   source('/datasets/work/lw-slga/work/Projects/Roper/Scripts/RFUtils.R')
# }

print(paste0('args : k=', k, ' patt=', patt, ' model=',modelFileName, ' chunklines=', chunkLines, ' isnumeric=', isnumeric))

scriptDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/Functions'
covDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'
workDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN/ASC'

source(paste0(scriptDir, '/RFUtils.R'))


templateR <- raster(paste0(covDir, '/Relief_dems_3s_mosaic1.tif'))

cat(paste0('Iteration No. = ', k, '\n'))

outChkDir <- paste0(workDir, '/Maps/', patt, '/Chunks')
if(!dir.exists(outChkDir)){dir.create(outChkDir, recursive = T)}

outfile <- paste0(workDir, '/Maps/', patt, '/Chunks/AllCellVals_', k, '.rds')
if ( !file.exists(outfile)){
  
  cat("\nLoading Data.....\n")
  cat("-------------------------\n")
  model <- readRDS(paste0(workDir, '/Models/', modelFileName))
  #templateR <- raster(paste0(rootDir, '/Templates/template.tif'))
  cat('Template\n')
  cat('------------------\n')
  print(templateR)
  
  cat("\nTemplate successfully loaded\n")
  
  cat('Covariates\n------------\n')
  cnames <- model$forest$independent.variable.names
  fls1 <- paste0(covDir, '/', cnames)
  fls2 <- str_replace_all(fls1, '[.]', '-')
  fls <- paste0(fls2, '.tif')
  cat(paste0(cnames,  '\n'))
  theStack <- stack(fls)
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
  
  valsi <- which(!is.na(covs[,2]))
  valsnd <- which(is.na(covs[,2]))
  
  tt <- Sys.time() - start_time
  
  cat('Covariate data successfully loaded in ', tt, ' mins\n\n')
  
  
  cat('Running model predictions on raster chunk .....\n')
  cat("--------------------------------------------------\n")
  
  #vname <- paste0(rootDir, '/Maps/', att, '/Chunks/cellSummaries_', k, '.rds')
  cat('Data will be written to ', outfile, '\n')
  
  
  if(length(valsi) > 0){
    
    prediction = predict(model, covs[valsi,-1],  predict.all = T, num.threads=1)$predictions
    
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

    #utils:::format.object_size(object.size(prediction), "auto")
    predsPlusCellID <- cbind(valsi, outM)
    
    ###  Added the line below back in to save the whole prediction tree

    saveRDS(predsPlusCellID, outfile)
    
    cat('Model predictions run successfully\n\n')
    
    ##### summarise the cell data
    
    chunkVals <- predsPlusCellID
    inds <- chunkVals[,1]

    if(model$forest$treetype == 'Classification'){

      sumData <- apply(chunkVals[,-1], 1, getCategoricalPredictionSummary)

    } else if(model$forest$treetype == 'Regression'){

      sumData <- apply(chunkVals[,-1], 1, getContinuousPredictionSummary)
    }

    b <- do.call(rbind, sumData)
    sumDataDF2 <- data.frame(inds, modalVal = unlist(b[,1]), ci = unlist(b[,2]))

    #outfile <- paste0(workDir, '/Maps/', patt, '/Chunks/AllCellVals_', k, '.rds')
    vname <- paste0(workDir, '/Maps/', patt, '/Chunks/cellSummaries_', k, '.rds')
    cat(paste0('Summary file = ' , vname, '\n\n'))
    saveRDS(sumDataDF2, vname)
    
    toc()
    cat(paste0('Iteration Finished Successfully'))
    
  }
}else{
  
  cat('\nFile exists - ', paste0('cellSummaries_', k, '.rds'))
  cat(paste0('Iteration Finished Successfully'))
}

