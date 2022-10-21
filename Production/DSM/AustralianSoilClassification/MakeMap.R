library(raster) 
library(rgdal)
library(ranger)
library(stringr)

###Run 1st to Run 2nd

makerasterFromTreePredictionChunks <- function(ProcessingDir = NULL, templateR = NULL, att = NULL, model = NULL , islog=F){
  
  rasterDir <- paste0(ProcessingDir, '/Maps/', att, '/Chunks')
  outRaster <- paste0(ProcessingDir, '/Maps/', att, '/', att, '.tif')
  #print(paste0("creating - ", outRaster))
  
  fls <- list.files(rasterDir, pattern = 'cellSummaries')
  
  #templateR <- raster(paste0(rootDir, '/Templates/template.tif'))
  chk <-	getChunkInfo(chunkLines, nrow(templateR))
  modelType <- model$treetype
  
  suffix <-''
  if(modelType == 'Classification'){
    suffix <- '_CI.tif'
  }else{
    suffix <- '_CoV.tif'
  }
  
  
  outRasterCI <- paste0(ProcessingDir, '/Maps/',  att, '/', att, suffix)
  
  predR<-raster(templateR)
  predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  predCI<-raster(templateR)
  predCI<-writeStart(predCI,filename=outRasterCI,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  print(paste0("Creating Raster ", outRaster) ) 
  
  for (i in 1:length(fls))
  {
    bname = paste0(rasterDir, '/cellSummaries_', i, '.rds')
    
    
    csize<-chk$nlines[i] * ncol(templateR)
    b <- rep(NAvalue(templateR), csize )
    c <- rep(NAvalue(templateR), csize )
    if(file.exists(bname)){
      print(bname)
      blockVals <- readRDS(bname)
      cat(paste(i, " ", sep=''))
      if(islog){
        exp(b[blockVals$inds] <- blockVals$modalVal)
        exp(c[blockVals$inds] <- blockVals$ci)
      }else{
        b[blockVals$inds] <- blockVals$modalVal
        c[blockVals$inds] <- blockVals$ci
      }
      
    }
    predR <- writeValues(predR, b, chk$starts[i])
    predCI <- writeValues(predCI, c, chk$starts[i])
  }
  
  
  predR<-writeStop(predR)
  predCI<-writeStop(predCI)
  
  
  
  print("Done")
  
  return(list(valsRaster=outRaster, uncertRaster=outRasterCI))
}

###run 2nd


rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+08,maxmemory=1e+10, overwrite=TRUE) # maxmemory = max no of cells to read into memory

args = commandArgs(trailingOnly=T)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  # rootDir <- 'Q:/3_Land_suitability/0_Working/Uta/Roper'
  # k=as.numeric(args[1])
  # att = 'SGG2b1w'
  # modelFileName = 'sgg.mod.2.b1.w.rds'
  # chunkLines = 20
  # source('Q:/3_Land_suitability/0_Working/Uta/Roper/Scripts/RFUtils.R')
  
}else{
  
  scriptDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/Functions'
  covDir <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m'
  workDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN/ASC'
  
  
  #rootDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN/ASC'
  #k = as.numeric(args[1])
  
  # att = 'ASC'
  # modelFileName = paste0('RFmodel_ASC.rds')
  
  att = 'ASCOnly'
  modelFileName = paste0('RFmodel_ASCOnly.rds')
  
  chunkLines = 20
}



source(paste0(scriptDir, '/RFUtils.R'))


templateR <- raster(paste0(covDir, '/Relief_dems_3s_mosaic1.tif'))

model <- readRDS(paste0(workDir, '/Models/', modelFileName))
#templateR <- raster(paste0(rootDir, '/Templates/template.tif'))


pPath <- makerasterFromTreePredictionChunks(ProcessingDir = workDir, templateR = templateR, att = att, model = model)

getRasterDecodes <- function(model = model){
  
  ids <- model$forest$class.values
  #cats <-  as.numeric(as.character(model$forest$levels[ids]))
  cats <-  as.character(model$forest$levels[ids])
  decodes <- data.frame( RID = ids, Category = cats)
  decodesOrd <- decodes[order(decodes$RID),]
  return (decodesOrd)
  
}

cdf <- getRasterDecodes(model)

write.csv(cdf, paste0(workDir, '/ASCOnlyRasterCodes.csv'), row.names = F)

