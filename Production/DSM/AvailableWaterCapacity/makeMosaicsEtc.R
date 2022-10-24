
#################################
###  Author : Ross Searle         
###  Date : Fri Apr 01 10:35:13 2022                      
###  Project : TERN Landscapes
###  Purpose : Publish the SLGA datasets as COGs
#################################

startTime <- Sys.time()
print(startTime)
      
library(terra)
library(stringr)

#terraOptions(progress=1, memmax=10)
terraOptions(progress=1)

source('/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/AWC/MeasuredTextures/V2/Util_DataPublishing.R')

gdalPath <- '/apps/gdal/3.2.2/bin'


depths <- c('005','015','030','060','100','200')
prods <- c('Mean', 'Median', '05', '95')

combos <- expand.grid(prods, depths)
nrow(combos)




args = commandArgs(trailingOnly=T)
k = as.numeric(args[1])
att = args[2]
method <- args[3]
dataDirRoot <- args[4]
adjustVal <- as.numeric(args[5])
sigFigures <- as.numeric(args[6])
makeCOG <- as.logical(args[7])


rec <- combos[k,]
print(rec)
prod <- rec$Var1
depth <- rec$Var2

print(args)
print(paste0('Attribute = ', att))
print(paste0('Method = ', method))
print(paste0('DataDir = ', dataDirRoot))
print(paste0('adjustVal = ', adjustVal))
print(paste0('Decimal places = ', sigFigures))
print(paste0('makeCOG = ', makeCOG))

combo=paste0(att, '_', method, '_', depth)

#outDir = paste0(dataDirRoot,'/', prod, '/Tiles/', combo)

infoDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Production/NationalMaps/DataPublishing'
outDir <- paste0('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/', att)
templateR <- '/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif'

inDir <- paste0(dataDirRoot, '/Tiles/', combo)
#fls <- list.files(inDir, full.names = T, pattern = paste0(att, '_'))
#inRasterPath <- fls[k]

mt <- read.csv(paste0(infoDir, '/MetaData/', att, '_Metadata.csv'), sep = ',', header = F)


op <- publishRaster(inRasterPath=dataDirRoot, outDir=outDir,  adjustVal=adjustVal, sigFigures=sigFigures, resampleTemplate=templateR, maskTemplate=templateR, metadataTable=mt, RATTable=NULL, MakeCOG=makeCOG)

system(paste0(gdalPath, '/bin/gdalinfo -mdd all ', op))

print(paste0('Is Valid COG = ', isValidCOG(op, quiet=T)))

endTime <- Sys.time()
print(endTime)
difft <- difftime(endTime, startTime, units = "auto")
print(difft)
print('Processing successfully completed')




