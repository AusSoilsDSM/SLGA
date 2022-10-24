library(terra)

source('PrivateInfo.R')

gdalPath <- '/apps/gdal/3.2.2/bin'

args = commandArgs(trailingOnly=T)
print(args)
k = as.numeric(args[1])

covDir = '/datasets/work/af-tern-mal-deb/work/datasets/national/alexandre/processed_30m'
fls <- list.files(covDir, full.names=T, pattern = '.tif$')

inRasterPath <- fls[k]
outPath <- paste0('/scratch2/', ident, '/compresssedCovs/', basename(inRasterPath))

cmd <- paste0(gdalPath, '/gdal_translate ', inRasterPath, ' ', outPath ,' --config GDAL_CACHEMAX 5000 -stats -co TILED=NO -co COMPRESS=DEFLATE -co PREDICTOR=3 -co BIGTIFF=YES -co COPY_SRC_OVERVIEWS=NO' ) 
system(cmd)
