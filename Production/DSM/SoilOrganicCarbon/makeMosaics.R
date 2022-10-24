startTime <- Sys.time()
print(startTime)

library(terra)

source('PrivateInfo.R')
terraOptions(progress=1)

gdalPath <- '/apps/gdal/3.2.2/bin'

args = commandArgs(trailingOnly=T)
k = as.numeric(args[1])


outDir <- paste0('/scratch1/', ident, '/TOC30m/Mosaics')
if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}

depths <- c("0_5", "5_15", "15_30", "30_60", "60_100", "100_200")
prods <- c('mean', 'predvar05', 'predvar95')
combos <- expand.grid(prods, depths)
nrow(combos)

rec <- combos[k,]
print(rec)
prod <- rec$Var1
depth <- rec$Var2

inDir = paste0('/scratch2/', ident, '/TOC30m2/',depth)

templateR <- rast('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/30m/Relief_dem1sv1_0.tif')
e <- ext(templateR)@ptr$vector
spatExt <- paste0(' -te ', e[1], ' ', e[3], ' ', e[2], ' ', e[4])

#######     Make VRTs using GDAL    #################################################################
filt <- paste0(prod, '_')

fls <- list.files(inDir, recursive = F, pattern = paste0(filt), full.names = T )
print(paste0("Number of tiles = ", length(fls)))
csvPath <- paste0(dirname(inDir), '/', filt, depth,'.csv' )
write.table(fls, csvPath, row.names = F,  col.names = F, quote = F)
print('Generating VRTs')
system(paste0(gdalPath, '/gdalbuildvrt ', spatExt, ' -input_file_list ',csvPath, ' ', paste0(dirname(inDir), '/', filt, depth,'.vrt' )))

print('Generating COG')
system(paste0(gdalPath, '/gdaladdo --config GDAL_CACHEMAX 5000 -ro ',dirname(inDir), '/', filt, depth,'.vrt' ))
cmd <- paste0(gdalPath, '/gdal_translate ', dirname(inDir), '/', filt, depth,'.vrt' , ' ', paste0(outDir, '/', filt, depth,'.tif')   ,' --config GDAL_CACHEMAX 5000 -stats -co TILED=YES -co COMPRESS=DEFLATE -co PREDICTOR=3 -co BIGTIFF=YES -co COPY_SRC_OVERVIEWS=YES' ) 
system(cmd)

endTime <- Sys.time()
print(endTime)
difft <- difftime(endTime, startTime, units = "auto")
print(difft)
print('Processing successfully completed')



