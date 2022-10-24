startTime <- Sys.time()
print(startTime)

library(terra)
terraOptions(progress=1)

gdalPath <- '/apps/gdal/3.2.2/bin'

args = commandArgs(trailingOnly=T)
k = as.numeric(args[1])
att <- args[2]

method <<- args[3]

outDir <- paste0('/', ident, '/', ident, '/AWCv3/', method, '/Mosaics')
if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}

depths <- c('005','015','030','060','100','200')
atts <- att
prods <- c('Mean', 'Median', '05', '95')

combos <- expand.grid(prods, atts, depths)
nrow(combos)

rec <- combos[k,]
print(rec)
prod <- rec$Var1
att <- rec$Var2
depth <- rec$Var3


inDir = paste0('/', ident, '/', ident, '/AWCv3/', method, '/Tiles/', att, '_', method, '_', depth)


#######     Make VRTs using GDAL    #################################################################
filt <- paste0(att, '_', depth, '_', prod)

fls <- list.files(inDir, recursive = F, pattern = paste0(filt, '.tif'), full.names = T )
print(paste0("Number of tiles = ", length(fls)))
csvPath <- paste0(dirname(inDir), '/', filt,'.csv' )
write.table(fls, csvPath, row.names = F,  col.names = F, quote = F)
print('Generating VRTs')
system(paste0(gdalPath, '/gdalbuildvrt -input_file_list ',csvPath, ' ', paste0(dirname(inDir), '/', filt,'.vrt' )))
tr <- terra::rast(paste0(dirname(inDir), '/', filt, '.vrt'))
terra::writeRaster(tr, paste0('/scratch2/', ident, '/tmp/', filt, '.tif'),  overwrite=TRUE)

templateR <- rast('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif')
print('Resampling ......')
tr <- terra::rast(paste0('/scratch2/', ident, '/tmp/', filt, '.tif'))
trs <- terra::resample(tr, templateR, method = 'near')
trsc <- terra::clamp(trs, lower=0, upper=60)

print('Applying mask ......')
mR <- terra::mask(trsc, templateR)
writeRaster(mR, paste0(outDir, '/', filt, '.tif'), overwrite=TRUE, gdal=c("COMPRESS=NONE", "TILED=NO"))
print(paste0('Raster written to - ', outDir, '/', filt, '.tif'))
# print('Generating COG')
# system(paste0(gdalPath, '/gdaladdo -ro ',paste0(outDir, '/', filt, '.tif')))
# cmd <- paste0(gdalPath, '/gdal_translate ', outDir, '/', filt, '.tif', ' ', outDir, '/', filt, 'cog.tif -stats -co TILED=YES -co COMPRESS=NONE -co BIGTIFF=YES -co COPY_SRC_OVERVIEWS=YES' ) 
# system(cmd)


endTime <- Sys.time()
print(endTime)
difft <- difftime(endTime, startTime, units = "auto")
print(difft)
print('Processing successfully completed')










#########  Mosaic tiles using R raster - NOT USED  #######################################################

# filt <- paste0(att, '_', depth, '_Mean')
# pathsL = list.files(inDir, pattern = paste0(filt, '.tif'), full.names = T, recursive =F)
# 
# input.rasters <- lapply(pathsL, raster)
# input.rasters$fun <- mean
# input.rasters$na.rm <- TRUE
# mos <- do.call(mosaic, input.rasters)
# writeRaster(mos, paste0('/scratch1/projects/slga/AWC_30m/', filt, '.tif'))
# 






