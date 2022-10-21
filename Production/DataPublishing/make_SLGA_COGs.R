startTime <- Sys.time()
print(startTime)

library(terra)
terraOptions(progress=1)



inPath <- '//datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/predictions/SLGA_ready/90m'
outPath <- '/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/PHW'


args = commandArgs(trailingOnly=T)
k = as.numeric(args[1])
inPath = args[2]
outPath = args[3]




templateR <- rast('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif')

if(!file.exists(outPath)){dir.create(outPath)}

fls <- list.files(inPath, pattern = '.tif$', full.names = T)

f <- fls[k]

#######     Make VRTs using GDAL    #################################################################
filt <- paste0(prods[k])
fls <- list.files(inDir, recursive = F, pattern = paste0(filt, '.tif'), full.names = T )
#print(length(fls))
csvPath <- paste0(dirname(inDir), '/', filt,'.csv' )
write.table(fls, csvPath, row.names = F,  col.names = F, quote = F)
print('Generating VRTs')
system(paste0(gdalPath, '/gdalbuildvrt -input_file_list ',csvPath, ' ', paste0(dirname(inDir), '/', filt,'.vrt' )))


print('Resampling ......')
tr <- terra::rast(paste0(dirname(inDir), '/', filt, '.vrt'))
trs <- terra::resample(tr, templateR, method = 'near')

print('Applying mask ......')
mR <- terra::mask(trs, templateR)
writeRaster(mR, paste0(outDir, '/', filt, '.tif'), overwrite=TRUE, gdal=c("COMPRESS=NONE", "TILED=NO"))

print('Generating COG')
#cmd <- paste0(gdalPath, '/gdal_translate ', paste0(dirname(inDir), '/', filt,'.vrt' ), ' ', dirname(inDir), '/', filt, '.tif -stats -co TILED=YES -co COMPRESS=LZW -co BIGTIFF=YES -co COPY_SRC_OVERVIEWS=YES' ) 
#system(cmd)
system(paste0(gdalPath, '/gdaladdo -ro ',paste0(outDir, '/', filt, '.tif')))
cmd <- paste0(gdalPath, '/gdal_translate ', outDir, '/', filt, '.tif', ' ', outDir, '/', filt, 'cog.tif -stats -co TILED=YES -co COMPRESS=NONE -co BIGTIFF=YES -co COPY_SRC_OVERVIEWS=YES' ) 
system(cmd)





# print('Generating COG')
# cmd <- paste0(gdalPath, '/gdal_addo ', paste0(dirname(inDir), '/', filt,'.vrt' ), ' ', outDir, '/', filt, '-COG.tif -of COG -stats -co COMPRESS=LZW -co BIGTIFF=YES -co PREDICTOR=YES' ) 
# system(cmd)
# cmd <- paste0(gdalPath, '/gdal_translate ', paste0(dirname(inDir), '/', filt,'.vrt' ), ' ', outDir, '/', filt, '.tif -stats -co TILED=YES -co COMPRESS=LZW -co BIGTIFF=YES' ) 
# system(cmd)
# cmd <- paste0(gdalPath, '/gdal_translate ', outDir, '/', filt, '.tif ', outDir, '/', filt, '-COG.tif -of COG -stats -co COMPRESS=LZW -co BIGTIFF=YES -co OVERVIEW_RESAMPLING=nearest' ) 
# cmd
# system(cmd)

endTime <- Sys.time()
print(endTime)
difft <- difftime(endTime, startTime, units = "auto")
print(difft)
print('Processing successfully completed')

#########  Mosaic tiles using R raster  #######################################################

# filt <- paste0(att, '_', depth, '_Mean')
# pathsL = list.files(inDir, pattern = paste0(filt, '.tif'), full.names = T, recursive =F)
# 
# input.rasters <- lapply(pathsL, raster)
# input.rasters$fun <- mean
# input.rasters$na.rm <- TRUE
# mos <- do.call(mosaic, input.rasters)
# writeRaster(mos, paste0('/scratch1/projects/slga/AWC_30m/', filt, '.tif'))
# 






