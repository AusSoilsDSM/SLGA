### Cogger - mosaics tiles and produce final products (5th, 50th and 95th percentile geotiffs) ###
#
# Original author Ross Searle - 19/08/2022
#
# Adapted by P.R. Zund - 19/08/2022

## User input
mod <- "TERN3" # Mod name

# Automatic processing from here on
source(paste0('/datasets/work/af-digiscapesm/work/Peter/Scripts/PrivateData.R'))
wd <- paste0('//scratch2/', ident, '/', mod)
setwd(wd)

library(terra)

depth <- list.dirs(path = paste(getwd(), "/Bootstrap/map/1000", sep = ""), full.names = FALSE, recursive = FALSE) # List of depths
tiles <- list.dirs(path = paste(getwd(),"/Bootstrap/map/", sep = ""), full.names = FALSE, recursive = FALSE)
gdalPath <- '/apps/gdal/3.2.2/bin'
inDir <-  paste0('/scratch2/', ident, '/TERN3/Bootstrap/map')

# Mosaic tiles depth by depth
for (d in 1:length(depth)) {
  # Mosaic 50th percentile
  rlist <- list.files(paste(getwd(), "/Bootstrap/map/", tiles[1], "/", depth[d], "/", sep = ""), pattern = "meanPred_", full.names = TRUE, recursive = TRUE)
  for (t in 2:length(tiles)) {
    r1 <- list.files(paste(getwd(), "/Bootstrap/map/", tiles[t], "/", depth[d], "/", sep = ""), pattern = "meanPred_", full.names = TRUE, recursive = TRUE)
    rlist <- c(rlist, r1)
  }
  fls <- rlist
  print(paste0("Number of tiles = ", length(fls)))

  csvPath <- paste0(mod, "_Predicted_50th_Percentile_Colwell_P_", depth[d], ".csv")
  write.table(fls, csvPath, row.names = F,  col.names = F, quote = F)
  print('Generating VRTs')
  system(paste0(gdalPath, '/gdalbuildvrt -input_file_list ',csvPath, ' ', paste0(dirname(inDir), '/P.vrt' )))
  tr <- terra::rast(paste0(dirname(inDir), '/P.vrt'))
  terra::writeRaster(tr, paste0('/datasets/work/af-digiscapesm/work/Peter/PMap/', paste0(mod,"_Predicted_50th_Percentile_Colwell_P_", depth[d], ".tif")),  overwrite=TRUE)
  print('Generating COG')
  inP <- paste0(dirname(inDir), '/P.vrt')
  system(paste0(gdalPath, '/gdaladdo --config GDAL_CACHEMAX 5000 -ro ',inP))
  cmd <- paste0(gdalPath, '/gdal_translate ', inP, ' ', paste0('/datasets/work/af-digiscapesm/work/Peter/PMap/', paste0(mod,"_Predicted_50th_Percentile_Colwell_P_", depth[d], "_cog33.tif")),' --config GDAL_CACHEMAX 5000 -stats -co TILED=YES -co COMPRESS=DEFLATE -co BIGTIFF=YES -co COPY_SRC_OVERVIEWS=YES' )
  system(cmd)

# Mosaic fifth percentile
  rlist <- list.files(paste(getwd(), "/Bootstrap/map/", tiles[1], "/", depth[d], "/", sep = ""), pattern = "lplPred_", full.names = TRUE, recursive = TRUE)
  for (t in 2:length(tiles)) {
    r1 <- list.files(paste(getwd(), "/Bootstrap/map/", tiles[t], "/", depth[d], "/", sep = ""), pattern = "lplPred_", full.names = TRUE, recursive = TRUE)
    rlist <- c(rlist, r1)
  }
  fls <- rlist
  print(paste0("Number of tiles = ", length(fls)))
  
  csvPath <- paste0(mod, "_Predicted_5th_Percentile_Colwell_P_", depth[d], ".csv")
  write.table(fls, csvPath, row.names = F,  col.names = F, quote = F)
  print('Generating VRTs')
  system(paste0(gdalPath, '/gdalbuildvrt -input_file_list ',csvPath, ' ', paste0(dirname(inDir), '/P.vrt' )))
  tr <- terra::rast(paste0(dirname(inDir), '/P.vrt'))
  terra::writeRaster(tr, paste0('/datasets/work/af-digiscapesm/work/Peter/PMap/', paste0(mod,"_Predicted_5th_Percentile_Colwell_P_", depth[d], ".tif")),  overwrite=TRUE)
  print('Generating COG')
  inP <- paste0(dirname(inDir), '/P.vrt')
  system(paste0(gdalPath, '/gdaladdo --config GDAL_CACHEMAX 5000 -ro ',inP))
  cmd <- paste0(gdalPath, '/gdal_translate ', inP, ' ', paste0('/datasets/work/af-digiscapesm/work/Peter/PMap/', paste0(mod,"_Predicted_5th_Percentile_Colwell_P_", depth[d], "_cog33.tif")),' --config GDAL_CACHEMAX 5000 -stats -co TILED=YES -co COMPRESS=DEFLATE -co BIGTIFF=YES -co COPY_SRC_OVERVIEWS=YES' )
  system(cmd)

  # Mosaic ninety fifth percentile
  rlist <- list.files(paste(getwd(), "/Bootstrap/map/", tiles[1], "/", depth[d], "/", sep = ""), pattern = "uplPred_", full.names = TRUE, recursive = TRUE)
  for (t in 2:length(tiles)) {
    r1 <- list.files(paste(getwd(), "/Bootstrap/map/", tiles[t], "/", depth[d], "/", sep = ""), pattern = "uplPred_", full.names = TRUE, recursive = TRUE)
    rlist <- c(rlist, r1)
  }
  fls <- rlist
  print(paste0("Number of tiles = ", length(fls)))
  
  csvPath <- paste0(mod, "_Predicted_95th_Percentile_Colwell_P_", depth[d], ".csv")
  write.table(fls, csvPath, row.names = F,  col.names = F, quote = F)
  print('Generating VRTs')
  system(paste0(gdalPath, '/gdalbuildvrt -input_file_list ',csvPath, ' ', paste0(dirname(inDir), '/P.vrt' )))
  tr <- terra::rast(paste0(dirname(inDir), '/P.vrt'))
  terra::writeRaster(tr, paste0('/datasets/work/af-digiscapesm/work/Peter/PMap/', paste0(mod,"_Predicted_95th_Percentile_Colwell_P_", depth[d], ".tif")),  overwrite=TRUE)
  print('Generating COG')
  inP <- paste0(dirname(inDir), '/P.vrt')
  system(paste0(gdalPath, '/gdaladdo --config GDAL_CACHEMAX 5000 -ro ',inP))
  cmd <- paste0(gdalPath, '/gdal_translate ', inP, ' ', paste0('/datasets/work/af-digiscapesm/work/Peter/PMap/', paste0(mod,"_Predicted_95th_Percentile_Colwell_P_", depth[d], "_cog33.tif")),' --config GDAL_CACHEMAX 5000 -stats -co TILED=YES -co COMPRESS=DEFLATE -co BIGTIFF=YES -co COPY_SRC_OVERVIEWS=YES' )
  system(cmd)
}
