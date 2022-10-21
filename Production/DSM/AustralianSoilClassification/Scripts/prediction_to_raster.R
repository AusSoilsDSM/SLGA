##########################################################################
###       Packages
##########################################################################

library(raster)
library(rgdal)

library(doParallel)

##########################################################################
###       Inputs
##########################################################################

#General directory
root.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry'
data.directory = paste0(root.directory, '/RDS_order')
tile.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/CoVariates/tiles'
output.directory = paste0(root.directory, '/ASC_ORD')

#Training data location
training.filename = 'ASC_ORD'      ## ".shp" extension not needed
training.fieldname = 'O_ASC_ORD'
training.id = "Observation_ID"

tile.template = "clim_ADI.tif"

##########################################################################
###       Convert to Raster
##########################################################################

#Tile numbers


cl = makeCluster(detectCores()-1,outfile="")
registerDoParallel(cl)
foreach(k = 192:length(tiles),  .packages=c('raster')) %dopar%  {
  
  #Template
  template = raster(paste0(tile.directory, '/' , tiles[k], '/', tile.template))
  rast = template
  
  #Read in RDS data
  data = readRDS(paste0(data.directory, '/', 'Order_Tile_', tiles[k] ,'.rds'))
  
  #Create Raster
  values(rast) = data[,1]
  
  #Mask and save as .tif
  rast.masked <- mask(rast, template)
  writeRaster(rast.masked, paste0(output.directory ,'/', training.filename, '_', tiles[k] ,'.tif'), format='GTiff', overwrite=T)
  
}
stopCluster(cl)
