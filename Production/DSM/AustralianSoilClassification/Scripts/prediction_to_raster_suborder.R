##########################################################################
###       Packages
##########################################################################

library(raster)
library(rgdal)

library(doParallel)

##########################################################################
###       Inputs
##########################################################################

#HPC indexing
args = commandArgs(trailingOnly=T)
print(args)
k = as.numeric(args[1])

#General directory
root.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry'
data.directory = paste0(root.directory, '/RDS_suborder')
tile.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/CoVariates/tiles'
output.directory = paste0(root.directory, '/ASC_SUBORD')

#Training data location
training.filename = 'ASC_SUBORD'      ## ".shp" extension not needed
training.fieldname = 'O_ASC_SUBORD'
training.id = "Observation_ID"

tile.template = "clim_ADI.tif"

##########################################################################
###       Convert to Raster
##########################################################################

#Tile numbers
tiles = list.files(path = tile.directory , full.names = F, include.dirs = T)

  
#Template
template = raster(paste0(tile.directory, '/' , tiles[k], '/', tile.template))
rast = template

#Read in RDS data
data = readRDS(paste0(data.directory, '/', 'Suborder_Tile_', tiles[k] ,'.rds'))

#Create Raster
values(rast) = data[,1]

#Mask and save as .tif
rast.masked <- mask(rast, template)
writeRaster(rast.masked, paste0(output.directory ,'/', training.filename, '_', tiles[k] ,'.tif'), format='GTiff', overwrite=T)

