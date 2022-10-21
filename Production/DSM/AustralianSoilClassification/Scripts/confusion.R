##########################################################################
###       Packages
##########################################################################

library(raster)
library(ranger)
library(rgdal)

##########################################################################
###       Inputs
##########################################################################

#HPC indexing
args = commandArgs(trailingOnly=T)
print(args)
k <- as.numeric(args[1])

#General directory
root.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry'
input.directory = paste0(root.directory, '/RDS_order')
tile.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/CoVariates/tiles'
predictions.all.directory = paste0(input.directory, '/AllPredictions')


output.directory = paste0(root.directory, '/ASC_ORD/Confusion')
tile.template = "clim_ADI.tif"

#Training data location
training.filename = 'ASC_ORD'      ## ".shp" extension not needed
training.fieldname = 'O_ASC_ORD'
training.id = "Observation_ID"

order.num = 14

##########################################################################
###       Initialise
##########################################################################

#Tile numbers
tiles = list.files(path = tile.directory , full.names = F, include.dirs = T)

for (k in www){

  #All predictions
  pred.all = readRDS(paste0(predictions.all.directory , '/', 'AllPred_Tile_', tiles[k], '.rds'))
  
  #Template to overwrite
  template = raster(paste0(tile.directory, '/' , tiles[k], '/', tile.template))
  rast = template
  
  ##########################################################################
  ###       Construct Confusion Index
  ##########################################################################
  
  #Max prediction
  max.pred = apply(pred.all, 1, max)
  
  #Second Prediction
  second.pred = apply(pred.all, 1, function(x){
    if (sum(is.na(x)) > 0){return(NA)
      }else {return(sort(x,partial=order.num-1)[order.num-1][[1]])}
  })
  
  #Create confusion vector
  confusion = data.frame(rep(NA, dim(pred.all)[1]))
  confusion = second.pred/max.pred
  
  #Create Raster
  values(rast) = confusion
  
  #Mask and save as .tif
  rast.masked <- mask(rast, template)
  writeRaster(rast.masked, paste0(output.directory ,'/', training.filename,'_Confusion' , '_', tiles[k] ,'.tif'), format='GTiff', overwrite=T)

}


