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
model.directory = paste0(root.directory, '/Models')
data.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/CoVariates/tiles'
extreme.values.directory = paste0(data.directory, '/ExtremeValues')

output.directory = paste0(root.directory, '/RDS_order')
predictions.all.directory = paste0(output.directory, '/AllPredictions')

#Training data location
training.filename = 'ASC_ORD'      ## ".shp" extension not needed
training.fieldname = 'O_ASC_ORD'
training.id = "Observation_ID"

order.num = 14

##########################################################################
###       Initialise
##########################################################################

#Model
model.ranger  = readRDS(paste0(model.directory, "/Ranger.rds"))

#Column names
column.names = readRDS(paste0(root.directory, "/CovariatesUsed.rds"))

#Tile numbers
tiles = list.files(path = data.directory , full.names = F, include.dirs = T)

#.tif locations
covariates.file = paste(data.directory, '/', tiles[k], '/' , column.names, '.tif', sep="")

#actual = list.files(paste0(data.directory,'/', tiles[k]), '.tif$', recursive = F, full.names = T )
#setdiff(covariates.file, actual)

#Construct the stack
covariates = stack(covariates.file)

##########################################################################
###       Further Pre-Processing 
##########################################################################

#This is where we should standardise if need be...

##########################################################################
###       Apply the model spatially
##########################################################################

#Convert to data frame
input = values(covariates)


#Find index of complete data
complete.index = complete.cases(input)
pred = data.frame(rep(NA, ncell(covariates)))

pred.all = data.frame(matrix(NA, nrow = ncell(covariates), ncol = order.num))
colnames(pred.all) = 1:order.num

#Predict
if(sum(complete.index) != 0){
  predicted = predict(model.ranger, data = input[complete.index,])$predictions
  pred[complete.index,] = as.numeric(levels(predicted))[predicted]
  
  predicted.all = predict(model.ranger, data = input[complete.index,], predict.all = T)$predictions
  for (i in 1:order.num){
    pred.all[complete.index,i] = rowSums(predicted.all == i)
  }
}

#Save as RDS
saveRDS(pred, paste0(output.directory, '/', 'Order_Tile_', tiles[k], '.rds'))
saveRDS(pred.all, paste0(predictions.all.directory , '/', 'AllPred_Tile_', tiles[k], '.rds'))


