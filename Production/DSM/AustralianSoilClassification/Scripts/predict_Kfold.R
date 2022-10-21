##########################################################################
###       Packages
##########################################################################

library(raster)
library(ranger)
library(rgdal)
library(caret)

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

output.directory = paste0(root.directory, '/RDS_order/Kfold')

#Training data location
training.filename = 'ASC_ORD'      ## ".shp" extension not needed
training.fieldname = 'O_ASC_ORD'
training.id = "Observation_ID"

order.num = 14

##########################################################################
###       Initialise
##########################################################################

#Model
model  = readRDS(paste0(model.directory, "/Ranger_Kfold.rds"))

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
input = as.matrix(input)


#Find index of complete data
complete.index = complete.cases(input)
pred = data.frame(rep(NA, ncell(covariates)))

#Predict
if(sum(complete.index) != 0){
  predicted = predict.train(model, newdata = input[complete.index,])
  pred[complete.index,] = as.numeric(levels(predicted))[predicted]
}

#Save as RDS
saveRDS(pred, paste0(output.directory, '/', 'Order_Tile_', tiles[k], '.rds'))



