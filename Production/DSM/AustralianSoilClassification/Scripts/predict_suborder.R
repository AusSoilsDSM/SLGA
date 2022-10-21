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

output.directory = paste0(root.directory, '/RDS')

#Training data location
training.filename = 'ASC_ORD'      ## ".shp" extension not needed
training.fieldname = 'O_ASC_ORD'
training.id = "Observation_ID"

##########################################################################
###       Initialise
##########################################################################

#Column names
column.names = readRDS(paste0(root.directory, "/CovariatesUsed.rds"))

#Tile numbers
tiles = list.files(path = data.directory , full.names = F, include.dirs = T)

#Original order levels
order.original = readRDS(paste0(root.directory, '/OriginalFactors.rds'))

#.tif locations
covariates.file = paste(data.directory, '/', tiles[k], '/' , column.names, '.tif', sep="")

#Order predictions
order.file = paste0(root.directory, '/ASC_ORD', '/ASC_ORD_' , tiles[k], '.tif')

#Construct the stack
covariates = stack(c(order.file, covariates.file))

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
pred = data.frame(rep(NA, ncell(covariates)))

cat("check 1 \n")

for (i in 1:length(order.original)){
  
  complete.index = complete.cases(input) & input[,1] == i
  
  model.suborder.ranger = readRDS(paste0(model.directory, "/Ranger.suborder_", i,".rds")) 
  
  #Predict
  if (sum(complete.index > 1)){
    predicted = predict(model.suborder.ranger, data = input[complete.index & input[,1] == i, -1])$predictions
    pred[complete.index,] = 100*i + as.numeric(levels(predicted))[predicted]
  }
  else if (sum(complete.index == 1)) {
    predicted = predict(model.suborder.ranger, data = as.matrix(data.frame(t(input[complete.index & input[,1] == i, -1]))))$predictions
    pred[complete.index,] = 100*i + as.numeric(levels(predicted))[predicted]
  } 
}

cat("check 2 \n")

#Save as RDS
saveRDS(pred, paste0(output.directory, '/', 'Suborder_Tile_', tiles[k], '.rds'))



