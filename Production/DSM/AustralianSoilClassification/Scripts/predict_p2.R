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
root.directory = '/OSM/CBR/AF_DIGISCAPESM/work/Harry/Project2'
model.directory = paste0(root.directory, '/Models')
data.directory = '/OSM/CBR/AF_DIGISCAPESM/work/CoVariates/tiles'

output.directory = paste0(root.directory, '/RDS_Group')
predictions.all.directory = paste0(output.directory, '/AllPredictions')

#Training data location
training.filename = 'ASC_ORD'      ## ".shp" extension not needed
training.fieldname = 'O_ASC_ORD'
training.id = "Observation_ID"

group.num = 6

##########################################################################
###       Initialise
##########################################################################

#Model
model.ranger  = readRDS(paste0(model.directory, "/Ranger_6.rds"))

#Column names
column.names = readRDS(paste0(root.directory, "/CovariatesUsed.rds"))

#Tile numbers
tiles = list.files(path = data.directory , full.names = F, include.dirs = T)

#.tif locations
covariates.file = paste(data.directory, '/', tiles[k], '/' , column.names, '.tif', sep="")

#actual = list.files(paste0(data.directory,'/', tiles[k]), '.tif$', recursive = F, full.names = T )
#setdiff(covariates.file, actual)
cat("check 1\n")

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

cat("check 2\n")

#Find index of complete data
complete.index = complete.cases(input)
pred = data.frame(rep(NA, ncell(covariates)))

pred.all = data.frame(matrix(NA, nrow = ncell(covariates), ncol = group.num))
colnames(pred.all) = 1:group.num

#Predict
if(sum(complete.index) != 0){
  predicted = predict(model.ranger, data = input[complete.index,])$predictions
  pred[complete.index,] = as.numeric(levels(predicted))[predicted]
  
  cat("check 3/n")
  predicted.all = predict(model.ranger, data = input[complete.index,], predict.all = T)$predictions
  for (i in 1:group.num){
    pred.all[complete.index,i] = rowSums(predicted.all == i)
  }
}

#Save as RDS of all predictions
#saveRDS(pred, paste0(output.directory, '/', 'Group_Tile_', tiles[k], '.rds'))
saveRDS(pred.all, paste0(predictions.all.directory , '/', 'AllPred_Tile_', tiles[k], '.rds'))

#pred to raster
cat("check 0\n")  

#Template
template = covariates[[1]]
rast = template

cat("check 1\n")

#Create Raster
values(rast) = pred[,1]

cat("check 2\n")

#Mask and save as .tif
rast.masked <- mask(rast, template)
writeRaster(rast.masked, paste0(output.directory ,'/', 'Group_Tile_', tiles[k], '.tif'), format='GTiff', overwrite=T)







