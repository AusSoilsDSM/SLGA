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

#Training data location
training.filename = 'ASC_ORD'      ## ".shp" extension not needed
training.fieldname = 'O_ASC_ORD'
training.id = "Observation_ID"

model.type = c("ranger", "hdda", "svmLinear", "svmPoly", "mlp", )

##########################################################################
###       Initialise
##########################################################################

#Import train and test dataset
data.train = readRDS(paste0(root.directory, '/Tune_ASC_ORD.rds'))
data.test = readRDS(paste0(root.directory, '/Test_ASC_ORD.rds'))

##########################################################################
###       Implement Algorithms
##########################################################################


model = train(x = data.train[complete.cases(data.train),][,-(1:4)], 
              y = as.factor(data.train[complete.cases(data.train),][[training.fieldname]]),
              method = model.type[k])

saveRDS(model, paste0(model.directory, '\ModelSelection_', k, '.rds'))

