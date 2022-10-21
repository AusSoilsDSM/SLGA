##########################################################################
###       Packages
##########################################################################

library(raster)
library(ranger)
library(rgdal)


##########################################################################
###       Inputs
##########################################################################

#General directory
root.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry'
model.directory = paste0(root.directory, '/Models')

#Training data location
training.filename = 'ASC_ORD'      ## ".shp" extension not needed
training.fieldname = 'O_ASC_ORD'
training.id = "Observation_ID"

modelling.percentage = 0.9

##########################################################################
###       Initialise
##########################################################################

input  = readRDS(paste0(root.directory, "/CovariateTraining.rds"))

set.seed(0)
sample.index = sample(nrow(input), modelling.percentage*floor(nrow(input)))

data.train = input[sample.index,]
data.test = input[-sample.index,]
saveRDS(data.train, paste0(root.directory, '/Train_ASC_ORD.rds'))
saveRDS(data.test, paste0(root.directory, '/Test_ASC_ORD.rds'))

##########################################################################
###       Further Pre-Processing 
##########################################################################

#This is where we should standardise if need be...

##########################################################################
###       Implement Algorithms
##########################################################################

#model.ranger = ranger(as.factor(input[complete.cases(input),][[training.fieldname]])~ ., data = input[complete.cases(input),][,-(1:4)], write.forest = TRUE)
model.ranger = ranger(as.factor(data.train[complete.cases(data.train),][[training.fieldname]])~ ., data = data.train[complete.cases(data.train),][,-(1:4)], write.forest = TRUE)

#Save model
saveRDS(model.ranger, paste0(model.directory, "/Ranger.rds"))






