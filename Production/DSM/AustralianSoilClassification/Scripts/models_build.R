##########################################################################
###       Packages
##########################################################################

library(raster)
library(ranger)
library(rgdal)
library(caret)
library(C50)


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

data.train  = readRDS(paste0(root.directory, "/CovariateTraining.rds"))


##########################################################################
###       Further Pre-Processing 
##########################################################################

#This is where we should standardise if need be...

##########################################################################
###       Implement Algorithms
##########################################################################

#SVM
train.control = trainControl(method = "boot", number = 10, p = 0.1)
tuneGrid = data.frame(c(0.25), c(3), c(0.01))
colnames(tuneGrid) = getModelInfo(model = "svmPoly")$svmPoly$parameters[,1]
model.svm = train(x = data.train[complete.cases(data.train),][,-(1:4)], 
              y = as.factor(data.train[complete.cases(data.train),][[training.fieldname]]),
              method = 'svmPoly', trControl = train.control, tuneGrid = tuneGrid)
saveRDS(model.svm, paste0(model.directory, "/Ranger_SVMpoly.rds"))


#Ranger
train.control = trainControl(method = "oob", number = 10, p = 0.1)
tuneGrid = data.frame(c(15), c('gini'), c(1))
colnames(tuneGrid) = getModelInfo(model = "ranger")$ranger$parameters[,1]
model.ranger = train(x = data.train[complete.cases(data.train),][,-(1:4)], 
              y = as.factor(data.train[complete.cases(data.train),][[training.fieldname]]),
              method = 'ranger', trControl = train.control, tuneGrid = tuneGrid)
saveRDS(model.ranger, paste0(model.directory, "/Ranger_Kfold.rds"))


#c50
model.C5 = C5.0(x = data.train[complete.cases(data.train),][,-(1:4)], 
                       y = as.factor(data.train[complete.cases(data.train),][[training.fieldname]]))
saveRDS(model.C5, paste0(model.directory, "/C5.rds"))


#c50 restrict to 30 rules
model.C5_30rule = C5.0(x = data.train[complete.cases(data.train),][,-(1:4)], 
                       y = as.factor(data.train[complete.cases(data.train),][[training.fieldname]]),
                       rules = 30)
saveRDS(model.C5, paste0(model.directory, "/C5_30rule.rds"))


