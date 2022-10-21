##########################################################################
###       Packages
##########################################################################

library(raster)
library(ranger)
library(Cubist)
library(rgdal)

library(epiR)
library(RColorBrewer)

library(doParallel)

library(caret)
library(caretEnsemble)

##########################################################################
###       Inputs
##########################################################################

#General directory
root.directory = 'E:/Harry/NT_DSM_Training'
data.directory = paste0(root.directory, '/covariates')
training.directory = paste0(root.directory, '/SoilSiteData')

#Covariate data location
climate_cov = list.files(paste0(data.directory,'/Climate'), '.tif$', recursive = F, full.names = T )
organism_cov = list.files(paste0(data.directory,'/Organisms'), '.tif$', recursive = F, full.names = T )
other_cov = list.files(paste0(data.directory,'/Other'), '.tif$', recursive = F, full.names = T )
parent_cov = list.files(paste0(data.directory,'/Parent_Material'), '.tif$', recursive = F, full.names = T )
parent_cov = c()
relief_cov = list.files(paste0(data.directory,'/Relief'), '.tif$', recursive = F, full.names = T )
soil_cov = list.files(paste0(data.directory,'/Soil'), '.tif$', recursive = F, full.names = T )

#Combing covariate data location list
all_cov = c(climate_cov,organism_cov,other_cov,parent_cov,relief_cov,soil_cov)

#Training data location
training.filename = 'ASC_ORD'      ## ".shp" extension not needed
training.fieldname = 'O_ASC_ORD'
training.id = "Observation_ID"

#eventualy nnet
model.type = c("C5.0" ,"ranger", "hdda", "svmLinear", "svmPoly", "mlp", "mlpWeightDecay")

#Results location
outputs.directory =  paste0(root.directory, '/Modelling_SoilTypes')
outputs.rds = paste0(outputs.directory, '/RDS')

#Modelling Variables
compute.validate = T
modelling.percentage = 0.5


##########################################################################
###       Create Output Directory 
##########################################################################

if(F){
  if(!(file.exists(outputs.directory))){
    dir.create(outputs.directory, recursive=T )
  }
  
  for (i in 1:length(model.type)){
    if(!(file.exists(paste0(outputs.directory, '/', model.type[i])))){
      dir.create(paste0(outputs.directory, '/', model.type[i]), recursive=T )
    }
  }
}

##########################################################################
###       Initialise
##########################################################################

#Covariate Stack
covariates = stack(all_cov)

#Training points
training.points = readRDS("E:/Harry/NT_DSM_Training/SoilSiteData/Soil_Type.rds")

#just for darwin 
training.points = training.points[-which(training.points$O_ASC_ORD %in% c("AN", "CA", "FE", "OR", "PO", "SO")),]

#Change facto names
#training.points[[training.fieldname]] = as.factor(training.points[[training.fieldname]])
#levels.original = levels(training.points[[training.fieldname]])
levels.original = unique(training.points[[training.fieldname]])
#should save them hear

for (i in 1:length(levels.original)){
  training.points[[training.fieldname]][training.points[[training.fieldname]] == levels.original[i]] = i
}
training.points[[training.fieldname]] = as.factor(training.points[[training.fieldname]])



##########################################################################
###       Extract covariates at trainng data locations
##########################################################################

#Extracting samples
covariate.training = as.data.frame(extract(covariates,training.points,method="simple"))
covariate.training.withloc = data.frame(training.points[[training.id]] ,training.points@coords, training.points[[training.fieldname]] ,covariate.training)

#Naming Columns
colnames(covariate.training.withloc)[1:4] <-c('SID', 'Easting','Northing',  training.fieldname)

#Csv of Covariates at drill spots
drill.file = paste0(outputs.directory, '/', training.filename, '_', 'CovariatesDrill.csv')
write.table(covariate.training.withloc, drill.file, sep=",", row.names=F)

##########################################################################
###       Training and validation data sets
##########################################################################

inSamples = read.table(drill.file, sep=",", header=T,strip.white=T)

#Generate split
sample.index = sample(nrow(inSamples), modelling.percentage*floor(nrow(inSamples)))

#Split into data frames
samples.train = as.data.frame(inSamples[sample.index,])
samples.test = as.data.frame(inSamples[-sample.index,])

#Column names
colnames(samples.train) = colnames(inSamples)
colnames(samples.test) = colnames(inSamples)

#Standardising data
input.standardise  = (input[-(1:4)] - minValue(covariates))/(maxValue(covariates) - minValue(covariates))
input.standardise = cbind(input[1:4], input.standardise)
test.standardise  = (test[-(1:4)] - minValue(covariates))/(maxValue(covariates) - minValue(covariates))
test.standardise = cbind(test[1:4], test.standardise)

#Write Csv
write.table(samples.train, paste0(outputs.directory, '/CovariatesDrill', '_', training.filename, '_TrainingData.csv'), sep=",", row.names=F)
write.table(samples.test, paste0(outputs.directory, '/CovariatesDrill', '_', training.filename, '_TestData.csv'), sep=",", row.names=F)
write.table(input.standardise, paste0(outputs.directory, '/CovariatesDrill', '_', training.filename, '_TrainingData_standardise.csv'), sep=",", row.names=F)
write.table(test.standardise, paste0(outputs.directory, '/CovariatesDrill', '_', training.filename, '_TestData_standardise.csv'), sep=",", row.names=F)

##########################################################################
###       Further Pre-Processing 
##########################################################################



##########################################################################
###       Implement Algorithms
##########################################################################

input = read.table(paste0(outputs.directory, '/CovariatesDrill', '_', training.filename, '_TrainingData.csv'), sep=",", header=T,strip.white=T)
input.standardise = read.table(paste0(outputs.directory, '/CovariatesDrill', '_', training.filename, '_TrainingData_standardise.csv'), sep=",", header=T,strip.white=T)

#model = svm(as.factor(input.standardise[complete.cases(input.standardise),][[training.fieldname]])~ ., data = input.standardise[complete.cases(input.standardise),][,-(1:4)], hidden = 3)

model = ranger(as.factor(input[complete.cases(input),][[training.fieldname]])~ ., data = input[complete.cases(input),][,-(1:4)], write.forest = TRUE)

##########################################################################
###       Validation
##########################################################################

test.standardise = read.csv(paste0(outputs.directory, '/CovariatesDrill', '_', training.filename, '_TestData_standardise.csv'), sep=",")
predictions = predict(model, test.standardise[complete.cases(test.standardise),][,-(1:4)])
pred = as.data.frame(as.factor(predictions))

pred = sapply(lapply(1:dim(predictions)[1], function(i){ which.max(predictions[i,])   }), function (x) x[1])
pred = as.factor(pred)

confusionMatrix(predictions, as.factor(test.standardise[complete.cases(test.standardise),][,4]))



#allcrops <- sapply(lapply(1:dim(predictions)[1], function(i){ which.max(predictions[i,])   }), function (x) x[1])


predictions = predict(model, test[complete.cases(test),][,-(1:4)])$predictions
confusionMatrix(predictions, as.factor(test[complete.cases(test),][,4]))


##########################################################################
###       Apply the model spatially
##########################################################################

#Block size
bs = blockSize(covariates, nlayers(covariates), minrows=10)

if (F){
#Compute Averages
averages = as.data.frame(rep(NA,0))
for (i in 2:nlayers(covariates)){
  averages[1,i] = mean(input[,names(covariates[[i]])],na.rm = T)
}
colnames(averages) = names(covariates)
}



#Predict in parallel
model.type = 'Ranger'
cl = makeCluster(detectCores()-1,outfile="")
registerDoParallel(cl)
foreach(k = 1:bs$n,  .packages=c('raster','rgdal', 'ranger')) %dopar%  {
  parallel_predict_categorical(covariates, covariates[[1]] , bs, outputs.rds, model.type)
}
stopCluster(cl)

raster.out.masked = prediction_raster(outputs.directory, training.filename, training.fieldname, covariates[[1]], bs, outputs.rds, model.type)


