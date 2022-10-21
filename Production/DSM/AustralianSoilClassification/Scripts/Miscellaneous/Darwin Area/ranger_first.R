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

#will need ithir ...


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
training.filename = 'Clay'      ## ".shp" extension not needed
training.fieldname = 'GSM1'
training.id = 'ID'

model.type = 'Ranger'

#Results location
outputs.directory =  paste0(root.directory, '/Modelling')
output.ranger = paste0(outputs.directory,'/Ranger')
output.raster = paste0(outputs.directory,'/Rasters')
output.bootstrap = paste0(outputs.directory, '/Bootstrap_Samples')
outputs.rds = paste0(outputs.directory, '/RDS')

#Modelling Variables
compute.validate = T
modelling.percentage = 0.8


##########################################################################
###       Create Output Directory 
##########################################################################

if(!(file.exists(output.ranger))){
  dir.create(output.ranger, recursive=T )
}

if(!(file.exists(output.raster))){
  dir.create(output.raster)
}

if(!(file.exists(output.bootstrap))){
  dir.create(output.bootstrap)
}

##########################################################################
###       Initialise
##########################################################################

#Covariate Stack
covariates = stack(all_cov)

#Training points
training.points = readOGR(dsn = training.directory, layer= training.filename)

##########################################################################
###       Extract covariates at trainng data locations
##########################################################################

#Extracting samples
covariate.training = as.data.frame(extract(covariates,training.points,method="simple"))
covariate.training.withloc = data.frame(training.points[[training.id]], training.points@coords, covariate.training)

#Naming Columns
colnames(covariate.training.withloc)[c(1,2,3)] <-c('SID','Easting','Northing')

#Csv of Covariates at drill spots
drill.file = paste0(outputs.directory, '/', training.filename, '_', 'CovariatesDrill.csv')
write.table(covariate.training.withloc, drill.file, sep=",", row.names=F)

##########################################################################
###       Training and validation data sets
##########################################################################

if (compute.validate){
  inSamples = read.table(drill.file, sep=",", header=T,strip.white=T)
  
  #Generate split
  sample.index = sample(nrow(inSamples), modelling.percentage*floor(nrow(inSamples)))
  
  #Split into data frames
  samples.train = as.data.frame(inSamples[sample.index,])
  samples.valid = as.data.frame(inSamples[-sample.index,])
  
  #Column names
  colnames(samples.train) = colnames(inSamples)
  colnames(samples.valid) = colnames(inSamples)
  
  #Write Csv
  write.table(samples.train, paste0(output.ranger, '/CovariatesDrill', '_', training.fieldname, '_TrainingData.csv'), sep=",", row.names=F)
  write.table(samples.valid, paste0(output.ranger, '/CovariatesDrill', '_', training.fieldname, '_ValidationData.csv'), sep=",", row.names=F)
}

##########################################################################
###       Generate ranger input file
##########################################################################

ranger.input = read.table(paste0(output.ranger,'/CovariatesDrill', '_', training.fieldname, '_TrainingData.csv'), sep=",", header=T,strip.white=T)
outcome.data = data.frame( training.points[[training.id]], as.numeric(as.character(training.points[[training.fieldname]])))
colnames(outcome.data) = c('SID', training.fieldname)

ranger.input = merge(outcome.data, ranger.input, by.x = 'SID', by.y = 'SID')

ranger.output.filename = paste0(outputs.directory, '/', training.filename, '_', 'CovariatesDrill', '_', training.fieldname, '.csv')
write.table(ranger.input, ranger.output.filename, sep=",", row.names=F)
  
##########################################################################
###       Implement ranger
##########################################################################

#Implement algorithm
model = ranger(ranger.input[complete.cases(ranger.input),][[training.fieldname]]~ ., data = ranger.input[complete.cases(ranger.input),][,-(1:4)], write.forest = TRUE)

#Saving the rules
model.filename = paste0(outputs.directory, '/', training.filename, '_', training.fieldname,'_Ranger')
saveRDS(model, paste0(model.filename, '.rds'))

##########################################################################
###       Validation
##########################################################################

#words

##########################################################################
###       Apply the model spatially
##########################################################################

#Block size
bs = blockSize(covariates, nlayers(covariates), minrows=10)


#Compute Averages
averages = as.data.frame(rep(NA,0))
for (i in 2:nlayers(covariates)){
  averages[1,i] = mean(ranger.input[,names(covariates[[i]])],na.rm = T)
}
colnames(averages) = names(covariates)


#Predict in parallel
cl = makeCluster(detectCores()-1,outfile="")
registerDoParallel(cl)
foreach(k = 1:bs$n,  .packages=c('raster','rgdal', 'ranger')) %dopar%  {
  parallel_predict(covariates, covariates[[1]] , bs, outputs.rds, model.type)
}
stopCluster(cl)

raster.out.masked = prediction_raster(outputs.directory, training.filename, training.fieldname, covariates[[1]], bs, outputs.rds, model.type)


  