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
relief_cov = list.files(paste0(data.directory,'/Relief'), '.tif$', recursive = F, full.names = T )
soil_cov = list.files(paste0(data.directory,'/Soil'), '.tif$', recursive = F, full.names = T )

#Combing covariate data location list
all_cov = c(climate_cov,organism_cov,other_cov,parent_cov,relief_cov,soil_cov)

#Training data location
training.filename = 'Clay'      ## ".shp" extension not needed
training.fieldname = 'GSM1'
training.id = 'ID'

#Results location
outputs.directory =  paste0(root.directory, '/Modelling')
output.cubist = paste0(outputs.directory,'/Cubist')
output.raster = paste0(outputs.directory,'/Rasters')
output.bootstrap = paste0(outputs.directory, '/Bootstrap_Samples')
outputs.rds = paste0(outputs.directory, '/RDS')

#Computing Variables
compute.parallel = F
compute.bootstrap = F
compute.validate = compute.bootstrap

#Modelling Variables
modelling.bootstraps = 5
modelling.percentage = 0.8
#modelling.GenerateUncertainties = T
#modelling.UncertLimits <- c(05, 95)

#Cubist Rules
cubist.rules = 30 #max
cubist.committees = 1


##########################################################################
###       Create Output Directory 
##########################################################################

if(!(file.exists(output.cubist))){
  dir.create(output.cubist, recursive=T )
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
  write.table(samples.train, paste0(output.cubist, '/CovariatesDrill', '_', training.fieldname, '_TrainingData.csv'), sep=",", row.names=F)
  write.table(samples.valid, paste0(output.cubist, '/CovariatesDrill', '_', training.fieldname, '_ValidationData.csv'), sep=",", row.names=F)
}

##########################################################################
###       Bootstrapping
##########################################################################

if (compute.bootstrap){
  #Call in data to be bootstrapped 
  bootSamples<-read.table(paste0(output.cubist,'/CovariatesDrill', '_', training.fieldname, '_TrainingData.csv'), sep=",", header=T,strip.white=T)
  
  #Bootstrap
  bootstrap.samples = list()
  for (i in 1:modelling.bootstraps){
    bootstrap.samples[[i]] = bootSamples[sample(nrow(bootSamples), nrow(bootSamples), replace = T),]
  }
  
  #Csv for bootstraps
  for (i in 1:modelling.bootstraps){
    write.table(bootstrap.samples[[i]], paste0(output.bootstrap, '/CovariatesDrill', '_', 'Bootstrap', '_', toString(i) ,'.csv'), sep=",", row.names=F)
  }
}

##########################################################################
###       Generate cubist input file
##########################################################################

if (!compute.bootstrap){
  #No Bootsrapping
  #Construct data frame applicable to cubist model
  cubist.input = data.frame(as.numeric(as.character(training.points[[training.fieldname]])), covariate.training.withloc[, -1:-3])
  colnames(cubist.input)[1] = training.fieldname
  
  #Csv for cubist input
  cubist.output.filename = paste0(outputs.directory, '/', training.filename, '_', 'CovariatesDrill', '_', training.fieldname, '.csv')
  write.table(cubist.input, cubist.output.filename, sep=",", row.names=F)
  
} else if (compute.bootstrap){
  #Bootstrapping 
  #Obtaining '''Outcome data'''
  outcome.data = data.frame( training.points[[training.id]], as.numeric(as.character(training.points[[training.fieldname]])))
  colnames(outcome.data) = c('SID', training.fieldname)
  
  for (i in 1:modelling.bootstraps){
    bootstrap.path = paste0(output.bootstrap, '/CovariatesDrill', '_', 'Bootstrap', '_', toString(i) ,'.csv')
    bootstrap.cubist = read.table(bootstrap.path, sep=",", header=T,strip.white=T)
  
    bootstrap.cubist = merge(outcome.data, bootstrap.cubist, by.x = 'SID', by.y = 'SID')
    write.table(bootstrap.cubist, paste0(output.cubist, '/Bootstrap_',toString(i),'_', training.fieldname, '_trainingData_With_Attributes.csv'), sep=",", row.names=F)
    
    
    #coordinates(bootstrap.cubist) = ~Easting+Northing
    
    #May need to write another file here
  }
  
  valid.path = paste0(output.cubist, '/CovariatesDrill', '_', training.fieldname, '_ValidationData.csv')
  valid.cubist = read.table(valid.path, sep=",", header=T,strip.white=T)

  valid.cubist = merge(outcome.data, valid.cubist, by.x = 'SID', by.y = 'SID')
  write.table(bootstrap.cubist, paste0(output.cubist, training.fieldname, '_ValidationData_With_Attributes.csv'), sep=",", row.names=F)
  
  
}


##########################################################################
###       Implement Cubist
##########################################################################

if (!compute.bootstrap){
  #No Bootstrapping
  #Implement algorithm
  model = cubist(x = cubist.input[, -1], y = cubist.input[,1], committees=1,  cubistControl( label = training.fieldname ))
  
  #Saving the rules
  model.filename = paste0(outputs.directory, '/', training.filename, '_', training.fieldname,'_Cubist')
  saveRDS(model, paste0(model.filename, '.rds'))
  output.filename <-paste0(model.filename, '.rules')
  file.create(output.filename)
  model.text = summary(model)
  writeLines(model.text$output, output.filename)
  
} else if (compute.bootstrap){
  #Bootstrapping 
  
  cl = makeCluster(detectCores()-1,outfile="")
  registerDoParallel(cl)
  foreach(k = 1:modelling.bootstraps ,  .packages=c('raster','rgdal', 'Cubist')) %dopar%  {
    cubist.bootinput = read.table(paste0(output.cubist, training.fieldname, '_ValidationData_With_Attributes.csv'), sep=",", header=T,strip.white=T)
    model <- cubist(x = cubist.bootinput[, -1:-4], y = cubist.bootinput[,2], committees= cubist.committees,  cubistControl( label = training.fieldname, cubist.rules))
    model.filename = paste0(output.cubist, '/RDS/', training.filename,'_CubistModel_', k)
    saveRDS(model, paste0(model.filename, '.rds'))
    outfilename = paste0(model.filename, '.rules')
    file.create(outfilename)
    model.text = summary(model)
    writeLines(model.text$output, outfilename)
  }
  stopCluster(cl)
  

}

##########################################################################
###       Validation
##########################################################################

if (compute.validate){
  #Observed Values
  observed = training.points[[training.fieldname]][-sample.index]
  
  #Computing predicted values
  ncells = length(observed)
  cubist.input = data.frame(seq(ncells))
  
  cubist.valid = covariate.training[-sample.index, ]
  
  bootOuts = data.frame(rep(0,ncells))
  for(j in 1:modelling.bootstraps){
    model = readRDS(paste0(output.cubist, '/RDS/', training.filename,'_CubistModel_', j, '.rds'))
    prediction = predict(model, cubist.valid)
    bootOuts[paste0('P_',j)] = prediction
  }

  predicted = apply(bootOuts[,-1], 1, mean, na.rm=TRUE)
  
  if (length(which(is.na(observed))) == 0){
    goof(observed, predicted)
  } else{
    goof(observed[-which(is.na(observed))], predicted[-which(is.na(observed))])
  }
  
}

##########################################################################
###       Apply the model spatially
##########################################################################
tic()
if(!compute.parallel && !compute.bootstrap){
  ##Computing from single core
  
  #Is there a better way????
  prediction1 = predict(covariates, model, progress='text', overwrite = T, na.rm=T)
  
}else if (compute.parallel && !compute.bootstrap){
  ##Computing in parallel, no bootstrap
  
  #Block size
  bs = blockSize(covariates, nlayers(covariates), minrows=10)
  
  #Predict in parallel
  cl = makeCluster(detectCores()-1,outfile="")
  registerDoParallel(cl)
  foreach(k = 1:bs$n,  .packages=c('raster','rgdal', 'Cubist')) %dopar%  {
    
    ncells = bs$nrows[k] * ncol(covariates[[1]])
    cubist.input = data.frame(seq(ncells))
    
    cubist.input[names(covariates)] = getValues(covariates,  row=bs$row[k], nrows=bs$nrows[k] )
    
    #Predict
    prediction = predict(model, cubist.input[, -1])
    
    #Save predition as RDS
    bname = paste0(outputs.rds , '/block_' , k, '.rds',  sep="")
    saveRDS(prediction, bname)
    
    
  }
  stopCluster(cl)
  
  #Create output raster
  outRaster = paste0(outputs.directory,'/Rasters', '/mean_', training.filename, '_', training.fieldname, '.tif')
  predR<-raster(covariates[[1]])
  predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  #Incoporating each of the blocks
  for (i in 1:bs$n)
  {
    bname = paste0(outputs.rds , '/block_' , i, '.rds',  sep="")
    if(file.exists(bname)){
      blockVals <-readRDS(bname)
      predR <-writeValues(predR, blockVals, bs$row[i])
    }
  }
  predR<-writeStop(predR)
  
  #Apply Raster mask
  outR <- raster(outRaster)
  outRasterMasked <- mask(outR, covariates[[1]])
  writeRaster(outRasterMasked, paste0(outputs.directory,'/Rasters', '/mean_', training.filename, '_', training.fieldname, '_masked.tif'), format='GTiff', overwrite=T)

  
} else if (compute.parallel && compute.bootstrap){
  ##Computing in parallel, with bootstrap
  #Block size
  bs = blockSize(covariates, nlayers(covariates), minrows=10)
  
  #Predict in parallel
  cl = makeCluster(detectCores()-1,outfile="")
  registerDoParallel(cl)
  foreach(k = 1:bs$n,  .packages=c('raster','rgdal', 'Cubist')) %dopar%  {
    
    ncells = bs$nrows[k] * ncol(covariates[[1]])
    cubist.input = data.frame(seq(ncells))
    
    cubist.input[names(covariates)] = getValues(covariates,  row=bs$row[k], nrows=bs$nrows[k] )

    
    bootOuts = data.frame(rep(0,ncells))
    for(j in 1:modelling.bootstraps){
      model = readRDS(paste0(output.cubist, '/RDS/', training.filename,'_CubistModel_', j, '.rds'))
      prediction = predict(model, cubist.input[, -1])
      bootOuts[paste0('P_',j)] = prediction
    }
    
    means = apply(bootOuts[,-1], 1, mean, na.rm=TRUE)
    
    means.file = paste0(outputs.rds , '/block_bootstrap_' , k, '.rds',  sep="")
    saveRDS(means, means.file )
    
  }
  stopCluster(cl)
  #Create output raster
  outRaster = paste0(outputs.directory,'/Rasters', '/mean_bootstrap_', training.filename, '_', training.fieldname, '.tif')
  predR = raster(covariates[[1]])
  predR = writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  #Incoporating each of the blocks
  for (i in 1:bs$n)
  {
    bname = paste0(outputs.rds , '/block_bootstrap_' , i, '.rds',  sep="")
    if(file.exists(bname)){
      blockVals <-readRDS(bname)
      predR <-writeValues(predR, blockVals, bs$row[i])
    }
  }
  predR<-writeStop(predR)
  
  #Apply Raster mask
  outR <- raster(outRaster)
  outRasterMasked <- mask(outR, covariates[[1]])
  writeRaster(outRasterMasked, paste0(outputs.directory,'/Rasters', '/mean_', training.filename, '_', training.fieldname, '_masked.tif'), format='GTiff', overwrite=T)

}
time =  toc()















