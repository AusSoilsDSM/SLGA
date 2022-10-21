#Prediction function which skips over rows with incomplete data (good for ranger)
parallel_predict = function(rasterStack, raster.template, bs, outputs.rds, model.type, training.data,averages)
{
  
  #Prediction function which skips over rows with incomplete data (good for ranger)
  
  #Calculate block size
  ncells = bs$nrows[k] * ncol(raster.template)
  
  #Load block in
  input = data.frame(getValues(covariates, row=bs$row[k], nrows=bs$nrows[k]))
  
  #Putting Averages in
  if(F){
    missing.cord = as.data.frame(which(is.na(input), arr.ind=TRUE))
    input[missing.cord$row, missing.cord$col] = averages[[missing.cord$col]]
  } 
  
  #Find index of complete data
  complete.index = complete.cases(input)
  prediction = data.frame(rep(NA, ncells))
  
  #Predict
  prediction[complete.index,] = predict(model, data = input[complete.index,])$predictions
  
  
  #Save predition as RDS
  bname = paste0(outputs.rds , '/', model.type, '_block_' , k, '.rds',  sep="")
  saveRDS(prediction, bname)
  return('words')
}


prediction_raster = function(outputs.directory, training.filename, training.fieldname, raster.template, bs, outputs.rds, model.type)
{
  
  #Constructs a prediction raster from dataframes
  #Data.frames must be saved to file already
  #Prediction must be a real number

  #Output Directory
  raster.out = paste0(outputs.directory,'/Rasters', '/mean_', training.filename, '_', training.fieldname, '.tif')
  
  #Begin writing raster
  predR = raster(raster.template)
  predR = writeStart(predR,filename=raster.out,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  #Incoporating each of the blocks
  for (i in 1:bs$n)
  {
    bname = paste0(outputs.rds , '/', model.type, '_block_' , i, '.rds',  sep="")
    if(file.exists(bname)){
      block.values = readRDS(bname)
      block.values = unlist(block.values, use.names=FALSE)
      predR = writeValues(predR, block.values, bs$row[i])
    }
  }
  
  #Stop writing raster
  predR<-writeStop(predR)
  
  #Apply Raster mask
  raster.out <- raster(raster.out)
  raster.out.masked <- mask(raster.out, raster.template)
  writeRaster(raster.out.masked, paste0(outputs.directory,'/Rasters', '/mean_', training.filename, '_', training.fieldname, '_masked.tif'), format='GTiff', overwrite=T)
  return(raster.out.masked)
}


parallel_predict_categorical = function(rasterStack, raster.template, bs, outputs.rds, model.type, training.data,averages)
{
  
  #Prediction function which skips over rows with incomplete data (good for ranger)
  
  #Calculate block size
  ncells = bs$nrows[k] * ncol(raster.template)
  
  #Load block in
  input = data.frame(getValues(covariates, row=bs$row[k], nrows=bs$nrows[k]))
  
  #Putting Averages in
  if(F){
    missing.cord = as.data.frame(which(is.na(input), arr.ind=TRUE))
    input[missing.cord$row, missing.cord$col] = averages[[missing.cord$col]]
  } 
  
  #Find index of complete data
  complete.index = complete.cases(input)
  prediction = data.frame(rep(NA, ncells))
  
  #Predict
  prediction[complete.index,] = predict(model, data = input[complete.index,])$predictions
  prediction[complete.index,] = as.numeric(prediction[complete.index,])
  
  
  #Save predition as RDS
  bname = paste0(outputs.rds , '/', model.type, '_block_' , k, '.rds',  sep="")
  saveRDS(prediction, bname)
  return('words')
}




  



