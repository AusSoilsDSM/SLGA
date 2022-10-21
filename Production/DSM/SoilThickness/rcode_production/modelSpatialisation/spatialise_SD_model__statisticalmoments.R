## Soil depth
## spatialising soil depth predictions: statistical moments
## Modified: 05/08/19
## handles categorical data too


## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster)


#what tiles are we working on?
#tile folders
dpw<- "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/spatialPrediction/tiles/"  #directory pathway
fols<- as.numeric(list.files(dpw, full.names = FALSE))
fols
length(fols)


###
# begin parallel cluster and register it with foreach
cpus<- 8
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)

###
# get raster (this will change from tile to tile)
oper1<- foreach(i=1:length(fols), .packages = c("raster", "sp", "rgdal")) %dopar% {
  
  # select tile folder 
  sfol<- fols[i]
  
  #general path to the rasters
  nm2<- paste0(dpw, sfol)
  
  ## Mapped predictions (continuous data)
  preds<- list.files(path = nm2, full.names=T, all.files = T,recursive = T, pattern = "soilDepth_pred.tif") 
  #preds
  
  ## kriged residuals
  resids<- list.files(path = nm2, full.names=T, all.files = T,recursive = T, pattern = "residual_krigOut.tif")
  #resids
  
  ## Mapped predictions (categorical data)
  preds_cat<- list.files(path = nm2, full.names=T, all.files = T,recursive = T, pattern = "soilDepth__cat_pred.tif") 
  #preds_cat
  
  ## Binary rock outcrop predictions
  preds_gar<- list.files(path = nm2, full.names=T, all.files = T,recursive = T, pattern = "soilDepth__GAR_cat_pred") 
  #preds_gar
  
  
  ## SUMMARISE CATEGORICAL DATA (deep vs. not deep)
  s2<- stack()
  for (j in 1:length(preds_cat)){
    r1<- as.factor(raster(preds_cat[j]))
    r2<- as.factor(r1)
    r2[r2==0]<- 2 # need to change zeros to a real number 
    #r2
    rat <- levels(r2)
    rat[["depth"]] <- c("HVT_00", "HVT_01")
    levels(r2) <- rat
    s2<- stack(s2, r2)
  }
  
  
  # count function
  cntF<- function(x){tabulate(x, nbins=2)}
  counts<- calc(s2,fun = cntF)
  counts
  #plot(counts[[2]])
  
  # generate probabilities from the count data
  probF<- function(x){x/50}
  probs<- calc(counts, fun = probF)
  #probs
  
  # write probability rasters to file
  # deep
  r1<- probs[[1]]
  names(r1)<- "deepSoils_prob_SD"
  ras1<- paste0(nm2, "/deepSoils_prob_SD.tif")
  writeRaster(r1,filename = ras1, format="GTiff",overwrite=T)
  # not that deep
  r1<- probs[[2]]
  names(r1)<- "not_deep_prob_SD"
  ras1<- paste0(nm2, "/not_deep_prob_SD.tif")
  writeRaster(r1,filename = ras1, format="GTiff",overwrite=T)
  
  ## SUMMARISE CATEGORICAL DATA (rock outcrop vs. not rock outcrop)
  s2<- stack()
  for (j in 1:length(preds_gar)){
    r1<- as.factor(raster(preds_gar[j]))
    r2<- as.factor(r1)
    r2[r2==0]<- 2 # need to change zeros to a real number 
    #r2
    rat <- levels(r2)
    rat[["depth"]] <- c("HVT_00", "HVT_01")
    levels(r2) <- rat
    s2<- stack(s2, r2)
  }
  
  
  # count function
  cntF<- function(x){tabulate(x, nbins=2)}
  counts<- calc(s2,fun = cntF)
  counts
  #plot(counts[[1]])
  
  # generate probabilities from the count data
  probF<- function(x){x/50}
  probs2<- calc(counts, fun = probF)
  #probs2
  
  # write probability rasters to file
  # rock outcrop
  r1<- probs2[[1]]
  names(r1)<- "rockoutcrop_prob_SD"
  ras1<- paste0(nm2, "/rockoutcrop_prob_SD.tif")
  writeRaster(r1,filename = ras1, format="GTiff",overwrite=T)
  # not rock outcrop
  r1<- probs2[[2]]
  names(r1)<- "not_rockoutcrop_prob_SD"
  ras1<- paste0(nm2, "/not_rockoutcrop_prob_SD.tif")
  writeRaster(r1,filename = ras1, format="GTiff",overwrite=T)
  
  
  ## CONTINUOUS DATA
  # stack predictions
  s1<- stack()
  for (j in 1:length(preds)){
    x1<- raster(preds[j])
    x2<- raster(resids[1]) # kriged residual
    x3<- x1+x2 # add kriged residual
    s1<- stack(s1, x3)
  }
  
 
  #backtransform
  btF<- function(x){(x^2)}
  s3<- calc(s1, fun = btF)
  #s3
  
  ## combine continous preds with the categorical probs
  z1<- stack(probs[[1]],probs2[[1]], s3)
  tempD <- data.frame(cellNos = seq(1:ncell(z1)))
  vals <- as.data.frame(getValues(z1))
  tempD<- cbind(tempD, vals)
  cellNos <- c(tempD$cellNos)
  gXY <- data.frame(xyFromCell(z1, cellNos, spatial = FALSE))
  tempD<- cbind(gXY, tempD)
  
  ## Add a bit of noise to the max depth of deep soils and rock outcrop
  ints<- which(tempD$layer.1.1>= 0.90) # deep soils
  ints2<- which(tempD$layer.1.2>= 0.90) # rock outcrop
  ints3<- which(tempD$layer.1.1>= 0.90 && tempD$layer.1.2>= 0.90)
  
  # deep soils
  if(length(ints!=0)){
    tempD[which(tempD$layer.1.1 >= 0.90),6:ncol(tempD)] <- 2 + abs(rnorm(n = length(which(tempD$layer.1.1 >= 0.90)) * nlayers(s3),mean = 0, sd = 0.1))}
  
  # rock outcrops
  if(length(ints2!=0)){
    tempD[which(tempD$layer.1.2 >= 0.90),6:ncol(tempD)] <- 0 + abs(rnorm(n = length(which(tempD$layer.1.2 >= 0.90)) * nlayers(s3),mean = 0, sd = 0.01))}
  
  # uncertainty gate
  # ambiguity gets treated as shallow rather than deep
  if(length(ints3!=0)){
    tempD[which(tempD$layer.1.2 >= 0.90 && tempD$layer.1.1 >= 0.90),6:ncol(tempD)] <- 0 + abs(rnorm(n = length(which(tempD$layer.1.2 >= 0.90 && tempD$layer.1.1 >= 0.90)) * nlayers(s3),mean = 0, sd = 0.01))}
  
  
######
  # rasterise
  z2<- stack()
  for (k in 6:ncol(tempD)){
    tmp.ras<-rasterFromXYZ(tempD[,c(1,2,k)])
    z2<- stack(tmp.ras,z2)
  }
  

  ####
  ## Statistical moments
  #calculate quantiles
  f2<- function(x)(quantile(x,probs = c(0.1,0.5,0.9),na.rm=TRUE)) 
  predQuantile <- calc(z2, fun=f2)
  
  ## write quantile rasters to file
  
  #10th percentile
  r1<- predQuantile[[1]]
  names(r1)<- "tenthPercentile_SD"
  ras1<- paste0(nm2, "/tenthPercentile_SD.tif")
  writeRaster(r1,filename = ras1, format="GTiff",overwrite=T)
  #plot(r1)
  
  #50th percentile
  r1<- predQuantile[[2]]
  names(r1)<- "median_SD"
  ras1<- paste0(nm2, "/median_SD.tif")
  writeRaster(r1,filename = ras1, format="GTiff",overwrite=T)
  #plot(r1)
  
  #90th percentile
  r1<- predQuantile[[3]]
  names(r1)<- "nintiethPercentile_SD"
  ras1<- paste0(nm2, "/ninetiethPercentile_SD.tif")
  writeRaster(r1,filename = ras1, format="GTiff",overwrite=T)
  plot(r1)
  
  #calculate prediction greater than a particular threshold
  #10cm
  f3<- function(x) (sum(x >= 0.1)) 
  predRange <- calc(z2, fun=f3)
  f4<- function(x)(x/nlayers(z2))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh10cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  #plot(predRange)
  
  #30cm
  f3<- function(x) (sum(x >= 0.3)) 
  predRange <- calc(z2, fun=f3)
  f4<- function(x)(x/nlayers(z2))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh30cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  #plot(predRange)
  
  #50cm
  f3<- function(x) (sum(x >= 0.5)) 
  predRange <- calc(z2, fun=f3)
  f4<- function(x)(x/nlayers(z2))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh50cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  #plot(predRange)
  
  #100cm
  f3<- function(x) (sum(x >= 1)) 
  predRange <- calc(z2, fun=f3)
  f4<- function(x)(x/nlayers(z2))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh100cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  #plot(predRange)
  
  #150cm
  f3<- function(x) (sum(x >= 1.5)) 
  predRange <- calc(z2, fun=f3)
  f4<- function(x)(x/nlayers(z2))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh150cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  #plot(predRange)
  
  ##200cm
  f3<- function(x) (sum(x >= 2)) 
  predRange <- calc(z2, fun=f3)
  f4<- function(x)(x/nlayers(z2))
  predRange <- calc(predRange, fun=f4)
  ras1<- paste0(nm2, "/thresh200cm_SD.tif")
  writeRaster(predRange,filename = ras1, format="GTiff",overwrite=T)
  #plot(predRange)
  
  ################3
  #write tile diognostic out for slurm
  write.table(nm2, 
              file = paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/rcode/slurm/slurmCheck/", i, "_", sfol, "_tile.txt"),
              row.names = F, col.names = F, sep="")
              
}

### END

  
  
  
  
  
 
  
  
  
 