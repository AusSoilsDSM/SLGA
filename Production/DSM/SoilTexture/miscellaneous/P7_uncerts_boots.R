##Some methods for the quantification of prediction uncertainties for digital soil mapping: 
## Bootstapping


#Library
library(ithir)
library(sp)
library(rgdal)
library(raster)



##DATA
#Point data
data(HV_subsoilpH)
str(HV_subsoilpH)

#Raster data
data(hunterCovariates_sub)
hunterCovariates_sub



#subset data for modeling
set.seed(667)
training <- sample(nrow(HV_subsoilpH), 0.7 * nrow(HV_subsoilpH))
cDat<- HV_subsoilpH[training, ]
vDat<- HV_subsoilpH[-training, ]

#Number of bootstraps
nbag<-50


#Fit cubist models for each bootstrap
library(Cubist)

for ( i in 1:nbag){
trainingREP <- sample.int(nrow(cDat), 0.7 * nrow(cDat), replace = TRUE)

fit_cubist <- cubist(x = cDat[trainingREP, c("Terrain_Ruggedness_Index", "AACN", "Landsat_Band1", "Elevation", 
                                  "Hillshading", "Light_insolation", "Mid_Slope_Positon", "MRVBF", "NDVI", "TWI","Slope")], 
                       y = cDat$pH60_100cm[trainingREP], cubistControl(rules = 5, extrapolation = 5), 
                     committees = 1)

### Note you will likely have different file path names ###
modelFile<- paste0("C:/Temp/uncerts/models/bootMod_",i,".rds")

saveRDS(object = fit_cubist, file = modelFile)}



#list all files in directory
### Note you will likely have different file path names ###
c.models<- list.files(path = "C:/Temp/uncerts/models/", pattern="\\.rds$", full.names=TRUE)
head(c.models)

## Goodness of fit
#calibration data
cubiMat<- matrix(NA, nrow= nbag, ncol=5)
for (i in 1:nbag){
fit_cubist<- readRDS(c.models[i])
cubiMat[i,]<- as.matrix(goof(observed= cDat$pH60_100cm , predicted= predict(fit_cubist, newdata = cDat)))}
cubiDat<- as.data.frame(cubiMat)
names(cubiDat)<- c("R2", "concordance", "MSE", "RMSE", "bias")
colMeans(cubiDat)


#Validation data
cubPred.V<- matrix(NA, ncol= nbag, nrow= nrow(vDat))
cubiMat<- matrix(NA, nrow= nbag, ncol=5)
for (i in 1:nbag){
  fit_cubist<- readRDS(c.models[i])
  cubPred.V[,i]<- predict(fit_cubist, newdata = vDat)
  cubiMat[i,]<- as.matrix(goof(observed= vDat$pH60_100cm , predicted= predict(fit_cubist, newdata = vDat)))}
  cubPred.V_mean<- rowMeans(cubPred.V)

cubiDat<- as.data.frame(cubiMat)
names(cubiDat)<- c("R2", "concordance", "MSE", "RMSE", "bias")
colMeans(cubiDat)

#Average validation MSE
avGMSE<- mean(cubiDat[,3])
avGMSE



## Mapping
## ### Note you will likely have different file path names ###
for (i in 1:nbag){
fit_cubist<- readRDS(c.models[i])
mapFile<- paste0("C:/Temp/uncerts/map/bootMap_",i,".tif")
predict(hunterCovariates_sub,fit_cubist,filename=mapFile,format="GTiff",overwrite=T)}


## statistical measures of maps
## #Pathway to rasters
## ### Note you will likely have different file path names ###
files<- list.files(path = "C:/Temp/uncerts/map/",  pattern="bootMap", full.names=TRUE)
files

#Raster stack
r1<- raster(files[1])
for(i in 2:length(files)){
   r1<- stack(r1,files[i])}
r1

#Calculate mean
meanFile<- paste0("C:/Temp/uncerts/map/","meanPred_.tif")
bootMap.mean<-writeRaster(mean(r1),filename = meanFile, format = "GTiff", overwrite = TRUE)
plot(bootMap.mean)

#Calculate variance
varFile<- paste0("C:/Temp/uncerts/map/","varPred_.tif")
bootMap.var <- calc(r1, var,filename = varFile, format = "GTiff", overwrite = TRUE)
plot(bootMap.var)

#Overall prediction variance (adding avGMSE)
varFile2<- paste0("C:/Temp/uncerts/map/","varPred_F.tif")
bootMap.varF<-writeRaster((bootMap.var + avGMSE) ,filename = varFile, format = "GTiff", overwrite = TRUE)
plot(bootMap.varF)

#Standard deviation
sdFile<- paste0("C:/Temp/uncerts/map/","stvPred_F.tif")
bootMap.sd<-writeRaster(sqrt(bootMap.varF) ,filename = sdFile, format = "GTiff", overwrite = TRUE)
plot(bootMap.sd)

#standard error
seFile<- paste0("C:/Temp/uncerts/map/","sePred_F.tif")
bootMap.se<-writeRaster((bootMap.sd * qnorm(0.95)) ,filename = seFile, format = "GTiff", overwrite = TRUE)

 
#upper prediction limit
uplFile<- paste0("C:/Temp/uncerts/map/","Pred_upper.tif")
bootMap.upl<-writeRaster((bootMap.mean + bootMap.se) ,filename = uplFile, format = "GTiff", overwrite = TRUE)
plot(bootMap.upl) 

#lower prediction limit
lplFile<- paste0("C:/Temp/uncerts/map/","Pred_lower.tif")
bootMap.lpl<-writeRaster((bootMap.mean - bootMap.se) ,filename = lplFile, format = "GTiff", overwrite = TRUE)
plot(bootMap.lpl) 

#prediction interval range
pirFile<- paste0("C:/Temp/uncerts/map/","Pred_pir.tif")
bootMap.pir<-writeRaster((bootMap.upl - bootMap.lpl) ,filename = pirFile, format = "GTiff", overwrite = TRUE)

##PLOTTING
phCramp<- c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", "#e6f598",
             "#abdda4", "#66c2a5" , "#3288bd","#5e4fa2","#542788", "#2d004b" )
brk <- c(2:14)
par(mfrow=c(2,2))
plot(bootMap.lpl, main= "90% Lower prediction limit", breaks=brk, col=phCramp)
plot(bootMap.mean, main= "Prediction",breaks=brk, col=phCramp)
plot(bootMap.upl, main= "90% Upper prediction limit",breaks=brk, col=phCramp)
plot(bootMap.pir, main= "Prediction limit range",col = terrain.colors( length(seq(0, 10, by = 1))-1),
      axes = FALSE, breaks= seq(0, 6.5, by = 1) )


## Validation
val.sd<- matrix(NA, ncol=1, nrow= nrow(cubPred.V))
for (i in 1:nrow(cubPred.V)){
val.sd[i,1]<- sqrt(var(cubPred.V[i,])+ avGMSE)}


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(cubPred.V), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(cubPred.V), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- cubPred.V_mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(cubPred.V), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- cubPred.V_mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(cubPred.V), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(vDat$pH60_100cm <= uMat[,i] & vDat$pH60_100cm >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)


#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))

## Prediction limit range 90% PI
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
colnames(lMat)<- cs
colnames(uMat)<- cs
quantile(uMat[,"90"] - lMat[,"90"])

# END



