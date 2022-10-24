### TERN LANDSCAPES 
# Soil pH 4b
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 12.9.22
# modified: 14.9.22

# CODE PURPOSE
# Evaluate model predictions 
# Krige residuals using fitted variogram
# evaluate models of prediction + residual vs 'measurment'

depth<- "d3"

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
data.root<- paste0(g.root,"models/ph_4b/",depth,"/data_obs_preds/ext/")
resmod.root<- paste0(g.root,"models/ph_4b/variograms/")
site.root<- paste0(g.root, "models/ph_4b/",depth,"/data_obs_preds/cal/")
fig.root<- paste0(g.root, "outs/4B_meth/external_evaluation/")

# goof function 
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/miscell/goof.R")

# libraries
library(raster);library(rgdal);library(sp);library(automap);library(gstat);library(MASS)

site.dat<- read.table(file = paste0(site.root, "ranger_CAL_preds_average_summaries_pH4b_depth_",depth,".txt"),header = T, sep = ",")
names(site.dat)
names(site.dat)[4:5]<- c("x", "y")
coordinates(site.dat)<- ~ x + y
# set coordinate reference system
crs(site.dat)<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# transform coordinates to projected
site.dat<- spTransform(site.dat,CRSobj = "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")



# residual variogram
resmod<- readRDS(file = paste0(resmod.root, "residuals_variogram_pH4b_",depth,".rds"))
resmod
plot(resmod)


# cycle through and read each prediciton frame and save target variable and prediction data
files<- list.files(path = data.root, full.names = T)
files[1]
tempfile<- read.table(file = files[1],header = T, sep = ",")

tv.frame<- matrix(NA, ncol = 100, nrow= nrow(tempfile))
pred.frame<- matrix(NA, ncol = 100, nrow= nrow(tempfile))

for (i in 1:length(files)){
  temp.read<- read.table(file = files[i],header = T, sep = ",")
  tv.frame[,i]<- temp.read$target 
  pred.frame[,i]<- temp.read$prediction}

tempfile$pred_avg<- rowMeans(pred.frame)
tempfile$target_avg<- rowMeans(tv.frame)


# Set coordinates of data
tempfile$residual<- NA
sp.tempfile<-tempfile
names(sp.tempfile)
names(sp.tempfile)[4:5]<- c("x","y")
coordinates(sp.tempfile)<- ~ x + y
proj4string(sp.tempfile)<- "+proj=longlat +datum=WGS84 +no_defs"
sp.tempfile <- spTransform(sp.tempfile, CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
sp.tempfile<- as.data.frame(sp.tempfile)
names(sp.tempfile)

for (i in 1:nrow(sp.tempfile)){
  mat1<- as.data.frame(site.dat)
  names(mat1)
  mat2<- as.matrix(mat1[,12:13]) # matrix of data coordinates
  
  centroid<- as.matrix(as.data.frame(sp.tempfile[i,14:15])) # matrix of centroid coordinate
  
  dist1<- spDistsN1(mat2,centroid,longlat = FALSE) #distance
  #summary(dist1)
  mat1<- cbind(mat1,dist1)
  mat3<- mat1[order(mat1$dist1),]
  mat3<- mat3[1:2000,] # get the nearest 2000 points
  
  coordinates(mat3)<- ~ x + y
  centroid.p<- sp.tempfile[i,]
  coordinates(centroid.p)<- ~ x + y 
  Xresid<- as.data.frame(krige(residual_avg~1, mat3, model=resmod$var_model, newdata= centroid.p,debug.level=0))[,3]
  tempfile$residual[i]<- Xresid
  print(i)
}


tempfile$pred_avgRK<- tempfile$pred_avg + tempfile$residual
names(tempfile)
tempfile.export<- tempfile[c(1:8,11,13:16)]

# write file to folder
write.csv(x = tempfile.export, file = paste0(data.root,"ranger_EXT_preds_pH4b_depth_",depth,"_summary.txt"))


# model predictions
obs.dat<- tempfile.export$target_avg
modpred.dat<- tempfile.export$pred_avg
goof(observed = obs.dat, predicted = modpred.dat)
ext.pred.goof.frame<- goof(observed = obs.dat, predicted = modpred.dat)


# plotting
xlimits= c(3,12)
ylimits= c(3,12) 
tiff(file=paste0(fig.root,"external_val_pred_",depth,".tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(obs.dat, modpred.dat,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted pH", xlab= "observed pH",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (obs.dat, modpred.dat,pch=1, col="black", cex=0.5)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# save goof output
ext.pred.goof.frame<- as.data.frame(ext.pred.goof.frame)
names(ext.pred.goof.frame)<- c("R2", "concordance", "MSE", "RMSE", "bias")
write.csv(x = ext.pred.goof.frame, file = paste0(fig.root,"external_val_pred_",depth,".csv"), row.names = F)



# regression kriging model predictions
obs.dat<- tempfile.export$target_avg
modpred.dat<- tempfile.export$pred_avgRK
goof(observed = obs.dat, predicted = modpred.dat)
ext.pred.goof.frame<- goof(observed = obs.dat, predicted = modpred.dat)


# plotting
xlimits= c(3,12)
ylimits= c(3,12) 
tiff(file=paste0(fig.root,"external_val_predRK_",depth,".tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(obs.dat, modpred.dat,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted pH", xlab= "observed pH",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (obs.dat, modpred.dat,pch=1, col="black", cex=0.5)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# save goof output
ext.pred.goof.frame<- as.data.frame(ext.pred.goof.frame)
names(ext.pred.goof.frame)<- c("R2", "concordance", "MSE", "RMSE", "bias")
write.csv(x = ext.pred.goof.frame, file = paste0(fig.root,"external_val_predRK_",depth,".csv"), row.names = F)


## Another r script for the uncertainty stuff


