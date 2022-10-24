### TERN LANDSCAPES 
# Soil pH model validation
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 12.5.21
# modified: 1.6.21

# CODE PURPOSE
# External validation of regression kriging model

library(ithir);library(MASS);library(sp);library(gstat);library(rgdal);library(automap)

model.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/models/"
data.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/data/curated_all/variogram_dat/4a1/"
fig.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/outs/external_validation/"

# residual variograms
# site data
site.dat<- readRDS(paste0(data.root,"tern_soilpH4a1_siteDat_covariates_CALVALDAT_SimulationResiduals_d1_ARD.rds"))
str(site.dat)
proj4string(site.dat)<- "+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"


# External validation
# depth 1
# variogram
afit<- readRDS(paste0(model.out,"variogram_models/4a1/residuals_variogram_4a1_d1.rds"))
plot(afit)
root.in<- paste0(model.out,"d1/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
for(i in 2:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- cbind(temp.file,in.dat$prediction)
}
names(temp.file)
temp.file$mean<- rowMeans(temp.file[,12:61])
temp.file<- temp.file[,c(1:11,62)]
temp.file$residual<- NA

# residual kriging

#validation data
kdat<- temp.file
names(kdat)[4:5]<- c("x","y")
coordinates(kdat)<- ~ x + y
proj4string(kdat)<- "+proj=longlat +datum=WGS84 +no_defs"
kdat <- spTransform(kdat, CRS("+proj=lcc +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
kdat<- as.data.frame(kdat)
names(kdat)

for (i in 1:nrow(kdat)){
  mat1<- as.data.frame(site.dat)
  #str(mat1)
  mat2<- as.matrix(mat1[,51:52]) # matrix of data coordinates
  
  centroid<- as.matrix(as.data.frame(kdat[i,12:13])) # matrix of centroid coordinate
  
  dist1<- spDistsN1(mat2,centroid,longlat = FALSE) #distance
  #summary(dist1)
  mat1<- cbind(mat1,dist1)
  mat3<- mat1[order(mat1$dist1),]
  mat3<- mat3[1:2000,] # get the nearest 2000 points
  
  coordinates(mat3)<- ~ x + y
  centroid.p<- kdat[i,]
  coordinates(centroid.p)<- ~ x + y 
  Xresid<- as.data.frame(krige(predResidual~1, mat3, model=afit$var_model, newdata= centroid.p,debug.level=0))[,3]
  kdat$residual[i]<- Xresid
  print(i)
  }


obs.dat<- kdat$target
pred.dat1<- kdat$mean
goof(observed = obs.dat, predicted = pred.dat1)
pred.dat2<- kdat$mean + kdat$residual
goof(observed = obs.dat, predicted = pred.dat2)

## capture output
var_nm1<- paste0(fig.root, "external_val_diogs_4a1_d1.txt")

jj1<- cbind("pred_",goof(observed = obs.dat, predicted = pred.dat1))
out1<- capture.output(jj1)
out1<- paste0(out1,collapse = "\r\n")
cat(out1, file = var_nm1, sep=",", append = T)

jj2<- cbind("predRK_",goof(observed = obs.dat, predicted = pred.dat2))
out2<- capture.output(jj2)
out2<- paste0(out2,collapse = "\r\n")
cat(out2, file = var_nm1, sep=",", append = T)


# plotting
xlimits= c(3,12)
ylimits= c(3,12) 
tiff(file=paste0(fig.root,"external_val_pred_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(obs.dat, pred.dat1,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted pH", xlab= "observed pH",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (obs.dat, pred.dat1,pch=1, col="black", cex=0.5)
abline(0, 1, lwd=1.5, col="red")
dev.off()

xlimits= c(3,12)
ylimits= c(3,12) 
tiff(file=paste0(fig.root,"external_val_predRK_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(obs.dat, pred.dat2,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted pH", xlab= "observed pH",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (obs.dat, pred.dat2,pch=1, col="black", cex=0.5)
abline(0, 1, lwd=1.5, col="red")
dev.off()


# write data to file
saveRDS(object = kdat, file = paste0(fig.root,"external_val_outs_d1_4a1.rds"))



#######################################
kdat<- readRDS(file = paste0(fig.root,"external_val_outs_d1_4a1.rds"))


# average out the target data
root.in<- paste0(model.out,"d1/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
for(i in 2:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- cbind(temp.file,in.dat$target)
}
names(temp.file)
temp.file$mean_target<- rowMeans(temp.file[,c(10,13:61)])
temp.file<- temp.file[,c(1:9,11,62)]


obs.dat<- temp.file$mean_target
pred.dat1<- kdat$mean
goof(observed = obs.dat, predicted = pred.dat1)
pred.dat2<- kdat$mean + kdat$residual
goof(observed = obs.dat, predicted = pred.dat2)

## capture output
var_nm1<- paste0(fig.root, "external_val_diogs_4a1_d1.txt")

jj1<- cbind("pred_",goof(observed = obs.dat, predicted = pred.dat1))
out1<- capture.output(jj1)
out1<- paste0(out1,collapse = "\r\n")
cat(out1, file = var_nm1, sep=",", append = T)

jj2<- cbind("predRK_",goof(observed = obs.dat, predicted = pred.dat2))
out2<- capture.output(jj2)
out2<- paste0(out2,collapse = "\r\n")
cat(out2, file = var_nm1, sep=",", append = T)


# plotting
xlimits= c(3,12)
ylimits= c(3,12) 
tiff(file=paste0(fig.root,"external_val_pred_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(obs.dat, pred.dat1,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted pH", xlab= "observed pH",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (obs.dat, pred.dat1,pch=1, col="black", cex=0.5)
abline(0, 1, lwd=1.5, col="red")
dev.off()

xlimits= c(3,12)
ylimits= c(3,12) 
tiff(file=paste0(fig.root,"external_val_predRK_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(obs.dat, pred.dat2,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted pH", xlab= "observed pH",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (obs.dat, pred.dat2,pch=1, col="black", cex=0.5)
abline(0, 1, lwd=1.5, col="red")
dev.off()



