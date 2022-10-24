### TERN LANDSCAPES 
# Soil pH model validation of SLGA products
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 21.10.22
# modified: 21.10.22

# CODE PURPOSE
# External validation of regression kriging model

library(MASS);library(sp);library(raster)
# goof function 
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/miscell/goof.R")


depth<- "d5"

orig.root<- "/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/pHc/"
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
data.root<- paste0(g.root,"models/ph_4b/",depth,"/data_obs_preds/ext/")
fig.root<- paste0(g.root, "outs/4B_meth/external_evaluation/")


# read data in
ext.dat<- read.table(file = paste0(data.root,"ranger_EXT_preds_pH4b_depth_",depth,"_summary.txt"),header = T,sep = ",")
#ext.dat<- ext.dat[1:100,]
names(ext.dat)
coordinates(ext.dat)<- ~ Longitude + Latitude
proj4string(ext.dat)<- "+proj=longlat +datum=WGS84 +no_defs"


# raster files
pred.files<- list.files(path = orig.root,full.names = T, pattern = ".tif")
pred.files<- pred.files[13:15]
s1<- stack(pred.files)
names(s1)

# raster point data extract
DSM_data <- extract(x = s1, y = ext.dat, sp = 1, method = "simple")
DSM_data<- as.data.frame(DSM_data)
names(DSM_data)
rmsl<- length(which(is.na(DSM_data[,16])))
rmsl
if (rmsl != 0){
  rms<- which(is.na(DSM_data[,16]))
  DSM_data<- DSM_data[-rms,]}

obs.dat<- DSM_data$target_avg
pred.dat<- DSM_data[,17]
lower.dat<- DSM_data[,15]
upper.dat<- DSM_data[,16]
pred.dat2<- DSM_data$pred_avgRK
  
# goodness of fit
SLGA1.goof.frame<- as.data.frame(goof(observed = obs.dat, predicted = pred.dat, plot.it = T))
SLGA1.goof.frame
SLGA2.goof.frame<- goof(observed = obs.dat, predicted = pred.dat2, plot.it = T)
SLGA2.goof.frame
#PICP
slga1.picp<- sum(obs.dat <= upper.dat & obs.dat >= lower.dat)/length(obs.dat)
slga1.picp
SLGA1.goof.frame$picp<- slga1.picp
write.csv(x = SLGA1.goof.frame, file = paste0(fig.root,"external_val_pred_SLGA1_",depth,".csv"))


# plotting
xlimits= c(3,12)
ylimits= c(3,12) 
tiff(file=paste0(fig.root,"external_val_pred_SLGA1_",depth,".tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(obs.dat, pred.dat,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted pH", xlab= "observed pH",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 3,to = 12,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (obs.dat, pred.dat,pch=1, col="black", cex=0.5)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# save goof output
write.csv(x = DSM_data, file = paste0(data.root,"ranger_EXT_preds_pH4b_depth_",depth,"_summary_wSLGA1_dat.csv"), row.names = F)


