### TERN LANDSCAPES 
# total N
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 18.11.23
# modified: 18.11.23

# CODE PURPOSE
# Evaluate model predictions from SLGA 1
# depth 6

depth<- 6
fsel1<- 16
fsel2<- 18

# libraries
library(terra);library(sf);library(MASS)
# goof function 
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/miscell/goof.R")

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_N/"
data.root<- paste0(g.root,"models/ranger_models/data_obs_preds/ext/")
SLGA1.root<- "/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/NTO/"
fig.root<- paste0(g.root, "outputs/external_evaluation/")


# load in rasters
files<- list.files(path = SLGA1.root, pattern = ".tif",full.names = T, recursive = F)
files
files<- files[fsel1:fsel2]
s1<- terra::rast(files)
s1

# Point data
vart<- "totalN"
test<- read.csv(file = paste0(data.root,"ranger_EXT_preds_",vart,"_depth_",1,"_summary.csv"))

# subset data
test.sel<- test[test$depth == depth, ]
names(test.sel)

# spatialise point date then do raster extract
test.sel.sf<- sf::st_as_sf(x = test.sel,coords = c("Longitude_copy", "Latitude_copy"))
DSM_data <- as.data.frame(terra::extract(x = s1, y = test.sel.sf, bind = T, method = "simple"))
names(DSM_data)
DSM_data<- DSM_data[complete.cases(DSM_data[,3:8]),]



# model predictions
obs.dat<- DSM_data$target
modpred.dat<- DSM_data[,8]

goof(observed = obs.dat, predicted = modpred.dat,plot.it = T)
ext.pred.goof.frame<- goof(observed = obs.dat, predicted = modpred.dat)

# picp calc
picp<- sum(DSM_data$target >= DSM_data[,6] & DSM_data$target <= DSM_data[,7])/nrow(DSM_data)
picp

ext.pred.goof.frame$picp<- picp


# plotting
xlimits= c(0,1.5)
ylimits= c(0,1.5) 
tiff(file=paste0(fig.root,"external_val_pred_SLGA1_d",depth,".tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(obs.dat, modpred.dat,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab= paste0("predicted ", vart), xlab= paste0("observed ", vart),col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = ylimits[1],to = ylimits[2],by = 0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = xlimits[1],to = xlimits[2],by = 0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (obs.dat, modpred.dat,pch=1, col="black", cex=0.5)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# save goof output
ext.pred.goof.frame<- as.data.frame(ext.pred.goof.frame)
names(ext.pred.goof.frame)<- c("R2", "concordance", "MSE", "RMSE", "bias","picp")
write.csv(x = ext.pred.goof.frame, file = paste0(fig.root,"external_val_pred_SLGA1_d",depth,".csv"), row.names = F)




