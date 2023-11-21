### TERN LANDSCAPES 
# total N
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 18.11.23
# modified: 18.11.23

# CODE PURPOSE
# Evaluate model predictions 
# depth 6

depth<- "all_depths"

# libraries
library(MASS)
# goof function 
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/miscell/goof.R")

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_N/"
data.root<- paste0(g.root,"models/ranger_models/data_obs_preds/ext/")
fig.root<- paste0(g.root, "outputs/external_evaluation/")

# Point data
vart<- "totalN"
test<- read.csv(file = paste0(data.root,"ranger_EXT_preds_",vart,"_depth_",1,"_summary.csv"))

# subset data
DSM_data<- test
names(DSM_data)

# model predictions
obs.dat<- DSM_data$target
modpred.dat<- DSM_data$pred_avg

goof(observed = obs.dat, predicted = modpred.dat,plot.it = T)
ext.pred.goof.frame<- goof(observed = obs.dat, predicted = modpred.dat)

# plotting
xlimits= c(0,1.5)
ylimits= c(0,1.5) 
tiff(file=paste0(fig.root,"external_val_pred_",depth,".tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(obs.dat, modpred.dat,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab= paste0("predicted ", vart), xlab= paste0("observed ", vart),col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = ylimits[1],to = ylimits[2],by = 0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = xlimits[1],to = xlimits[2],by = 0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (obs.dat, modpred.dat,pch=1, col="black", cex=0.5)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# save goof output
ext.pred.goof.frame<- as.data.frame(ext.pred.goof.frame)
names(ext.pred.goof.frame)<- c("R2", "concordance", "MSE", "RMSE", "bias")
write.csv(x = ext.pred.goof.frame, file = paste0(fig.root,"external_val_pred_",depth,".csv"), row.names = F)

# END

