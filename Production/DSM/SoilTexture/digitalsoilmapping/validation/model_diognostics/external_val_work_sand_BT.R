## external validation plots
## TERN landscapes
# back transformed
root<- "Z:/projects/ternlandscapes_2019/soiltexture/outs/dsm_externalvalidation/BT/data/"
fig.root<- "Z:/projects/ternlandscapes_2019/soiltexture/outs/dsm_externalvalidation/"






### sand


## D1
# prediction data
pred.data<- readRDS(file = paste0(root,"sand_d1_pred_data_BT.rds"))
dim(pred.data)

# observation data
observation.data<- readRDS(file = paste0(root,"d1_observed_data_BT.rds"))

# row means
val.mean<- rowMeans(pred.data)

## fancy plotting
# sand
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/tern_v2_val_sand_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(observation.data[,2], val.mean,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted sand (%)", xlab= "observed sand (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (observation.data[,2], val.mean,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



## D2
# prediction data
pred.data<- readRDS(file = paste0(root,"sand_d2_pred_data_BT.rds"))
dim(pred.data)

# observation data
observation.data<- readRDS(file = paste0(root,"d2_observed_data_BT.rds"))

# row means
val.mean<- rowMeans(pred.data)

## fancy plotting
# sand
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/tern_v2_val_sand_d2.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(observation.data[,2], val.mean,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted sand (%)", xlab= "observed sand (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (observation.data[,2], val.mean,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



## D3
# prediction data
pred.data<- readRDS(file = paste0(root,"sand_d3_pred_data_BT.rds"))
dim(pred.data)

# observation data
observation.data<- readRDS(file = paste0(root,"d3_observed_data_BT.rds"))

# row means
val.mean<- rowMeans(pred.data)

## fancy plotting
# sand
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/tern_v2_val_sand_d3.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(observation.data[,2], val.mean,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted sand (%)", xlab= "observed sand (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (observation.data[,2], val.mean,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


## D4
# prediction data
pred.data<- readRDS(file = paste0(root,"sand_d4_pred_data_BT.rds"))
dim(pred.data)

# observation data
observation.data<- readRDS(file = paste0(root,"d4_observed_data_BT.rds"))

# row means
val.mean<- rowMeans(pred.data)

## fancy plotting
# sand
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/tern_v2_val_sand_d4.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(observation.data[,2], val.mean,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted sand (%)", xlab= "observed sand (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (observation.data[,2], val.mean,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



## D5
# prediction data
pred.data<- readRDS(file = paste0(root,"sand_d5_pred_data_BT.rds"))
dim(pred.data)

# observation data
observation.data<- readRDS(file = paste0(root,"d5_observed_data_BT.rds"))

# row means
val.mean<- rowMeans(pred.data)

## fancy plotting
# sand
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/tern_v2_val_sand_d5.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(observation.data[,2], val.mean,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted sand (%)", xlab= "observed sand (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (observation.data[,2], val.mean,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


## D6
# prediction data
pred.data<- readRDS(file = paste0(root,"sand_d6_pred_data_BT.rds"))
dim(pred.data)

# observation data
observation.data<- readRDS(file = paste0(root,"d6_observed_data_BT.rds"))

# row means
val.mean<- rowMeans(pred.data)

## fancy plotting
# sand
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/tern_v2_val_sand_d6.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(observation.data[,2], val.mean,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted sand (%)", xlab= "observed sand (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (observation.data[,2], val.mean,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


