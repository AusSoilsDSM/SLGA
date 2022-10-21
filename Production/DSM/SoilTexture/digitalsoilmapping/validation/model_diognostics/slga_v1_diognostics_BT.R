### Checking of version 1 predictions against observations
library(compositions);library(ithir);library(MASS)

# files where directories are:
out.root<- "Z:/projects/ternlandscapes_2019/soiltexture/data/SLGA_V1_ext_val/"
list.files(out.root)

fig.root<- "Z:/projects/ternlandscapes_2019/soiltexture/outs/dsm_externalvalidation/"


# Depth 1
dat<- readRDS(file = paste0(out.root, "slga_v1_data_d1_.rds"))
names(dat)
se.dat<- dat[,c(1:13,16,34,52)]

#columns for compositions
se.dat$obs_clay<- NA
se.dat$obs_sand<- NA
se.dat$obs_silt<- NA
names(se.dat)[14:16]<- c("pred_clay", "pred_sand", "pred_silt")
names(se.dat)
se.dat[,17:19]<- as.data.frame(ilrInv(se.dat[,c(6:7)]))*100
se.dat<- se.dat[complete.cases(se.dat),] 

# composition clay
d1.clay.out<- goof(observed = se.dat$obs_clay,predicted = se.dat$pred_clay, plot.it = T)
d1.clay.out

# composition sand
d1.sand.out<- goof(observed = se.dat$obs_sand,predicted = se.dat$pred_sand, plot.it = T)
d1.sand.out

# composition silt
d1.silt.out<- goof(observed = se.dat$obs_silt,predicted = se.dat$pred_silt, plot.it = T)
d1.silt.out

## fancy plotting
# clay
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_clay_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_clay, se.dat$pred_clay,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted clay (%)", xlab= "observed clay (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_clay, se.dat$pred_clay,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# sand
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_sand_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_sand, se.dat$pred_sand,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted sand (%)", xlab= "observed sand (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_sand, se.dat$pred_sand,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# silt
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_silt_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_silt, se.dat$pred_silt,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted silt (%)", xlab= "observed silt (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_silt, se.dat$pred_silt,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


# Depth 2
dat<- readRDS(file = paste0(out.root, "slga_v1_data_d2_.rds"))
names(dat)
se.dat<- dat[,c(1:13,19,37,55)]

#columns for compositions
se.dat$obs_clay<- NA
se.dat$obs_sand<- NA
se.dat$obs_silt<- NA
names(se.dat)[14:16]<- c("pred_clay", "pred_sand", "pred_silt")
names(se.dat)
se.dat[,17:19]<- as.data.frame(ilrInv(se.dat[,c(6:7)]))*100
se.dat<- se.dat[complete.cases(se.dat),] 

# composition clay
d2.clay.out<- goof(observed = se.dat$obs_clay,predicted = se.dat$pred_clay, plot.it = T)
d2.clay.out

# composition sand
d2.sand.out<- goof(observed = se.dat$obs_sand,predicted = se.dat$pred_sand, plot.it = T)
d2.sand.out

# composition silt
d2.silt.out<- goof(observed = se.dat$obs_silt,predicted = se.dat$pred_silt, plot.it = T)
d2.silt.out

## fancy plotting
# clay
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_clay_d2.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_clay, se.dat$pred_clay,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted clay (%)", xlab= "observed clay (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_clay, se.dat$pred_clay,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# sand
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_sand_d2.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_sand, se.dat$pred_sand,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted sand (%)", xlab= "observed sand (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_sand, se.dat$pred_sand,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# silt
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_silt_d2.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_silt, se.dat$pred_silt,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted silt (%)", xlab= "observed silt (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_silt, se.dat$pred_silt,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


# Depth 3
dat<- readRDS(file = paste0(out.root, "slga_v1_data_d3_.rds"))
names(dat)
se.dat<- dat[,c(1:13,22,40,58)]

#columns for compositions
se.dat$obs_clay<- NA
se.dat$obs_sand<- NA
se.dat$obs_silt<- NA
names(se.dat)[14:16]<- c("pred_clay", "pred_sand", "pred_silt")
names(se.dat)
se.dat[,17:19]<- as.data.frame(ilrInv(se.dat[,c(6:7)]))*100
se.dat<- se.dat[complete.cases(se.dat),] 

# composition clay
d3.clay.out<- goof(observed = se.dat$obs_clay,predicted = se.dat$pred_clay, plot.it = T)
d3.clay.out

# composition sand
d3.sand.out<- goof(observed = se.dat$obs_sand,predicted = se.dat$pred_sand, plot.it = T)
d3.sand.out

# composition silt
d3.silt.out<- goof(observed = se.dat$obs_silt,predicted = se.dat$pred_silt, plot.it = T)
d3.silt.out

## fancy plotting
# clay
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_clay_d3.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_clay, se.dat$pred_clay,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted clay (%)", xlab= "observed clay (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_clay, se.dat$pred_clay,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# sand
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_sand_d3.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_sand, se.dat$pred_sand,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted sand (%)", xlab= "observed sand (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_sand, se.dat$pred_sand,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# silt
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_silt_d3.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_silt, se.dat$pred_silt,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted silt (%)", xlab= "observed silt (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_silt, se.dat$pred_silt,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


# Depth 4
dat<- readRDS(file = paste0(out.root, "slga_v1_data_d4_.rds"))
names(dat)
se.dat<- dat[,c(1:13,25,43,61)]

#columns for compositions
se.dat$obs_clay<- NA
se.dat$obs_sand<- NA
se.dat$obs_silt<- NA
names(se.dat)[14:16]<- c("pred_clay", "pred_sand", "pred_silt")
names(se.dat)
se.dat[,17:19]<- as.data.frame(ilrInv(se.dat[,c(6:7)]))*100
se.dat<- se.dat[complete.cases(se.dat),] 

# composition clay
d4.clay.out<- goof(observed = se.dat$obs_clay,predicted = se.dat$pred_clay, plot.it = T)
d4.clay.out

# composition sand
d4.sand.out<- goof(observed = se.dat$obs_sand,predicted = se.dat$pred_sand, plot.it = T)
d4.sand.out

# composition silt
d4.silt.out<- goof(observed = se.dat$obs_silt,predicted = se.dat$pred_silt, plot.it = T)
d4.silt.out

## fancy plotting
# clay
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_clay_d4.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_clay, se.dat$pred_clay,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted clay (%)", xlab= "observed clay (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_clay, se.dat$pred_clay,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# sand
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_sand_d4.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_sand, se.dat$pred_sand,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted sand (%)", xlab= "observed sand (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_sand, se.dat$pred_sand,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# silt
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_silt_d4.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_silt, se.dat$pred_silt,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted silt (%)", xlab= "observed silt (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_silt, se.dat$pred_silt,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



# Depth 5
dat<- readRDS(file = paste0(out.root, "slga_v1_data_d5_.rds"))
names(dat)
se.dat<- dat[,c(1:13,28,46,64)]

#columns for compositions
se.dat$obs_clay<- NA
se.dat$obs_sand<- NA
se.dat$obs_silt<- NA
names(se.dat)[14:16]<- c("pred_clay", "pred_sand", "pred_silt")
names(se.dat)
se.dat[,17:19]<- as.data.frame(ilrInv(se.dat[,c(6:7)]))*100
se.dat<- se.dat[complete.cases(se.dat),] 

# composition clay
d5.clay.out<- goof(observed = se.dat$obs_clay,predicted = se.dat$pred_clay, plot.it = T)
d5.clay.out

# composition sand
d5.sand.out<- goof(observed = se.dat$obs_sand,predicted = se.dat$pred_sand, plot.it = T)
d5.sand.out

# composition silt
d5.silt.out<- goof(observed = se.dat$obs_silt,predicted = se.dat$pred_silt, plot.it = T)
d5.silt.out

## fancy plotting
# clay
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_clay_d5.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_clay, se.dat$pred_clay,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted clay (%)", xlab= "observed clay (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_clay, se.dat$pred_clay,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# sand
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_sand_d5.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_sand, se.dat$pred_sand,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted sand (%)", xlab= "observed sand (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_sand, se.dat$pred_sand,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# silt
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_silt_d5.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_silt, se.dat$pred_silt,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted silt (%)", xlab= "observed silt (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_silt, se.dat$pred_silt,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



# Depth 6
dat<- readRDS(file = paste0(out.root, "slga_v1_data_d6_.rds"))
names(dat)
se.dat<- dat[,c(1:13,31,49,67)]

#columns for compositions
se.dat$obs_clay<- NA
se.dat$obs_sand<- NA
se.dat$obs_silt<- NA
names(se.dat)[14:16]<- c("pred_clay", "pred_sand", "pred_silt")
names(se.dat)
se.dat[,17:19]<- as.data.frame(ilrInv(se.dat[,c(6:7)]))*100
se.dat<- se.dat[complete.cases(se.dat),] 

# composition clay
d6.clay.out<- goof(observed = se.dat$obs_clay,predicted = se.dat$pred_clay, plot.it = T)
d6.clay.out

# composition sand
d6.sand.out<- goof(observed = se.dat$obs_sand,predicted = se.dat$pred_sand, plot.it = T)
d6.sand.out

# composition silt
d6.silt.out<- goof(observed = se.dat$obs_silt,predicted = se.dat$pred_silt, plot.it = T)
d6.silt.out

## fancy plotting
# clay
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_clay_d6.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_clay, se.dat$pred_clay,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted clay (%)", xlab= "observed clay (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_clay, se.dat$pred_clay,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# sand
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_sand_d6.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_sand, se.dat$pred_sand,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted sand (%)", xlab= "observed sand (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_sand, se.dat$pred_sand,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# silt
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"BT/slga_v1_val_silt_d6.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$obs_silt, se.dat$pred_silt,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted silt (%)", xlab= "observed silt (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$obs_silt, se.dat$pred_silt,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


# compile outs
clay.all.outs<- rbind(d1.clay.out,d2.clay.out,d3.clay.out,d4.clay.out,d6.clay.out,d6.clay.out)
write.csv(clay.all.outs, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/SLGA_V1_ext_val/clay_slga_v1_val_summary_BT.csv",row.names = F)

# sand
sand.all.outs<- rbind(d1.sand.out,d2.sand.out,d3.sand.out,d4.sand.out,d5.sand.out,d6.sand.out)
write.csv(sand.all.outs, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/SLGA_V1_ext_val/sand_slga_v1_val_summary_BT.csv",row.names = F)

# silt
silt.all.outs<- rbind(d1.silt.out,d2.silt.out,d3.silt.out,d4.silt.out,d5.silt.out,d6.silt.out)
write.csv(silt.all.outs, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/SLGA_V1_ext_val/silt_slga_v1_val_summary_BT.csv",row.names = F)


