### Checking of version 1 predictions against observations
library(compositions);library(ithir)

# files where directories are:
out.root<- "Z:/projects/ternlandscapes_2019/soiltexture/data/SLGA_V1_ext_val/"
list.files(out.root)

fig.root<- "Z:/projects/ternlandscapes_2019/soiltexture/outs/dsm_externalvalidation/"


# Depth 1
dat<- readRDS(file = paste0(out.root, "slga_v1_data_d1_.rds"))
names(dat)
se.dat<- dat[,c(1:13,16,34,52)]

#columns for compositions
se.dat$comp1_clay<- NA
se.dat$comp2_sand<- NA
names(se.dat)

for (i in 1:nrow(se.dat)){
  it<- ilr(se.dat[i,c(14:16)])
  itc<- c(it)
  itc
  ilrInv(itc)
  se.dat[i,17:18]<- itc
  print(i)}

# composition clay
d1.clay.out<- goof(observed = se.dat$clay_0.5.cm,predicted = se.dat$comp1_clay, plot.it = T)
d1.clay.out

# composition sand
d1.sand.out<- goof(observed = se.dat$sand_0.5.cm,predicted = se.dat$comp2_sand, plot.it = T)
d1.sand.out

## fancy plotting
# clay
xlimits= c(-2,5)
ylimits= c(-2,5) 
tiff(file=paste0(fig.root,"slga_v1_val_clay_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$clay_0.5.cm, se.dat$comp1_clay,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted irl comp clay", xlab= "observed irl comp clay",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = -2,to = 5,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = -2,to = 5,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$clay_0.5.cm, se.dat$comp1_clay,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# sand
xlimits= c(-5,2)
ylimits= c(-5,2) 
tiff(file=paste0(fig.root,"slga_v1_val_sand_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$sand_0.5.cm, se.dat$comp2_sand,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted irl comp sand", xlab= "observed irl comp sand",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = -5,to = 2,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = -5,to = 2,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$sand_0.5.cm, se.dat$comp2_sand,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


# Depth 2
dat<- readRDS(file = paste0(out.root, "slga_v1_data_d2_.rds"))
names(dat)
se.dat<- dat[,c(1:13,19,37,55)]

#columns for compositions
se.dat$comp1_clay<- NA
se.dat$comp2_sand<- NA
names(se.dat)

for (i in 1:nrow(se.dat)){
  it<- ilr(se.dat[i,c(14:16)])
  itc<- c(it)
  itc
  ilrInv(itc)
  se.dat[i,17:18]<- itc
  print(i)}

# composition clay
d2.clay.out<- goof(observed = se.dat$clay_5.15.cm,predicted = se.dat$comp1_clay, plot.it = T)
d2.clay.out

# composition sand
d2.sand.out<- goof(observed = se.dat$sand_5.15.cm,predicted = se.dat$comp2_sand, plot.it = T)
d2.sand.out

## fancy plotting
# clay
xlimits= c(-2,5)
ylimits= c(-2,5) 
tiff(file=paste0(fig.root,"slga_v1_val_clay_d2.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$clay_5.15.cm, se.dat$comp1_clay,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted irl comp clay", xlab= "observed irl comp clay",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = -2,to = 5,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = -2,to = 5,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$clay_5.15.cm, se.dat$comp1_clay,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# sand
xlimits= c(-5,2)
ylimits= c(-5,2) 
tiff(file=paste0(fig.root,"slga_v1_val_sand_d2.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$sand_5.15.cm, se.dat$comp2_sand,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted irl comp sand", xlab= "observed irl comp sand",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = -5,to = 2,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = -5,to = 2,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$sand_5.15.cm, se.dat$comp2_sand,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


# Depth 3
dat<- readRDS(file = paste0(out.root, "slga_v1_data_d3_.rds"))
names(dat)
se.dat<- dat[,c(1:13,22,40,58)]

#columns for compositions
se.dat$comp1_clay<- NA
se.dat$comp2_sand<- NA
names(se.dat)

for (i in 1:nrow(se.dat)){
  it<- ilr(se.dat[i,c(14:16)])
  itc<- c(it)
  itc
  ilrInv(itc)
  se.dat[i,17:18]<- itc
  print(i)}

# composition clay
d3.clay.out<- goof(observed = se.dat$clay_15.30.cm,predicted = se.dat$comp1_clay, plot.it = T)
d3.clay.out

# composition sand
d3.sand.out<- goof(observed = se.dat$sand_15.30.cm,predicted = se.dat$comp2_sand, plot.it = T)
d3.sand.out

## fancy plotting
# clay
xlimits= c(-2,5)
ylimits= c(-2,5) 
tiff(file=paste0(fig.root,"slga_v1_val_clay_d3.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$clay_15.30.cm, se.dat$comp1_clay,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted irl comp clay", xlab= "observed irl comp clay",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = -2,to = 5,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = -2,to = 5,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$clay_15.30.cm, se.dat$comp1_clay,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# sand
xlimits= c(-5,2)
ylimits= c(-5,2) 
tiff(file=paste0(fig.root,"slga_v1_val_sand_d3.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$sand_15.30.cm, se.dat$comp2_sand,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted irl comp sand", xlab= "observed irl comp sand",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = -5,to = 2,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = -5,to = 2,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$sand_15.30.cm, se.dat$comp2_sand,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


# Depth 4
dat<- readRDS(file = paste0(out.root, "slga_v1_data_d4_.rds"))
names(dat)
se.dat<- dat[,c(1:13,25,43,61)]

#columns for compositions
se.dat$comp1_clay<- NA
se.dat$comp2_sand<- NA
names(se.dat)

for (i in 1:nrow(se.dat)){
  it<- ilr(se.dat[i,c(14:16)])
  itc<- c(it)
  itc
  ilrInv(itc)
  se.dat[i,17:18]<- itc
  print(i)}

# composition clay
d4.clay.out<- goof(observed = se.dat$clay_30.60.cm,predicted = se.dat$comp1_clay, plot.it = T)
d4.clay.out

# composition sand
d4.sand.out<- goof(observed = se.dat$sand_30.60.cm,predicted = se.dat$comp2_sand, plot.it = T)
d4.sand.out

## fancy plotting
# clay
xlimits= c(-2,5)
ylimits= c(-2,5) 
tiff(file=paste0(fig.root,"slga_v1_val_clay_d4.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$clay_30.60.cm, se.dat$comp1_clay,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted irl comp clay", xlab= "observed irl comp clay",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = -2,to = 5,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = -2,to = 5,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$clay_30.60.cm, se.dat$comp1_clay,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# sand
xlimits= c(-5,2)
ylimits= c(-5,2) 
tiff(file=paste0(fig.root,"slga_v1_val_sand_d4.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$sand_30.60.cm, se.dat$comp2_sand,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted irl comp sand", xlab= "observed irl comp sand",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = -5,to = 2,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = -5,to = 2,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$sand_30.60.cm, se.dat$comp2_sand,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



# Depth 5
dat<- readRDS(file = paste0(out.root, "slga_v1_data_d5_.rds"))
names(dat)
se.dat<- dat[,c(1:13,28,46,64)]

#columns for compositions
se.dat$comp1_clay<- NA
se.dat$comp2_sand<- NA
names(se.dat)

for (i in 1:nrow(se.dat)){
  it<- ilr(se.dat[i,c(14:16)])
  itc<- c(it)
  itc
  ilrInv(itc)
  se.dat[i,17:18]<- itc
  print(i)}

# composition clay
d5.clay.out<- goof(observed = se.dat$clay_60.100.cm,predicted = se.dat$comp1_clay, plot.it = T)
d5.clay.out

# composition sand
d5.sand.out<- goof(observed = se.dat$sand_60.100.cm,predicted = se.dat$comp2_sand, plot.it = T)
d5.sand.out

## fancy plotting
# clay
xlimits= c(-2,5)
ylimits= c(-2,5) 
tiff(file=paste0(fig.root,"slga_v1_val_clay_d5.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$clay_60.100.cm, se.dat$comp1_clay,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted irl comp clay", xlab= "observed irl comp clay",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = -2,to = 5,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = -2,to = 5,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$clay_60.100.cm, se.dat$comp1_clay,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# sand
xlimits= c(-5,2)
ylimits= c(-5,2) 
tiff(file=paste0(fig.root,"slga_v1_val_sand_d5.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$sand_60.100.cm, se.dat$comp2_sand,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted irl comp sand", xlab= "observed irl comp sand",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = -5,to = 2,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = -5,to = 2,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$sand_60.100.cm, se.dat$comp2_sand,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



# Depth 6
dat<- readRDS(file = paste0(out.root, "slga_v1_data_d6_.rds"))
names(dat)
se.dat<- dat[,c(1:13,31,49,67)]

#columns for compositions
se.dat$comp1_clay<- NA
se.dat$comp2_sand<- NA
names(se.dat)

for (i in 1:nrow(se.dat)){
  it<- ilr(se.dat[i,c(14:16)])
  itc<- c(it)
  itc
  ilrInv(itc)
  se.dat[i,17:18]<- itc
  print(i)}

# composition clay
d6.clay.out<- goof(observed = se.dat$clay_100.200.cm,predicted = se.dat$comp1_clay, plot.it = T)
d6.clay.out

# composition sand
d6.sand.out<- goof(observed = se.dat$sand_100.200.cm,predicted = se.dat$comp2_sand, plot.it = T)
d6.sand.out

## fancy plotting
# clay
xlimits= c(-2,5)
ylimits= c(-2,5) 
tiff(file=paste0(fig.root,"slga_v1_val_clay_d6.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$clay_100.200.cm, se.dat$comp1_clay,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted irl comp clay", xlab= "observed irl comp clay",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = -2,to = 5,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = -2,to = 5,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$clay_100.200.cm, se.dat$comp1_clay,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

# sand
xlimits= c(-5,2)
ylimits= c(-5,2) 
tiff(file=paste0(fig.root,"slga_v1_val_sand_d6.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(se.dat$sand_100.200.cm, se.dat$comp2_sand,xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="predicted irl comp sand", xlab= "observed irl comp sand",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = -5,to = 2,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = -5,to = 2,by = 1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (se.dat$sand_100.200.cm, se.dat$comp2_sand,pch=1, col="black", cex=0.1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


# compile outs
clay.all.outs<- rbind(d1.clay.out,d2.clay.out,d3.clay.out,d4.clay.out,d6.clay.out,d6.clay.out)
write.csv(clay.all.outs, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/SLGA_V1_ext_val/clay_slga_v1_val_summary.csv",row.names = F)

# sand
sand.all.outs<- rbind(d1.sand.out,d2.sand.out,d3.sand.out,d4.sand.out,d5.sand.out,d6.sand.out)
write.csv(sand.all.outs, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/SLGA_V1_ext_val/sand_slga_v1_val_summary.csv",row.names = F)


