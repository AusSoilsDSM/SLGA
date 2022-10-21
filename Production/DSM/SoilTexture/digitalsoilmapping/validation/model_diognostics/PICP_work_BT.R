## PICP work for soil texture analysis
## TERN landscapes
root<- "Z:/projects/ternlandscapes_2019/soiltexture/models/"
fig.root<- "Z:/projects/ternlandscapes_2019/soiltexture/outs/dsm_externalvalidation/"

# average mse 
avgMSE<- readRDS("Z:/projects/ternlandscapes_2019/soiltexture/data/soiltexture_models_avgMSE.rds")


### CLAY
## D1
vari<- "clay"
depth<- "d1"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

val.mean<- rowMeans(dat[,7:56])

# variance of the data
rowVar<- c()
for (i in 1:nrow(dat)){
  rowVar[i]<- var(c(as.matrix(dat[i,7:56])))}

# total standard deviation
val.sd<- sqrt(rowVar + as.numeric(as.matrix(avgMSE$avgmse[1])))


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(dat$target <= uMat[,i] & dat$target >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_clay_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



## D2
vari<- "clay"
depth<- "d2"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

val.mean<- rowMeans(dat[,7:56])

# variance of the data
rowVar<- c()
for (i in 1:nrow(dat)){
  rowVar[i]<- var(c(as.matrix(dat[i,7:56])))}

# total standard deviation
val.sd<- sqrt(rowVar + as.numeric(as.matrix(avgMSE$avgmse[2])))


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(dat$target <= uMat[,i] & dat$target >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_clay_d2.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



## D3
vari<- "clay"
depth<- "d3"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

val.mean<- rowMeans(dat[,7:56])

# variance of the data
rowVar<- c()
for (i in 1:nrow(dat)){
  rowVar[i]<- var(c(as.matrix(dat[i,7:56])))}

# total standard deviation
val.sd<- sqrt(rowVar + as.numeric(as.matrix(avgMSE$avgmse[3])))


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(dat$target <= uMat[,i] & dat$target >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_clay_d3.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


## D4
vari<- "clay"
depth<- "d4"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

val.mean<- rowMeans(dat[,7:56])

# variance of the data
rowVar<- c()
for (i in 1:nrow(dat)){
  rowVar[i]<- var(c(as.matrix(dat[i,7:56])))}

# total standard deviation
val.sd<- sqrt(rowVar + as.numeric(as.matrix(avgMSE$avgmse[4])))


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(dat$target <= uMat[,i] & dat$target >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_clay_d4.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



## D5
vari<- "clay"
depth<- "d5"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

val.mean<- rowMeans(dat[,7:56])

# variance of the data
rowVar<- c()
for (i in 1:nrow(dat)){
  rowVar[i]<- var(c(as.matrix(dat[i,7:56])))}

# total standard deviation
val.sd<- sqrt(rowVar + as.numeric(as.matrix(avgMSE$avgmse[5])))


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(dat$target <= uMat[,i] & dat$target >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_clay_d5.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


## D6
vari<- "clay"
depth<- "d6"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

val.mean<- rowMeans(dat[,7:56])

# variance of the data
rowVar<- c()
for (i in 1:nrow(dat)){
  rowVar[i]<- var(c(as.matrix(dat[i,7:56])))}

# total standard deviation
val.sd<- sqrt(rowVar + as.numeric(as.matrix(avgMSE$avgmse[6])))


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(dat$target <= uMat[,i] & dat$target >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_clay_d6.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


## D1
vari<- "sand"
depth<- "d1"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_sand",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

val.mean<- rowMeans(dat[,7:56])

# variance of the data
rowVar<- c()
for (i in 1:nrow(dat)){
  rowVar[i]<- var(c(as.matrix(dat[i,7:56])))}

# total standard deviation
val.sd<- sqrt(rowVar + as.numeric(as.matrix(avgMSE$avgmse[7])))


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(dat$target <= uMat[,i] & dat$target >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_sand_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()

## D2
vari<- "sand"
depth<- "d2"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_sand",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

val.mean<- rowMeans(dat[,7:56])

# variance of the data
rowVar<- c()
for (i in 1:nrow(dat)){
  rowVar[i]<- var(c(as.matrix(dat[i,7:56])))}

# total standard deviation
val.sd<- sqrt(rowVar + as.numeric(as.matrix(avgMSE$avgmse[8])))


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(dat$target <= uMat[,i] & dat$target >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_sand_d2.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


## D3
vari<- "sand"
depth<- "d3"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_sand",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

val.mean<- rowMeans(dat[,7:56])

# variance of the data
rowVar<- c()
for (i in 1:nrow(dat)){
  rowVar[i]<- var(c(as.matrix(dat[i,7:56])))}

# total standard deviation
val.sd<- sqrt(rowVar + as.numeric(as.matrix(avgMSE$avgmse[9])))


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(dat$target <= uMat[,i] & dat$target >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_sand_d3.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


## D4
vari<- "sand"
depth<- "d4"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_sand",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

val.mean<- rowMeans(dat[,7:56])

# variance of the data
rowVar<- c()
for (i in 1:nrow(dat)){
  rowVar[i]<- var(c(as.matrix(dat[i,7:56])))}

# total standard deviation
val.sd<- sqrt(rowVar + as.numeric(as.matrix(avgMSE$avgmse[10])))


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(dat$target <= uMat[,i] & dat$target >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_sand_d4.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


## D5
vari<- "sand"
depth<- "d5"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_sand",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

val.mean<- rowMeans(dat[,7:56])

# variance of the data
rowVar<- c()
for (i in 1:nrow(dat)){
  rowVar[i]<- var(c(as.matrix(dat[i,7:56])))}

# total standard deviation
val.sd<- sqrt(rowVar + as.numeric(as.matrix(avgMSE$avgmse[11])))


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(dat$target <= uMat[,i] & dat$target >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_sand_d5.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


## D6
vari<- "sand"
depth<- "d6"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_sand",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

val.mean<- rowMeans(dat[,7:56])

# variance of the data
rowVar<- c()
for (i in 1:nrow(dat)){
  rowVar[i]<- var(c(as.matrix(dat[i,7:56])))}

# total standard deviation
val.sd<- sqrt(rowVar + as.numeric(as.matrix(avgMSE$avgmse[12])))


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(dat), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(dat$target <= uMat[,i] & dat$target >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_sand_d6.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()
