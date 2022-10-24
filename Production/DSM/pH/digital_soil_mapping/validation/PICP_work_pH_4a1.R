### TERN LANDSCAPES 
# Soil pH model model fitting
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 12.5.21
# modified: 13.5.21

# CODE PURPOSE
# PICP work on external data
library(ithir);library(MASS)

model.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/models/"
plot.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/outs/external_validation/"

# average model error summaries
avg.mse.dat<- read.csv(file = paste0(model.out, "ranger_VAL_diogs_pH_summary_alldepths.csv"))
avg.mse.dat

# External validation
# depth 1
root.in<- paste0(model.out,"d1/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
for(i in 2:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- cbind(temp.file,in.dat$prediction)
}

names(temp.file)
# prediction data
pred.data<- temp.file[,12:61]
dim(pred.data)

# observation data
observation.data<- temp.file$X0.5.cm

# average mse 
avgMSE<- avg.mse.dat$MSE[1]
avgMSE

# row means
val.mean<- rowMeans(pred.data)

# variance of the data
rowVar<- c()
for (i in 1:nrow(pred.data)){
  rowVar[i]<- var(c(as.matrix(pred.data[i,])))}

# total standard deviation
val.sd<- sqrt(rowVar + avgMSE )


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(observation.data <= uMat[,i] & observation.data >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(plot.out,"picp_ph_4a1_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



# depth 2
root.in<- paste0(model.out,"d2/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
for(i in 2:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- cbind(temp.file,in.dat$prediction)
}

names(temp.file)
# prediction data
pred.data<- temp.file[,12:61]
dim(pred.data)

# observation data
observation.data<- temp.file$X5.15.cm

# average mse 
avgMSE<- avg.mse.dat$MSE[2]
avgMSE

# row means
val.mean<- rowMeans(pred.data)

# variance of the data
rowVar<- c()
for (i in 1:nrow(pred.data)){
  rowVar[i]<- var(c(as.matrix(pred.data[i,])))}

# total standard deviation
val.sd<- sqrt(rowVar + avgMSE )


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(observation.data <= uMat[,i] & observation.data >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(plot.out,"picp_ph_4a1_d2.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


# depth 3
root.in<- paste0(model.out,"d3/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
for(i in 2:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- cbind(temp.file,in.dat$prediction)
}

names(temp.file)
# prediction data
pred.data<- temp.file[,12:61]
dim(pred.data)

# observation data
observation.data<- temp.file$X15.30.cm

# average mse 
avgMSE<- avg.mse.dat$MSE[3]
avgMSE

# row means
val.mean<- rowMeans(pred.data)

# variance of the data
rowVar<- c()
for (i in 1:nrow(pred.data)){
  rowVar[i]<- var(c(as.matrix(pred.data[i,])))}

# total standard deviation
val.sd<- sqrt(rowVar + avgMSE )


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(observation.data <= uMat[,i] & observation.data >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(plot.out,"picp_ph_4a1_d3.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



# depth 4
root.in<- paste0(model.out,"d4/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
for(i in 2:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- cbind(temp.file,in.dat$prediction)
}

names(temp.file)
# prediction data
pred.data<- temp.file[,12:61]
dim(pred.data)

# observation data
observation.data<- temp.file$X30.60.cm

# average mse 
avgMSE<- avg.mse.dat$MSE[4]
avgMSE

# row means
val.mean<- rowMeans(pred.data)

# variance of the data
rowVar<- c()
for (i in 1:nrow(pred.data)){
  rowVar[i]<- var(c(as.matrix(pred.data[i,])))}

# total standard deviation
val.sd<- sqrt(rowVar + avgMSE )


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(observation.data <= uMat[,i] & observation.data >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(plot.out,"picp_ph_4a1_d4.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


# depth 5
root.in<- paste0(model.out,"d5/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
for(i in 2:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- cbind(temp.file,in.dat$prediction)
}

names(temp.file)
# prediction data
pred.data<- temp.file[,12:61]
dim(pred.data)

# observation data
observation.data<- temp.file$X60.100.cm

# average mse 
avgMSE<- avg.mse.dat$MSE[5]
avgMSE

# row means
val.mean<- rowMeans(pred.data)

# variance of the data
rowVar<- c()
for (i in 1:nrow(pred.data)){
  rowVar[i]<- var(c(as.matrix(pred.data[i,])))}

# total standard deviation
val.sd<- sqrt(rowVar + avgMSE )


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(observation.data <= uMat[,i] & observation.data >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(plot.out,"picp_ph_4a1_d5.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


# depth 6
root.in<- paste0(model.out,"d6/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
for(i in 2:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- cbind(temp.file,in.dat$prediction)
}

names(temp.file)
# prediction data
pred.data<- temp.file[,12:61]
dim(pred.data)

# observation data
observation.data<- temp.file$X100.200.cm

# average mse 
avgMSE<- avg.mse.dat$MSE[6]
avgMSE

# row means
val.mean<- rowMeans(pred.data)

# variance of the data
rowVar<- c()
for (i in 1:nrow(pred.data)){
  rowVar[i]<- var(c(as.matrix(pred.data[i,])))}

# total standard deviation
val.sd<- sqrt(rowVar + avgMSE )


# Percentiles of normal distribution
qp<- qnorm(c(0.995,0.9875,0.975,0.95,0.9,0.8,0.7,0.6,0.55,0.525)) 

#zfactor multiplication 
vMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  vMat[,i] <- val.sd * qp[i]}


#upper prediction limit
uMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  uMat[,i] <- val.mean + vMat[,i]}

#lower prediction limit
lMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:length(qp)){
  lMat[,i] <- val.mean - vMat[,i]}

## PICP
bMat<- matrix(NA,nrow= nrow(pred.data), ncol= length(qp))
for (i in 1:ncol(bMat)){
  bMat[,i] <- as.numeric(observation.data <= uMat[,i] & observation.data >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(plot.out,"picp_ph_4a1_d6.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()
