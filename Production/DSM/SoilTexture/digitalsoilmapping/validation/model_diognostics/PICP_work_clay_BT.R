## PICP work for soil texture analysis
## TERN landscapes
# back transformed
root<- "Z:/projects/ternlandscapes_2019/soiltexture/outs/dsm_externalvalidation/BT/data/"
fig.root<- "Z:/projects/ternlandscapes_2019/soiltexture/outs/dsm_externalvalidation/"






### CLAY


## D1
# prediction data
pred.data<- readRDS(file = paste0(root,"clay_d1_pred_data_BT.rds"))
dim(pred.data)

# observation data
observation.data<- readRDS(file = paste0(root,"d1_observed_data_BT.rds"))

# diognostics
diog.data<- readRDS(file = paste0(root,"clay_d1_diogs_BT.rds"))
# average mse 
avgMSE<- mean(diog.data$MSE)

# row means
val.mean<- rowMeans(pred.data)

# variance of the data
rowVar<- c()
for (i in 1:nrow(pred.data)){
  rowVar[i]<- var(pred.data[i,])}

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
  bMat[,i] <- as.numeric(observation.data[,1] <= uMat[,i] & observation.data[,1] >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_clay_d1_BT.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



## D2
# prediction data
pred.data<- readRDS(file = paste0(root,"clay_d2_pred_data_BT.rds"))
dim(pred.data)

# observation data
observation.data<- readRDS(file = paste0(root,"d2_observed_data_BT.rds"))

# diognostics
diog.data<- readRDS(file = paste0(root,"clay_d2_diogs_BT.rds"))
# average mse 
avgMSE<- mean(diog.data$MSE)

# row means
val.mean<- rowMeans(pred.data)

# variance of the data
rowVar<- c()
for (i in 1:nrow(pred.data)){
  rowVar[i]<- var(pred.data[i,])}

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
  bMat[,i] <- as.numeric(observation.data[,1] <= uMat[,i] & observation.data[,1] >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_clay_d2_BT.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



## D3
# prediction data
pred.data<- readRDS(file = paste0(root,"clay_d3_pred_data_BT.rds"))
dim(pred.data)

# observation data
observation.data<- readRDS(file = paste0(root,"d3_observed_data_BT.rds"))

# diognostics
diog.data<- readRDS(file = paste0(root,"clay_d3_diogs_BT.rds"))
# average mse 
avgMSE<- mean(diog.data$MSE)

# row means
val.mean<- rowMeans(pred.data)

# variance of the data
rowVar<- c()
for (i in 1:nrow(pred.data)){
  rowVar[i]<- var(pred.data[i,])}

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
  bMat[,i] <- as.numeric(observation.data[,1] <= uMat[,i] & observation.data[,1] >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_clay_d3_BT.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


## D4
# prediction data
pred.data<- readRDS(file = paste0(root,"clay_d4_pred_data_BT.rds"))
dim(pred.data)

# observation data
observation.data<- readRDS(file = paste0(root,"d4_observed_data_BT.rds"))

# diognostics
diog.data<- readRDS(file = paste0(root,"clay_d4_diogs_BT.rds"))
# average mse 
avgMSE<- mean(diog.data$MSE)

# row means
val.mean<- rowMeans(pred.data)

# variance of the data
rowVar<- c()
for (i in 1:nrow(pred.data)){
  rowVar[i]<- var(pred.data[i,])}

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
  bMat[,i] <- as.numeric(observation.data[,1] <= uMat[,i] & observation.data[,1] >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_clay_d4_BT.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()



## D5
# prediction data
pred.data<- readRDS(file = paste0(root,"clay_d5_pred_data_BT.rds"))
dim(pred.data)

# observation data
observation.data<- readRDS(file = paste0(root,"d5_observed_data_BT.rds"))

# diognostics
diog.data<- readRDS(file = paste0(root,"clay_d5_diogs_BT.rds"))
# average mse 
avgMSE<- mean(diog.data$MSE)

# row means
val.mean<- rowMeans(pred.data)

# variance of the data
rowVar<- c()
for (i in 1:nrow(pred.data)){
  rowVar[i]<- var(pred.data[i,])}

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
  bMat[,i] <- as.numeric(observation.data[,1] <= uMat[,i] & observation.data[,1] >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_clay_d5_BT.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


## D6
# prediction data
pred.data<- readRDS(file = paste0(root,"clay_d6_pred_data_BT.rds"))
dim(pred.data)

# observation data
observation.data<- readRDS(file = paste0(root,"d6_observed_data_BT.rds"))

# diognostics
diog.data<- readRDS(file = paste0(root,"clay_d6_diogs_BT.rds"))
# average mse 
avgMSE<- mean(diog.data$MSE)

# row means
val.mean<- rowMeans(pred.data)

# variance of the data
rowVar<- c()
for (i in 1:nrow(pred.data)){
  rowVar[i]<- var(pred.data[i,])}

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
  bMat[,i] <- as.numeric(observation.data[,1] <= uMat[,i] & observation.data[,1] >= lMat[,i])} 

colSums(bMat)/ nrow(bMat)

#make plot
cs<- c(99,97.5,95,90,80,60,40,20,10,5) # confidence level
plot(cs,((colSums(bMat)/ nrow(bMat))*100))
abline(0, 1)

### fancy plot
xlimits= c(0,100)
ylimits= c(0,100) 
tiff(file=paste0(fig.root,"picp_clay_d6_BT.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(cs,((colSums(bMat)/ nrow(bMat))*100),xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="PICP (%)", xlab= "Confidence level (%)",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from =0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (cs,((colSums(bMat)/ nrow(bMat))*100),,pch=1, col="black", cex=1)
abline(0, 1, lwd=1.5, col="red")
dev.off()


