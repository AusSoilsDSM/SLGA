## hex bin plotting of soil thickness estimates
### XY-plotting
# Model 2 (ranger regression)
root<- "Z:/projects/ternlandscapes_2019/soiltexture/models/"
root.data<- "Z:/projects/ternlandscapes_2019/soiltexture/outs/dsm_externalvalidation/BT/data/"
library(hexbin);library(reshape2);library(RColorBrewer);library(ggplot2);library(compositions)


# external validation files

## D1
### CLAY
vari<- "clay"
depth<- "d1"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# 
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
clay.dat<- read.table(files[1],header = T,sep = ",")
names(clay.dat)
clay.dat<- clay.dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  clay.dat<- cbind(clay.dat, datz$prediction)
}

### SAND
vari<- "sand"
depth<- "d1"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# 
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_sand",full.names = T)
files
sand.dat<- read.table(files[1],header = T,sep = ",")
names(sand.dat)
sand.dat<- sand.dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  sand.dat<- cbind(sand.dat, datz$prediction)
}

## observation data
observed.dat<- matrix(NA, nrow=nrow(clay.dat),ncol=3)
comp.obs<- cbind(clay.dat$target,sand.dat$target)
observed.dat<- as.data.frame(ilrInv(comp.obs))*100

# prediction frames
pred.clay<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
pred.sand<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
pred.silt<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
#diognostics
diog.clay<- matrix(NA, nrow=50,ncol=5 )
diog.sand<- matrix(NA, nrow=50,ncol=5 )
diog.silt<- matrix(NA, nrow=50,ncol=5 )

names(clay.dat)
cnt<- 1
for (j in 7:56){
  subs<- cbind(clay.dat[,j],sand.dat[,j])
  prediction.dat<- as.data.frame(ilrInv(subs))*100
  # diogs
  diog.clay[cnt,]<- as.matrix(goof(observed =observed.dat[,1],predicted = prediction.dat[,1]))
  diog.sand[cnt,]<- as.matrix(goof(observed =observed.dat[,2],predicted = prediction.dat[,2]))
  diog.silt[cnt,]<- as.matrix(goof(observed =observed.dat[,3],predicted = prediction.dat[,3]))
  # save outputs for PICP analysis
  pred.clay[,cnt]<- prediction.dat[,1]
  pred.sand[,cnt]<- prediction.dat[,2]
  pred.silt[,cnt]<- prediction.dat[,3]
  cnt<- cnt+1}
diog.clay<- as.data.frame(diog.clay)
diog.sand<- as.data.frame(diog.sand)
diog.silt<- as.data.frame(diog.silt)
names(diog.clay)<- c("R2", "CCC", "MSE", "RMSE","bias")
names(diog.sand)<- c("R2", "CCC", "MSE", "RMSE","bias")
names(diog.silt)<- c("R2", "CCC", "MSE", "RMSE","bias")

# save diognostics
saveRDS(object = diog.clay, file = paste0(root.data,"clay_d1_diogs_BT.rds"))
saveRDS(object = diog.sand, file = paste0(root.data,"sand_d1_diogs_BT.rds"))
saveRDS(object = diog.silt, file = paste0(root.data,"silt_d1_diogs_BT.rds"))

d1.clay.out<- colMeans(diog.clay)
d1.sand.out<- colMeans(diog.sand)
d1.silt.out<- colMeans(diog.silt)

# save data
saveRDS(object = pred.clay, file = paste0(root.data,"clay_d1_pred_data_BT.rds"))
saveRDS(object = pred.sand, file = paste0(root.data,"sand_d1_pred_data_BT.rds"))
saveRDS(object = pred.silt, file = paste0(root.data,"silt_d1_pred_data_BT.rds"))
saveRDS(object = observed.dat, file = paste0(root.data,"d1_observed_data_BT.rds"))



## D2
### CLAY
vari<- "clay"
depth<- "d2"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# 
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
clay.dat<- read.table(files[1],header = T,sep = ",")
names(clay.dat)
clay.dat<- clay.dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  clay.dat<- cbind(clay.dat, datz$prediction)
}

### SAND
vari<- "sand"
depth<- "d2"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# 
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_sand",full.names = T)
files
sand.dat<- read.table(files[1],header = T,sep = ",")
names(sand.dat)
sand.dat<- sand.dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  sand.dat<- cbind(sand.dat, datz$prediction)
}

## observation data
observed.dat<- matrix(NA, nrow=nrow(clay.dat),ncol=3)
comp.obs<- cbind(clay.dat$target,sand.dat$target)
observed.dat<- as.data.frame(ilrInv(comp.obs))*100

# prediction frames
pred.clay<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
pred.sand<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
pred.silt<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
#diognostics
diog.clay<- matrix(NA, nrow=50,ncol=5 )
diog.sand<- matrix(NA, nrow=50,ncol=5 )
diog.silt<- matrix(NA, nrow=50,ncol=5 )

names(clay.dat)
cnt<- 1
for (j in 7:56){
  subs<- cbind(clay.dat[,j],sand.dat[,j])
  prediction.dat<- as.data.frame(ilrInv(subs))*100
  # diogs
  diog.clay[cnt,]<- as.matrix(goof(observed =observed.dat[,1],predicted = prediction.dat[,1]))
  diog.sand[cnt,]<- as.matrix(goof(observed =observed.dat[,2],predicted = prediction.dat[,2]))
  diog.silt[cnt,]<- as.matrix(goof(observed =observed.dat[,3],predicted = prediction.dat[,3]))
  # save outputs for PICP analysis
  pred.clay[,cnt]<- prediction.dat[,1]
  pred.sand[,cnt]<- prediction.dat[,2]
  pred.silt[,cnt]<- prediction.dat[,3]
  cnt<- cnt+1}
diog.clay<- as.data.frame(diog.clay)
diog.sand<- as.data.frame(diog.sand)
diog.silt<- as.data.frame(diog.silt)
names(diog.clay)<- c("R2", "CCC", "MSE", "RMSE","bias")
names(diog.sand)<- c("R2", "CCC", "MSE", "RMSE","bias")
names(diog.silt)<- c("R2", "CCC", "MSE", "RMSE","bias")

# save diognostics
saveRDS(object = diog.clay, file = paste0(root.data,"clay_d2_diogs_BT.rds"))
saveRDS(object = diog.sand, file = paste0(root.data,"sand_d2_diogs_BT.rds"))
saveRDS(object = diog.silt, file = paste0(root.data,"silt_d2_diogs_BT.rds"))

d2.clay.out<- colMeans(diog.clay)
d2.sand.out<- colMeans(diog.sand)
d2.silt.out<- colMeans(diog.silt)

# save data
saveRDS(object = pred.clay, file = paste0(root.data,"clay_d2_pred_data_BT.rds"))
saveRDS(object = pred.sand, file = paste0(root.data,"sand_d2_pred_data_BT.rds"))
saveRDS(object = pred.silt, file = paste0(root.data,"silt_d2_pred_data_BT.rds"))
saveRDS(object = observed.dat, file = paste0(root.data,"d2_observed_data_BT.rds"))



## D3
### CLAY
vari<- "clay"
depth<- "d3"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# 
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
clay.dat<- read.table(files[1],header = T,sep = ",")
names(clay.dat)
clay.dat<- clay.dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  clay.dat<- cbind(clay.dat, datz$prediction)
}

### SAND
vari<- "sand"
depth<- "d3"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# 
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_sand",full.names = T)
files
sand.dat<- read.table(files[1],header = T,sep = ",")
names(sand.dat)
sand.dat<- sand.dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  sand.dat<- cbind(sand.dat, datz$prediction)
}

## observation data
observed.dat<- matrix(NA, nrow=nrow(clay.dat),ncol=3)
comp.obs<- cbind(clay.dat$target,sand.dat$target)
observed.dat<- as.data.frame(ilrInv(comp.obs))*100

# prediction frames
pred.clay<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
pred.sand<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
pred.silt<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
#diognostics
diog.clay<- matrix(NA, nrow=50,ncol=5 )
diog.sand<- matrix(NA, nrow=50,ncol=5 )
diog.silt<- matrix(NA, nrow=50,ncol=5 )

names(clay.dat)
cnt<- 1
for (j in 7:56){
  subs<- cbind(clay.dat[,j],sand.dat[,j])
  prediction.dat<- as.data.frame(ilrInv(subs))*100
  # diogs
  diog.clay[cnt,]<- as.matrix(goof(observed =observed.dat[,1],predicted = prediction.dat[,1]))
  diog.sand[cnt,]<- as.matrix(goof(observed =observed.dat[,2],predicted = prediction.dat[,2]))
  diog.silt[cnt,]<- as.matrix(goof(observed =observed.dat[,3],predicted = prediction.dat[,3]))
  # save outputs for PICP analysis
  pred.clay[,cnt]<- prediction.dat[,1]
  pred.sand[,cnt]<- prediction.dat[,2]
  pred.silt[,cnt]<- prediction.dat[,3]
  cnt<- cnt+1}
diog.clay<- as.data.frame(diog.clay)
diog.sand<- as.data.frame(diog.sand)
diog.silt<- as.data.frame(diog.silt)
names(diog.clay)<- c("R2", "CCC", "MSE", "RMSE","bias")
names(diog.sand)<- c("R2", "CCC", "MSE", "RMSE","bias")
names(diog.silt)<- c("R2", "CCC", "MSE", "RMSE","bias")

# save diognostics
saveRDS(object = diog.clay, file = paste0(root.data,"clay_d3_diogs_BT.rds"))
saveRDS(object = diog.sand, file = paste0(root.data,"sand_d3_diogs_BT.rds"))
saveRDS(object = diog.silt, file = paste0(root.data,"silt_d3_diogs_BT.rds"))

d3.clay.out<- colMeans(diog.clay)
d3.sand.out<- colMeans(diog.sand)
d3.silt.out<- colMeans(diog.silt)

# save data
saveRDS(object = pred.clay, file = paste0(root.data,"clay_d3_pred_data_BT.rds"))
saveRDS(object = pred.sand, file = paste0(root.data,"sand_d3_pred_data_BT.rds"))
saveRDS(object = pred.silt, file = paste0(root.data,"silt_d3_pred_data_BT.rds"))
saveRDS(object = observed.dat, file = paste0(root.data,"d3_observed_data_BT.rds"))




## D4
### CLAY
vari<- "clay"
depth<- "d4"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# 
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
clay.dat<- read.table(files[1],header = T,sep = ",")
names(clay.dat)
clay.dat<- clay.dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  clay.dat<- cbind(clay.dat, datz$prediction)
}

### SAND
vari<- "sand"
depth<- "d4"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# 
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_sand",full.names = T)
files
sand.dat<- read.table(files[1],header = T,sep = ",")
names(sand.dat)
sand.dat<- sand.dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  sand.dat<- cbind(sand.dat, datz$prediction)
}

## observation data
observed.dat<- matrix(NA, nrow=nrow(clay.dat),ncol=3)
comp.obs<- cbind(clay.dat$target,sand.dat$target)
observed.dat<- as.data.frame(ilrInv(comp.obs))*100

# prediction frames
pred.clay<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
pred.sand<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
pred.silt<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
#diognostics
diog.clay<- matrix(NA, nrow=50,ncol=5 )
diog.sand<- matrix(NA, nrow=50,ncol=5 )
diog.silt<- matrix(NA, nrow=50,ncol=5 )

names(clay.dat)
cnt<- 1
for (j in 7:56){
  subs<- cbind(clay.dat[,j],sand.dat[,j])
  prediction.dat<- as.data.frame(ilrInv(subs))*100
  # diogs
  diog.clay[cnt,]<- as.matrix(goof(observed =observed.dat[,1],predicted = prediction.dat[,1]))
  diog.sand[cnt,]<- as.matrix(goof(observed =observed.dat[,2],predicted = prediction.dat[,2]))
  diog.silt[cnt,]<- as.matrix(goof(observed =observed.dat[,3],predicted = prediction.dat[,3]))
  # save outputs for PICP analysis
  pred.clay[,cnt]<- prediction.dat[,1]
  pred.sand[,cnt]<- prediction.dat[,2]
  pred.silt[,cnt]<- prediction.dat[,3]
  cnt<- cnt+1}
diog.clay<- as.data.frame(diog.clay)
diog.sand<- as.data.frame(diog.sand)
diog.silt<- as.data.frame(diog.silt)
names(diog.clay)<- c("R2", "CCC", "MSE", "RMSE","bias")
names(diog.sand)<- c("R2", "CCC", "MSE", "RMSE","bias")
names(diog.silt)<- c("R2", "CCC", "MSE", "RMSE","bias")

# save diognostics
saveRDS(object = diog.clay, file = paste0(root.data,"clay_d4_diogs_BT.rds"))
saveRDS(object = diog.sand, file = paste0(root.data,"sand_d4_diogs_BT.rds"))
saveRDS(object = diog.silt, file = paste0(root.data,"silt_d4_diogs_BT.rds"))

d4.clay.out<- colMeans(diog.clay)
d4.sand.out<- colMeans(diog.sand)
d4.silt.out<- colMeans(diog.silt)

# save data
saveRDS(object = pred.clay, file = paste0(root.data,"clay_d4_pred_data_BT.rds"))
saveRDS(object = pred.sand, file = paste0(root.data,"sand_d4_pred_data_BT.rds"))
saveRDS(object = pred.silt, file = paste0(root.data,"silt_d4_pred_data_BT.rds"))
saveRDS(object = observed.dat, file = paste0(root.data,"d4_observed_data_BT.rds"))



## D5
### CLAY
vari<- "clay"
depth<- "d5"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# 
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
clay.dat<- read.table(files[1],header = T,sep = ",")
names(clay.dat)
clay.dat<- clay.dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  clay.dat<- cbind(clay.dat, datz$prediction)
}

### SAND
vari<- "sand"
depth<- "d5"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# 
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_sand",full.names = T)
files
sand.dat<- read.table(files[1],header = T,sep = ",")
names(sand.dat)
sand.dat<- sand.dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  sand.dat<- cbind(sand.dat, datz$prediction)
}

## observation data
observed.dat<- matrix(NA, nrow=nrow(clay.dat),ncol=3)
comp.obs<- cbind(clay.dat$target,sand.dat$target)
observed.dat<- as.data.frame(ilrInv(comp.obs))*100

# prediction frames
pred.clay<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
pred.sand<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
pred.silt<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
#diognostics
diog.clay<- matrix(NA, nrow=50,ncol=5 )
diog.sand<- matrix(NA, nrow=50,ncol=5 )
diog.silt<- matrix(NA, nrow=50,ncol=5 )

names(clay.dat)
cnt<- 1
for (j in 7:56){
  subs<- cbind(clay.dat[,j],sand.dat[,j])
  prediction.dat<- as.data.frame(ilrInv(subs))*100
  # diogs
  diog.clay[cnt,]<- as.matrix(goof(observed =observed.dat[,1],predicted = prediction.dat[,1]))
  diog.sand[cnt,]<- as.matrix(goof(observed =observed.dat[,2],predicted = prediction.dat[,2]))
  diog.silt[cnt,]<- as.matrix(goof(observed =observed.dat[,3],predicted = prediction.dat[,3]))
  # save outputs for PICP analysis
  pred.clay[,cnt]<- prediction.dat[,1]
  pred.sand[,cnt]<- prediction.dat[,2]
  pred.silt[,cnt]<- prediction.dat[,3]
  cnt<- cnt+1}
diog.clay<- as.data.frame(diog.clay)
diog.sand<- as.data.frame(diog.sand)
diog.silt<- as.data.frame(diog.silt)
names(diog.clay)<- c("R2", "CCC", "MSE", "RMSE","bias")
names(diog.sand)<- c("R2", "CCC", "MSE", "RMSE","bias")
names(diog.silt)<- c("R2", "CCC", "MSE", "RMSE","bias")

# save diognostics
saveRDS(object = diog.clay, file = paste0(root.data,"clay_d5_diogs_BT.rds"))
saveRDS(object = diog.sand, file = paste0(root.data,"sand_d5_diogs_BT.rds"))
saveRDS(object = diog.silt, file = paste0(root.data,"silt_d5_diogs_BT.rds"))

d5.clay.out<- colMeans(diog.clay)
d5.sand.out<- colMeans(diog.sand)
d5.silt.out<- colMeans(diog.silt)

# save data
saveRDS(object = pred.clay, file = paste0(root.data,"clay_d5_pred_data_BT.rds"))
saveRDS(object = pred.sand, file = paste0(root.data,"sand_d5_pred_data_BT.rds"))
saveRDS(object = pred.silt, file = paste0(root.data,"silt_d5_pred_data_BT.rds"))
saveRDS(object = observed.dat, file = paste0(root.data,"d5_observed_data_BT.rds"))


## D6
### CLAY
vari<- "clay"
depth<- "d6"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# 
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
clay.dat<- read.table(files[1],header = T,sep = ",")
names(clay.dat)
clay.dat<- clay.dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  clay.dat<- cbind(clay.dat, datz$prediction)
}

### SAND
vari<- "sand"
depth<- "d6"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# 
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_sand",full.names = T)
files
sand.dat<- read.table(files[1],header = T,sep = ",")
names(sand.dat)
sand.dat<- sand.dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  sand.dat<- cbind(sand.dat, datz$prediction)
}

## observation data
observed.dat<- matrix(NA, nrow=nrow(clay.dat),ncol=3)
comp.obs<- cbind(clay.dat$target,sand.dat$target)
observed.dat<- as.data.frame(ilrInv(comp.obs))*100

# prediction frames
pred.clay<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
pred.sand<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
pred.silt<- matrix(NA, nrow=nrow(clay.dat),ncol=50 )
#diognostics
diog.clay<- matrix(NA, nrow=50,ncol=5 )
diog.sand<- matrix(NA, nrow=50,ncol=5 )
diog.silt<- matrix(NA, nrow=50,ncol=5 )

names(clay.dat)
cnt<- 1
for (j in 7:56){
  subs<- cbind(clay.dat[,j],sand.dat[,j])
  prediction.dat<- as.data.frame(ilrInv(subs))*100
  # diogs
  diog.clay[cnt,]<- as.matrix(goof(observed =observed.dat[,1],predicted = prediction.dat[,1]))
  diog.sand[cnt,]<- as.matrix(goof(observed =observed.dat[,2],predicted = prediction.dat[,2]))
  diog.silt[cnt,]<- as.matrix(goof(observed =observed.dat[,3],predicted = prediction.dat[,3]))
  # save outputs for PICP analysis
  pred.clay[,cnt]<- prediction.dat[,1]
  pred.sand[,cnt]<- prediction.dat[,2]
  pred.silt[,cnt]<- prediction.dat[,3]
  cnt<- cnt+1}
diog.clay<- as.data.frame(diog.clay)
diog.sand<- as.data.frame(diog.sand)
diog.silt<- as.data.frame(diog.silt)
names(diog.clay)<- c("R2", "CCC", "MSE", "RMSE","bias")
names(diog.sand)<- c("R2", "CCC", "MSE", "RMSE","bias")
names(diog.silt)<- c("R2", "CCC", "MSE", "RMSE","bias")

# save diognostics
saveRDS(object = diog.clay, file = paste0(root.data,"clay_d6_diogs_BT.rds"))
saveRDS(object = diog.sand, file = paste0(root.data,"sand_d6_diogs_BT.rds"))
saveRDS(object = diog.silt, file = paste0(root.data,"silt_d6_diogs_BT.rds"))

d6.clay.out<- colMeans(diog.clay)
d6.sand.out<- colMeans(diog.sand)
d6.silt.out<- colMeans(diog.silt)

# save data
saveRDS(object = pred.clay, file = paste0(root.data,"clay_d6_pred_data_BT.rds"))
saveRDS(object = pred.sand, file = paste0(root.data,"sand_d6_pred_data_BT.rds"))
saveRDS(object = pred.silt, file = paste0(root.data,"silt_d6_pred_data_BT.rds"))
saveRDS(object = observed.dat, file = paste0(root.data,"d6_observed_data_BT.rds"))


## Summary outputs
# compile outs
clay.all.outs<- rbind(d1.clay.out,d2.clay.out,d3.clay.out,d4.clay.out,d6.clay.out,d6.clay.out)
write.csv(clay.all.outs, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/SLGA_V1_ext_val/clay_tern_v2_val_summary_BT.csv",row.names = F)

# sand
sand.all.outs<- rbind(d1.sand.out,d2.sand.out,d3.sand.out,d4.sand.out,d5.sand.out,d6.sand.out)
write.csv(sand.all.outs, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/SLGA_V1_ext_val/sand_tern_v2_val_summary_BT.csv",row.names = F)

# silt
silt.all.outs<- rbind(d1.silt.out,d2.silt.out,d3.silt.out,d4.silt.out,d5.silt.out,d6.silt.out)
write.csv(silt.all.outs, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/SLGA_V1_ext_val/silt_tern_v2_val_summary_BT.csv",row.names = F)

  









