## TERN modelling of soil texture
## compile predictions from the internal and external validations
## model diognostics

root<- "Z:/projects/ternlandscapes_2019/soiltexture/models/"


### CLAY
## D1
vari<- "clay"
depth<- "d1"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "CAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
# write data to file
nm1<- paste0(new.root, "ranger_CAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# cross validation files
files<- list.files(path = new.root,pattern = "VAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
# write data to file
nm1<- paste0(new.root, "ranger_VAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# external validation files
files<- list.files(path = new.root,pattern = "EXT_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
# write data to file
nm1<- paste0(new.root, "ranger_EXT_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)


## D2
vari<- "clay"
depth<- "d2"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "CAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_CAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# cross validation files
files<- list.files(path = new.root,pattern = "VAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_VAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# external validation files
files<- list.files(path = new.root,pattern = "EXT_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_EXT_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)


## D3
vari<- "clay"
depth<- "d3"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "CAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_CAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# cross validation files
files<- list.files(path = new.root,pattern = "VAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_VAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# external validation files
files<- list.files(path = new.root,pattern = "EXT_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_EXT_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)


## D4
vari<- "clay"
depth<- "d4"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "CAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_CAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# cross validation files
files<- list.files(path = new.root,pattern = "VAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_VAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# external validation files
files<- list.files(path = new.root,pattern = "EXT_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_EXT_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)


## D5
vari<- "clay"
depth<- "d5"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "CAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_CAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# cross validation files
files<- list.files(path = new.root,pattern = "VAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_VAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# external validation files
files<- list.files(path = new.root,pattern = "EXT_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_EXT_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)


## D6
vari<- "clay"
depth<- "d6"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "CAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_CAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# cross validation files
files<- list.files(path = new.root,pattern = "VAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_VAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# external validation files
files<- list.files(path = new.root,pattern = "EXT_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_EXT_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)




### SAND
## D1
vari<- "sand"
depth<- "d1"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "CAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_CAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# cross validation files
files<- list.files(path = new.root,pattern = "VAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_VAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# external validation files
files<- list.files(path = new.root,pattern = "EXT_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_EXT_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)


## D2
vari<- "sand"
depth<- "d2"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "CAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_CAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# cross validation files
files<- list.files(path = new.root,pattern = "VAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_VAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# external validation files
files<- list.files(path = new.root,pattern = "EXT_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_EXT_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)


## D3
vari<- "sand"
depth<- "d3"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "CAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_CAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# cross validation files
files<- list.files(path = new.root,pattern = "VAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_VAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# external validation files
files<- list.files(path = new.root,pattern = "EXT_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_EXT_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)


## D4
vari<- "sand"
depth<- "d4"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "CAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_CAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# cross validation files
files<- list.files(path = new.root,pattern = "VAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_VAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# external validation files
files<- list.files(path = new.root,pattern = "EXT_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_EXT_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)


## D5
vari<- "sand"
depth<- "d5"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "CAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_CAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# cross validation files
files<- list.files(path = new.root,pattern = "VAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_VAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# external validation files
files<- list.files(path = new.root,pattern = "EXT_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_EXT_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)


## D6
vari<- "sand"
depth<- "d6"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "CAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_CAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# cross validation files
files<- list.files(path = new.root,pattern = "VAL_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_VAL_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)

# external validation files
files<- list.files(path = new.root,pattern = "EXT_diogs",full.names = T)
files
dat<- read.table(file = files[1],sep = ",",header = T)
for (i in 2:length(files)){
  dat<- rbind(dat,read.table(file = files[i],sep = ",",header = T))}
colMeans(dat)
# write data to file
nm1<- paste0(new.root, "ranger_EXT_diogs_",vari,"_",depth,"_model_summary.csv")
write.csv(dat,file = nm1)




files<- list.files(path = root, pattern = "_model_summary.csv", recursive = T,full.names = T)
files
files<- files[c(2,5,8,11,14,17,20,23,26,29,32,35)]
files

varb<- c(rep("clay",6), rep("sand",6))
dp<- rep(c("d1","d2", "d3","d4","d5","d6"),2)
avgmse<- c() 
for (i in 1:length(files)){
  dat<- read.csv(files[i])
  avgmse[i]<- mean(dat$MSE)}
out.dat<- as.data.frame(cbind(varb,dp,avgmse))
saveRDS(out.dat, "Z:/projects/ternlandscapes_2019/soiltexture/data/soiltexture_models_avgMSE.rds")
