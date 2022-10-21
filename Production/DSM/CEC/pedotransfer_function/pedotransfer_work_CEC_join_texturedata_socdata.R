### TERN Landscapes 
# Cation Exchange Capacity
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 15.11.21
# modified: 22.11.21

# CODE PURPOSE
# load soil texture and soc data 
# look for spatial joins
##
library(rgdal);library(sp)


# root
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/data_curation/"


# soil texture data
psa.data<- readRDS("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/data/federator/soiltexture/process5/lab_morph_psa_data_splined_naturalunits.rds")
psa.data.uniqs<- paste0(psa.data$Longitude, "_", psa.data$Latitude)
length(unique(psa.data.uniqs))
psa.data<- psa.data[-which(duplicated(psa.data.uniqs)),]

# SOC data
soc.data<- read.csv("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilCarbon_CNN/data/aus_soc_processed_splined_23032021.csv")
nrow(soc.data)
soc.data.uniqs<- paste0(soc.data$Longitude, "_", soc.data$Latitude)
length(unique(soc.data.uniqs))
soc.data<- soc.data[-which(duplicated(soc.data.uniqs)),]


## set up up data frame that can take both texture and soc data
all.frame<- cbind(psa.data[1:10,],soc.data[1:10,])
all.frame<- all.frame[0,]


# go through soc data and look for matches
text.cols<- c(1:24)
soc.cols<- c(25:40)

for (i in 1:nrow(soc.data)){
  subs<- soc.data[i,]
  dists<- spDistsN1(pts = as.matrix(psa.data[,3:4]), pt = as.matrix(subs[4:5]), longlat = T)
  dists.ord<- order(dists)
  min.dist<- dists[dists.ord[1]]
  if (min.dist < 5){
    sel.psa<- psa.data[dists.ord[1],]
    subs.all<- cbind(sel.psa,subs)
    all.frame<- rbind(all.frame,subs.all)} else {
      next}
  print(i)}


# save data
which(duplicated(all.frame$Observation_ID))
names(all.frame)
all.frameX<- all.frame[,c(1:24,33:38)]
saveRDS(object = all.frameX, file = paste0(g.root,"pedotransferWork_1_texture_soc_data_combined.rds"))
all.frameX <- readRDS(paste0(g.root,"pedotransferWork_1_texture_soc_data_combined.rds"))

## Bring in the CEC data
cec.data<- readRDS(file = paste0(g.root, "splineoutslab_CEC_data_splined_labelled.rds"))


## set up up data frame that can take both texture and soc data and cec data
all.frame2<- cbind(all.frameX[1:10,],cec.data[1:10,])
all.frame2<- all.frame2[0,]


# go through soc data and look for matches
comb.cols<- c(1:30)
cec.cols<- c(31:48)
names(all.frameX)

for (i in 1:nrow(cec.data)){
  subs<- cec.data[i,]
  dists<- spDistsN1(pts = as.matrix(all.frameX[,3:4]), pt = as.matrix(subs[7:8]), longlat = T)
  dists.ord<- order(dists)
  min.dist<- dists[dists.ord[1]]
  if (min.dist < 5){
    sel.psa<- all.frameX[dists.ord[1],]
    subs.all<- cbind(sel.psa,subs)
    all.frame2<- rbind(all.frame2,subs.all)} else {
      next}
  print(i)}

# save data
names(all.frame2)[42:47]<- paste0("CEC_",names(all.frame2)[42:47])
names(all.frame2)[25:30]<- paste0("SOC_",names(all.frame2)[25:30])
names(all.frame2)
all.frame2X<- all.frame2[,c(1:30,42:47)]
saveRDS(object = all.frame2X, file = paste0(g.root,"pedotransferWork_2_texture_soc_cec_data_combined.rds"))
all.frame2X<- readRDS(paste0(g.root,"pedotransferWork_2_texture_soc_cec_data_combined.rds"))

# unflatten the data

temp.mat<- matrix(NA, nrow=6,ncol = 12)
template.dat<- as.data.frame(temp.mat)
names(template.dat)<- c("DataStore",
                    "Observation_ID",
                    "Longitude",
                    "Latitude",
                    "type", "upper","lower","clay","sand","silt", "soc", "cec")
names(all.frame2X)
clay.names<- c(7:12)
sand.names<- c(13:18)
silt.names<- c(19:24)
soc.names<- c(25:30)
cec.names<- c(31:36)

out.dat.pedotransfer<- template.dat[0,]
for (i in 1:nrow(all.frame2X)){
  subs<- all.frame2X[i,]
  temp.dat<- template.dat
  df<- subs[rep(seq_len(nrow(subs)), each = 6),1:5 ]
  temp.dat[1:6,1:5]<- df
  temp.dat$upper<- c(0,5,15,30,60,100)
  temp.dat$lower<- c(5,15,30,60,100,200)
  temp.dat$clay<- c(as.matrix(subs[clay.names]))
  temp.dat$sand<- c(as.matrix(subs[sand.names]))
  temp.dat$silt<- c(as.matrix(subs[silt.names]))
  temp.dat$soc<- c(as.matrix(subs[soc.names]))
  temp.dat$cec<- c(as.matrix(subs[cec.names]))
  
  # append to big table
  out.dat.pedotransfer<- rbind(out.dat.pedotransfer,temp.dat)
  print(i)}

names(out.dat.pedotransfer)
out.dat.pedotransfer[out.dat.pedotransfer== -9999]<-NA
out.dat.pedotransferX<- out.dat.pedotransfer[complete.cases(out.dat.pedotransfer[,8:12]),]  

# save output
saveRDS(object = out.dat.pedotransferX, file = paste0(g.root,"pedotransferWork_3_allsoildata_4_cecpred.rds"))


# set up data for model extension

# unflatten the data
temp.mat<- matrix(NA, nrow=6,ncol = 12)
template.dat<- as.data.frame(temp.mat)
names(template.dat)<- c("DataStore",
                        "Observation_ID",
                        "Longitude",
                        "Latitude",
                        "type", "upper","lower","clay","sand","silt", "soc", "cec")
names(all.frameX)
clay.names<- c(7:12)
sand.names<- c(13:18)
silt.names<- c(19:24)
soc.names<- c(25:30)

out.dat.mextension<- template.dat[0,]
for (i in 1:nrow(all.frameX)){
  subs<- all.frameX[i,]
  temp.dat<- template.dat
  df<- subs[rep(seq_len(nrow(subs)), each = 6),1:5 ]
  temp.dat[1:6,1:5]<- df
  temp.dat$upper<- c(0,5,15,30,60,100)
  temp.dat$lower<- c(5,15,30,60,100,200)
  temp.dat$clay<- c(as.matrix(subs[clay.names]))
  temp.dat$sand<- c(as.matrix(subs[sand.names]))
  temp.dat$silt<- c(as.matrix(subs[silt.names]))
  temp.dat$soc<- c(as.matrix(subs[soc.names]))
  temp.dat$cec<- -5555
  
  # append to big table
  out.dat.mextension<- rbind(out.dat.mextension,temp.dat)
  print(i)}

names(out.dat.mextension)
out.dat.mextension[out.dat.mextension== -9999]<-NA
out.dat.mextensionX<- out.dat.mextension[complete.cases(out.dat.mextension[,8:11]),]  

# save output
saveRDS(object = out.dat.mextensionX, file = paste0(g.root,"pedotransferWork_3_allsoildata_4_cecExtension.rds"))
out.dat.mextensionX<- readRDS(file =  paste0(g.root,"pedotransferWork_3_allsoildata_4_cecExtension.rds"))
plot(out.dat.mextensionX$Longitude,out.dat.mextensionX$Latitude)


# consolidate files
out.dat.pedotransferX$uniq_loc<- paste0(out.dat.pedotransferX$lower, "_", out.dat.pedotransferX$Longitude, "_", out.dat.pedotransferX$Latitude)
out.dat.mextensionX$uniq_loc<- paste0(out.dat.mextensionX$lower, "_", out.dat.mextensionX$Longitude, "_", out.dat.mextensionX$Latitude)


for (i in 1:nrow(out.dat.pedotransferX)){
  sel.len<- length(which(out.dat.mextensionX$uniq_loc == out.dat.pedotransferX$uniq_loc[i]))
  if (sel.len != 0){
    sel<- which(out.dat.mextensionX$uniq_loc == out.dat.pedotransferX$uniq_loc[i])
    out.dat.mextensionX<- out.dat.mextensionX[-sel,]
    print(i)}}

names(out.dat.mextensionX)
names(out.dat.pedotransferX)

output.all<- rbind(out.dat.pedotransferX, out.dat.mextensionX)
output.all<- output.all[-which(duplicated(output.all$uniq_loc)),]

saveRDS(object = output.all, file = paste0(g.root,"pedotransferWork_3_allsoildata_pred_and_extension.rds"))

