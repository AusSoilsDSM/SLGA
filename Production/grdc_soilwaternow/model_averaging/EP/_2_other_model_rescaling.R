### GRDC soil-water NOW
# Unit rescaling of ANU and AWRA outputs
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 28.3.22
# modified: 28.3.22

# CODE PURPOSE
# Rescaling of model output. namely:
# ANU, AWRA,
# 

library(raster);library(rgdal);library(sp);library(scales)
rescale.2p.lo <- function(x, theta_wp, theta_fc, obs_fc, obs_min){scales::rescale(x, to=c(theta_wp, theta_fc),from=c(obs_min, obs_fc))}



models.gen<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/models_output/"

# EP soil probe locations
probe.locs<- read.csv("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/soil_probe_data/EP/EP_probesToUse_SLGA_DUL_LL.csv")

# get the SLGA data only
names(probe.locs)
ll.slga<- probe.locs[,6:11]
dul.slga<- probe.locs[,12:17]
dul.slga$avg<- rowMeans(dul.slga[,1:6])/100
ll.slga$avg<- rowMeans(ll.slga[,1:6])/100
ll.slga$mean.ll<- ll.slga$avg*900



### ANU models
anu.sm<- read.csv(file = paste0(models.gen,"ANU/EP/EP_root_zone_wetness_2020_2021.csv"),header = T)
anu.p.names<- names(anu.sm)[2:ncol(anu.sm)]
anu.p.names
names(anu.sm)[1]<- "date"


# set up data frame to put new inputs
anu.scaled.sm<- anu.sm

for (i in 2:ncol(anu.sm)){
  #select the probe
  probe.sel<- names(anu.sm)[i]
  probe.sel
  
  # get the slga dul and ll
  loc.sel<- which(probe.locs$SiteID == probe.sel)
  loc.sel
  slga.dul<- dul.slga[loc.sel,7]
  slga.ll<- ll.slga[loc.sel,7]
  slga.dul;slga.ll
  
  # the data
  channel<- anu.sm[,i]
  
  # scale
  ndat<- rescale.2p.lo(x = channel, theta_fc = slga.dul, theta_wp = slga.ll, obs_fc = 1, obs_min = 0) * 900
  
  # put into data frame
  anu.scaled.sm[,i]<- ndat}

## account for lower limit
for (i in 2:ncol(anu.scaled.sm)){
  sel.probe<- names(anu.scaled.sm)[i]
  sel.probe
  
  sel.row<- which(probe.locs$SiteID == sel.probe)
  ll.mm<- ll.slga[sel.row,"mean.ll"]
  anu.ll<- anu.scaled.sm[,i] - ll.mm
  anu.scaled.sm[,i]<- anu.ll}




# save data
write.csv(x = anu.scaled.sm,file = paste0(models.gen,"ANU/EP/EP_root_zone_wetness_2020_2021_scaled.csv"),row.names = F)



### AWRA models
awra.sm<- read.csv(file = paste0(models.gen,"AWRA/ep/AWRA_EP_TotalBucket.csv"),header = T)
awra.sm<- awra.sm[,-1]
names.awra<- names(awra.sm)[2:ncol(awra.sm)]
new.awra.names<- substr(names.awra,start = 4, stop = nchar(names.awra))
names(awra.sm)[2:ncol(awra.sm)]<- new.awra.names
names(awra.sm)

# set up data frame to put new inputs
awra.scaled.sm<- awra.sm

for (i in 2:ncol(awra.sm)){
  #select the probe
  probe.sel<- names(awra.sm)[i]
  
  # get the slga dul and ll
  loc.sel<- which(probe.locs$SiteID == probe.sel)
  loc.sel
  slga.dul<- dul.slga[loc.sel,7]
  slga.ll<- ll.slga[loc.sel,7]
  slga.dul;slga.ll
  
  # the data
  channel<- awra.sm[,i]
  
  # scale
  ndat<- rescale.2p.lo(x = channel, theta_fc = slga.dul, theta_wp = slga.ll, obs_fc = 1, obs_min = 0) * 900
  
  # put into data frame
  awra.scaled.sm[,i]<- ndat}


## account for lower limit
for (i in 2:ncol(awra.scaled.sm)){
  sel.probe<- names(awra.scaled.sm)[i]
  sel.probe
  
  sel.row<- which(probe.locs$SiteID == sel.probe)
  sel.row
  ll.mm<- ll.slga[sel.row,"mean.ll"]
  awra.ll<- awra.scaled.sm[,i] - ll.mm
  awra.scaled.sm[,i]<- awra.ll}




# save data
write.csv(x = awra.scaled.sm,file = paste0(models.gen,"AWRA/ep/AWRA_EP_TotalBucket_scaled.csv"),row.names = F)

  
  


