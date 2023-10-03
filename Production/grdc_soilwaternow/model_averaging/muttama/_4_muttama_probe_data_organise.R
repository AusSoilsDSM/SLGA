### GRDC soil-water NOW
# Organise Muttama soil probe data
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 30.3.22
# modified: 30.3.22

# CODE PURPOSE
# read each available file of probe data and organise it 
# 


# root directory
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/soil_probe_data/muttama/"

# soil probe data
all.data<- read.csv(file = paste0(gen.root,"ProbesMuttamaLocalNorm.csv"))



# selected soil probes
probe.locs<- read.csv("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/soil_probe_data/muttama/muttama_probelocs_SLGA_dul_ll.csv")
probe.locs$SiteName
sels<- c(1,4,5,6,7,8,12,14,15)
probe.locs<- probe.locs[sels,]
probe.locs$SiteName
probe.locs$shortname<- c("Braw03", "Dais","YarrN", "Yarr", "Braw02", "GlenNew",   "DinT"  ,"GlenTop" ,  "GlenMid")
probe.locs<- probe.locs[,c(2,3,18,4:17)]

# get the SLGA data only
names(probe.locs)
ll.slga<- probe.locs[,6:11]
dul.slga<- probe.locs[,12:17]
dul.slga$avg<- rowMeans(dul.slga[,1:6])/100
ll.slga$avg<- rowMeans(ll.slga[,1:6])/100
ll.slga$mean.ll<- ll.slga$avg*900


# semi-process soil probe data
probe.names<- unique(all.data$Probe)
probe.names


# build up data frame
matrix.out<- matrix(NA, ncol = length(probe.names)+1, nrow=365)
matrix.out<- as.data.frame(matrix.out)
names(matrix.out)[1]<- "date"
names(matrix.out)[2:ncol(matrix.out)]<- probe.locs$SiteID
matrix.out$date<- seq(as.Date("2021/1/1"), as.Date("2021/12/31"), "days")


# go through each file and process
for (i in 1:length(probe.names)){
  read.in<- all.data[all.data$Probe == probe.names[i],]
  read.in<- read.in[read.in$Date >= "2021-01-01" & read.in$Date <= "2021-12-31", ]
  probe.id<- probe.locs[which(probe.locs$shortname == probe.names[i]),"SiteID"]
  sel.col<- which(names(matrix.out) == probe.id)
  matrix.out[matrix.out$date %in% as.Date(read.in$Date),sel.col] <- read.in$weightedAv_0_100
}

names(matrix.out)
matrix.out<- matrix.out[,c(1:7,10)]  

## account for lower limit
for (i in 2:ncol(matrix.out)){
  sel.probe<- names(matrix.out)[i]
  sel.probe
  
  sel.row<- which(probe.locs$SiteID == sel.probe)
  sel.row
  ll.mm<- ll.slga[sel.row,"mean.ll"]
  mutt.ll<- (matrix.out[,i]*0.9) - ll.mm
  mutt.ll
  matrix.out[,i]<- mutt.ll}
  
  
# save the results output
write.csv(matrix.out,  file = paste0(gen.root,"ProbesMuttamaLocalNorm_scaled.csv"),row.names = F)
gen.root



