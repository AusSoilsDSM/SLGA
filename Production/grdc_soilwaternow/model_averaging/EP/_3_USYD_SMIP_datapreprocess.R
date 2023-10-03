### GRDC soil-water NOW
# Pre-processing USYD and SMIPS model estimates
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.3.22
# modified: 29.3.22

# CODE PURPOSE
# Get the model outputs into a form that it can be combined easily with other model outputs.
# 

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


# USYD Model
usyd.files<- list.files(path = paste0(models.gen,"USYD/EP/"),".csv", full.names = T, recursive = F)
usyd.files
usyd.files<- usyd.files[2:18]
usyd.files.short<- list.files(path = paste0(models.gen,"USYD/EP/"),".csv", full.names = F, recursive = F)
usyd.files.short<- usyd.files.short[2:18]
substr(usyd.files.short,start = 9, stop = 19)
usyd.files.short<- substr(usyd.files.short,start = 9, stop = 19)
usyd.files.short



# set up empty matrix
template<- read.csv(usyd.files[1])
result.matrix<- matrix(NA, nrow = nrow(template), ncol = length(usyd.files.short)+1)
result.matrix<- as.data.frame(result.matrix)
names(result.matrix)[1]<- "Date"
result.matrix$Date<- template$date
names(result.matrix)[2:ncol(result.matrix)]<- usyd.files.short

# cycle through each file

cnt<- 2
for (i in 1:17){
  read.in<- read.csv(usyd.files[i])
  names(read.in)
  result.matrix[,cnt]<- rowSums(read.in[,7:10]) + (0.75*read.in[,11])
  cnt<- cnt+1
}

# account for lower limit
for (i in 2:ncol(result.matrix)){
  sel.probe<- names(result.matrix)[i]
  sel.probe
  
  sel.row<- which(probe.locs$SiteID == sel.probe)
  sel.row
  ll.mm<- ll.slga[sel.row,"mean.ll"]
  usyd.ll<- result.matrix[,i] - ll.mm
  result.matrix[,i]<- usyd.ll}


# save data
write.csv(x = result.matrix,file = paste0(models.gen,"USYD/EP/USYD_EP_all_ARD_scaled.csv"),row.names = F)




### SMIPS
smips.file<- read.csv(file = paste0(models.gen,"SMIPS/EP/EP_SMIPS_totalbucket.csv"))
smips.file<- smips.file[,-1]
names.smips<- names(smips.file)[2:ncol(smips.file)]
names.smips
probe.locs

# how it matches up with probe data
sels<- which(probe.locs$Use == "Yes")
sels
new.probe.locs<- probe.locs[probe.locs$Use == "Yes",]
new.dul.slga<- dul.slga[sels,]
new.ll.slga<- ll.slga[sels,]

names(smips.file)[2:ncol(smips.file)]<- new.probe.locs$SiteID


write.csv(x = smips.file,file = paste0(models.gen,"SMIPS/EP/EP_SMIPS_totalbucket_awc.csv"),row.names = F)


