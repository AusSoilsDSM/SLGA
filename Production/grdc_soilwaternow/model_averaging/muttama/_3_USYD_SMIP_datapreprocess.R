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

# bars soil probe locations
probe.locs<-  read.csv("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/soil_probe_data/muttama/muttama_probelocs_SLGA_dul_ll.csv")

# get the SLGA data only
names(probe.locs)
ll.slga<- probe.locs[,6:11]
dul.slga<- probe.locs[,12:17]
dul.slga$avg<- rowMeans(dul.slga[,1:6])/100
ll.slga$avg<- rowMeans(ll.slga[,1:6])/100
ll.slga$mean.ll<- ll.slga$avg*900


# USYD Model
usyd.files<- list.files(path = paste0(models.gen,"USYD/Muttama/"),".csv", full.names = T, recursive = F)
usyd.files<- usyd.files[1:16]
usyd.files.short<- list.files(path = paste0(models.gen,"USYD/Muttama/"),".csv", full.names = F, recursive = F)
usyd.files.short<- usyd.files.short[1:16]
usyd.files.short<- strsplit(usyd.files.short, "_")
usyd.files.short
usyd.files.short<- as.numeric(sapply(usyd.files.short, `[`, 3))
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
for (i in 1:16){
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
  ll.mm<- ll.slga[sel.row,"mean.ll"]
  usyd.ll<- result.matrix[,i] - ll.mm
  result.matrix[,i]<- usyd.ll}


# save data
write.csv(x = result.matrix,file = paste0(models.gen,"USYD/Muttama/USYD_muttama_all_ARD_scaled.csv"),row.names = F)




### SMIPS
smips.file<- read.csv(file = paste0(models.gen,"SMIPS/muttama/Mutt_totalbucket.csv"))
names.smips<- names(smips.file)[2:ncol(smips.file)]
names.smips

# how it matches up with probe data
mat1<- c(1,4,5,6,7,8,12,14,15)
new.probe.locs<- probe.locs[mat1,]
new.dul.slga<- dul.slga[mat1,]
new.ll.slga<- ll.slga[mat1,]

names(smips.file)[3:ncol(smips.file)]<- new.probe.locs$SiteID


write.csv(x = smips.file,file = paste0(models.gen,"SMIPS/muttama/Mutt_totalbucket_awc.csv"),row.names = F)


