### GRDC soil-water NOW
# Organise EP soil probe data
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.3.22
# modified: 29.3.22

# CODE PURPOSE
# read each available file of probe data and organise it 
# 


# root directory
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/soil_probe_data/EP/"

# soil probe data files
files<- list.files(path = gen.root, pattern = "opSID", full.names = T,recursive = F)
files
files.short<- list.files(path = gen.root, pattern = "opSID", full.names = F,recursive = F)
files.short<- substr(files.short,start = 1, stop = nchar(files.short)-4)
files.short<- substr(files.short,start = nchar(files.short)-10, stop = nchar(files.short))
files.short

# selected soil probes
probe.locs<- read.csv("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/soil_probe_data/EP/EP_probesToUse_SLGA_DUL_LL.csv")
sels<- which(probe.locs$Use == "Yes")
sels
new.probe.locs<- probe.locs[probe.locs$Use == "Yes",]
new.sites<- new.probe.locs$SiteID
new.sites

# build up data frame
matrix.out<- matrix(NA, ncol = 18, nrow=365)
matrix.out<- as.data.frame(matrix.out)
names(matrix.out)[1]<- "date"
names(matrix.out)[2:ncol(matrix.out)]<- new.sites
matrix.out$date<- seq(as.Date("2021/1/1"), as.Date("2021/12/31"), "days")


# go through each file and process
cnt<- 2
for (i in 1:length(new.sites)){
  sel.file<- which(files.short == new.sites[i])
  read.in<- read.csv(files[sel.file])
  read.in<- read.in[read.in$Date >= "2021-01-01" & read.in$Date <= "2021-12-31", ]
  matrix.out[matrix.out$date %in% as.Date(read.in$Date),cnt] <- read.in$VolTot
  cnt<- cnt + 1}
  
  
  
# save the results output
write.csv(matrix.out, "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/soil_probe_data/EP/EP_probes_rawdata_combined.csv",row.names = F)




