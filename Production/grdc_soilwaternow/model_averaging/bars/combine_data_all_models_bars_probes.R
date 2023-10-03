### GRDC soil-water NOW
# harmonisation of probe data and each model estimates
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 8.3.22
# modified: 22.3.22

# CODE PURPOSE
# Bring in all model outputs, combine with soil probe outputs
# 

models.gen<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/"
data.out<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/analysis/bars/"

# bars soil probe locations
probe.locs<- read.csv("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/soil_probe_data/bars/bars_probelocs_SLGA_dul_ll.csv")


# bars probe data
bars.files<- list.files(path =paste0(models.gen,"soil_probe_data/bars/rescaled/"),pattern = "mm", full.names = T, recursive = F ) 
bars.files
bars.files.short<- list.files(path =paste0(models.gen,"soil_probe_data/bars/rescaled/"),pattern = "mm", full.names = F, recursive = F ) 
bars.files.short
bars.files.short<- as.numeric(substr(bars.files.short,start = 25, stop = 27))
bars.files.short


# model predictions
usyd.mod<- read.csv(file = paste0(models.gen,"models_output/USYD/bars/USYD_all_ARD_scaled.csv"))
smips.mod<- read.csv(file = paste0(models.gen,"models_output/SMIPS/BARS/totalbucket_awc.csv"))
anu.mod<- read.csv(file = paste0(models.gen,"models_output/ANU/bars/BARS_root_zone_wetness_2020_2021_scaled.csv"))
awra.mod<- read.csv(file = paste0(models.gen,"models_output/AWRA/bars/BARS_AWRA_scaled.csv"))


# pre-process the data a bit to get it ready for combining
start.date<- "2021-01-01"
end.date<- "2021-12-30"

# harmonize date range of data sets
usyd.sel<- usyd.mod[which(usyd.mod$Date == start.date):which(usyd.mod$Date == end.date),]
smips.sel<- smips.mod[which(smips.mod$Date == start.date):which(smips.mod$Date == end.date),]
anu.sel<- anu.mod[which(anu.mod$date == start.date):which(anu.mod$date == end.date),]
awra.sel<- awra.mod[which(awra.mod$date == start.date):which(awra.mod$date == end.date),]

# get column names harmonised
usyd.probes<- names(usyd.sel)[2:ncol(usyd.sel)]
usyd.probes<- as.numeric(substr(usyd.probes,start = 2, stop = nchar(usyd.probes)))
names(usyd.sel)[2:ncol(usyd.sel)]<- usyd.probes

smips.probes<- names(smips.sel)[2:ncol(smips.sel)]
smips.probes<- as.numeric(substr(smips.probes,start = 2, stop = nchar(smips.probes)))
names(smips.sel)[2:ncol(smips.sel)]<- smips.probes

anu.probes<- names(anu.sel)[2:ncol(anu.sel)]
anu.probes<- as.numeric(substr(anu.probes,start = 2, stop = nchar(anu.probes)))
names(anu.sel)[2:ncol(anu.sel)]<- anu.probes

awra.probes<- names(awra.sel)[2:ncol(awra.sel)]
awra.probes<- as.numeric(substr(awra.probes,start = 2, stop = nchar(awra.probes)))
names(awra.sel)[2:ncol(awra.sel)]<- awra.probes


# start process of combining probe data with model predictions

for (i in 1:nrow(probe.locs)){
  # establish a data frame
  emp.mat<- matrix(NA, nrow = nrow(smips.sel), ncol = 6)
  emp.mat<- as.data.frame(emp.mat)
  names(emp.mat)<- c("date", "bars_probe","smips","anu","usyd","awra")
  emp.mat$date<- smips.sel$Date
  
  # select the probe
  sel.probe.site<- probe.locs$SITE.NUMBE[i]
  sel.probe.bars<- probe.locs$Probe_SenA[i]
  
  # load in the bars probe data
  find.probe<- which(bars.files.short == sel.probe.bars)
  bars.mod<- readRDS(file = bars.files[find.probe])
  bars.sel<- bars.mod[which(bars.mod$GD == start.date):which(bars.mod$GD == end.date),]
  
  # start filling up the frame
  emp.mat$bars_probe<- bars.sel$total
  
  emp.mat$smips<- smips.sel[,which(names(smips.sel) == sel.probe.site)]
  emp.mat$anu<- anu.sel[,which(names(anu.sel) == sel.probe.site)]
  emp.mat$usyd<- usyd.sel[,which(names(usyd.sel) == sel.probe.site)]
  emp.mat$awra<- awra.sel[,which(names(awra.sel) == sel.probe.site)]
  
  # write data to file
  nm1<- paste0(data.out,"BARS_SM_probe_",sel.probe.bars,"_site_",sel.probe.site, "_ARD_MA.csv")
  nm1
  write.csv(x = emp.mat,file = nm1,row.names = F)
  print(i)}
  





                    