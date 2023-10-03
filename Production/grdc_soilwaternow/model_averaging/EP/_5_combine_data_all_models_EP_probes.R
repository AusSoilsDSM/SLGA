### GRDC soil-water NOW
# harmonisation of probe data and each model estimates
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.3.22
# modified: 29.3.22

# CODE PURPOSE
# Bring in all model outputs, combine with soil probe outputs
# EP

models.gen<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/"
data.out<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/analysis/EP/"

# EP soil probe locations
probe.locs<- read.csv("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/soil_probe_data/EP/EP_probesToUse_SLGA_DUL_LL.csv")


# EP probe data
EP.probe.data<- read.csv("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/soil_probe_data/EP/EP_probes_rawdata_combined.csv")


# model predictions
usyd.mod<- read.csv(file = paste0(models.gen,"models_output/USYD/EP/USYD_EP_all_ARD_scaled.csv"))
smips.mod<- read.csv(file = paste0(models.gen,"models_output/SMIPS/EP/EP_SMIPS_totalbucket_awc.csv"))
anu.mod<- read.csv(file = paste0(models.gen,"models_output/ANU/EP/EP_root_zone_wetness_2020_2021_scaled.csv"))
awra.mod<- read.csv(file = paste0(models.gen,"models_output/AWRA/ep/AWRA_EP_TotalBucket_scaled.csv"))
names(awra.mod)[1]<- "date"

# pre-process the data a bit to get it ready for combining
start.date<- "2021-01-01"
end.date<- "2021-12-31"

# harmonize date range of data sets
usyd.sel<- usyd.mod[which(usyd.mod$Date == start.date):which(usyd.mod$Date == end.date),]
smips.sel<- smips.mod[which(smips.mod$Date == start.date):which(smips.mod$Date == end.date),]
anu.sel<- anu.mod[which(anu.mod$date == start.date):which(anu.mod$date == end.date),]
awra.sel<- awra.mod[which(awra.mod$date == start.date):which(awra.mod$date == end.date),]

# probe names
probe.names<- names(EP.probe.data)[2:ncol(EP.probe.data)]


# start process of combining probe data with model predictions

for (i in 1:length(probe.names)){
  # establish a data frame
  emp.mat<- matrix(NA, nrow = nrow(smips.sel), ncol = 6)
  emp.mat<- as.data.frame(emp.mat)
  names(emp.mat)<- c("date", "EP_probe","smips","anu","usyd","awra")
  emp.mat$date<- smips.sel$Date
  
  # select the probe
  sel.probe.site<- probe.names[i]
  
  # load in the bars probe data
  find.probe<- which(names(EP.probe.data) == sel.probe.site)
  EP.mod<- EP.probe.data[,c(1,find.probe)]
  
  
  # start filling up the frame
  emp.mat$EP_probe<- EP.mod[,2]
  
  emp.mat$smips<- smips.sel[,which(names(smips.sel) == sel.probe.site)]
  emp.mat$anu<- anu.sel[,which(names(anu.sel) == sel.probe.site)]
  emp.mat$usyd<- usyd.sel[,which(names(usyd.sel) == sel.probe.site)]
  emp.mat$awra<- awra.sel[,which(names(awra.sel) == sel.probe.site)]
  emp.mat<- emp.mat[complete.cases(emp.mat),]
  
  # write data to file
  nm1<- paste0(data.out,"EP_SM_probe_",sel.probe.site, "_ARD_MA.csv")
  nm1
  write.csv(x = emp.mat,file = nm1,row.names = F)
  print(i)}
  





                    