### GRDC soil-water NOW
# Pre-procesing USYD and SMIPS model estimates
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 8.3.22
# modified: 22.3.22

# CODE PURPOSE
# Get the model outputs into a form that it can be combined easily with other model outputs.
# 

models.gen<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/models_output/"

# bars soil probe locations
probe.locs<- read.csv("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/soil_probe_data/bars/bars_probelocs_SLGA_dul_ll.csv")


# USYD Model
usyd.files<- list.files(path = paste0(models.gen,"USYD/bars/"),".csv", full.names = T, recursive = F)
usyd.files.short<- list.files(path = paste0(models.gen,"USYD/bars/"),".csv", full.names = F, recursive = F)
usyd.files.short<- substr(usyd.files.short,start = 1,stop = nchar(usyd.files.short)-4)[1:33]
usyd.files.short<- as.numeric(substr(usyd.files.short,start = 11,stop = nchar(usyd.files.short)))
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
for (i in 1:33){
  read.in<- read.csv(usyd.files[i])
  names(read.in)
  result.matrix[,cnt]<- rowSums(read.in[,7:10]) + (0.75*read.in[,11])
  cnt<- cnt+1
}

# account for lower limit
natsoil.dat<- read.csv("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/soil_probe_data/bars/bars_probelocs_SLGA_dul_ll.csv")
names(natsoil.dat)
natsoil.dat$mean.ll<- (rowMeans(natsoil.dat[,44:48])/100)*900
natsoil.dat$mean.ll

for (i in 2:ncol(result.matrix)){
  sel.probe<- names(result.matrix)[i]
  sel.probe
  
  sel.row<- which(natsoil.dat$SITE.NUMBE == sel.probe)
  ll.mm<- natsoil.dat[sel.row,"mean.ll"]
  usyd.ll<- result.matrix[,i] - ll.mm
  result.matrix[,i]<- usyd.ll}


# save data
write.csv(x = result.matrix,file = paste0(models.gen,"USYD/bars/USYD_all_ARD_scaled.csv"),row.names = F)




### SMIPS
smips.file<- read.csv(file = paste0(models.gen,"SMIPS/BARS/SMIPS_BARS_totalbucket.csv"))
names.smips<- names(smips.file)[2:ncol(smips.file)]
names(smips.file)[2:ncol(smips.file)]<- as.numeric(substr(names.smips,start = 2, stop = nchar(names.smips)))

# dul and ll data
natsoil.dat<- read.csv("/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/data/soil_probe_data/bars/bars_probelocs_SLGA_dul_ll.csv")
names(natsoil.dat)
natsoil.dat$mean.ll<- (rowMeans(natsoil.dat[,44:48])/100)*900

new.smips.file<- smips.file

for (i in 2:ncol(smips.file)){
  sel.probe<- names(smips.file)[i]
  
  sel.row<- which(natsoil.dat$SITE.NUMBE == sel.probe)
  ll.mm<- natsoil.dat[sel.row,"mean.ll"]
  new.smips.file[,i]<- smips.file[,i]}

write.csv(x = new.smips.file,file = paste0(models.gen,"SMIPS/BARS/totalbucket_awc.csv"),row.names = F)


