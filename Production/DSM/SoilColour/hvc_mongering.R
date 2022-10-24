## Soil color data

# function
source("Z:/projects/ternlandscapes_2019/soilColour/rcode/hvc_stripper_func.R")

root<- "Z:/projects/ternlandscapes_2019/soilColour/data/"

# files
files<- list.files(path = root, pattern = ".rds", full.names = T)
files

dat<- readRDS(files[8])

#HVC dat
dat<- dat[dat$ObservedProperty == "col_hue_val_chrom",]

uniq.dat<- as.data.frame(unique(dat$Value))
uniq.dat$hue<- NA
uniq.dat$angle<- NA
uniq.dat$value<- NA
uniq.dat$chroma<- NA
names(uniq.dat)[1]<- "orig"
uniq.dat<- uniq.dat[which(is.na(uniq.dat$chroma)),]
uniq.dat_out<- HVC_splitter(uniq.dat = uniq.dat)
uniq.dat_out[which(is.na(uniq.dat_out$chroma)),]
rms<- which(is.na(uniq.dat_out$chroma))
uniq.dat_out<- uniq.dat_out[-rms,]
rms<- which(is.na(uniq.dat_out$value))
if(length(rms) != 0){uniq.dat_out<- uniq.dat_out[-rms,]}



### link up with main data
dat$HVC_HUE<- NA
dat$HVC_ANGLE<- NA
dat$HVC_VALUE<- NA
dat$HVC_CHROMA<- NA

for (i in 1:nrow(uniq.dat_out)){
  subs<- uniq.dat_out$orig[i]
  sel<- which(dat$Value == subs)
  dat[sel,19:22]<- uniq.dat_out[i,2:5]
}

files[8]
## save outputs
saveRDS(dat, "Z:/projects/ternlandscapes_2019/soilColour/data/processed_1/QLDGovernment_soil_color_data_2020-09-03_MONGERED.rds")
