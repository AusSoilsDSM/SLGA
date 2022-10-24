## Post processing of soil color data [process 2]
## remove missing HVC data and sites with no coordinates
## combine all data and remove duplicates

root<- "Z:/projects/ternlandscapes_2019/soilColour/data/processed_1/"

files<- list.files(path = root, pattern = "MONGERED.rds",full.names = T)
files

dat<- readRDS(files[1])
names(dat)
head(dat)

for(i in 2:length(files)){
  tempdat<- readRDS(files[i])
  print(nrow(tempdat))
  dat<- rbind(dat,tempdat)
}

## clean up missing coordinates
rms.long<- which(is.na(dat$Longitude))
rms.long
dat<- dat[-rms.long,]
rms.lat<- which(is.na(dat$Latitude))
rms.lat
#dat<- dat[-rms.lat,]

# clean up missing color data
rms.hue<- which(is.na(dat$HVC_HUE))
rms.hue
dat<- dat[-rms.hue,]
rms.ang<- which(is.na(dat$HVC_ANGLE))
rms.ang
rms.val<- which(is.na(dat$HVC_VALUE))
rms.val
rms.chr<- which(is.na(dat$HVC_CHROMA))
rms.chr

## UNIQUE COORDINATES
dat$uniqcoord<- paste0(dat$Latitude, "_",dat$Longitude)
length(unique(dat$uniqcoord))
uniq.coords<- unique(dat$uniqcoord)


# GO THROUGH DATA AND REMOVE DUPLICATES
datX<- dat[0,]

for (i in 1:length(unique(dat$uniqcoord))){
  uniqid<- uniq.coords[i]
  subs<- dat[dat$uniqcoord == uniqid,]
  split.subs<- split(subs, subs$Provider)
  datX<- rbind(datX, split.subs[[1]])
  print(i)}

saveRDS(object = datX,file = paste0(root,"combined_soil_color_out_process1_2ndround.rds"))



