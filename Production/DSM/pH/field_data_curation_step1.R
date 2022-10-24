## Data Mongering: TERN soil pH
## Field Measurements
## Step 1: Unique locations and selection of specific lab methods



root<- "Z:/projects/ternlandscapes_2019/soil_pH/data/fieldObs/"
root.out<- "Z:/projects/ternlandscapes_2019/soil_pH/data/fieldObs/curated_field/"


# files
files<- list.files(path = root, pattern = "FIELD", full.names = T)
files
files.short<- list.files(path = root, pattern = "FIELD", full.names = F)
files.short


## compile data into a single table
sel.dat<- readRDS(files[1])
sel.dat<- sel.dat[,c(1:14)]
sel.dat<- sel.dat[0,]
for (i in 1:length(files)){
  imp.dat<- readRDS(files[i])
  print(nrow(imp.dat))
  sel.dat<- rbind(sel.dat,imp.dat[,1:14])
}




# join to main table
all.data<- sel.dat



## unique locations
all.data$uniqLoc<- paste0(all.data$Longitude, "_", all.data$Latitude)


uniqs<- unique(all.data$uniqLoc)
length(uniqs)


# cycle through the data get one obs per location
all.data$myLABS<- NA
new.data<- all.data[0,]
for (i in 1:length(uniqs)){
  sel.lab<- uniqs[i]
  sel.lab
  subdat<- all.data[which(all.data$uniqLoc == sel.lab),]
  subdat.split<- split(subdat,subdat$Dataset)
  print(c(length(uniqs)-i,length(subdat.split)))
  subdat<- subdat.split[[1]]
  subdat<- subdat[order(subdat$UpperDepth),]
  subdat$myLABS<- i
  # append to new table
  new.data<- rbind(new.data,subdat)
}

# Basic lab data curations
new.data$Value<- as.numeric(new.data$Value)
names(new.data)
new.data.sub<- new.data[complete.cases(new.data[,c(8,9,10,11,14,16)]),]

rm1<- which(new.data.sub$Value<=2)
rm1
View(new.data.sub[rm1,])
new.data.sub<- new.data.sub[-rm1,]
rm1<- which(new.data.sub$Value>=11) 
new.data.sub<- new.data.sub[-rm1,]
hist(new.data.sub$Value)

which(new.data.sub$UpperDepth == new.data.sub$LowerDepth)
new.data.sub[new.data.sub$myLABS == 41394,]

saveRDS(object = new.data.sub, file = paste0(root.out,"curated_step1_fieldpH.rds"))



