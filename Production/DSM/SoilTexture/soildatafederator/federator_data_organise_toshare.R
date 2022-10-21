# Lab measured soil texture data. 
# Need to combine all methods into 1 table
library(httr);library(jsonlite);

# lab data location
lab.files<- list.files(path = "S:/projects/ternlandscapes_2019/soiltexture/data/",
                       pattern = "PSA_lab_data_2019-12-11.rds",full.names = T )
lab.files


# Organising lab PSA data for each available dataset 
clay.lab<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/Properties?PropertyGroup=PSA&usr=brendan.malone%40csiro.au&key=YOUR_KEY")
nosP<- length(clay.lab$Property)
nosP
meth<- clay.lab$Property
meth

# Initiate an empty data frame
lab.files[13]
labs<- readRDS(file = lab.files[13])

# create an empty data.frame
emp.labs<- labs[[45]]
names(emp.labs)
emp.labs<- emp.labs[1, 1:10]
emp.labs$dummy<- NA
emp.labs[,1:11]<- -99999
emp.labs

## Go through each data set

# Dataset: 
sa.sel<- 8

lab.files[sa.sel]
labs<- readRDS(file = lab.files[sa.sel])

dat<- emp.labs


# cycle through each method
#for (i in 1:7){
for (i in 1:length(meth)){
  subs<- as.data.frame(labs[i])
  
  if(nrow(subs)==0){subs<- emp.labs[,1:11]
  names(subs)[11]<- meth[i]
  subs[,1:11]<- -99999} else {
    subs<- subs[,c(1:10,13)]
    names(subs)[11]<- meth[i]}
  
  # merge
  dat<- merge(dat,subs, 
             by=c("DataStore","Dataset","Provider","Observation_ID",
                  "SampleID","SampleDate","Longitude","Latitude",
                  "UpperDepth","LowerDepth" ),all=T)
  rms<- which(duplicated(dat[,1:10]))
  if(length(rms)==0){dat<- dat}else{dat<- dat[-rms, ]}
  print(c(i,nrow(dat), nrow(subs)))}

nm1<- substr(lab.files[sa.sel],start = 1,stop = nchar(lab.files[sa.sel])-4)
nm1<- paste0(nm1,"_combi.csv")
write.csv(x = dat, file = nm1, row.names = F)
