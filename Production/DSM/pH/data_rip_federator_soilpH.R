# Pull soil pH data down from the soil data federator
pwd<- "Z:/projects/ternlandscapes_2019/soil_pH/data/"

library(jsonlite);library(httr);library(RCurl);library(curl)
#library(leaflet);library(magrittr);library(sp)


# Check what datasets are available
avail.datasets<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets")
avail.datasets

## DATA KEY
# usr: Your Email
# key: Your Passkey

# FIELD DATA
org.names<- c()
avail.datasets$DataSet
for (m in 1:length(avail.datasets$DataSet)){
  org.names[m]<- paste0(avail.datasets$DataSet[m], "_soil_FIELD_pH_data")}
org.names
#i=3
# go through each dataset 
for (i in 1:length(avail.datasets$DataSet)){
nm1<- paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=PH_VALUE&DataSet=",avail.datasets$DataSet[i])
nm1
tt<- try(stop_for_status(GET(nm1)),TRUE) # stop if the response is an error
if(isTRUE(class(tt)=="try-error")){next}
pdat<- as.data.frame(fromJSON(nm1))
print(paste0(avail.datasets$DataSet[i],": ", nrow(pdat)))
if(nrow(pdat)!= 0){
  saveRDS(pdat, file = paste0(pwd,"fieldObs/",org.names[i],"_", Sys.Date(),".rds"))} else {
    next}
assign(org.names[i], fromJSON(nm1))}


# LAB DATA
lab.out<- paste0(pwd,"labObs/")
# data sets
org.names<- c()
org.names
data.sets<- avail.datasets$DataSet[c(1:14,16,17,20,21,22,23)]
for (m in 1:length(data.sets)){
  org.names[m]<- paste0(data.sets[m], "_soil_LAB_pH_data_YY")}
org.names

# lab methods
lab.methods<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=soil%20ph")
lab.methods$Property


# go through each data set and each method and download the data
for (j in 1:length(lab.methods$Property)){
  print(lab.methods$Property[j])
  # create a directory
  new.dir<- dir.create(path = paste0(lab.out,lab.methods$Property[j],"/"))
  lab.sum<- data.frame(data=NULL)
  for (i in 1:length(data.sets)){
    # form the url
    nm1<- paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=",lab.methods$Property[j],"&DataSet=",data.sets[i])
    tt<- try(stop_for_status(GET(nm1)),TRUE) # stop if the response is an error
    if(isTRUE(class(tt)=="try-error")){next}
    pdat<- as.data.frame(fromJSON(nm1))
    print(paste0(data.sets[i],": ", nrow(pdat)))
    lab.sum[i,1]<- paste0(data.sets[i],": ", nrow(pdat))
    if(nrow(pdat)!= 0){
      saveRDS(pdat, file = paste0(lab.out,lab.methods$Property[j],"/",org.names[i],"_", Sys.Date(),".rds"))} else {
        next}
    assign(org.names[i], fromJSON(nm1))}
  # write lab method summary
  write.csv(x = lab.sum,file = paste0(lab.out,lab.methods$Property[j],"/summary_",lab.methods$Property[j],".csv"),row.names = F)
  
  }




