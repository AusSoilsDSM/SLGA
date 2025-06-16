### TERN Landscapes 
# Cation Exchange Capacity
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 8.6.21
# modified: 4.11.21

# CODE PURPOSE
# Search by Dataset and Lab Method code 
##

# root
root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/"

library(jsonlite);library(httr);library(RCurl);library(curl)

## DATA KEY
# usr: Your Email
# key: Your Passkey


# Check what datasets are available
avail.datasets<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets")
avail.datasets$DataSet
avail.datasets<- avail.datasets[c(1:8,10:23),]

# lab method codes
lab_codes<- read.csv(file = paste0(root,"available_CEC_methods.csv") )


# LAB DATA
lab.out<- paste0(root,"lab_method_codes/")
# data sets
org.names<- c()
org.names
data.sets<- avail.datasets$DataSet
data.sets
for (m in 1:length(data.sets)){
  org.names[m]<- paste0(data.sets[m], "_soil_LAB_CEC_data_YY")}
org.names


# go through each data set and each method and download the data
for (j in 1:nrow(lab_codes)){
  print(lab_codes$Property[j])
  # create a directory
  new.dir<- dir.create(path = paste0(lab.out,lab_codes$Property[j],"/"))
  lab.sum<- data.frame(data=NULL)
  for (i in 1:length(data.sets)){
    # form the url
    nm1<- paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=",lab_codes$Property[j],"&DataSet=",data.sets[i])
    tt<- try(stop_for_status(GET(nm1)),TRUE) # stop if the response is an error
    if(isTRUE(class(tt)=="try-error")){next}
    pdat<- as.data.frame(fromJSON(nm1))
    print(paste0(data.sets[i],": ", nrow(pdat)))
    lab.sum[i,1]<- paste0(data.sets[i],": ", nrow(pdat))
    if(nrow(pdat)!= 0){
      saveRDS(pdat, file = paste0(lab.out,lab_codes$Property[j],"/",org.names[i],"_", Sys.Date(),".rds"))} else {
        next}
    assign(org.names[i], fromJSON(nm1))}
  # write lab method summary
  write.csv(x = lab.sum,file = paste0(lab.out,lab_codes$Property[j],"/summary_",lab_codes$Property[j],".csv"),row.names = F)}




