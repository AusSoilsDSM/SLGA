# Pull soil texture data down from the soil data federator
pwd<- "Z:/projects/ternlandscapes_2019/soiltexture/data/"

library(jsonlite);
#library(leaflet);library(magrittr);library(sp)

ddir<- "Z:/projects/ternlandscapes_2019/soiltexture/data/"

# Check what datasets are available
avail.datasets<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/DataSets?YOUR_CREDENTIALS")


### Morphology 
# texture class and qualifiers
# search: H_TEXTURE and H_TEXTURE_QUAL

org.names<- c()
avail.datasets$DataSet
for (m in 1:length(avail.datasets$DataSet)){
  org.names[m]<- paste0(avail.datasets$DataSet[m], "_PSA_morph_data")}
org.names

for (i in 1:length(avail.datasets$DataSet)){
nm1<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=H_TEXTURE%3BH_TEXTURE_QUAL&DataSet=",avail.datasets$DataSet[i],"&YOUR_CREDENTIALS")
pdat<- as.data.frame(fromJSON(nm1))
if(nrow(pdat)!= 0){
  saveRDS(pdat, file = paste0(pwd,org.names[i],"_", Sys.Date(),".rds"))} else {
    next}
assign(org.names[i], fromJSON(nm1)) 
print(i)}

### Lab Data
# Lab property groups
clay.lab<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/Properties?PropertyGroup=PSA&YOUR_CREDENTIALS")
nosP<- length(clay.lab$Property)
nosP

# Check what datasets are available
avail.datasets<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/DataSets?YOUR_CREDENTIALS")



org.names<- c()
avail.datasets$DataSet
for (m in 1:length(avail.datasets$DataSet)){
  org.names[m]<- paste0(avail.datasets$DataSet[m], "_PSA_lab_data")}
org.names


for (i in 1:length(avail.datasets$DataSet)){
  empty_list <- vector(mode = "list")
  for (j in 1:nosP){
    nml<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=",clay.lab$Property[j],"&DataSet=",avail.datasets$DataSet[i],"&YOUR_CREDENTIALS")
    getdata<- as.data.frame(fromJSON(nml))
    empty_list[[j]]<- getdata}
  # save output
  saveRDS(empty_list, file = paste0(pwd,org.names[i],"_", Sys.Date(),".rds"))
  assign(org.names[i],empty_list) 
  print(i)}



### Soil classes
# search: O_PPF;O_GSG;O_ASC_ORD;O_ASC_SUBORD

org.names<- c()
avail.datasets$DataSet
for (m in 1:length(avail.datasets$DataSet)){
  org.names[m]<- paste0(avail.datasets$DataSet[m], "_soil_class_data")}
org.names

for (i in 1:length(avail.datasets$DataSet)){
  nm1<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=O_PPF%3BO_GSG%3BO_ASC_ORD%3BO_ASC_SUBORD&DataSet=",avail.datasets$DataSet[i],"&YOUR_CREDENTIALS")
  pdat<- as.data.frame(fromJSON(nm1))
  if(nrow(pdat)!= 0){
    saveRDS(pdat, file = paste0(pwd,org.names[i],"_", Sys.Date(),".rds"))} else {
      next}
  assign(org.names[i], fromJSON(nm1)) 
  print(i)}

# Dont appear to be data for: TERNSurveillance, WAGovernment, TasGovernment,SAGovernment, NTGovernment, not retreiving much by way of soil classification NatGeoChemicalSurvey


# site observations
org.names<- c()
avail.datasets$DataSet
for (m in 1:length(avail.datasets$DataSet)){
  org.names[m]<- paste0(avail.datasets$DataSet[m], "_site_observations")}
org.names

for (i in 1:length(avail.datasets$DataSet)){
  nm1<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/ObservationLocations?DataSet=",avail.datasets$DataSet[i])
  pdat<- as.data.frame(fromJSON(nm1))
  if(nrow(pdat)!= 0){
    saveRDS(pdat, file = paste0(pwd,org.names[i],"_", Sys.Date(),".rds"))} else {
      next}
  assign(org.names[i], fromJSON(nm1)) 
  print(i)}


# scarp data
nm1<- "http://asris-daas02/NatSoil_restricted_Services/api/LabResults"
scarp.dat<- as.data.frame(fromJSON(nm1))




