# Pull soil texture data down from the soil data federator
pwd<- "Z:/projects/ternlandscapes_2019/soilColour/data/"

library(jsonlite);
#library(leaflet);library(magrittr);library(sp)

ddir<- "Z:/projects/ternlandscapes_2019/soilColour/data/"

# Check what datasets are available
avail.datasets<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets?")
avail.datasets

### Morphology 
# texture class and qualifiers
# search: H_TEXTURE and H_TEXTURE_QUAL

org.names<- c()
avail.datasets$DataSet
for (m in 1:length(avail.datasets$DataSet)){
  org.names[m]<- paste0(avail.datasets$DataSet[m], "_soil_color_data")}
org.names

i=5
for (i in 1:length(avail.datasets$DataSet)){
nm1<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=col_hue_val_chrom&DataSet=",avail.datasets$DataSet[i])
pdat<- as.data.frame(fromJSON(nm1))
if(nrow(pdat)!= 0){
  saveRDS(pdat, file = paste0(pwd,org.names[i],"_", Sys.Date(),".rds"))} else {
    next}
assign(org.names[i], fromJSON(nm1)) 
print(i)}





