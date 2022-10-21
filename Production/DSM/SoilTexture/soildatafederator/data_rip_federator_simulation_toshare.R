# Pull soil texture data down from the soil data federator
# checking for repeatability of the pull down

library(jsonlite)


ddir<- "Z:/projects/ternlandscapes_2019/soiltexture/data/"

# Check what datasets are available
avail.datasets<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/DataSets?usr=brendan.malone%40csiro.au&key=YOUR_KEY")

morph.mat<- matrix(NA, nrow = 10, ncol = 13)
lab.mat<- matrix(NA, nrow = 10, ncol = 13)
class.mat<- matrix(NA, nrow = 10, ncol = 13)

for (j in 1:10){

### Morphology 
for (i in 1:length(avail.datasets$DataSet)){
nm1<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=H_TEXTURE%3BH_TEXTURE_QUAL&DataSet=",avail.datasets$DataSet[i],"&usr=brendan.malone%40csiro.au&key=YOUR_KEY")
down1<- try(as.data.frame(fromJSON(nm1)),TRUE)
if(isTRUE(class(down1)=="try-error")){morph.mat[j,i]<- -9999}else {morph.mat[j,i]<- nrow(down1)}
print(paste0("Morph_",i))
}


### Lab Data
for (i in 1:length(avail.datasets$DataSet)){
  nm1<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedPropertyGroup=PSA&DataSet=",avail.datasets$DataSet[i],"&usr=brendan.malone%40csiro.au&key=YOUR_KEY")
  down1<- try(as.data.frame(fromJSON(nm1)),TRUE)
  if(isTRUE(class(down1)=="try-error")){lab.mat[j,i]<- -9999}else {lab.mat[j,i]<- nrow(down1)}
  print(paste0("Lab_",i))
  }


### Soil classes
for (i in 1:length(avail.datasets$DataSet)){
  nm1<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=O_PPF%3BO_GSG%3BO_ASC_ORD%3BO_ASC_SUBORD&DataSet=",avail.datasets$DataSet[i],"&usr=brendan.malone%40csiro.au&key=YOUR_KEY")
  down1<- try(as.data.frame(fromJSON(nm1)),TRUE)
  if(isTRUE(class(down1)=="try-error")){class.mat[j,i]<- -9999}else {class.mat[j,i]<- nrow(down1)}
  print(paste0("Class_",i))}
}




