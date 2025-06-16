### TERN Landscapes 
# Cation Exchange Capacity
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 8.6.21
# modified: 8.6.21

# CODE PURPOSE
# Exploratory work getting CEC data from federator
##

# root
root<- "Z:/projects/ternlandscapes_2019/soil_CEC/data/"

library(jsonlite);library(httr);library(RCurl);library(curl)

## DATA KEY
# usr: Your Email
# key: Your Passkey


# Check what datasets are available
avail.datasets<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets")
avail.datasets

# Check what soil properties are available
avail.properties<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties")
head(avail.properties)
unique(avail.properties$PropertyGroup)
# narrow down on just the ion exchange properties
subs<- avail.properties[avail.properties$PropertyGroup == "Ion-exchange properties",]
write.csv(x = subs, file = paste0(root, "available_ion_methods.csv"), row.names = F)



