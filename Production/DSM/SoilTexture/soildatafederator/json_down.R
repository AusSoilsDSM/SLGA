library(jsonlite);library(leaflet)
library(magrittr);library(sp)

json.file<- fromJSON("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/ObservationLocations?bbox=130.876%3B131.526%3B-13.388%3B-13.874")
names(json.file)
