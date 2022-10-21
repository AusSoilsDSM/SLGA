library(raster)
library(ranger)
library(rgdal)
library(ithir)

library(sf)

##########################################################################
###       Inputs
##########################################################################

#General directory
root.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry'
model.directory = paste0(root.directory, '/Models')
test.file = paste0(root.directory, '/Test_ASC_ORD.rds')

asris.file = "//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Ross/ASRIS/ASRISCombinedGeo.shp"

original.factors.file = "//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry/OriginalFactors.rds"

##########################################################################
###       Initialise
##########################################################################

#Original factors
original.factors = readRDS(original.factors.file)

#Test data
test = readRDS(test.file)

#turn validation data in spatial data
input = test[,-(1:4)]
test.true = test[,4]
coordinates(test)  <- ~Easting+Northing

#Extract atlas at validation points
asris = sf::st_read(asris.file)

#Convert points to a sf object
points = st_as_sf(test)
st_crs(points) = crs(asris)


#extract
out <- st_intersection(points, asris)

data = out[,c(2,310)]
qqq = data
st_geometry(qqq) = NULL

qqq[,2] = as.character(qqq[,2])
qqq = qqq[which(as.character(qqq[,2]) %in% original.factors),]

for (i in 1:length(original.factors)){
  index = qqq[,2] == original.factors[i]
  qqq[index,2] = i
}


qqq = goofcat(observed = qqq[,1], predicted = qqq[,2])



