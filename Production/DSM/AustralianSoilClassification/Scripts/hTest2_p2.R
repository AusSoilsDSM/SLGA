##########################################################################
###       Packages
##########################################################################

library(raster)
library(doParallel)
library(sf)

##########################################################################
###       Inputs
##########################################################################

#HPC indexing
args = commandArgs(trailingOnly=T)
print(args)
k <- as.numeric(args[1])

#Soild order codes
soil.orders = c("AN", "CA", "CH", "DE", "FE", "HY", "KA", "KU", "OR", "PO", "RU", "SO", "TE", "VE")

#Directory
root.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/CoVariates'
data.directory = paste0(root.directory, '/tiles')
outcome.directory = paste0('//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry/Project2')
RDS.directory = paste0(outcome.directory, '/RDS_samples')
borders.filename = "//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/CoVariates/vectors/squareBounds/allTiles_combined.shp"

##########################################################################
###       Initialise
##########################################################################

#Tile numbers
tiles = list.files(path = data.directory , full.names = F, include.dirs = T)

#Read in borders shape file
border = st_read(borders.filename, "allTiles_combined")

#Read in soil type data
soil.data = read.csv(paste0(outcome.directory, "/Apsoil.csv"))

##########################################################################
###       Drill
##########################################################################

#Tif file location
covariates.file = list.files(paste0(data.directory, '/', tiles[k]), '.tif$', recursive = F, full.names = T )

#No spurious data
bboxExt.aus <- extent(110,153,-43,-9)
idxs.aus <- which(soil.data$Longitude >= bboxExt.aus@xmin & soil.data$Longitude <= bboxExt.aus@xmax & soil.data$Latitude >= bboxExt.aus@ymin & soil.data$Latitude <= bboxExt.aus@ymax)
soil.data <- soil.data[idxs.aus, ]

#Ensure data  within tile
sf = sf::st_as_sf(soil.data, coords = c("Longitude","Latitude"))
st_crs(sf) = st_crs(border)
idxs= st_contains(border[tiles[k],], sf)[[1]]

#Drill if tile has data points
if (length(idxs) != 0) { 
  
  covariates = stack(covariates.file)
  
  training.points = soil.data[idxs, ]
  coordinates(training.points)  <- ~Longitude+Latitude
  
  #Extracting samples
  covariate.training = as.data.frame(extract(covariates,training.points,method="simple"))
  input = data.frame(training.points@coords, training.points[,2:13], covariate.training)
  input = input[,c(-(6:9), -(15:17))]
  
  #Naming Columns
  colnames(input)[1:2] <-c('Easting','Northing')
  
  #Save relevant RDS
  saveRDS(input, paste0(RDS.directory, '/Tile_', tiles[k], '.rds') )
}  


cat("Complete tile: k = ", k)  




