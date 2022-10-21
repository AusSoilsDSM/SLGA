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
outcome.directory = paste0('//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry')
RDS.directory = paste0(outcome.directory, '/RDS')
borders.filename = "//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/CoVariates/vectors/squareBounds/allTiles_combined.shp"

#Training data location
training.filename = 'ASC_ORD'      ## ".shp" extension not needed
training.fieldname = 'O_ASC_ORD'
training.id = "Observation_ID"

##########################################################################
###       Initialise
##########################################################################

#Tile numbers
tiles = list.files(path = data.directory , full.names = F, include.dirs = T)

#Read in borders shape file
border = st_read(borders.filename, "allTiles_combined")

#Read in soil type data
soil.data = readRDS(paste0(outcome.directory, "/SoilTypes.rds"))

#Remove incorrect data
soil.data = soil.data[which(soil.data$O_ASC_ORD %in% soil.orders),]

##########################################################################
###       Convert Levels to numbers
##########################################################################

levels.original = unique(soil.data[[training.fieldname]])
saveRDS(levels.original, (paste0(outcome.directory, "/OriginalFactors.rds")))

for (i in 1:length(levels.original)){
  soil.data[[training.fieldname]][soil.data[[training.fieldname]] == levels.original[i]] = i
}
soil.data[[training.fieldname]] = as.factor(soil.data[[training.fieldname]])

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
  input = data.frame(training.points[[training.id]], training.points@coords, training.points[[training.fieldname]], covariate.training)
  
  #Naming Columns
  colnames(input)[c(1,2,3,4)] <-c('SID','Easting','Northing', training.fieldname)
  
  #Save relevant RDS
  saveRDS(input, paste0(RDS.directory, '/Tile_', tiles[k], '.rds') )
  saveRDS(minValue(covariates), paste0(RDS.directory, '/ExtremeValues/Min_Tile_', tiles[k], '.rds'))
  saveRDS(maxValue(covariates), paste0(RDS.directory, '/ExtremeValues/Max_Tile_', tiles[k], '.rds'))
}  


cat("Fininished tile: k = ", k)  




