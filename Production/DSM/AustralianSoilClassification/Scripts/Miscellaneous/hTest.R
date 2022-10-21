#library 
library(raster)
library(doParallel)

args = commandArgs(trailingOnly=T)
print(args)
k <- as.numeric(args[1])


soil.orders = c("AN", "CA", "CH", "DE", "FE", "HY", "KA", "KU", "OR", "PO", "RU", "SO", "TE", "VE")

#Directory
root.directory = '/OSM/CBR/AF_DIGISCAPESM/work/CoVariates'
data.directory = paste0(root.directory, '/tiles')
outcome.directory = '/OSM/CBR/AF_DIGISCAPESM/work/Harry'
RDS.directory = paste0(outcome.directory, '/RDS')

#Tile numbers
tiles = list.files(path = data.directory , full.names = F, include.dirs = T)

#Training data location
training.filename = 'ASC_ORD'      ## ".shp" extension not needed
training.fieldname = 'O_ASC_ORD'
training.id = "Observation_ID"


#Read in soil type data
soil.data = readRDS(paste0(outcome.directory, "/SoilTypes.rds"))

#Remove incorrect data
soil.data = soil.data[which(soil.data$O_ASC_ORD %in% soil.orders),]

levels.original = readRDS(paste0(outcome.directory, "/OriginalFactors.rds"))

for (i in 1:length(levels.original)){
  soil.data[[training.fieldname]][soil.data[[training.fieldname]] == levels.original[i]] = i
}
soil.data[[training.fieldname]] = as.factor(soil.data[[training.fieldname]])

#Tif file location
covariates.file = list.files(paste0(data.directory, '/', tiles[k]), '.tif$', recursive = F, full.names = T )

#Construc test raster
covariates.test = raster(list.files[1])
bboxExt.test = extent(covariates.test)
idxs.test = which(soil.data$Longitude >= bboxExt.test @xmin & soil.data$Longitude <= bboxExt.test @xmax & soil.data$Latitude >= bboxExt.test @ymin & soil.data$Latitude <= bboxExt.test @ymax)


if (length(idxs.test) != 0) { 
  
  covariates = stack(covariates.file)
  
  bboxExt = extent(covariates)
  idxs = which(soil.data$Longitude >= bboxExt@xmin & soil.data$Longitude <= bboxExt@xmax & soil.data$Latitude >= bboxExt@ymin & soil.data$Latitude <= bboxExt@ymax)
  training.points = soil.data[idxs, ]
  head(training.points )
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




