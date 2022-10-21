
#################################
###  Author : Ross Searle         
###  Date : Wed Oct  6 09:06:22 2021                      
###  Project : TERN Landscapes
###  Purpose : Generate COGs for the covariate stack
#################################

startTime <- Sys.time()
print(startTime)
print(paste0('R Version = ', version[13]))

library(stringr)
source('/datasets/work/af-digiscapesm/work/Ross/SLGA/PrivateInfo.R')


args = commandArgs(trailingOnly=T)

k = as.numeric(args[1])

# rootDir = paste0('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates')
# outDir = paste0('/scratch2/', ident, '/covariates/30mCOGs')
 rootDir = paste0('/datasets/work/clw-tern-soil/work/Products/National_digital_soil_property_maps')
 outDir = paste0('/scratch1/', ident, '/SLGA2')
 gdalPath <- '/apps/gdal/3.2.2/bin'



#mdata <- read.csv(paste0(rootDir, '/Metadata/CovariateMetaData - 01-12-2020.csv'), stringsAsFactors = F)

if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}

#fls <- list.files(paste0(rootDir, '/mosaics/90m'), pattern = '.tif', recursive = F, full.names = T)
fls <- list.files(paste0(rootDir), pattern = '.tif$', recursive = T, full.names = T)
length(fls)
f <- fls[k]
b <- basename(f)
print(paste0('Processing ', b))

odf <- dirname(f)
rdir <- paste0(outDir, '/', basename(odf))
if(!dir.exists(rdir)){dir.create(rdir, recursive = T)}


# mrec <- mdata[mdata$CoVariateName== str_remove(b, '.tif'), ]
# 
# 
# metastring <- str_replace_all( paste0('
# -mo "ACCESS_CONSTRAINTS=Creative Commons Attribution 4.0 CCBy"
# -mo "ACKNOWLEDGEMENTS=TERN acknowledges the original creators of these publicly available data sets"
# -mo "AREA_OR_POINT=Area"
# -mo "COLLABORATING_ORGANISATIONS=CSIRO"
# -mo "COMPLETENESS=The Soil and Landscape Grid of Australia covers all of continental Australia and near coastal islands land areas"
# -mo "CONTACT=esupport@tern.org.au"
# -mo "CONTACT_EMAIL=esupport@tern.org.au"
# -mo "CONTACT_ORGANISATION=Terrestrial Ecosystem Research Network - TERN"
# -mo "CONTACT_POSTAL=Long Pocket Precinct, Foxtail Building #1019 Level 5 The University of Queensland 80 Meiers Road Indooroopilly QLD 4068 Australia"
# -mo "CONTACT_STREET_ADDRESS=Long Pocket Precinct, Foxtail Building #1019 Level 5 The University of Queensland 80 Meiers Road Indooroopilly QLD 4068 Australia"
# -mo "CREDIT=Access to this data has been made possible by CSIRO and the Terrestrial Ecosystem Research Network which is supported by the Australian Government through the National Collaborative Research Infrastructure Strategy and the Super Science Initiative and by agreement from the data custodians of the background data. All of the organisations listed as collaborating agencies have also contributed significantly to the production of this dataset."
# -mo "CUSTODIAN=CSIRO"
# -mo "DATASET_TITLE=', str_replace(mrec$Description, '\"', ' Second'), '"
# -mo "DATA_CREATION_DATE=01/01/2015"
# -mo "DESCRIPTION=These raster data sets have been sourced from publicly available datasets and resampled to a consistent spatial extent and resolution to generate a stack of environmental covariate data to support national scale Digital Soil Modelling activities in Australia. There are over 150 national rasters representing the SCORPAN soil forming factors across climate, parent material, biology, relief, soil and location. Covariate rasters are available as full national mosaics or as smaller tiled sections across the whole of the continent. Tiles and mosaics are available at 90m -3 arcsec, resolution and a new set at  30m - 1 arcsec resolution."
# -mo "JURISDICTION=Australia"
# -mo "LICENCE=http://creativecommons.org/licenses/by/4.0/"
# -mo "LINEAGE=https://aussoilsdsm.esoil.io/dsm-covariates/covariate-data"
# -mo "MAINTENANCE_AND_UPDATE_FREQUENCY=Not Planned"
# -mo "ORIGINAL_DATASET_LOCATION=', mrec$OriginalDataset, '" 
# -mo "ORIGINAL_DATASET_METADATA=', mrec$MetaDataLink, '" 
# -mo "ORIGINAL_RESOLUTION=', mrec$NativeResolution, '" 
# -mo "ORIGINAL_SOURCE=', mrec$Source, '" 
# -mo "POINT_OF_CONTACT_PERSON=ross.searle@csiro.au" 
# -mo "POSITIONAL_ACCURACY=The horizontal positional error is the same as for the raw SRTM 3 second data with 90 percent of tested locations within 7.2 m for Australia. See Rodriguez et al. 2006for more information."
# -mo "PROGRESS=Complete"
# -mo "PROJECT_DESCRIPTION=The TERN Soil and Landscape Grid of Australia Facility has produced a comprehensive fine-resolution grid of soil attributes and important land surface parameters. The data is consistent with the Specifications of the GlobalSoilMap and is managed as part of the Australian Soil Resource Information System - ASRIS."
# -mo "RELATED_LINKS=https://aussoilsdsm.esoil.io/slga-version-2-products/australian-soil-classification-map"
# -mo "STORED_DATA_FORMAT=GeoTiff-COG"
# -mo "VALUE_DATA_TYPE=', mrec$ValueType, '"
# -mo "VALUE_UNITS=', mrec$Units, '"
# 
# 
# 
# '), '\n', ' ')
# 
# 
# metastring <- str_remove_all(metastring, '[)]')
# metastring <- str_remove_all(metastring, '[(]')

#cmd <- paste0(gdalPath, '/gdal_translate ', f, ' ', paste0(rdir, '/',b  ), ' -of COG ', ' -co RESAMPLING=NEAREST -co OVERVIEW_RESAMPLING=NEAREST -co COMPRESS=DEFLATE -co ZLEVEL=9 -co BIGTIFF=YES' ) 
#system(cmd)

system(paste0(gdalPath, '/gdaladdo --config GDAL_CACHEMAX 5000 -ro ',f))
cmd <- paste0(gdalPath, '/gdal_translate ', f, ' ', paste0(rdir, '/',b  ) ,' --config GDAL_CACHEMAX 5000 -stats -co TILED=YES -co COMPRESS=DEFLATE -co PREDICTOR=3 -co BIGTIFF=YES -co COPY_SRC_OVERVIEWS=YES' ) 
system(cmd)

endTime <- Sys.time()
print(endTime)
difft <- difftime(endTime, startTime, units = "auto")
print(difft)
print('Processing successfully completed')


