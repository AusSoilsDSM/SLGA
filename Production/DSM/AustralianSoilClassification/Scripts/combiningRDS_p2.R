##########################################################################
###       Packages
##########################################################################

library(tidyverse)
library(data.table)
library(dplyr)
library(purrr)
library(raster)
library(rgdal)
library(ranger)

##########################################################################
###       Inputs
##########################################################################

#Directories 
root.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry/Project2'
data.directory = paste0(root.directory, '/RDS_samples')

##########################################################################
###       Construct dataset
##########################################################################

#Bind RDS of each tile
covariates.file = list.files(data.directory, '.rds$', recursive = F, full.names = T )
data = covariates.file %>% map(readRDS) %>% data.table::rbindlist(fill = T)

##########################################################################
###       Construct columns to remove 
##########################################################################

#Obtain unique column names
column.names = lapply( 1:length(covariates.file), function(i) colnames(readRDS(covariates.file[i])) )
names.all = unique(unlist(column.names))
columns.missing = c()
for (i in 1:length(column.names)){
  columns.missing = c(columns.missing, setdiff(names.all, column.names[[i]]))
}
column.incomplete = unique(columns.missing)

#need to save the column names that we use

#Columns to remove
remove.columns = read.table(paste0(root.directory, '/CovariatesToUse.csv'), sep=",", header=T,strip.white=T)
remove.columns = as.character(remove.columns$Clim_minann[remove.columns$X1 == 0])
veg = c("Veg_FractCover_Min_PV", "Veg_FractCover_Std_BS", "Veg_FractCover_Std_NPV", "Veg_FractCover_Std_PV", "Veg_FractCover_Max_BS",
        "Veg_FractCover_Max_NPV", "Veg_FractCover_Max_PV", "Veg_FractCover_Mean_BS", "Veg_FractCover_Mean_NPV", "Veg_FractCover_Mean_PV",
        "Veg_FractCover_Mean_PV", "Veg_FractCover_Min_NPV", "Veg_FractCover_Min_BS")
other = c("LOC_distance_to_coast", "LOC_lats", "LOC_longs","Veg_mvs31e_aus1", "gg_clip", "Veg_preEuropeanVeg", "Veg_HS_ICESatGLAS")

#Column to use
column.use = colnames(data)[-which(colnames(data) %in% c(column.incomplete,remove.columns, other, veg))]
saveRDS(column.use[-(1:4)], paste0(root.directory, "/CovariatesUsed.rds"))

##########################################################################
###       Remove relevant columns
##########################################################################

#Construct dataset with correct columns
input = as.data.frame(data)[,-which(colnames(data) %in% c(column.incomplete,remove.columns, other, veg))]

##########################################################################
###       Remove Duplicates 
##########################################################################

#Remove duplicates 
data = unique(input)

#Remove rows of NA's (just to be sure)
data = data[rowSums(is.na(data)) != ncol(data),]

##########################################################################
###       Save as RDS
##########################################################################

saveRDS(data, paste0(root.directory, "/CovariateTraining.rds"))


