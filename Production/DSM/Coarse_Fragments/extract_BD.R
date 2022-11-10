#################################################################################
# Objective: SOC fraction maps at 90m resolution 
# Date: 17/11/2021
# Author: Mercedes Roman
# Project: SOC fraction maps for TERN
### Extract covariates for coarse fragments

### Load packages
library(sp)
library(raster)
library(rgdal)

# list all the raster files available
InputDir <-"/project/RDS-FSC-SoilBiodSOC2019-RW/SOCfractions/Input/"
setwd(InputDir)
list_ras <- list.files(pattern="BDW")

# load all the rasters
bulk.density <- stack(paste0(InputDir,list_ras))
print(bulk.density)

### Load spatialdataframe
load("cf.grav.4.sp.RData")
extr_bd <- raster::extract(bulk.density, cf.grav.4.sp)
print(str(extr_bd))
save(extr_bd, file="/project/RDS-FSC-SoilBiodSOC2019-RW/SOCfractions/Output/Coarse_fragments/extr_bd_cfgrav.RData")
### end of script