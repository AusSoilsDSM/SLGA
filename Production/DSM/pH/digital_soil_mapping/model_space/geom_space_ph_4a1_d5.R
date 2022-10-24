### TERN LANDSCAPES 
# Soil pH model model fitting
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 5.8.21
# modified: 31.8.21

# CODE PURPOSE
# # Depth 5 [60-100cm]
# Create a model data space object to use for assessing where model extrapolation is good and problematic
# Do for lab and lab + field

# fixed parameters
vart<- "pH_4a1"
depth<- "d5"


# root directory
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"

data.root<- paste0(gen.root,"data/curated_all/")
model.out<- paste0(gen.root,"models/")
funcs.out<- paste0(gen.root,"rcode/miscell/")

# model geometry function
source(file = paste0(funcs.out,"model_spec_dimension_fn.R"))
source(file = paste0(funcs.out,"uniq_combs_fn.R"))

# libraries
library(raster);library(rgdal);library(sp);
library(tripack);library(spatialEco)

#data
# site data
site.dat<- readRDS(paste0(data.root,"tern_soilpH4a1_siteDat_covariates_20210705_CALVALDAT_ARD.rds"))
# external validation data
ext.dat<- readRDS(paste0(data.root,"tern_soilpH4a1_siteDat_covariates_20210705_EXTERNAL_ARD.rds"))
# combine data
comb.dat<- rbind(site.dat,ext.dat)

# taget the data down
rms<- which(comb.dat$X60.100.cm == -9999.000000)
length(rms)
all.dat<- comb.dat[-rms,c(1,16:55)]


# just lab data
lab.dat<- all.dat[which(all.dat$type == "L"),]


#### Model analysis [for checking applicability of models to new data]
names(all.dat)

# All data
model_geom_all<- model_spec_dimension(spectra = all.dat[,3:ncol(all.dat)],proba = 0.95)
# save object
saveRDS(object = model_geom_all, file = paste0(model.out, depth, "/geoms/",vart,"_",depth,"_covdat_hull_object_ALLDAT.rds"))

# Lab data
model_geom_lab<- model_spec_dimension(spectra = lab.dat[,3:ncol(lab.dat)],proba = 0.95)
# save object
saveRDS(object = model_geom_lab, file = paste0(model.out, depth, "/geoms/",vart,"_",depth,"_covdat_hull_object_LABDAT.rds"))

# END

