### TERN LANDSCAPES 
# CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 9.2.22
# modified: 24.2.22

# CODE PURPOSE
# # Depth 1 [0-5cm]
# Create a model data space object to use for assessing where model extrapolation is good and problematic


# fixed parameters
vart<- "cec"
depth<- 1


# root directory
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/"

data.root<- paste0(gen.root,"data/digitalsoilmapping/")
model.out<- paste0(gen.root,"models/")
funcs.out<- paste0(gen.root,"rcode/miscell/")

# model geometry function
source(file = paste0(funcs.out,"model_spec_dimension_dsm_coobs.R"))
source(file = paste0(funcs.out,"uniq_combs_fn.R"))

# libraries
library(raster);library(rgdal);library(sp);
library(tripack);library(sf)

#data
# site data
site.dat<- readRDS(paste0(data.root,"calibration/tern_cec_siteDat_covariates_CALVAL_updated.rds"))
# external validation data
ext.dat<- readRDS(paste0(data.root,"test/tern_cec_siteDat_covariates_TEST_updated.rds"))
# combine data
comb.dat<- rbind(site.dat,ext.dat)

# taget the data down
names(comb.dat)
sels<- which(comb.dat$depth == depth) # change this for each depth interval

all.dat<- comb.dat[sels,c(66:104)]


#### Model analysis [for checking applicability of models to new data]
names(all.dat)

# All data
model_geom_all<- model_spec_dimension(spectra = all.dat,proba = 0.975)
hist(model_geom_all$max_dist_vector)
min(model_geom_all$max_dist_vector)
model_geom_all$max_dist
# save object
saveRDS(object = model_geom_all, file = paste0(model.out, "cec/geoms/","cec_d",depth,"_covdat_hull_object_ALLDAT.rds"))
# END

