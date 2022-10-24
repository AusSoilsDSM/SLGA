### TERN LANDSCAPES 
# Soil pH (CaCl2)
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 20.9.22
# modified: 20.9.22

# CODE PURPOSE
# # Depth 4 [30-60cm]
# Create a model data space object to use for assessing where model extrapolation is good and problematic


# fixed parameters
vart<- "ph_4b"
depth<- 4 # change for each depth
colsel<- c(9:14)
sel.colsel<-  colsel[depth]
sel.colsel

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
data.root<- paste0(g.root, "data/curated_all/")
dists.root<- paste0(g.root, "data/field_2_4B_dists/")
model.root<- paste0(g.root,"models/",vart,"/d",depth,"/geoms/")
funcs.out<- paste0(g.root,"rcode/miscell/")

# model geometry function
source(file = paste0(funcs.out,"model_spec_dimension_dsm_coobs.R"))
source(file = paste0(funcs.out,"uniq_combs_fn.R"))

# libraries
library(raster);library(rgdal);library(sp);
library(tripack);library(sf)

#data
# site data
site.dat<- readRDS(paste0(data.root,"tern_soilpH4B_siteDat_covariates_20223008_CALVALDAT_ARD.rds"))
# external validation data
ext.dat<- readRDS(paste0(data.root,"tern_soilpH4B_siteDat_covariates_20223008_EXTERNAL_ARD.rds"))
# combine data
comb.dat<- rbind(site.dat,ext.dat)

# target the data down
sel.dat<- comb.dat[,c(1:8, sel.colsel,17:55)]
rms<- length(which(sel.dat[,9] < 0))
rms
if (rms != 0){
  sel.dat<- sel.dat[-which(sel.dat[,9] < 0),]}

names(sel.dat)
new.col.names<- substr(x = names(sel.dat)[10:47],start = 1, stop = nchar(names(sel.dat)[10:47])-4)
new.col.names
names(sel.dat)[10:47]<- new.col.names



#### Model analysis [for checking applicability of models to new data]
cov.dat<- sel.dat[,10:48]
names(cov.dat)

# All data
model_geom_all<- model_spec_dimension(spectra = cov.dat,proba = 0.975)
hist(model_geom_all$max_dist_vector)
min(model_geom_all$max_dist_vector)
model_geom_all$max_dist
# save object
saveRDS(object = model_geom_all, file = paste0(model.root, vart, "_d",depth,"_covdat_hull_object_ALLDAT.rds"))


# END

