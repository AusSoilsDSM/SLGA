### TERN LANDSCAPES 
# total P
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 22.11.23
# modified: 22.11.23

# CODE PURPOSE
# # Depth 5 [60-100cm]
# Create a model data space object to use for assessing where model extrapolation is good and problematic


# fixed parameters
vart<- "totalP"
depth<- 5


# root directory
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_P/"
data.root<- paste0(gen.root, "data/ross/model_ready/")
funcs.out<- paste0(gen.root,"rcode/miscell/")
model.out<- paste0(gen.root, "models/geoms/")

# model geometry function
source(file = paste0(funcs.out,"model_spec_dimension_dsm_coobs.R"))
source(file = paste0(funcs.out,"uniq_combs_fn.R"))

# libraries
library(terra)
library(tripack);library(sf)

#data
# site data
comb.dat<- readRDS(paste0(data.root,"tern_totalP_labelled_covariates_ALL.rds"))


# taget the data down
names(comb.dat)
sels<- which(comb.dat$depth == depth) # change this for each depth interval
length(sels)

all.dat<- comb.dat[sels,c(6:44)]


#### Model analysis [for checking applicability of models to new data]
names(all.dat)

# All data
model_geom_all<- model_spec_dimension(spectra = all.dat,proba = 0.975)
hist(model_geom_all$max_dist_vector)
min(model_geom_all$max_dist_vector)
model_geom_all$max_dist #200
# save object
saveRDS(object = model_geom_all, file = paste0(model.out, vart, "_d",depth,"_covdat_hull_object_ALLDAT.rds"))
# END

