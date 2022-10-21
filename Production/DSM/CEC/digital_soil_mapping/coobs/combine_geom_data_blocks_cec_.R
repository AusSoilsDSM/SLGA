### TERN LANDSCAPES 
# CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 11.2.22
# modified: 24.2.22

# CODE PURPOSE
# Pull together geom and coobs output files from previous analysis

# libraries
library(sp);library(rgdal);library(raster);library(parallel);library(doParallel)


### root directories
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC//"
data.root<- paste0(gen.root,"data/geoms/")

# fixed parameters
vart<- "cec"
depth<- "d1"

data.files<- list.files(path = data.root, pattern = paste0(vart, "_", depth, "_block"),full.names = T)
data.files


#set template file
temp.file<- readRDS(data.files[1])
temp.file<- temp.file[0,]

for (i in 1:length(data.files)){
  temp.file<- rbind(temp.file,readRDS(data.files[i]))
}


saveRDS(object = temp.file, file = paste0(data.root,"pca_sites_covariates_90m_stack_wgs_geomwork_",vart,"_",depth, "_allblocks.rds"))
hist(temp.file$alldat_sim80_prob)
hist(temp.file$alldat_sim80_val)
hist(temp.file$alldat_geom)
length(which(temp.file$alldat_sim80_val > 100))

