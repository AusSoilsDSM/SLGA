### TERN LANDSCAPES 
# pH
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 21.9.22
# modified: 21.9.22

# CODE PURPOSE
# Pull together geom and coobs output files from previous analysis

# libraries
library(sp);library(rgdal);library(raster)

# fixed parameters
vart<- "ph_4b"
depth<- 4


### root directories
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
data.out<- paste0(g.root, "outs/4B_meth/geoms/d",depth,"/")



data.files<- list.files(path = data.out, pattern = paste0("pca_sites_covariates_90m_stack_wgs_geomwork_",vart,"_d",depth,"_block_"),full.names = T)
data.files


#set template file
temp.file<- readRDS(data.files[1])
temp.file<- temp.file[0,]

for (i in 1:length(data.files)){
  temp.file<- rbind(temp.file,readRDS(data.files[i]))
}


saveRDS(object = temp.file, file = paste0(data.out,"pca_sites_covariates_90m_stack_wgs_geomwork_",vart,"_",depth, "_allblocks.rds"))
hist(temp.file$alldat_sim80_prob)
summary(temp.file$alldat_sim80_prob)
hist(temp.file$alldat_sim80_val)
hist(temp.file$alldat_geom)
length(which(temp.file$alldat_sim80_val > 10000))

