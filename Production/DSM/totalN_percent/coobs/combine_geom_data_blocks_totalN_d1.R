### TERN LANDSCAPES 
# Bulk Density
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 15.11.23
# modified: 15.11.23

# CODE PURPOSE
# Pull together geom and coobs output files from previous analysis

# libraries


# fixed parameters
vart<- "totalN"
depth<- 1


### root directories
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_N/"
data.out<-paste0(g.root, "data/coobs/d",depth,"/")



data.files<- list.files(path = data.out, pattern = paste0("pca_sites_covariates_90m_stack_wgs_geomwork_",vart,"_d",depth,"_block_"),full.names = T)
data.files


#set template file
temp.file<- readRDS(data.files[1])
temp.file<- temp.file[0,]

for (i in 1:length(data.files)){
  temp.file<- rbind(temp.file,readRDS(data.files[i]))
}


saveRDS(object = temp.file, file = paste0(data.out,"pca_sites_covariates_90m_stack_wgs_geomwork_",vart,"_",depth, "_allblocks.rds"))
hist(temp.file$alldat_sim90_prop)
summary(temp.file$alldat_sim90_prop)
hist(temp.file$alldat_sim90_val)
hist(temp.file$alldat_geom)
length(which(temp.file$alldat_sim90_val > 5000))

