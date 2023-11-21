### TERN LANDSCAPES
# total N
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# date created: 17.11.23
# date modified: 17.11.23

# CODE PURPOSE
# mosaic all prediction outcomes
# outputs are GTIFF files 
#

# fixed parameters
args = commandArgs(trailingOnly = T)
sel.depth<- as.numeric(args[1]) # depths
runs<- as.numeric(args[2]) # number of outputs per depth
vart<-  "totalN"

nmss<- c(paste0("modpred_clustering_lowerPI_d",sel.depth), 
         paste0("modpred_median_fin_d",sel.depth),
         paste0("modpred_clustering_upperPI_d",sel.depth),
         paste0("modpred_clustering_PIrange_d",sel.depth),
         paste0(vart,"_d",sel.depth, "_geomsimsFP_alldat_modelfit")
)
length(nmss)

## libraries
library(raster)

# root directory
g.root<-  "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_N/"
root.data.in<- paste0(g.root,"/predictions/tiles/")
root.data.out<- paste0(g.root,"/data/mosaics/")
root.mosaic<- paste0(g.root,"/predictions/d",sel.depth,"/")
root.slurm<- paste0(g.root,"/rcode/slurm/outs/digital_soil_mapping/mosaic/d",sel.depth,"/")

# tile names
tile.folnames<- as.numeric(list.dirs(path = root.data.in,full.names = F,recursive = F))
tile.folnames<- tile.folnames[order(tile.folnames)]
#tile.folnames

# raster list
raster_list <- list() # initialise the list of rasters

# select the output filename
file.in<- nmss[runs]
file.in

cnt<- 1
for (i in 1:length(tile.folnames)){
#for (i in 1:50){
  fpath1<- paste0(root.data.in,tile.folnames[i],"/d",sel.depth,"/")
  len.files<- list.files(fpath1, pattern = file.in, full.names=F)
  if(length(len.files) == 1){
    len.files.long<- list.files(fpath1, pattern = file.in, full.names=T)
    r1<- raster::raster(len.files.long)
    raster_list[[cnt]]<- r1
    cnt<- cnt + 1} else {print(paste0("missing_tile: ", fpath1))}
}


#raster_list
raster_list$filename <- paste0(root.mosaic,file.in, "_legway.tif")
raster_list$datatype <- "FLT4S"
raster_list$overwrite <- TRUE
raster_list$na.rm <- TRUE

# do the mosaic
mos <- do.call(merge, raster_list)

## slurm outputs
itOuts<- c(runs,as.character(Sys.time()))
nmy<- paste0(root.slurm,"slurmckeck_legway",file.in,"_", runs, ".txt")
write.table(itOuts, file = nmy, row.names = F, col.names = F, sep=",")




