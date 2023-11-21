### TERN LANDSCAPES
# total N
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# date created: 17.11.23
# date modified: 20.11.23

# CODE PURPOSE
# mosaic all prediction outcomes [using gdal]
# outputs are cog files
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
library(sf)
#gdalPath <- '/apps/gdal/3.2.2/bin/'
gdalPath <- "/apps/gdal/3.2.2/bin/"


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
raster_list <- c() # initialise the list of rasters

file.in<- nmss[runs]

cnt<- 1
for (i in 1:length(tile.folnames)){
  fpath1<- paste0(root.data.in,tile.folnames[i],"/d",sel.depth,"/")
  len.files<- list.files(fpath1, pattern = file.in, full.names=F)
  if(length(len.files) == 1){
    len.files.long<- list.files(fpath1, pattern = file.in, full.names=T)
    raster_list[cnt]<- len.files.long
    cnt<- cnt + 1} else {print(paste0("missing_tile: ", fpath1))}
}


# write all lists to file
csvPath_1 <- paste0(root.data.out, "raster_list_", file.in,'.csv' )
write.table(raster_list, csvPath_1, row.names = F,  col.names = F, quote = F)

print('Generating VRTs')
system(paste0(gdalPath, '/gdalbuildvrt -input_file_list ',csvPath_1, ' ', paste0(root.data.out, "raster_list_", file.in,'.vrt' )))

print('Generating external overviews')
system(paste0(gdalPath, '/gdaladdo --config GDAL_CACHEMAX 5000 -ro ',paste0(root.data.out, "raster_list_", file.in,'.vrt' )))

print('Generating COG')
cmd <- paste0(gdalPath, '/gdal_translate ', paste0(root.data.out, "raster_list_", file.in,'.vrt' ), ' ', root.mosaic, file.in, '-COG.tif --config GDAL_CACHEMAX 5000 -stats -co TILED=YES -co COMPRESS=DEFLATE -co BIGTIFF=YES -co COPY_SRC_OVERVIEWS=YES' ) 
system(cmd)


## slurm outputs
itOuts<- c(runs,as.character(Sys.time()))
nmy<- paste0(root.slurm,"slurmckeck_",file.in,"_", runs, ".txt")
write.table(itOuts, file = nmy, row.names = F, col.names = F, sep=",")

# END


