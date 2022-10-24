### TERN LANDSCAPES
# pH
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# date created: 20.10.22
# date modified: 20.10.22

# CODE PURPOSE
# mosaic of coobs outputs
# depth 1
#

# fixed parameters
args = commandArgs(trailingOnly = T)
sel.depth<- as.numeric(args[1])
runs<- as.numeric(args[2])
vart<-  "ph_4b"

nmss<- c(paste0(vart, "_d",sel.depth, "_geomsimsFP"),
         paste0("RKpred_", vart, "_lowerPI_d",sel.depth), 
         paste0("RKpred_", vart, "_mean_d",sel.depth),
         paste0("RKpred_", vart, "_range_d",sel.depth),
         paste0("RKpred_", vart, "_upperPI_d",sel.depth))

## libraries
library(sp);library(rgdal);library(raster)
gdalPath <- '/apps/gdal/3.2.2/bin/'
#gdalPath <- "/apps/gdal/3.1.2/bin/"


# root directory
g.root<-  "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
root.data.in<- paste0(g.root,"/predictions/tiles/")
root.data.out<- paste0(g.root,"/data/mosaics/",vart, "/")
root.mosaic<- paste0(g.root,"/predictions/d",sel.depth,"/", vart, "/")
root.slurm<- paste0(g.root,"/rcode/slurm/",vart,"/temp_outs/digital_soil_mapping/mosaic/d",sel.depth,"/")

# tile names
tile.folnames<- as.numeric(list.dirs(path = root.data.in,full.names = F,recursive = F))
tile.folnames<- tile.folnames[order(tile.folnames)]
#tile.folnames

# organism raster list
raster_list <- c() # initialise the list of rasters

file.in<- nmss[runs]

#for (i in 1:50){
for (i in 1:length(tile.folnames)){
  fpath1<- paste0(root.data.in,tile.folnames[i],"/d",sel.depth,"/", vart, "/")
  len.files<- list.files(fpath1, pattern = file.in, full.names=F)
  if(length(len.files) == 1){
    len.files.long<- list.files(fpath1, pattern = file.in, full.names=T)
    raster_list[i]<- len.files.long} else {break}
  #print(i)
}


# write all lists to file
csvPath_1 <- paste0(root.data.out, "raster_list_", file.in,'.csv' )
write.table(raster_list, csvPath_1, row.names = F,  col.names = F, quote = F)

print('Generating VRTs')
system(paste0(gdalPath, '/gdalbuildvrt -input_file_list ',csvPath_1, ' ', paste0(root.data.out, "raster_list_", file.in,'.vrt' )))

print('Generating COG')
cmd <- paste0(gdalPath, '/gdal_translate ', paste0(root.data.out, "raster_list_", file.in,'.vrt' ), ' ', root.mosaic, file.in, '-COG.tif -of COG -co COMPRESS=LZW -co BIGTIFF=YES -co PREDICTOR=YES' ) 
system(cmd)

## slurm outputs
itOuts<- c(runs,as.character(Sys.time()))
nmy<- paste0(root.slurm,"slurmckeck_",file.in,"_", runs, ".txt")
write.table(itOuts, file = nmy, row.names = F, col.names = F, sep=",")

# END


