### TERN LANDSCAPES
# CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# date created: 3.3.22
# date modified: 3.3.22

# CODE PURPOSE
# mosaic of coobs outputs
# depth 1 Median estimate
#


## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster)
vars<- c("rkpred_median_BT", "rkpred_rangePI_BT", "rkpred_upperPI_BT", "rkpred_lowerPI_BT")
gdalPath <- '/apps/gdal/3.2.2/bin/'
#gdalPath <- "/apps/gdal/3.1.2/bin/"

# select depth
depth<- 6
sel.vars<- vars[4]

# root directory
root.in<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/predictions/90m/tiles/cec/"
root.data<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/mosaic/"
root.mosaic<- paste0("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/predictions/90m/d",depth,"/")
slurm.file<- paste0("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/rcode/digital_soil_mapping/mosaic/model_preds/slurm/d",depth,"/")
r.file<- paste0("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/rcode/digital_soil_mapping/mosaic/model_preds/d",depth,"/")


# tile names
tile.folnames<- as.numeric(list.dirs(path = root.in,full.names = F,recursive = F))
tile.folnames<- tile.folnames[order(tile.folnames)]
tile.folnames

# organism raster list
raster_list <- c() # initialise the list of rasters

file.in<- paste0(sel.vars,"_d",depth)
file.in

#for (i in 1:50){
for (i in 1:length(tile.folnames)){
  fpath1<- paste0(root.in,tile.folnames[i],"/d",depth,"/")
  len.files<- list.files(fpath1, pattern = file.in, full.names=F)
  if(length(len.files) == 1){
    len.files.long<- list.files(fpath1, pattern = file.in, full.names=T)
    raster_list[i]<- len.files.long} else {break}
  print(i)
}


# write all lists to file
csvPath_1 <- paste0(root.data, "Rraster_list_",sel.vars,"_d",depth,'.csv')
csvPath_1
write.table(raster_list, csvPath_1, row.names = F,  col.names = F, quote = F)

print('Generating VRTs')
system(paste0(gdalPath, '/gdalbuildvrt -srcnodata -32768 -input_file_list ',csvPath_1, ' ', paste0(root.data, "Rraster_list_",sel.vars,"_d",depth,'.vrt' )))

print('Generating COG')
cmd <- paste0(gdalPath, '/gdal_translate ', paste0(root.data, "Rraster_list_",sel.vars,"_d",depth,'.vrt' ), ' ', root.mosaic, file.in, '-COG.tif -of COG -co COMPRESS=LZW -co BIGTIFF=YES -co PREDICTOR=YES' ) 
system(cmd)

# END


