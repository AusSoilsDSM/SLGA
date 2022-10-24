
## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster)

source.tiles<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/spatialPredictions/tiles/"
root.slurm<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/rcode/slurm/spatialprediction/rgb/type1/"

fols<- as.numeric(list.files(source.tiles, full.names = FALSE))
length(fols)
fols<- fols[order(fols)]
fols
length(fols)

files<- list.files(root.slurm,pattern = "slurmckeck_",full.names = F,all.files = T)
files<- substr(files,start = 1,stop = nchar(files)-4)
files

files.split<- strsplit(files,"_")
sca.num<- as.numeric(sapply(files.split, `[`, 3))
sca.num<- sca.num[order(sca.num)]

ss<- match(fols,sca.num)
which(is.na(ss))

fols[which(is.na(ss))]
