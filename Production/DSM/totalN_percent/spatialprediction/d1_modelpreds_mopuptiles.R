### TERN LANDSCAPES 
# Bulk Density
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 6.12.22
# modified: 6.12.22

# CODE PURPOSE
# Find tiles that were missed or passed over by HPC
##

# root directories
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/bulk_density/"
file.root<- paste0(g.root, "rcode/slurm/outs/digital_soil_mapping/spatialprediction/ranger/")
tile.root<- paste0(g.root, "predictions/tiles/")

fols<- as.numeric(list.files(tile.root, full.names = FALSE))
fols<- fols[order(fols)]
length(fols)
fols


depths<- c("d1", "d2", "d3", "d4", "d5", "d6")


# list the slurm files
files<- list.files(path = paste0(file.root,depths[6], "/outs/"), full.names = F, recursive = F)
#files
files<- substr(x = files,start = 1, stop = nchar(files)-4)
files<- strsplit(files, split = "_")
files<- as.numeric(sapply(files, `[`, 1))
files<- files[order(files)]
#files

it_cnt<- 1:2172

missings<- it_cnt[!it_cnt %in% files]
missings
fols[missings]

# write to file
saveRDS(object = missings, file = paste0(g.root, "data/miscells/",depths[1],"_missingtile_modelpreds.rds"))



fols[missings]
