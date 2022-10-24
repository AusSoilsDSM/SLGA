### TERN LANDSCAPES 
# Soil pH
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 10.10.22
# modified: 14.10.22

# CODE PURPOSE
# Mop up of missing tiles due to issue on HPC

# root directory
depths<- 2
vart<- "ph_4b"
wstring<- "coobs"

gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
root.slurm<- paste0(gen.root,"/rcode/slurm/",vart,"/temp_outs/digital_soil_mapping/spatialprediction/",wstring,"/d",depths,"/")


its<- c(1:2172)
its

# checks
files<- list.files(path = root.slurm,pattern = ".txt")
length(files)
f1<- strsplit(files,"_")
f1_1<- as.numeric(sapply(f1, `[`, 3))
#f1_2<- as.numeric(sapply(f1, `[`, 4))
#f1_3<- c(f1_1,f1_2)
#f1_3<- f1_3[-which(is.na(f1_3))]
#duplicated(f1_3)
f1_1<- f1_1[order(f1_1)]
f1_1

miss.tiles.its<- subset(its, !(its %in% f1_1))
miss.tiles.its
length(miss.tiles.its)
saveRDS(object = miss.tiles.its, file = paste0(gen.root, "data/miscells/tilemopups/",vart,"/tilemopup_",wstring,"_",vart,"_d",depths,".rds"))

