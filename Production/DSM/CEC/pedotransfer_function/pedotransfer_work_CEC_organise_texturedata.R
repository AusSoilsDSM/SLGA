### TERN Landscapes 
# Cation Exchange Capacity
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 12.11.21
# modified: 12.11.21

# CODE PURPOSE
# load and and prepare all available soil texture data
##


# root
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/data/federator/soiltexture/"


# lab measured data
lab.psa<- readRDS(paste0(g.root,"process5/lab_psa_data_splined_process5.rds"))

# texture data simulations of morphological data
morph.files<- list.files(path = paste0(g.root, "process5/morph_sims/"), pattern = "morph_psa_data_splined_dsm_extract_prepared",full.names = T)

## Compile all simulations to calculate an average soil texture fraction
# create a bunch of matrices
clay_1<- matrix(data = NA, nrow = 189113, ncol= 50)
clay_2<- matrix(data = NA, nrow = 189113, ncol= 50)
clay_3<- matrix(data = NA, nrow = 189113, ncol= 50)
clay_4<- matrix(data = NA, nrow = 189113, ncol= 50)
clay_5<- matrix(data = NA, nrow = 189113, ncol= 50)
clay_6<- matrix(data = NA, nrow = 189113, ncol= 50)

sand_1<- matrix(data = NA, nrow = 189113, ncol= 50)
sand_2<- matrix(data = NA, nrow = 189113, ncol= 50)
sand_3<- matrix(data = NA, nrow = 189113, ncol= 50)
sand_4<- matrix(data = NA, nrow = 189113, ncol= 50)
sand_5<- matrix(data = NA, nrow = 189113, ncol= 50)
sand_6<- matrix(data = NA, nrow = 189113, ncol= 50)

silt_1<- matrix(data = NA, nrow = 189113, ncol= 50)
silt_2<- matrix(data = NA, nrow = 189113, ncol= 50)
silt_3<- matrix(data = NA, nrow = 189113, ncol= 50)
silt_4<- matrix(data = NA, nrow = 189113, ncol= 50)
silt_5<- matrix(data = NA, nrow = 189113, ncol= 50)
silt_6<- matrix(data = NA, nrow = 189113, ncol= 50)

clay.names<- c(19:24)
sand.names<- c(25:30)
silt.names<- c(31:36)

for (i in 1:length(morph.files)){
  
  temp.dat<- readRDS(morph.files[i])
  clay.t<- temp.dat[,clay.names]
  sand.t<- temp.dat[,sand.names]
  silt.t<- temp.dat[,silt.names]
  
  # put into matrices
  clay_1[,i]<- clay.t[,1]
  clay_2[,i]<- clay.t[,2]
  clay_3[,i]<- clay.t[,3]
  clay_4[,i]<- clay.t[,4]
  clay_5[,i]<- clay.t[,5]
  clay_6[,i]<- clay.t[,6]
  
  sand_1[,i]<- sand.t[,1]
  sand_2[,i]<- sand.t[,2]
  sand_3[,i]<- sand.t[,3]
  sand_4[,i]<- sand.t[,4]
  sand_5[,i]<- sand.t[,5]
  sand_6[,i]<- sand.t[,6]
  
  silt_1[,i]<- silt.t[,1]
  silt_2[,i]<- silt.t[,2]
  silt_3[,i]<- silt.t[,3]
  silt_4[,i]<- silt.t[,4]
  silt_5[,i]<- silt.t[,5]
  silt_6[,i]<- silt.t[,6]
  
  print(i)}


# calculate row means
morph.psa<- readRDS(paste0(g.root, "process5/morph_sims/morph_psa_data_splined_dsm_extract_prepared_sim_1.rds"))
names(morph.psa)
morph.psa<- morph.psa[,c(1:6,19:36)]

morph.psa[,7]<- rowMeans(clay_1)
morph.psa[,8]<- rowMeans(clay_2)
morph.psa[,9]<- rowMeans(clay_3)
morph.psa[,10]<- rowMeans(clay_4)
morph.psa[,11]<- rowMeans(clay_5)
morph.psa[,12]<- rowMeans(clay_6)

morph.psa[,13]<- rowMeans(sand_1)
morph.psa[,14]<- rowMeans(sand_2)
morph.psa[,15]<- rowMeans(sand_3)
morph.psa[,16]<- rowMeans(sand_4)
morph.psa[,17]<- rowMeans(sand_5)
morph.psa[,18]<- rowMeans(sand_6)

morph.psa[,19]<- rowMeans(silt_1)
morph.psa[,20]<- rowMeans(silt_2)
morph.psa[,21]<- rowMeans(silt_3)
morph.psa[,22]<- rowMeans(silt_4)
morph.psa[,23]<- rowMeans(silt_5)
morph.psa[,24]<- rowMeans(silt_6)


# join lab and morph data
lab.psa$type<- "lab"
morph.psa$type<- "morph"
names(lab.psa)
lab.psa<- lab.psa[,c(3:6,38,7,20:37)]
names(morph.psa)
morph.psaX<- morph.psa[,c(1,3,5,6,25,4,7:24)]  
names(lab.psa)[1:6]
names(morph.psaX)[1:6]<- names(lab.psa)[1:6]

all.psa<- rbind(lab.psa,morph.psaX)
saveRDS(object = all.psa,file = paste0(g.root,"process5/lab_morph_psa_data_splined_naturalunits.rds"))



