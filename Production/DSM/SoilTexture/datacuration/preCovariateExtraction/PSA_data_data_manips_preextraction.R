## Soil texture digital soil mapping

## Prep data prior to covariate data intersection

# Lab data
lab.dat<- readRDS(file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/process2/combined_lab_PSA_data.rds")
lab.dat$Observation_ID<- as.character(lab.dat$Observation_ID)
unique.ids<- unique(lab.dat$Observation_ID)

lab.dat$num_obs_ID<- NA
lab.dat$num_horiz_ID<- NA

for (i in 1:length(unique(lab.dat$Observation_ID))){
  sel.id<- unique.ids[i]
  sel.id
  sub<- lab.dat[which(lab.dat$Observation_ID == sel.id),]
  sub$num_obs_ID<- i
  sub$num_horiz_ID<- c(1:nrow(sub))
  lab.dat[which(lab.dat$Observation_ID == sel.id),]<- sub
  print(i)
  
}
saveRDS(object = lab.dat, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/process2/combined_lab_PSA_data.rds")

# 17892 number of lab data profiles

## morphological data
morph.files<- list.files(path = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/process4/morph_sims",full.names = T)


morph.dat1<- readRDS(morph.files[1])

morph.dat1$Observation_ID<- as.character(morph.dat1$Observation_ID)
unique.ids<- unique(morph.dat1$Observation_ID)
length(unique.ids)

morph.dat1$num_obs_ID<- NA
morph.dat1$num_horiz_ID<- NA

cnt<- 17892
for (i in 1:length(unique(morph.dat1$Observation_ID))){
  sel.id<- unique.ids[i]
  sel.id
  sub<- morph.dat1[which(morph.dat1$Observation_ID == sel.id),]
  sub$num_obs_ID<- i + cnt
  sub$num_horiz_ID<- c(1:nrow(sub))
  morph.dat1[which(morph.dat1$Observation_ID == sel.id),]<- sub
  print(i)
  
}
