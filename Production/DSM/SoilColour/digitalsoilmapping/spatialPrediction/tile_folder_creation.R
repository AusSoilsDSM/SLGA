## BARE EARTH IMAGERY
# TILE CREATION: 

root<- "Z:/projects/ternlandscapes_2019/soilColour/spatialPredictions/tiles/"
home.root<- "Z:/datasets/national/covariates/tiles/"


tile.dirs<- list.dirs(path = home.root, recursive = F,full.names = F)
tile.dirs

aux_fols<- paste0("model_",c(1:12))
aux_fols

for (i in 1:length(tile.dirs)){
  sel<- tile.dirs[i]
  dir.create(paste0(root,sel))
  sel.dir<- paste0(root,sel,"/")
  for (j in 1:length(aux_fols)){
    add.dir<- paste0(sel.dir,aux_fols[j])
    dir.create(add.dir)}
  print(i)
}
