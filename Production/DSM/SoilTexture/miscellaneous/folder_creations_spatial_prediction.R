## creation of folders to put outputs in
## tern soil texture


# the folders to create
tile.dirs<- list.dirs(path = "Z:/projects/soilDepth_2019/soilDepth/spatialPrediction/tiles/",recursive = F, full.names = F)


# root directory
root<- "Z:/projects/ternlandscapes_2019/soiltexture/predictions/tiles/"

# create tile folders
for (i in 1:length(tile.dirs)){
  dir.create(path = paste0(root,tile.dirs[i]), showWarnings = F)
}

# create variable folders within each tile
vart<- c("clay", "sand")
for (i in 1:length(tile.dirs)){
  dir.create(paste0(root,tile.dirs[i],"/",vart[1]))
  dir.create(paste0(root,tile.dirs[i],"/",vart[2]))}


# within each variable create folders for each 
for (i in 1:length(vart)){
  print(vart[i])
  for (j in 1:length(tile.dirs)){
    print(tile.dirs[j])
    for(k in 1:50){   # iteration
      it.lab<- paste0("it_",k)
      dir.create(paste0(root,tile.dirs[j],"/",vart[i],"/",it.lab),showWarnings = F)
      for (l in 1:6){  # depth
        d.lab<- paste0("d",l)
        dir.create(paste0(root,tile.dirs[j],"/",vart[i],"/",it.lab,"/",d.lab),showWarnings = F)
        }
    }
    
  }
  }






