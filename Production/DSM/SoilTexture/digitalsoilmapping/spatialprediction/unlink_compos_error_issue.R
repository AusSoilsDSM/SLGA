# unlink dodgy files

library(raster)

root.tiles<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/predictions/tiles/"


### Folders where the predictions are
fols<- as.numeric(list.files(root.tiles, full.names = FALSE))
length(fols)

for (i in 1:length(fols)){
  
  #select the folder
  sfol<- fols[i]
  sfol
  nm1<- paste0(root.tiles,sfol)
  nm1
  
  # files with compositional data
  files<- list.files(path = nm1,  pattern= "compos", full.names=TRUE, recursive = F)
  files
  unlink(files)
}

