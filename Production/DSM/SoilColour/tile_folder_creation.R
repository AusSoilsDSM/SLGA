# folder creation for NIR iterative predictions

root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/spatialPredictions/tiles/"

fols<- as.numeric(list.files(root, full.names = FALSE))
length(fols)
fols<- fols[order(fols)]
fols


for (i in 1:length(fols)){
  
  #make path
  se.fol<- fols[i]
  se.fol
  pat<- paste0(root,se.fol,"/NIR_its/")
  pat
  dir.create(pat)
  print(i)}
