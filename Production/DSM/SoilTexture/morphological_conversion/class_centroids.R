## NSC data
## matched lab data with morphological descriptions
## TODO: Define centroids for given soil texture classes

library(Compositional)

data<- read.csv(file = "S:/projects/ternlandscapes_2019/soiltexture/data/texture_rawdat_4_centroids.csv")

# Number of unique soil texture grades
codes<- length(unique(data$h_texture.of.morph_lab_texturedata_clay))

# levels
levs<- levels(data$h_texture.of.morph_lab_texturedata_clay)

# outputs
clayF<- c()
siltF<- c()
sandF<- c()
cntF<- c()


for (i in 1:codes){
  sel<- data[data$h_texture.of.morph_lab_texturedata_clay == levs[i],]
  #clay
  p1<- median(sel$texturedata_clay)
  #silt
  p2<- median(sel$texturedata_silt)
  #sand
  #clay
  p3<- median(sel$texturedata_sand)
  #sum
  ps<- sum(p1,p2,p3)
  #adjust
  p1<- p1/ps
  p2<- p2/ps
  p3<- p3/ps
  p4<- nrow(sel)
  #append
  clayF<- c(clayF,p1)
  siltF<- c(siltF,p2)
  sandF<- c(sandF,p3)
  cntF<- c(cntF,p4)}

# data table
centroids<- data.frame(levs,cntF, clayF, siltF, sandF)
write.csv(x = centroids, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/texture_class_centroids.csv")



## Some Checking
# sample from distribution


# actual data
sel<- data[data$h_texture.of.morph_lab_texturedata_clay == "C",]
hist(sel$texturedata_clay,add=F)
#hist(x[,1]*100, add=F)

centroids$levs
sel1<- centroids[centroids$levs =="C",]
sel1
x <- rdiri(n = nrow(sel), c(sel1[,"clayF"]*100, sel1[,"siltF"]*100, sel1[,"sandF"]*100) )
hist(x[,1])
plot(sel$texturedata_clay, x[,1])

