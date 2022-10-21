# txture class diognostics

root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/diognostics/"

files<- list.files(path = root, full.names = T)
files

# depth 1
tc.out<- readRDS(file = files[1]) 

d1.vect<- c()
for (i in 1:10){
  d1.vect[i]<- length(which(tc.out[,i] >= 75))/nrow(tc.out)
}
d1.vect


# depth 2
tc.out<- readRDS(file = files[2]) 

d2.vect<- c()
for (i in 1:10){
  d2.vect[i]<- length(which(tc.out[,i] >= 75))/nrow(tc.out)
}
d2.vect


# depth 3
tc.out<- readRDS(file = files[3]) 

d3.vect<- c()
for (i in 1:10){
  d3.vect[i]<- length(which(tc.out[,i] >= 75))/nrow(tc.out)
}
d3.vect


# depth 4
tc.out<- readRDS(file = files[4]) 

d4.vect<- c()
for (i in 1:10){
  d4.vect[i]<- length(which(tc.out[,i] >= 75))/nrow(tc.out)
}
d4.vect


# depth 5
tc.out<- readRDS(file = files[5]) 

d5.vect<- c()
for (i in 1:10){
  d5.vect[i]<- length(which(tc.out[,i] >= 75))/nrow(tc.out)
}
d5.vect


# depth 6
tc.out<- readRDS(file = files[6]) 

d6.vect<- c()
for (i in 1:10){
  d6.vect[i]<- length(which(tc.out[,i] >= 75))/nrow(tc.out)
}
d6.vect


out.vect<- rbind(d1.vect,d2.vect,d3.vect,d4.vect,d5.vect,d6.vect)
saveRDS(out.vect, file = paste0(root, "compiled_texture_class_summaries_extdata.rds") )
