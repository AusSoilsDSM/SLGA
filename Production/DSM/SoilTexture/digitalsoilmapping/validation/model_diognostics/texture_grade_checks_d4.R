## soil texture class validation
library(compositions);library(matrixStats)

root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/"


# class centroids
class.centroids<- read.csv(file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/texture_class_centroids_ordered.csv")
class.centroids<- class.centroids[1:20,]

#columns for compositions
class.centroids$comp1_clay<- NA
class.centroids$comp2_sand<- NA
names(class.centroids)

# cycle through each row and estimate the ILR composiiton
for (i in 1:nrow(class.centroids)){
  it<- ilr(class.centroids[i,c(4,6,5)])
  itc<- c(it)
  itc
  ilrInv(itc)
  class.centroids[i,7:8]<- itc
  print(i)}
cov.mat<- as.matrix(cov(class.centroids[,7:8]))



## DEPTH 3
### clay external observations and predictions: depth 4
vart<- "clay"
depth<- "d4"
clay.files<- list.files(path = paste0(root, vart, "/", depth, "/"), pattern = "ranger_EXT_preds", full.names = T, recursive = F)
clay.files

clay.dat<- read.table(clay.files[1],header = T,sep = ",")
names(clay.dat)
clay.dat<- clay.dat[,1:6]

for (i in 1:length(clay.files)){
  datz<- read.table(clay.files[i],header = T,sep = ",")
  clay.dat<- cbind(clay.dat, datz$prediction)
}
names(clay.dat)


### sand external observations and predictions: depth 4
vart<- "sand"
depth<- "d4"
sand.files<- list.files(path = paste0(root, vart, "/", depth, "/"), pattern = "ranger_EXT_preds", full.names = T, recursive = F)
sand.files

sand.dat<- read.table(sand.files[1],header = T,sep = ",")
names(sand.dat)
sand.dat<- sand.dat[,1:6]


for (i in 1:length(sand.files)){
  datz<- read.table(sand.files[i],header = T,sep = ",")
  sand.dat<- cbind(sand.dat, datz$prediction)}
names(sand.dat)


d1.summary<- matrix(NA, nrow= nrow(sand.dat), ncol=10) # summary outputs

for (g in 1:10){
  d1.check<- matrix(NA, nrow= nrow(sand.dat), ncol=51) # iterative summary check
  for (j in 1:nrow(clay.dat)){
    cnt<- 1
    
    for (z in 7:56){
      
      # prediction and observed value
      pred.vals<- t(as.matrix(c(clay.dat[j,z],sand.dat[j,z])))
      obs.vals<- t(as.matrix(c(clay.dat$target[j],sand.dat$target[j])))
      
      # mahalanobis distance
      x.vals<- as.matrix(class.centroids[,7:8])
      obs.dist<- mahalanobis(x = x.vals,center = obs.vals,cov = cov.mat)
      pred.dist<- mahalanobis(x = x.vals,center = pred.vals,cov = cov.mat)
      dist.sum<-  sum(as.numeric(order(pred.dist)[1:g] %in% order(obs.dist)[1:g]))
      if (dist.sum >= 1){d1.check[j,cnt]<- 1}else{d1.check[j,cnt]<- 0}
      cnt<- cnt + 1} 
    # summarise the iteration
    d1.check[j,51]<- (sum(d1.check[j,1:50])/50)*100
    print(j)}
  d1.summary[,g]<- d1.check[,51]}

# save output
d1.summary<- as.data.frame(d1.summary)
saveRDS(d1.summary, file = paste0(root, "diognostics/d4_texture_class_check.rds"))








