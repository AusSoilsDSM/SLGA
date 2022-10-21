## soil texture class validation
library(compositions);library(matrixStats)

root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/"
out.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/SLGA_V1_ext_val/"
list.files(out.root)

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

# SLGA data
# Depth 1
depth<- "d1"
dat<- readRDS(file = paste0(out.root, "slga_v1_data_",depth,"_.rds"))
names(dat)
se.dat<- dat[,c(1:13,16,34,52)]

#columns for compositions
se.dat$comp1_clay<- NA
se.dat$comp2_sand<- NA
names(se.dat)

for (i in 1:nrow(se.dat)){
  it<- ilr(se.dat[i,c(14:16)])
  itc<- c(it)
  itc
  ilrInv(itc)
  se.dat[i,17:18]<- itc
  print(i)}


d1.summary<- matrix(NA, nrow= nrow(se.dat), ncol=10) # summary outputs

for (g in 1:10){
  for (j in 1:nrow(se.dat)){
    
    # prediction and observed value
    pred.vals<- as.matrix(se.dat[j,17:18])
    obs.vals<- as.matrix(se.dat[j,6:7])
      
      # mahalanobis distance
      x.vals<- as.matrix(class.centroids[,7:8])
      obs.dist<- mahalanobis(x = x.vals,center = obs.vals,cov = cov.mat)
      pred.dist<- mahalanobis(x = x.vals,center = pred.vals,cov = cov.mat)
      dist.sum<-  sum(as.numeric(order(pred.dist)[1:g] %in% order(obs.dist)[1:g]))
      if (dist.sum >= 1){d1.summary[j,g]<- 1}else{d1.summary[j,g]<- 0}}
 print(g)}

# save output
d1.summary<- as.data.frame(d1.summary)
saveRDS(d1.summary, file = paste0(root, "diognostics/d1_texture_class_check_SLGA_v1_check.rds"))
d1.out<- colSums(d1.summary)/nrow(d1.summary)*100
plot(seq(from = 1, to = 10,length.out = 10),d1.out)


# Depth 2
depth<- "d2"
dat<- readRDS(file = paste0(out.root, "slga_v1_data_",depth,"_.rds"))
names(dat)
se.dat<- dat[,c(1:13,19,37,55)]

#columns for compositions
se.dat$comp1_clay<- NA
se.dat$comp2_sand<- NA
names(se.dat)

for (i in 1:nrow(se.dat)){
  it<- ilr(se.dat[i,c(14:16)])
  itc<- c(it)
  itc
  ilrInv(itc)
  se.dat[i,17:18]<- itc
  print(i)}


d1.summary<- matrix(NA, nrow= nrow(se.dat), ncol=10) # summary outputs

for (g in 1:10){
  for (j in 1:nrow(se.dat)){
    
    # prediction and observed value
    pred.vals<- as.matrix(se.dat[j,17:18])
    obs.vals<- as.matrix(se.dat[j,6:7])
    
    # mahalanobis distance
    x.vals<- as.matrix(class.centroids[,7:8])
    obs.dist<- mahalanobis(x = x.vals,center = obs.vals,cov = cov.mat)
    pred.dist<- mahalanobis(x = x.vals,center = pred.vals,cov = cov.mat)
    dist.sum<-  sum(as.numeric(order(pred.dist)[1:g] %in% order(obs.dist)[1:g]))
    if (dist.sum >= 1){d1.summary[j,g]<- 1}else{d1.summary[j,g]<- 0}}
  print(g)}

# save output
d1.summary<- as.data.frame(d1.summary)
saveRDS(d1.summary, file = paste0(root, "diognostics/d2_texture_class_check_SLGA_v1_check.rds"))
d2.out<- colSums(d1.summary)/nrow(d1.summary)*100
plot(seq(from = 1, to = 10,length.out = 10),d2.out)


# Depth 3
depth<- "d3"
dat<- readRDS(file = paste0(out.root, "slga_v1_data_",depth,"_.rds"))
names(dat)
se.dat<- dat[,c(1:13,22,40,58)]

#columns for compositions
se.dat$comp1_clay<- NA
se.dat$comp2_sand<- NA
names(se.dat)

for (i in 1:nrow(se.dat)){
  it<- ilr(se.dat[i,c(14:16)])
  itc<- c(it)
  itc
  ilrInv(itc)
  se.dat[i,17:18]<- itc
  print(i)}


d1.summary<- matrix(NA, nrow= nrow(se.dat), ncol=10) # summary outputs

for (g in 1:10){
  for (j in 1:nrow(se.dat)){
    
    # prediction and observed value
    pred.vals<- as.matrix(se.dat[j,17:18])
    obs.vals<- as.matrix(se.dat[j,6:7])
    
    # mahalanobis distance
    x.vals<- as.matrix(class.centroids[,7:8])
    obs.dist<- mahalanobis(x = x.vals,center = obs.vals,cov = cov.mat)
    pred.dist<- mahalanobis(x = x.vals,center = pred.vals,cov = cov.mat)
    dist.sum<-  sum(as.numeric(order(pred.dist)[1:g] %in% order(obs.dist)[1:g]))
    if (dist.sum >= 1){d1.summary[j,g]<- 1}else{d1.summary[j,g]<- 0}}
  print(g)}

# save output
d1.summary<- as.data.frame(d1.summary)
saveRDS(d1.summary, file = paste0(root, "diognostics/d3_texture_class_check_SLGA_v1_check.rds"))
d3.out<- colSums(d1.summary)/nrow(d1.summary)*100
plot(seq(from = 1, to = 10,length.out = 10),d3.out)


# Depth 4
depth<- "d4"
dat<- readRDS(file = paste0(out.root, "slga_v1_data_",depth,"_.rds"))
se.dat<- dat[,c(1:13,25,43,61)]

#columns for compositions
se.dat$comp1_clay<- NA
se.dat$comp2_sand<- NA
names(se.dat)

for (i in 1:nrow(se.dat)){
  it<- ilr(se.dat[i,c(14:16)])
  itc<- c(it)
  itc
  ilrInv(itc)
  se.dat[i,17:18]<- itc
  print(i)}


d1.summary<- matrix(NA, nrow= nrow(se.dat), ncol=10) # summary outputs

for (g in 1:10){
  for (j in 1:nrow(se.dat)){
    
    # prediction and observed value
    pred.vals<- as.matrix(se.dat[j,17:18])
    obs.vals<- as.matrix(se.dat[j,6:7])
    
    # mahalanobis distance
    x.vals<- as.matrix(class.centroids[,7:8])
    obs.dist<- mahalanobis(x = x.vals,center = obs.vals,cov = cov.mat)
    pred.dist<- mahalanobis(x = x.vals,center = pred.vals,cov = cov.mat)
    dist.sum<-  sum(as.numeric(order(pred.dist)[1:g] %in% order(obs.dist)[1:g]))
    if (dist.sum >= 1){d1.summary[j,g]<- 1}else{d1.summary[j,g]<- 0}}
  print(g)}

# save output
d1.summary<- as.data.frame(d1.summary)
saveRDS(d1.summary, file = paste0(root, "diognostics/d4_texture_class_check_SLGA_v1_check.rds"))
d4.out<- colSums(d1.summary)/nrow(d1.summary)*100
plot(seq(from = 1, to = 10,length.out = 10),d4.out)


# Depth 5
depth<- "d5"
dat<- readRDS(file = paste0(out.root, "slga_v1_data_",depth,"_.rds"))
se.dat<- dat[,c(1:13,28,46,64)]

#columns for compositions
se.dat$comp1_clay<- NA
se.dat$comp2_sand<- NA
names(se.dat)

for (i in 1:nrow(se.dat)){
  it<- ilr(se.dat[i,c(14:16)])
  itc<- c(it)
  itc
  ilrInv(itc)
  se.dat[i,17:18]<- itc
  print(i)}


d1.summary<- matrix(NA, nrow= nrow(se.dat), ncol=10) # summary outputs

for (g in 1:10){
  for (j in 1:nrow(se.dat)){
    
    # prediction and observed value
    pred.vals<- as.matrix(se.dat[j,17:18])
    obs.vals<- as.matrix(se.dat[j,6:7])
    
    # mahalanobis distance
    x.vals<- as.matrix(class.centroids[,7:8])
    obs.dist<- mahalanobis(x = x.vals,center = obs.vals,cov = cov.mat)
    pred.dist<- mahalanobis(x = x.vals,center = pred.vals,cov = cov.mat)
    dist.sum<-  sum(as.numeric(order(pred.dist)[1:g] %in% order(obs.dist)[1:g]))
    if (dist.sum >= 1){d1.summary[j,g]<- 1}else{d1.summary[j,g]<- 0}}
  print(g)}

# save output
d1.summary<- as.data.frame(d1.summary)
saveRDS(d1.summary, file = paste0(root, "diognostics/d5_texture_class_check_SLGA_v1_check.rds"))
d5.out<- colSums(d1.summary)/nrow(d1.summary)*100
plot(seq(from = 1, to = 10,length.out = 10),d5.out)


# Depth 6
depth<- "d6"
dat<- readRDS(file = paste0(out.root, "slga_v1_data_",depth,"_.rds"))
se.dat<- dat[,c(1:13,31,49,67)]


#columns for compositions
se.dat$comp1_clay<- NA
se.dat$comp2_sand<- NA
names(se.dat)

for (i in 1:nrow(se.dat)){
  it<- ilr(se.dat[i,c(14:16)])
  itc<- c(it)
  itc
  ilrInv(itc)
  se.dat[i,17:18]<- itc
  print(i)}


d1.summary<- matrix(NA, nrow= nrow(se.dat), ncol=10) # summary outputs

for (g in 1:10){
  for (j in 1:nrow(se.dat)){
    
    # prediction and observed value
    pred.vals<- as.matrix(se.dat[j,17:18])
    obs.vals<- as.matrix(se.dat[j,6:7])
    
    # mahalanobis distance
    x.vals<- as.matrix(class.centroids[,7:8])
    obs.dist<- mahalanobis(x = x.vals,center = obs.vals,cov = cov.mat)
    pred.dist<- mahalanobis(x = x.vals,center = pred.vals,cov = cov.mat)
    dist.sum<-  sum(as.numeric(order(pred.dist)[1:g] %in% order(obs.dist)[1:g]))
    if (dist.sum >= 1){d1.summary[j,g]<- 1}else{d1.summary[j,g]<- 0}}
  print(g)}

# save output
d1.summary<- as.data.frame(d1.summary)
saveRDS(d1.summary, file = paste0(root, "diognostics/d6_texture_class_check_SLGA_v1_check.rds"))
d6.out<- colSums(d1.summary)/nrow(d1.summary)*100
plot(seq(from = 1, to = 10,length.out = 10),d6.out)


#save output
out.dat<- rbind(d1.out,d2.out,d3.out,d4.out,d5.out,d6.out)
saveRDS(out.dat, file ="/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/diognostics/compiled_texture_class_summaries_extdata_SLGA_v1_check.rds")

   
  





