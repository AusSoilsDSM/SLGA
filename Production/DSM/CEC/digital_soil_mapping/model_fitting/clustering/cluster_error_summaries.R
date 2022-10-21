### TERN LANDSCAPES 
# Soil CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 16.12.21
# modified: 28.01.22

# CODE PURPOSE
# Derive quantiles of the model errors for each cluster configuration


# roots
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/"
input.root<- paste0(g.root,"models/cec/goofs/")
cluster.out<- paste0(g.root,"models/cec/cluster_centroids/")

input.data<- readRDS(file = paste0(cluster.out, "summaries/cluster_centers_oob_cec_modelPreds_all.rds"))


names(input.data)

input.data$model_error<- input.data$obs.means - input.data$rk.mat.means
summary(input.data$model_error)
hist(input.data$model_error)


# cluster data
cluster.data<- input.data[,61:165]
clus.nos<- c(1:100,200,250,500,750,1000)
clus.nos

# error quantiles
pqants<- c(0.005,0.0125,0.025,0.05,0.1,0.2,0.3,0.4,0.45,0.475,
           0.5250,0.5500,0.6000,0.7000,0.8000,0.9000,0.9500,0.9750,0.9875,0.9950)
length(pqants)

for (i in 1:length(clus.nos)){
  
  sel.clus<- clus.nos[i]
  sel.clus.dat<- cluster.data[,i]
  # matrix to save quantiles
  out.quants<- matrix(NA, nrow = sel.clus, ncol = length(pqants)+1 )
  for (j in 1:sel.clus){
    out.quants[j,1]<- j
    sel.rows<- which(sel.clus.dat == j)
    sel.errors<- input.data$model_error[sel.rows]
    sel.quants<- quantile(sel.errors, probs = pqants,type = 7)
    out.quants[j,2:ncol(out.quants)]<- sel.quants}
  out.quants<- as.data.frame(out.quants)
  names(out.quants)[1]<- "cluster_nos"  
  names(out.quants)[2:ncol(out.quants)]<- pqants
  # save output
  saveRDS(object = out.quants,file = paste0(cluster.out,"cluster_errors/",i,"_cluster_errors_cluster.rds"))
  print(i)}


