### TERN LANDSCAPES 
# total N
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 8.11.23
# modified: 8.11.23

# CODE PURPOSE
# Derive quantiles of the model errors for each cluster configuration


# roots
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_N/"
cluster.out<- paste0(g.root,"models/uncertainty/cluster_centroids/")
site.root<- paste0(g.root, "models/ranger_models/data_obs_preds/val/")


input.data<- readRDS(file = paste0(cluster.out, "cluster_centers_totalN_all.rds"))
site.data<- read.table(file = paste0(site.root, "ranger_VAL_preds_average_summaries_totalN_depth_alldepth.txt"),header = T, sep = ",")
hist(site.data$residual_avg)

names(site.data)

# distribution of model errors
hist(site.data$residual_avg)


# cluster data
names(input.data)
cluster.data<- input.data[,7:27]


# error quantiles
pqants<- c(0.005,0.0125,0.025,0.05,0.1,0.2,0.3,0.4,0.45,0.475,
           0.5250,0.5500,0.6000,0.7000,0.8000,0.9000,0.9500,0.9750,0.9875,0.9950)
length(pqants)

for (i in 1:ncol(cluster.data)){
  
  sel.clus<- length(unique(cluster.data[,i]))
  sel.clus.dat<- cluster.data[,i]
  # matrix to save quantiles
  out.quants<- matrix(NA, nrow = sel.clus, ncol = length(pqants)+1 )
  for (j in 1:sel.clus){
    out.quants[j,1]<- j
    sel.rows<- which(sel.clus.dat == j)
    sel.errors<- site.data$residual_avg[sel.rows]
    sel.quants<- quantile(sel.errors, probs = pqants,type = 7)
    out.quants[j,2:ncol(out.quants)]<- sel.quants}
  out.quants<- as.data.frame(out.quants)
  names(out.quants)[1]<- "cluster_nos"  
  names(out.quants)[2:ncol(out.quants)]<- pqants
  # save output
  saveRDS(object = out.quants,file = paste0(cluster.out,"cluster_errors/",i,"_classNos_",sel.clus,"_cluster_errors_cluster.rds"))
  print(i)}

## END


