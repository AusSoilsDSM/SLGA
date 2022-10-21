### PEDOGENONS FOR AUSTRALIA
### Date: 02/07/2022
### Author: Mercedes Roman Dobarco
### Email: mercedes.romandobarco@sydney.edu.au
### Secondary email: mercetadzio@gmail.com
### Affiliation: The University of Sydney

### Objectives:
### 1. Examine the centroids generated with k-means clustering with Python in R and explore dendrogram
### 2. Choose the Color palettes that can help identify patterns of similarities among pedogenon classes
### 3. PReprocess the Pedogenon maps generated with GEE - mosaic, correct values.

### Desired extent: Australia
### Resolution: 90 m

### Load packages
library(rgdal)
library(sp)
library(sf)
library(raster)
library(gdalUtils)
library(nngeo)
library(gstat)
library(dplyr)
library(tidyverse)
library(foreach)
library(parallel)
library(doParallel)
library(snow)
library(doSNOW)
library(lattice)
library(ggplot2)
library(rasterVis)
library(viridis)
library(scales)
library(gridExtra)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(shiny)   # for web applications
library(RColorBrewer)
library(MASS)
library(RcppCNPy)
library(ClusterR)
library(rlang)
library(ClusterR)
library(rgdal)
library(gdalUtils)
library(raster)
library(sp)
library(sf)
library(dplyr)
library(tidyverse)
library(ggmap)
library(ggplot2) 
library(viridis) # color palettes
library(scales)
library(rasterVis)
library(lattice)
library(gridExtra)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(shiny)   # for web applications
library(foreach)
library(doParallel)
library(geosphere)
library(dendsort)
library(gplots)
library(dendextend)
library(colorspace)
library(elsa)
library(vegan)
library(multcomp)
library(multcompView)
library(nlme)


# Load pedogenon centroids ------------------------------------------------

### Remember who is who
# CLORPT.27.df.vl        environmental covariates (as they come originally, no scaling, nothing) - > 9M pixels
# CLORPT.rs.27.vl        environmental covariates, rescaled with the inverse of the cholesky decomposition
# coords9M               coordinates for these >9M pixels

### Directories

### Here change with yours
HomerDir <- "C:/Users/mrom8073/Desktop/USydney/Postdoc/Projects/TERN_Deliverables/Pedogenons_Australia/" 
InputDir <- paste0(HomerDir,"Input/")
OutDir <- paste0(HomerDir,"Output/")

### Keep coordinates
setwd(OutDir)
load("CLORPT.27_vl.RData")
coords9M <- CLORPT.27.df.coords.vl
rm(CLORPT.27.df.coords.vl)

# ### What is the distance between points?
a <- pointDistance(coords9M[1:100,],
                   coords9M[1:100,], lonlat=TRUE, allpairs=TRUE)
a[1:20,1:20];rm(a) ## ~750-900m

setwd(paste0(OutDir,"Output_GADI/"))
centroids <- npyLoad("centroids_21.npy")
str(centroids)
centroids <- as.data.frame(centroids)
colnames(centroids) <- colnames(CLORPT.rs.27.vl)
summary(centroids)
save(centroids, file=paste0(OutDir,"centroids.RData"))


# Google Earth Engine map -------------------------------------------------

### Bring map from Google Earth Engine and mosaic 
gee.dir <- paste0(OutDir,"GEE_Output/")
setwd(gee.dir)

# list all the raster files available 
list_ras <- list.files(gee.dir, pattern="PedogenonsAus_1370")
rast.list <- lapply(list_ras,raster)
rast.list$fun <- mean
rast.list$na.rm <- TRUE
### Mosaic
PedogenonsAus <- do.call(mosaic, rast.list)

### Save into file
setwd(OutDir)
writeRaster(PedogenonsAus, filename ="PedogenonsAus1370.tif", overwrite = TRUE)
PedogenonsAus <- raster("PedogenonsAus1370.tif") # (this file is deleted afterwards)
plot(PedogenonsAus) ### we see the areas of ocean have assigned -9999 and areas with 0 are also NA
#plot(PedogenonsAus+9999)

## Mask value == 0 and -9999
fNA <- function(x) { x[x==-9999 | x==0] <- NA; return(x)}
beginCluster(7)
z1 <- clusterR(PedogenonsAus, calc, args=list(fun=fNA), filename="PdgnAus_k1370.tif", overwrite=TRUE)
endCluster()
plot(z1);rm(z1)

PedogenonsAus <- raster("PdgnAus_k1370.tif")
plot(PedogenonsAus)

### Project to EPSGG:3857 and save to file
system.time(
PedogenonsAus.3857 <- projectRaster(PedogenonsAus, 
                                    filename="PdgnAus_k1370_3857.tif",
                                    method='ngb',
                                    crs="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs",
                                    overwrite=TRUE)
)
PedogenonsAus.3857 <- raster("PdgnAus_k1370_3857.tif")
plot(PedogenonsAus.3857)


# Assign the pedogenon class to the calibration data -------------------------------------

### Assign cluster to calibration data in for loop

library(ClusterR)
CLORPT.27.df.vl$k1370 <- NA
### split
sp <- sample(1:20, nrow(CLORPT.27.df.vl), replace = T)
CLORPT.27.df.vl$sp <- sp
CLORPT.rs.27.vl$sp <- sp

for(i in 1:20){
  print(i)
  CLORPT.rs.27.vl.i <- CLORPT.rs.27.vl[CLORPT.rs.27.vl$sp == i,]
  pr <- ClusterR::predict_MBatchKMeans(data=as.matrix(CLORPT.rs.27.vl.i[, 1:27]), CENTROIDS = as.matrix(centroids), fuzzy = FALSE)
  k1370 <- as.vector(pr)
  CLORPT.27.df.vl[CLORPT.27.df.vl$sp==i,]$k1370 <- k1370
  gc()
}

K1370 <- cbind(coords9M, as.vector(pr)); rm(pr)
colnames(K1370) <- c("x","y","k1370")
rm(CLORPT.27.df.vl, CLORPT.rs.27.vl, coords9M)
gc()

### Add coordinates
CLORPT.27.df.vl.k1370 <- cbind(coords9M, CLORPT.27.df.vl)
CLORPT.27.df.vl.k1370 <- CLORPT.27.df.vl.k1370[,colnames(CLORPT.27.df.vl.k1370)!="sp"]
save(CLORPT.27.df.vl.k1370, file="CLORPT.27.df.vl.k1370.RData")


# ### Hierarchical clustering of centroids --------------------------------

### Choose optimal number of branches for the dendrogram
viz.map.legend.hclust.Obj <- function(CENTROIDS, Min.nc, Max.nc, indexC = c("silhouette","dunn")) {
  
  ### Extract centroids from model
  centroids <- CENTROIDS
  ### Extract the index of the centroids that are na/nan/Inf
  Kcent <- as.data.frame(centroids)
  Kcent.nan <- which(apply(Kcent, MARGIN = 1, FUN = function(x) {any(is.na(x))}))
  ### Exclude these clusters from everywhere
  if(length(Kcent.nan) >0) {
    Kcent.exist <- Kcent[-Kcent.nan,]
  } else if(length(Kcent.nan) == 0) {
    Kcent.exist <- Kcent
  }
  # Kcent.exist <- Kcent[-Kcent.nan,]
  ### Hierarchical clustering
  #hc <- hclust(dist(Kcent.exist), method="ward.D2")
  ### Determine the optimal number of clsuters with the Dunn or Shilhhouete index
  hc.op <- NbClust(data = Kcent.exist, distance = "euclidean",
                   min.nc = Min.nc, max.nc = Max.nc, method = "ward.D2", index=indexC)
  # plot(hc.op, main="Hierarchical clustering of kmeans centroids", sub="", xlab="")
  return(hc.op)
}

library(NbClust)
set.seed(2011)
hc.k1370.op <- NbClust(data = centroids, distance = "euclidean",
                       min.nc = 10, max.nc = 100, method = "ward.D2", index= "all")
str(hc.k1370.op$Best.nc)
bestN <- as.data.frame(hc.k1370.op$Best.nc)
hist(as.vector(hc.k1370.op$Best.nc[1,]), breaks=100, xlim=c(0,100))

hc.k1370.S<- viz.map.legend.hclust.Obj(CENTROIDS = centroids, 
                                       Max.nc = 100, Min.nc = 2, indexC ="silhouette" )
hc.k1370.S$Best.nc
plot(x=2:30, y=hc.k1370.S$All.index)

set.seed(2011)
hc.k1370.D<- viz.map.legend.hclust.Obj(CENTROIDS = centroids, 
                                       Max.nc = 100, Min.nc = 2, indexC ="dunn" )
hc.k1370.D$Best.nc
plot(x=2:30, y=hc.k1370.D$All.index)

branches.choice <- data.frame(cbind(cluster=c(2:100, 2:100), 
                                    IndexValue =(c(as.numeric(hc.k1370.S$All.index), as.numeric(hc.k1370.D$All.index))),
                                    Index = c(rep("Silhouette", 99), rep("Dunn", 99))))
branches.choice$IndexValue <- as.numeric(branches.choice$IndexValue)
branches.choice$cluster <- as.numeric(branches.choice$cluster)
branches.choice$Index <- as.factor(branches.choice$Index)

ggplot()+
  geom_point(data=branches.choice, aes(x=cluster, y=IndexValue, colour=Index),size=2)+
  xlab("Number of clusters")+
  ylab("Index")+ theme(legend.text=element_text(size=16),
                       legend.title = element_text(size=16),axis.text =element_text(size=14),
                       axis.title =element_text(size=14))+
  theme(legend.position=c(0.85,0.85))+
  xlim(0,30)


# Hierarchical clustering of pedogenons and color legend ------------

### First, perform the hierarchical clustering and save it to plot
### Input: kcentroids - centroids from Python kmeans
### Output: Hierarchical cluster (ward.D2 distance) of pedogenon centroids, hclust object
viz.map.legend.hclust <- function(kcentroids) {
  
  ### Extract centroids from model
  centroids <- kcentroids
  ### Extract the index of the centroids that are na/nan/Inf
  Kcent <- as.data.frame(centroids)
  Kcent.nan <- which(apply(Kcent, MARGIN = 1, FUN = function(x) {any(is.na(x))}))
  ### Exclude these clusters from everywhere
  if(length(Kcent.nan) >0) {
    Kcent.exist <- Kcent[-Kcent.nan,]
  } else if(length(Kcent.nan) == 0) {
    Kcent.exist <- Kcent
  }
  # Kcent.exist <- Kcent[-Kcent.nan,]
  ### Hierarchical clustering
  hc <- hclust(dist(Kcent.exist), method="ward.D2")
  plot(dendsort(hc), main="Hierarchical clustering of kmeans centroids", sub="", xlab="")
  return(hc)
  
}

### function to choose the number of branches for color ramps
### Input:
### hc.object - hclust object, hierarchical cluster, output from viz.map.legend.hclust function
### branchN - number of branches that we are considering for this kmeans model
### Output: a plot with the dendrogram and colored branches
viz.branches <- function(hc.object, branchN) {
  hc.object %>% as.dendrogram(.) %>% color_branches(., k = branchN) %>%
    plot(., main = paste0("Colored ",branchN," branches"))
}

#### Let's map the k=1000, including pre-1750s vegetation
### check hierarchical histogram
hc.k1370 <- hclust(dist(centroids), method="ward.D2")

hc.k1370 <- viz.map.legend.hclust(kcentroids = centroids)
plot(hc.k1370)

hc.k1370 %>% as.dendrogram(.) %>% color_branches(., k = 15) %>%
  plot(., main = paste0("Colored 15 branches"))

### Choose number of branches
plot.branches.k1370 <- viz.branches(hc.k1370, 15)

library("colorspace")
hcl_palettes(plot = TRUE)
### Default choice of palettes, for the package colorspace
my_palette <- c("Viridis","OrYel","PurpOr","BurgYl","TealGrn",
                "RdPu","GnBu","YlOrRd","Peach","Turku",
                "Lajolla","OrRd","Greens","Burg","Heat 2",
                "Dark Mint","Blues","SunsetDark","PuBuGn","Heat",
                "Light Grays", "Reds 3", "Reds 2", "Oslo", "Grays")

# my_palette <- c("OrYel","PurpOr","TealGrn","Burg","RdPu",
#                 "Peach","Turku","Red-Purple", "GnBu","Greens",
#                 "Purple-Blue", "Lajolla", "Burg", "Heat 2", "Dark Mint",
#                 "Blues", "SunsetDark", "PuBuGn", "Viridis", "Heat")

### Function to map with the selected color palettes, based on the dedrogram, the Pedogenons for NSW (or any study area)
### input:
### kcentroids - centroids from Python kmeans
### branchN - number of branches
### pal.names - selection of palettes from colorspace
### legend.name - name for the pdf to plot the legend (dendrogram)
### kmap - raster layer with Pedogenons
### newdata - dataframe with coordinates and cluster assignment
### Output: 
### $hc - Hierarchical clustering of the pedogenon centroids
### $branch.centroids.ord - Table with centroid (Pedogenon number), branch code, and assigned color 
### $legend.plot - dendrogram, legend with assigned colors
### $binpal - for leaflet

viz.points.legend.pal <- function(kcentroids, branchN, pal.names, legend.name){
  
  ### Extract centroids from model
  centroids <- kcentroids
  ### Extract the index of the centroids that are na/nan/Inf
  Kcent <- as.data.frame(centroids)
  Kcent$Pedogenon <- c(1:nrow(Kcent))
  Kcent.nan <- which(apply(Kcent, MARGIN = 1, FUN = function(x) {any(is.na(x))}))
  ### Exclude these clusters from everywhere
  if(length(Kcent.nan) >0) {
    Kcent.exist <- Kcent[-Kcent.nan,]
  } else if(length(Kcent.nan) == 0) {
    Kcent.exist <- Kcent
  }
  
  ### Perform hierarchical clustering on centroids, with Ward.D2 method  "ward.D2"
  hc <- hclust(dist(Kcent.exist[,!names(Kcent.exist) %in% c("Pedogenon")]), method="ward.D2")
  
  ### Extract labels
  hc.labels <- hc %>% as.dendrogram(.) %>% labels %>% as.numeric()
  
  ### Extract the membership from the tree
  dend <- hc %>% as.dendrogram(.)
  Kcent.exist$branch <- dend %>% dendextend:::cutree.dendrogram(., k = branchN)
  
  # branch.centroids <- as.data.frame(cbind(c(as.numeric(as.character(rownames(Kcent.exist)))),
  #                                         as.numeric(as.character(dendextend:::cutree.dendrogram(dend,k = branchN)))))
  
  branch.centroids <- Kcent.exist[,c("Pedogenon", "branch")]
  branch.centroids$Pedogenon <- as.numeric(branch.centroids$Pedogenon)
  branch.centroids$branch <- as.numeric(branch.centroids$branch)
  colnames(branch.centroids) <- c("Centroid", "Branch")
  
  ### sort the dataframe of branch and Pedogenon by the dendrogram labels
  # This line, using functions from dplyr or tidyverse does not work anymore
  # branch.centroids.ord <- branch.centroids %>% left_join(tibble(Centroid = hc.labels), by = "Centroid")
  
  reorder_idx <- match(hc.labels, branch.centroids$Centroid) # Saving indices for how to reorder `branch.centroids$Centroid` to match `hc.labels`
  branch.centroids.ord <- branch.centroids[reorder_idx,]
  
  numbs.pal <- c((table(Kcent.exist$branch)))
  branch.count <- as.data.frame(cbind(c(1:branchN), numbs.pal))
  colnames(branch.count) <- c("Branch", "Count")
  #branch.count <- branch.count[order(- branch.count$Count),]
  branch.count <- branch.count %>% arrange(., -Count)
  
  ###Assign color to each 
  branch.centroids.ord$colors <- NA
  
  for(i in 1:length(numbs.pal)){
    ## Generate as many colors for each pallete as centroids in the branch
    branch.centroids.ord[branch.centroids.ord$Branch == branch.count[i,]$Branch,]$colors <-
      sequential_hcl(pal.names[[i]], n = branch.count[i,]$Count)
  }
  
  ### Create legend
  ### Reorder the colors depending on the labels
  legend.plot <- dend %>%  set("labels_col", branch.centroids.ord$colors) %>% 
    set("branches_k_color", branch.centroids.ord$colors)
  
  pdf(file = paste0("Map_legend",legend.name,".pdf"), width = 10, height = 100 )
  plot(legend.plot,
       main = "Hierarchical histogram of pedogenon centroids with the map colors",
       horiz = TRUE) # change color 
  dev.off()
  
  ### Now, reorder by Pedogenon class
  branch.centroids.ord <- branch.centroids.ord %>% arrange(., Centroid)
  #branch.centroids.ord <- branch.centroids.ord[order(branch.centroids.ord$Centroid),]
  
  ### Create palette for leaflet
  #pal <- branch.centroids.ord$colors
  
  # binpal <- colorBin(palette = branch.centroids.ord$colors,
  #                    bins = c(as.numeric(as.character(rownames(Kcent.exist))),
  #                             tail(as.numeric(as.character(rownames(Kcent.exist))),1)+1),
  #                    na.color = "transparent")
  
  # ### Project the map into the leaflet projection
  # if (need.proj == TRUE) {
  #   kmap <- projectRaster(kmap,
  #                         crs="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs",
  #                         method = "ngb")
  # } else if (need.proj == FALSE) { 
  #   kmap <- kmap
  #   }
  # 
  # binpal <- colorBin(palette = branch.centroids.ord$colors,
  #                    bins = c(branch.centroids.ord$Centroid,
  #                             tail(branch.centroids.ord$Centroid,1)+1),
  #                    na.color = "transparent")
  # 
  # map.out <- leaflet(options = leafletOptions(doubleClickZoom= FALSE)) %>%
  #   # Base groups
  #   addTiles(group="OSM (default)") %>%
  #  
  #  # addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>% # , group = "World Imagery"
  #   #addProviderTiles("OpenTopoMap", group = "Topo Map") %>%
  #   addRasterImage(kmap, opacity = 1, colors=binpal, project=need.proj, 
  #                  maxBytes = 300000000, group = "Pedogenons") %>%
  #   #fitBounds(lng1=140, lat1=-38, lng2=154, lat2=-28) %>%
  #   leafem::addMouseCoordinates() #%>%
  #   # addLayersControl(
  #   #   baseGroups = c("OSM (default)","World Imagery", "Topo Map"),
  #   #   overlayGroups = c("Pedogenons"),
  #   #   options = layersControlOptions(collapsed = FALSE)
  #   # )
  
  #mapshot(map.out, file = paste0(OutDir,"/Map_",legend.name,".pdf"), remove_url = FALSE)
  output <- list("hc"=hc, "branch.centroids.ord"=branch.centroids.ord,
                 "legend.plot"=legend.plot)#, "map.out"=map.out)
  return(output)
  
}

setwd(paste0(HomerDir,"Evaluation_Files/"))

my_palette <- c("Viridis","RdPu","OrYel","PurpOr","BurgYl",
                "GnBu","Turku","YlOrRd","Light Grays","TealGrn",
                "Lajolla","OrRd","Greens","Burg","Heat 2",
                "Dark Mint","Blues","SunsetDark","PuBuGn","Heat",
                "Peach" , "Reds 3", "Reds 2", "Oslo", "Grays")

k1370.out <- viz.points.legend.pal(kcentroids =  centroids,
                                   branchN = 25,
                                   pal.names = my_palette,
                                   legend.name =  "k1370_25Br")

### Export to 
colores <- gsub(x=k1370.out$branch.centroids.ord$colors, pattern="#", replacement = "")
colores <- paste(colores, collapse=",")
saveRDS(colores, "colores2.rds")

dim(centroids)
#length(unique(getValues(pedogenonsAus)))

# test.df <- K1370[1:1000,]
# unique(test.df$k1370)
# pal.subset <- k1370.out$branch.centroids.ord[k1370.out$branch.centroids.ord$Centroid %in% unique(test.df$k1370),]

### Map the points with assigned cluster
K1370 <- CLORPT.27.df.vl.k1370; rm(CLORPT.27.df.vl.k1370)

library(ggmap)
AusMap <- get_stamenmap(bbox = c(left=112, bottom=-45, right=155, top=-9),
                        maptype="terrain-background", zoom=5,
                        source="stamen", crop=TRUE)
ggmap(AusMap) +
  geom_point(data=K1370, aes(x=x, y=y, colour=as.factor(k1370))) +
  scale_colour_manual(breaks = k1370.out$branch.centroids.ord$Centroid,
                      values = k1370.out$branch.centroids.ord$colors)+
  theme(legend.position = "none")

ggplot(data=K1370) +
  geom_point(aes(x=x, y=y, colour=as.factor(k1370))) +
  scale_colour_manual(breaks = k1370.out$branch.centroids.ord$Centroid,
                      values = k1370.out$branch.centroids.ord$colors)+
  theme(legend.position = "none")

### Plot the legend
plot(k1370.out$legend.plot)

###### Map the raster with simple R
### with simple plot
plot(PedogenonsAus, breaks=c(0,k1370.out$branch.centroids.ord$Centroid), col=k1370.out$branch.centroids.ord$colors)

# ### Visualization with leaflet - Pre shiny server -----------------------

### Aggregate to 900 m resolution
setwd(OutDir)
PdgnAus_k1370.3857 <- raster::aggregate(PedogenonsAus.3857,fact=9, fun=modal,filename="PdgnAus_k1370_3857_800m.tif")
PdgnAus_k1370.3857 <-raster("PdgnAus_k1370_3857_800m.tif")
library(rgeos)
Poly_k1370.3857 <- rasterToPolygons(PdgnAus_k1370.3857, n=16, dissolve=TRUE)
plot(Poly_k1370.3857)


library(leaflet.multiopacity)

binpal <- colorBin(palette = k1370.out$branch.centroids.ord$colors,
                   bins = c(k1370.out$branch.centroids.ord$Centroid,
                            tail(k1370.out$branch.centroids.ord$Centroid,1)+1),
                   na.color = "transparent")
leaflet() %>%
  # Base groups
  addTiles(group="OSM (default)") %>%
  addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>% # , group = "World Imagery"
  addProviderTiles("OpenTopoMap", group = "Topo Map") %>%
  addRasterImage(PdgnAus_k1370.3857, 
                 layerId = "Pedogenons",
                 colors=binpal, project=FALSE,
                 maxBytes = 300000000, group = "Pedogenons") %>%
  #fitBounds(lng1=140, lat1=-38, lng2=154, lat2=-28) %>%
  leafem::addMouseCoordinates() %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World Imagery", "Topo Map"),
    overlayGroups = c("Pedogenons"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addOpacityControls(collapsed = FALSE,
                     #layerId = c("Pedogenons"),
                     group = "Pedogenons",
                     position = "bottomright", 
                     title = "Opacity Control")

###### Lets' try a different hierarchical dendrogram


# Hierarchical clustering of pedogenons and color legend - without rare classes ------------

### Keep original Pedogenon number in rowname
rownames(centroids)
exclude = c(14,472,95,522,576,309,799)
centroids.2 <- centroids[-exclude,]
rownames(centroids.2)

hc.k1370.S<- viz.map.legend.hclust.Obj(CENTROIDS = centroids.2, 
                                       Max.nc = 30, Min.nc = 2, indexC ="silhouette" )
library(NbClust)
hc.k1370.S$Best.nc
plot(x=2:30, y=hc.k1370.S$All.index)

set.seed(2011)
hc.k1370.op <- NbClust(data = centroids.2, distance = "euclidean",
                       min.nc = 10, max.nc = 30, method = "ward.D2", index= "all")

### First, perform the hierarchical clustering and save it to plot
### Input: kcentroids - centroids from Python kmeans
### Exclucde certain centroids that are very different - by rowname
### Output: Hierarchical cluster (ward.D2 distance) of pedogenon centroids, hclust object
viz.map.legend.hclust.excludeSome <- function(kcentroids, exclude) {
  
  ### Extract centroids from model (here is a dataframe already)
  centroids <- kcentroids
    ### Exclude these
  centroids <- centroids[-exclude, ]
    ### Extract the index of the centroids that are na/nan/Inf
  Kcent <- as.data.frame(centroids)
  Kcent.nan <- which(apply(Kcent, MARGIN = 1, FUN = function(x) {any(is.na(x))}))
  ### Exclude these clusters from everywhere
  if(length(Kcent.nan) >0) {
    Kcent.exist <- Kcent[-Kcent.nan,]
  } else if(length(Kcent.nan) == 0) {
    Kcent.exist <- Kcent
  }
  # Kcent.exist <- Kcent[-Kcent.nan,]
  ### Hierarchical clustering
  hc <- hclust(dist(Kcent.exist), method="ward.D2")
  plot(dendsort(hc), main="Hierarchical clustering of kmeans centroids", sub="", xlab="")
  return(hc)
  
}

### Test
exclude = c(14,472,95,522,576,309,799)
hc.k1370 <- viz.map.legend.hclust.excludeSome(centroids,exclude = c(14,472,95,522,576,309,799))
plot(hc.k1370)

### function to choose the number of branches for color ramps , excluding some problematic centroids
### Input:
### hc.object - hclust object, hierarchical cluster, output from viz.map.legend.hclust function
### branchN - number of branches that we are considering for this kmeans model
### Output: a plot with the dendrogram and colored branches
viz.branches <- function(hc.object, branchN) {
  hc.object %>% as.dendrogram(.) %>% color_branches(., k = branchN) %>%
    plot(., main = paste0("Colored ",branchN," branches"))
}

hc.k1370 %>% as.dendrogram(.) %>% color_branches(., k = 21) %>%
  plot(., main = paste0("Colored 21 branches"))

### Choose number of branches
plot.branches.k1370 <- viz.branches(hc.k1370, 21)

### Function to map with the selected color palettes, based on the dedrogram, the Pedogenons for NSW (or any study area)
### input:
### kcentroids - centroids from Python kmeans
### branchN - number of branches
### pal.names - selection of palettes from colorspace
### legend.name - name for the pdf to plot the legend (dendrogram)
### kmap - raster layer with Pedogenons
### newdata - dataframe with coordinates and cluster assignment
### Output: 
### $hc - Hierarchical clustering of the pedogenon centroids
### $branch.centroids.ord - Table with centroid (Pedogenon number), branch code, and assigned color 
### $legend.plot - dendrogram, legend with assigned colors
### $binpal - for leaflet
viz.points.legend.pal.excl <- function(kcentroids, branchN, pal.names, legend.name, exclude, exclude_colors){
  
  ### Extract centroids from model
  centroids <- kcentroids
  
  ### Extract the index of the centroids that are na/nan/Inf
  Kcent <- as.data.frame(centroids)
  Kcent$Pedogenon <- c(1:nrow(Kcent))
  Kcent.nan <- which(apply(Kcent, MARGIN = 1, FUN = function(x) {any(is.na(x))}))
  ### Exclude these clusters from everywhere
  if(length(Kcent.nan) >0) {
    Kcent.exist <- Kcent[-Kcent.nan,]
  } else if(length(Kcent.nan) == 0) {
    Kcent.exist <- Kcent
  }
  
  ### Exclude these
  #centroids.2 <- Kcent.exist[-exclude, ]
  
  ### Perform hierarchical clustering on centroids, with Ward.D2 method  "ward.D2"
  hc <- hclust(dist(Kcent.exist[-exclude,!names(Kcent.exist) %in% c("Pedogenon")]), method="ward.D2")
  
  ### Extract labels
  hc.labels <- hc %>% as.dendrogram(.) %>% labels %>% as.numeric()
  
  ### Extract the membership from the tree
  dend <- hc %>% as.dendrogram(.)
  Kcent.exist$branch <- NA
  Kcent.exist[-exclude,]$branch <- dend %>% dendextend:::cutree.dendrogram(., k = branchN)
  
  # branch.centroids <- as.data.frame(cbind(c(as.numeric(as.character(rownames(Kcent.exist)))),
  #                                         as.numeric(as.character(dendextend:::cutree.dendrogram(dend,k = branchN)))))
  
  branch.centroids <- Kcent.exist[,c("Pedogenon", "branch")]
  branch.centroids$Pedogenon <- as.numeric(branch.centroids$Pedogenon)
  branch.centroids$branch <- as.numeric(branch.centroids$branch)
  colnames(branch.centroids) <- c("Centroid", "Branch")
  
  ### Separate the excluded ones
  branch.centroids.excl <- branch.centroids[exclude,]
  branch.centroids.in <- branch.centroids[-exclude,]
  
  ### sort the dataframe of branch and Pedogenon by the dendrogram labels
  # This line, using functions from dplyr or tidyverse does not work anymore
  # branch.centroids.ord <- branch.centroids %>% left_join(tibble(Centroid = hc.labels), by = "Centroid")
  
  reorder_idx <- match(hc.labels, branch.centroids.in$Centroid) # Saving indices for how to reorder `branch.centroids$Centroid` to match `hc.labels`
  branch.centroids.in.ord <- branch.centroids.in[reorder_idx,]
  
  numbs.pal <- c((table(branch.centroids.in.ord$Branch)))
  branch.count <- as.data.frame(cbind(c(1:branchN), numbs.pal))
  colnames(branch.count) <- c("Branch", "Count")
  #branch.count <- branch.count[order(- branch.count$Count),]
  branch.count <- branch.count %>% arrange(., -Count)
  
  ###Assign color to each 
  branch.centroids.in.ord$colors <- NA
  
  for(i in 1:length(numbs.pal)){
    ## Generate as many colors for each pallete as centroids in the branch
    branch.centroids.in.ord[branch.centroids.in.ord$Branch == branch.count[i,]$Branch,]$colors <-
      sequential_hcl(pal.names[[i]], n = branch.count[i,]$Count)
  }
  
  ### Create legend
  ### Reorder the colors depending on the labels
  legend.plot <- dend %>%  set("labels_col", branch.centroids.in.ord$colors) %>% 
    set("branches_k_color", branch.centroids.in.ord$colors)
  
  pdf(file = paste0("Map_legend",legend.name,".pdf"), width = 10, height = 100 )
  plot(legend.plot,
       main = "Hierarchical histogram of pedogenon centroids with the map colors",
       horiz = TRUE) # change color 
  dev.off()
  
  ### Assign some random colors to the excluded ones
  branch.centroids.excl$colors <- NA
  #branch.centroids.excl$colors <- sequential_hcl(exclude_colors, n = length(branch.centroids.excl$colors))
  branch.centroids.excl$colors <- "black"
  branch.centroids.excl$Branch <- "Outlier"
  
  ### bind
  branch.centroids.ord <- rbind(branch.centroids.excl,branch.centroids.in.ord)
  
  
  ### Now, reorder by Pedogenon class
  branch.centroids.ord <- branch.centroids.ord %>% arrange(., Centroid)
  #branch.centroids.ord <- branch.centroids.ord[order(branch.centroids.ord$Centroid),]
  
  ### Create palette for leaflet
  #pal <- branch.centroids.ord$colors
  
  # binpal <- colorBin(palette = branch.centroids.ord$colors,
  #                    bins = c(as.numeric(as.character(rownames(Kcent.exist))),
  #                             tail(as.numeric(as.character(rownames(Kcent.exist))),1)+1),
  #                    na.color = "transparent")
  
  # ### Project the map into the leaflet projection
  # if (need.proj == TRUE) {
  #   kmap <- projectRaster(kmap,
  #                         crs="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs",
  #                         method = "ngb")
  # } else if (need.proj == FALSE) { 
  #   kmap <- kmap
  #   }
  # 
  # binpal <- colorBin(palette = branch.centroids.ord$colors,
  #                    bins = c(branch.centroids.ord$Centroid,
  #                             tail(branch.centroids.ord$Centroid,1)+1),
  #                    na.color = "transparent")
  # 
  # map.out <- leaflet(options = leafletOptions(doubleClickZoom= FALSE)) %>%
  #   # Base groups
  #   addTiles(group="OSM (default)") %>%
  #  
  #  # addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>% # , group = "World Imagery"
  #   #addProviderTiles("OpenTopoMap", group = "Topo Map") %>%
  #   addRasterImage(kmap, opacity = 1, colors=binpal, project=need.proj, 
  #                  maxBytes = 300000000, group = "Pedogenons") %>%
  #   #fitBounds(lng1=140, lat1=-38, lng2=154, lat2=-28) %>%
  #   leafem::addMouseCoordinates() #%>%
  #   # addLayersControl(
  #   #   baseGroups = c("OSM (default)","World Imagery", "Topo Map"),
  #   #   overlayGroups = c("Pedogenons"),
  #   #   options = layersControlOptions(collapsed = FALSE)
  #   # )
  
  #mapshot(map.out, file = paste0(OutDir,"/Map_",legend.name,".pdf"), remove_url = FALSE)
  output <- list("hc"=hc, "branch.centroids.ord"=branch.centroids.ord,
                 "legend.plot"=legend.plot)#, "map.out"=map.out)
  return(output)
  
}

my_palette <- c("PurpOr","GnBu","PuBuGn","RdPu","TealGrn",
                "Burg","OrYel","Peach","SunsetDark",
                "Lajolla","OrRd","Greens","BurgYl","Heat 2","Turku",
                "Dark Mint","Blues",
                "Light Grays","Light Grays","Light Grays","Light Grays")

k1370_excl.out <- viz.points.legend.pal.excl(kcentroids =  centroids,
                                             branchN = 21,
                                             pal.names = my_palette,
                                             legend.name =  "k1370_25Br_Excl",
                                             exclude=c(14,472,95,522,576,309,799))
                                           
plot(k1370_excl.out$legend.plot)

### Single branches and outliers are colored in gray
k1370_excl.out$branch.centroids.ord[k1370_excl.out$branch.centroids.ord$Branch %in% c("15", "18", "20",  "21", "Outlier"),]$colors <- "gray"

length(unique(k1370_excl.out$branch.centroids.ord$Branch))

### Reclassify by branch
rcl <- k1370_excl.out$branch.centroids.ord[,c("Centroid", "Branch")]
rcl$Branch <- ifelse(rcl$Branch=="Outlier", "22", rcl$Branch)
rcl$Branch <- as.numeric(rcl$Branch)
PdgnAus_Br21.3857 <- reclassify(PdgnAus_k1370.3857,rcl,filename="PdgnAus_Br21.3857.800m.tif")

binpal <- colorBin(palette = k1370_excl.out$branch.centroids.ord$colors,
                   bins = c(k1370_excl.out$branch.centroids.ord$Centroid,
                            tail(k1370_excl.out$branch.centroids.ord$Centroid,1)+1),
                   na.color = "transparent")

leaflet() %>%
  # Base groups
  addTiles(group="OSM (default)") %>%
  addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>% # , group = "World Imagery"
  #addProviderTiles("OpenTopoMap", group = "Topo Map") %>%
  addRasterImage(PdgnAus_k1370.3857, 
                 layerId = "Pedogenons",
                 colors=binpal, project=FALSE,
                 maxBytes = 300000000, group = "Pedogenons") %>%
  #fitBounds(lng1=140, lat1=-38, lng2=154, lat2=-28) %>%
  leafem::addMouseCoordinates() %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World Imagery"),
    overlayGroups = c("Pedogenons"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addOpacityControls(collapsed = FALSE,
                     #layerId = c("Pedogenons"),
                     group = "Pedogenons",
                     position = "bottomright", 
                     title = "Opacity Control")

### Color each branch
k1370_excl.out$branch.centroids.ord$BranchColor <- NA
unique(k1370_excl.out$branch.centroids.ord$Branch)

branches.col <- c(as.character(1:21),"Outlier")
library(randomcoloR)
mycols <- distinctColorPalette(k = length(branches.col))
  #c(rainbow(length(branches.col)-1), "black")
  #c(sequential_hcl((length(branches.col)-1), palette = "plasma"), "black")
  #
for(i in 1:length(branches.col)){
  ## Generate as many colors as branches
  k1370_excl.out$branch.centroids.ord[k1370_excl.out$branch.centroids.ord$Branch == branches.col[[i]],]$BranchColor <-
    #viridis_pal(option="C")(length(branches.col))[[i]]
    mycols[[i]]
}
unique(k1370_excl.out$branch.centroids.ord$BranchColor)
k1370_excl.out$branch.centroids.ord[k1370_excl.out$branch.centroids.ord$Branch=="Outlier",]$BranchColor <- "grey"

binpal <- colorBin(palette = k1370_excl.out$branch.centroids.ord$BranchColor,
                   bins = c(k1370_excl.out$branch.centroids.ord$Centroid,
                            tail(k1370_excl.out$branch.centroids.ord$Centroid,1)+1),
                   na.color = "transparent")
leaflet() %>%
  # Base groups
  addTiles(group="OSM (default)") %>%
  addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>% # , group = "World Imagery"
  #addProviderTiles("OpenTopoMap", group = "Topo Map") %>%
  addRasterImage(PdgnAus_k1370.3857, 
                 layerId = "Pedogenon branch",
                 colors=binpal, project=FALSE,
                 maxBytes = 300000000, group = "Pedogenons") %>%
  #fitBounds(lng1=140, lat1=-38, lng2=154, lat2=-28) %>%
  leafem::addMouseCoordinates() %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World Imagery"),
    overlayGroups = c("Pedogenons"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addOpacityControls(collapsed = FALSE,
                     #layerId = c("Pedogenons"),
                     group = "Pedogenons",
                     position = "bottomright", 
                     title = "Opacity Control")

### Can I visualize the centroids with umap?
library(umap)
#library(uwot)
#detach("package:uwot", unload=TRUE)
centroids.umap = umap(centroids)
head(centroids.umap$layout, 3)
plot((centroids.umap$layout[,1]),(centroids.umap$layout[,2]),
     col=(k1370_excl.out$branch.centroids.ord$colors),
     xlab= "Umap Centroids 1", ylab="Umap Centroids 2",
     pch=19)

plot((centroids.umap$layout[,1]),(centroids.umap$layout[,2]),
     col=(k1370_excl.out$branch.centroids.ord$BranchColor),
     pch=19)

####Random 
library(randomcoloR)
mycolors <- distinctColorPalette(k = 1370)

binpal <- colorBin(palette = mycolors,
                   bins = c(k1370_excl.out$branch.centroids.ord$Centroid,
                            tail(k1370_excl.out$branch.centroids.ord$Centroid,1)+1),
                   na.color = "transparent")
leaflet() %>%
  # Base groups
  addTiles(group="OSM (default)") %>%
  addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>% # , group = "World Imagery"
  #addProviderTiles("OpenTopoMap", group = "Topo Map") %>%
  addRasterImage(PdgnAus_k1370.3857, 
                 layerId = "Pedogenon branch",
                 colors=binpal, project=FALSE,
                 maxBytes = 300000000, group = "Pedogenons") %>%
  #fitBounds(lng1=140, lat1=-38, lng2=154, lat2=-28) %>%
  leafem::addMouseCoordinates() %>%
  addLayersControl(
    baseGroups = c("OSM (default)","World Imagery"),
    overlayGroups = c("Pedogenons"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addOpacityControls(collapsed = FALSE,
                     #layerId = c("Pedogenons"),
                     group = "Pedogenons",
                     position = "bottomright", 
                     title = "Opacity Control")


# Calculate summary statistics of the covariates in the original --------

### CLORPT.27.df.vl.k1370 is the dataframe with coordinates, covariates, and pedogenon class
CLORPT.27.df.vl.k1370

### Change colnames
colnames(CLORPT.27.df.vl.k1370) <- c("x","y","PTA","PTS1","PTS2","RSM","TNM",
                                     "TXM","TRA","TRX","TRI","ADM",
                                     "Gamma_Dose","Gamma_pctk","Gamma_ppmt",
                                     "Gamma_tk","Gravity","WII",
                                     "Clay_30_60","Clay_60_100","Clay_100_200",
                                     "Sand_30_60","Sand_60_100","Sand_100_200",
                                     "elevation","mrrtf","mrvbf","slope","TWI","k1370")

### Store original prototypes in a dataframe
### Calculate new prototypes
Centroids_Summary_Covariates <- CLORPT.27.df.vl.k1370[,3:30] %>% ### Exclude coordinates
  group_by(.,k1370) %>% ### Group by Pedogenon class
  summarize_all(., list(~min(.), ~quantile(., probs=0.25), ~mean(.), ~quantile(., probs=0.75), ~max(.), ~sd(.,na.rm=TRUE)))
Centroids_Summary_Covariates <- as.data.frame(Centroids_Summary_Covariates)

### change names
names.covariates <- c("PTA","PTS1","PTS2","RSM","TNM",
                      "TXM","TRA","TRX","TRI","ADM",
                      "Gamma_Dose","Gamma_pctk","Gamma_ppmt",
                      "Gamma_tk","Gravity","WII",
                      "Clay_30_60","Clay_60_100","Clay_100_200",
                      "Sand_30_60","Sand_60_100","Sand_100_200",
                      "elevation","mrrtf","mrvbf","slope","TWI")


colnames(Centroids_Summary_Covariates) <- c("k1370", paste0(names.covariates, ".min"),
                                            paste0(names.covariates, ".q25"),
                                            paste0(names.covariates, ".mean"),
                                            paste0(names.covariates, ".q75"),
                                            paste0(names.covariates, ".max"),
                                            paste0(names.covariates, ".sd"))

Centroids_Summary_Covariates2 <- Centroids_Summary_Covariates %>%
  mutate(., PTA = paste0(round(PTA.mean,0)," \u00b1 ",round(PTA.sd,0))) %>%
  mutate(., PTS1 = paste0(round(PTS1.mean,2)," \u00b1 ",round(PTS1.sd,2))) %>%
  mutate(., PTS2 = paste0(round(PTS2.mean,2)," \u00b1 ",round(PTS2.sd,2))) %>%
  mutate(., RSM = paste0(round(RSM.mean,2)," \u00b1 ",round(RSM.sd,2))) %>%
  mutate(., TNM = paste0(round(TNM.mean,2)," \u00b1 ",round(TNM.sd,2))) %>%
  mutate(., TXM = paste0(round(TXM.mean,2)," \u00b1 ",round(TXM.sd,2))) %>%
  mutate(., TRA = paste0(round(TRA.mean,2)," \u00b1 ",round(TRA.sd,2))) %>%
  mutate(., TRX = paste0(round(TRX.mean,2)," \u00b1 ",round(TRX.sd,2))) %>%
  mutate(., TRI = paste0(round(TRI.mean,2)," \u00b1 ",round(TRI.sd,2))) %>%
  mutate(., ADM = paste0(round(ADM.mean,2)," \u00b1 ",round(ADM.sd,2))) %>%
  mutate(., Gamma_Dose = paste0(round(Gamma_Dose.mean,2)," \u00b1 ",round(Gamma_Dose.sd,2))) %>%
  mutate(., Gamma_pctk = paste0(round(Gamma_pctk.mean,2)," \u00b1 ",round(Gamma_pctk.sd,2))) %>%
  mutate(., Gamma_ppmt = paste0(round(Gamma_ppmt.mean,2)," \u00b1 ",round(Gamma_ppmt.sd,2))) %>%
  mutate(., Gamm_tk = paste0(round(Gamma_tk.mean,2)," \u00b1 ",round(Gamma_tk.sd,2))) %>%
  mutate(., Gravity = paste0(round(Gravity.mean,2)," \u00b1 ",round(Gravity.sd,2))) %>%
  mutate(., WII = paste0(round(WII.mean,2)," \u00b1 ",round(WII.sd,2))) %>%
  mutate(., Clay_30_60 = paste0(round(Clay_30_60.mean,2)," \u00b1 ",round(Clay_30_60.sd,2))) %>%
  mutate(., Clay_60_100 = paste0(round(Clay_60_100.mean,2)," \u00b1 ",round(Clay_60_100.sd,2))) %>%
  mutate(., Clay_100_200 = paste0(round(Clay_100_200.mean,2)," \u00b1 ",round(Clay_100_200.sd,2))) %>%
  mutate(., Sand_30_60 = paste0(round(Sand_30_60.mean,2)," \u00b1 ",round(Sand_30_60.sd,2))) %>%
  mutate(., Sand_60_100 = paste0(round(Sand_60_100.mean,2)," \u00b1 ",round(Sand_60_100.sd,2))) %>%
  mutate(., Sand_100_200 = paste0(round(Sand_100_200.mean,2)," \u00b1 ",round(Sand_100_200.sd,2))) %>%
  mutate(., Elevation = paste0(round(elevation.mean,2)," \u00b1 ",round(elevation.sd,2))) %>%
  mutate(., MRRTF = paste0(round(mrrtf.mean,2)," \u00b1 ",round(mrrtf.sd,2))) %>%
  mutate(., MRVBF = paste0(round(mrvbf.mean,2)," \u00b1 ",round(mrvbf.sd,2))) %>%
  mutate(., Slope = paste0(round(slope.mean,2)," \u00b1 ",round(slope.sd,2))) %>%
  mutate(., TWI = paste0(round(TWI.mean,2)," \u00b1 ",round(TWI.sd,2)))

Centroids_Summary_Covariates2 <- Centroids_Summary_Covariates2[,164:ncol(Centroids_Summary_Covariates2)]

setwd(paste0(HomerDir,"Evaluation_Files/"))
save(Centroids_Summary_Covariates, Centroids_Summary_Covariates2, file="Centroids_Summary_Covariates.RData")

### Save the hclust objects for Shiny
save(k1370_excl.out, k1370.out, file="hclust_legend.RData")

### End for now