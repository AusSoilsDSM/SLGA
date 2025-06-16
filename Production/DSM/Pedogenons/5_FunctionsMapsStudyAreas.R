####################################################################################################################################
### Explore Pedogenon models and functions for visualizing them, particularly for smaller areas
### Author: Mercedes Roman
### Date: 19/08/2020
### Objectives:
### 1. Table with k-prototypes for that study area (subset)
### 2. Table with Pedogenon present, Area (in study area and outside the study area), Closer Pedogenon, Mahalanobis distance to this Pedogenon
### 3. Present dendrogram, were the present Pedogenons are highlighted.
### 4. Represent with new color palette, to improve the differentiation and visibility (subset)
### 5. Overlay Phenosoil layer. Summarize which Pedogenon classes have remnant genosoils or phenosoils

list.of.packages <- c("ClusterR", "rgdal", "gdalUtils", "raster", "sp", "sf", "dplyr", "tidyverse",
                      "ggmap", "ggplot2", "viridis", "scales", "rasterVis", "lattice", "gridExtra",
                      "tmap", "leaflet", "mapview", "geosphere", "gplots", "dendextend", "colorspace","dendsort")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

##Load packages
#library(Rtools)
#library(rlang)
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
#library(shiny)   # for web applications
#library(foreach)
#library(doParallel)
library(geosphere) # calculate distances
library(dendsort) 
library(gplots)
library(dendextend) # visualize dendrograms
library(colorspace)

### Functions for examining the study areas

# ### 1. Calculate area by Pedogenon class -------------------------------------

### Function that calculates the area of any Pedogenon map (any k) and returns a summary table
### Inputs:
### kmap - a raster with Pedogenon classes
### fname - name of the file for saving the table into a csv file
### Output: Returns a dataframe with the area of each Pedogenon, k.area.df 
pedogenon.area.func <- function(kmap, fname) {
  
  areaPixels <-raster::area(kmap, na.rm=TRUE)
  s <- stack(kmap, areaPixels)
  k.A <-getValues(s)
  k.A <- as.data.frame(k.A)
  colnames(k.A) <- c("Pedogenon", "Area_km2")
  k.A <- k.A[!is.na(k.A$Pedogenon),]
  
  k.area.df <-  k.A %>% 
    group_by(.,as.factor(Pedogenon), .drop=TRUE ) %>% ## Group by Pedogenon
    summarise(Pedogenon_area = sum(Area_km2, na.rm=TRUE)) ### sum area by Pedogenon class
    k.area.df <- as.data.frame(k.area.df)
  colnames(k.area.df) <- c("Pedogenon", "Area_Km2")
  write.csv(k.area.df, file=paste0(fname,".csv")) ### Write table to csv file
  return(k.area.df) ## and return
}

### Returns a table with a row per Pedogenon indicating the closer Pedogenon class,
### the Mahalanobis distance between Pedogenons calculated with CLORPT covariates,
### and the areas that they occupy in NSW, or the study area.
### Note: the distance is calculated with the Euclidean method, but since the data of the
### training dataset was rescaled with the inverse Cholesky transformation, 
### the resulting distance is the same as the Mahalanobis distance calculated on the original data
### Inputs:
### kmodel - kmeans model from the package ClusterR
### k.area.df - is the output of the pedogenon.area.func function
### fname - name to export the table to csv
centroid.dist.func <- function(kmodel, k.area.df, fname){
  
  ### kmodel is a kmeans model
  ### k.area.df is the output of the Pedogenon.area.func function
 
  # extract the centroids
  K.centroids <- kmodel$centroids
  K.centroids <- as.data.frame(K.centroids)
  K.centroids$Pedogenon <- c(1:nrow(K.centroids))
  #rownames(K.centroids) <- c(1:nrow(K.centroids))
  
  ## Is any centroid NA?
  ### Extract the index of the centroids that are na/nan/Inf
  Kcent.nan <- which(apply(K.centroids, MARGIN = 1, FUN = function(x) {any(is.na(x))}))
  
  ### Calculate distance between all centroids
  dist.centroids <- dist(x=K.centroids[,!names(K.centroids) %in% c("Pedogenon")],
                         method = "euclidean")
  
  ### Create empty dataframe to store output
  outs <- data.frame(Pedogenon=rep(as.integer(NA),nrow(K.centroids)),
                     ClosestP=rep(as.integer(NA),nrow(K.centroids)),
                     Distance=rep(as.double(NA), nrow(K.centroids)))
  
  outs$Pedogenon <- K.centroids$Pedogenon ### Assign Pedogenon
  Gs <- as.numeric(as.character(outs$Pedogenon))
  dist.centroids <- as.matrix(dist.centroids)
  ### Calculate distance to the closest Pedogenon
  for(i in 1:nrow(outs)){
    min.dist <- sort(dist.centroids[rownames(dist.centroids)[Gs[[i]]],])[2]
    outs[i,"Distance"] <- min.dist
    outs[i,"ClosestP"] <- names(min.dist)
  }
  ### Remember that those Pedogenons that don't exist are NA
  outs$ClosestP <- ifelse(outs$Pedogenon %in% Kcent.nan, NA, outs$ClosestP )
  outs$Distance <- ifelse(outs$Pedogenon %in% Kcent.nan, NA, outs$Distance )
  colnames(outs) <- c("Pedogenon", "Closest Pedogenon", "Distance")
  outs$Distance <-round(outs$Distance, digits = 3)
  
  ### Join with the Pedogenon area
  outs$Pedogenon <- as.character(outs$Pedogenon)
  k.area.df$Pedogenon <- as.character(k.area.df$Pedogenon)
  outs <- left_join(outs, k.area.df, by ="Pedogenon")
  
  ### Create column with area of the closest Pedogenon
  outs$Pedo2.Area <- NA
  if(length(Kcent.nan) >0) {
    G.exists <- c(1:nrow(outs))[-Kcent.nan]
  } else if(length(Kcent.nan) == 0) {
    G.exists <- c(1:nrow(outs))
  }
  
  for(i in 1:length(G.exists)){
    target.G <- outs[outs$Pedogenon == G.exists[[i]], ]$`Closest Pedogenon`
    target.A <- outs[outs$Pedogenon == target.G, ]$Area_Km2
    outs[outs$Pedogenon == G.exists[[i]], ]$Pedo2.Area <- target.A
  }
  
  colnames(outs) <- c("Pedogenon", "Closest.Pedogenon", "MahabDist", "Area_Km2", "Closests.Pedo.Area_Km2")
  
  write.csv(outs, file=paste0(fname,".csv")) ### Write table to csv file
  return(as.data.frame(outs)) ## and return
}

### Function to join a table with the area per Pedogenon for a particular study area (small study area), 
### which results from applying the function pedogenon.area.func, with
### the output from centroid.dist.func for all NSW (or larger study area)
### Inputs: 
### study.area.df - is the output of the pedogenon.area.func function for the study area (small)
### LARGE.centroid.dist.area.df - is the output from the centroid.dist.func, applied to the full (large) study area
### fname - name of the file to save the output
### Output: a dataframe with 6 columns and a row per pedogenon class.
# Pedogenon - Pedogenon class
# Study_area_km2 - area (km2) within the study area
# LARGE_area_Km2 - Total area of the pedogenon class in the whole (LARGE) area of study (e.g., New South Wales)
# Closest.Pedogenon - number designation of the closest pedogenon
# MahabDist - Mahalanobis distance between this centroid to the closest Pedogenon class
# Cl.Pedo.LARGE_area_Km2 - Total area of the closest pedogenon in the whole area (LARGE)  (e.g., New South Wales)

### Function to join the table for the study area and all NSW
study.pedogenon.area.func<- function(study.area.df, LARGE.centroid.dist.area.df, fname) {
  ### Change column names in study.area.df
  colnames(study.area.df) <- c("Pedogenon", "Study_area_km2")
  study.area.df$Pedogenon <- as.character(study.area.df$Pedogenon)
  colnames(LARGE.centroid.dist.area.df) <- c("Pedogenon", "Closest.Pedogenon","MahabDist","LARGE_area_Km2","Cl.Pedo.LARGE_area_Km2")
  study.area.df <- left_join(study.area.df, LARGE.centroid.dist.area.df, by="Pedogenon")
  study.area.df <- study.area.df[,c("Pedogenon","Study_area_km2","LARGE_area_Km2",
                                    "Closest.Pedogenon","MahabDist","Cl.Pedo.LARGE_area_Km2")]
  study.area.df <- arrange(study.area.df,- Study_area_km2) ### From larger to smaller Pedogenon class in the study area
  #head(study.area.df)
  study.area.df$Study_area_km2 <-round(study.area.df$Study_area_km2 , digits = 2)
  study.area.df$LARGE_area_Km2 <-round(study.area.df$LARGE_area_Km2 , digits = 2)
  study.area.df$Cl.Pedo.LARGE_area_Km2 <-round(study.area.df$Cl.Pedo.LARGE_area_Km2 , digits = 2)
  write.csv(study.area.df, file=paste0(fname,".csv")) ### Write table to csv file
  return(study.area.df) ## and return
  
}


# ### 2. Hierarchical clustering of pedogenons and color legend ------------

### First, perform the hierarchical clustering and save it to plot
### Input: kmodel - kmeans model from the package ClusterR
### Output: Hierarchical cluster (ward.D2 distance) of pedogenon centroids, hclust object
viz.map.legend.hclust <- function(kmodel) {
  
  ### Extract centroids from model
  centroids <- kmodel$centroids
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

# pal.names <- c("OrYel","PurpOr", "Dark Mint", "BurgYl","Turku",
#                "YlOrRd", "RdPu", "Peach", "GnBu","Lajolla", 
#                "OrRd", "Greens", "Burg", "Heat 2","Blues", 
#                "BuPu")

### Default choice of palettes, for the package colorspace
my_palette <- c("OrYel","PurpOr","TealGrn","BurgYl","RdPu",
                "GnBu","YlOrRd","Peach","Turku","Lajolla",
                "OrRd", "Greens", "Burg", "Heat 2", "Dark Mint",
                "Blues", "SunsetDark", "PuBuGn", "Viridis", "Heat")

### Function to map with the selected color palettes, based on the dedrogram, the Pedogenons for NSW (or any study area)
### input:
### kmodel - our kmeans model from the ClusterR package
### branchN - number of branches
### pal.names - selection of palettes from colorspace
### legend.name - name for the pdf to plot the legend (dendrogram)
### kmap - raster layer with Pedogenons
### Output: 
### $hc - Hierarchical clustering of the pedogenon centroids
### $branch.centroids.ord - Table with centroid (Pedogenon number), branch code, and assigned color 
### $legend.plot - dendrogram, legend with assigned colors
### $binpal - for leaflet
### $map.out - leaflet map

viz.map.legend.pal <- function(kmodel, branchN, pal.names, legend.name, kmap, need.proj){
  
  ### Extract centroids from model
  centroids <- kmodel$centroids
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
  
  ### Perform hierarchical clustering on centroids, with Ward.D2 method
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
  
  reorder_idx <- match(hc.labels,branch.centroids$Centroid) # Saving indices for how to reorder `branch.centroids$Centroid` to match `hc.labels`
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
  
  ### Project the map into the leaflet projection
  if (need.proj == TRUE) {
    kmap <- projectRaster(kmap, crs=CRS("+init=EPSG:3857"), method = "ngb")
  } else if (need.proj == FALSE) { 
    kmap <- kmap
    }
  
  binpal <- colorBin(palette = branch.centroids.ord$colors,
                     bins = c(branch.centroids.ord$Centroid,
                              tail(branch.centroids.ord$Centroid,1)+1),
                     na.color = "transparent")
  
  map.out <- leaflet() %>%
    # Base groups
    addTiles(group="OSM (default)") %>%
    addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>% # , group = "World Imagery"
    #addProviderTiles("OpenTopoMap", group = "Topo Map") %>%
    #addProviderTiles("Thunderforest.MobileAtlas", group = "Thunderforest.MobileAtlas")%>%
    addRasterImage(kmap, opacity = 1, colors=binpal, project=need.proj, 
                   maxBytes = 300000000, group = "Pedogenons") %>%
    #fitBounds(lng1=140, lat1=-38, lng2=154, lat2=-28) %>%
    leafem::addMouseCoordinates() %>%
    addLayersControl(
      baseGroups = c("OSM (default)","World Imagery"),
      overlayGroups = c("Pedogenons"),
      options = layersControlOptions(collapsed = FALSE)
    )
    # 
  #mapshot(map.out, file = paste0(OutDir,"/Map_",legend.name,".pdf"), remove_url = FALSE)
  output <- list("hc"=hc, "branch.centroids.ord"=branch.centroids.ord,
                 "legend.plot"=legend.plot, "map.out"=map.out)
  return(output)
  
}


### Function to calculate dendrogram only for the Pedogenons present in the study area
### Input: kmodel - kmeans model
### study.area.map - clip of raster Pedogenons only for the study area
### Output: Hierarchical cluster (ward.D2 distance) of centroids
viz.map.hclust.study.area <- function(kmodel, study.area.map) {
  ### Extract centroids from model
  K.centroids <- kmodel$centroids
  K.centroids <- as.data.frame(K.centroids)
  K.centroids$Pedogenon <- c(1:nrow(K.centroids))
  #rownames(K.centroids) <- c(1:nrow(K.centroids))
  ### Extract the unique values from the Pedogenon maps
  Unique.Geno.sa <- unique(getValues(study.area.map))
  ## Exclude NA
  Unique.Geno.sa <- Unique.Geno.sa[!is.na(Unique.Geno.sa)]
  ## Extract the index of the centroids that are na/nan/Inf
  ### Exclude these clusters from everywhere
  Kcent.exist <- K.centroids[K.centroids$Pedogenon %in% Unique.Geno.sa,]
  ### Hierarchical clustering
  hc <- hclust(dist(Kcent.exist[,!names(Kcent.exist) %in% c("Pedogenon")]), method="ward.D2")
  plot(dendsort(hc), main="Hierarchical clustering of kmeans centroids", sub="", xlab="")
  return(hc)
}


### Function to map with the selected color palettes, based on the dedrogram, the Pedogenons for the study area
### input:
### kmodel - our kmeans model
### branchN - number of branches
### pal.names - selection of palettes from colorspace
### legend.name - name for the pdf to plot the legend (dendrogram)
### study.area.map - clip of raster Pedogenons only for the study area
### Output: 
### $hc - Hierarchical cluster
### $branch.centroids.ord - Table with centroid, branch, and color
### $legend.plot - dendrogram, legend with colors.
### $binpal 
### $map.out - leaflet map
viz.map.legend.pal.study.area <- function(kmodel, branchN, pal.names, study.area.map, legend.name, need.proj){
  
  ### Subset number of palettes
  pal.names <- pal.names[1:branchN]
  
  ### Extract centroids from model
  K.centroids <- kmodel$centroids
  K.centroids <- as.data.frame(K.centroids)
  K.centroids$Pedogenon <- c(1:nrow(K.centroids))
  #rownames(K.centroids) <- c(1:nrow(K.centroids))
  ### Extract the unique values from the Pedogenon maps
  Unique.Geno.sa <- unique(getValues(study.area.map))
  ## Exclude NA
  Unique.Geno.sa <- Unique.Geno.sa[!is.na(Unique.Geno.sa)]
  ## Extract the index of the centroids that are na/nan/Inf
  ### Exclude these clusters from everywhere
  Kcent.exist <- K.centroids[K.centroids$Pedogenon %in% Unique.Geno.sa,]
  ### Hierarchical clustering
  hc <- hclust(dist(Kcent.exist[,!names(Kcent.exist) %in% c("Pedogenon")]), method="ward.D2")
  
  ### Extract labels
  hc.labels <- hc %>% as.dendrogram(.) %>% labels %>% as.numeric()
  
  ### Extract the membership from the tree
  dend <- hc %>% as.dendrogram(.)
  Kcent.exist$branch <- dend %>% dendextend:::cutree.dendrogram(., k = branchN)
  
  #branch.centroids <- as.data.frame(cbind(c(as.numeric(as.character(rownames(Kcent.exist)))),
  #                                        as.numeric(as.character(dendextend:::cutree.dendrogram(dend,k = branchN)))))
  branch.centroids <- Kcent.exist[,c("Pedogenon", "branch")]
  branch.centroids$Pedogenon <- as.numeric(branch.centroids$Pedogenon)
  branch.centroids$branch <- as.numeric(branch.centroids$branch)
  colnames(branch.centroids) <- c("Centroid", "Branch")
  
  ### sort them by the dendrogram labels
  #branch.centroids.ord <- branch.centroids %>% right_join(tibble(Centroid = hc.labels), by = "Centroid")
  reorder_idx <- match(hc.labels,branch.centroids$Centroid) # Saving indices for how to reorder `branch.centroids$Centroid` to match `hc.labels`
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
  
  legend.plot <- dend %>%  set("labels_col", branch.centroids.ord$colors) %>% 
    set("branches_k_color", branch.centroids.ord$colors)
  
  pdf(file = paste0("Map_legend",legend.name,".pdf"), width = 10, height = 40 )
  plot(legend.plot,
       main = "Hierarchical histogram of pedogenon centroids with the map colors",
       horiz = TRUE) # change color 
  dev.off()
  
  ### Now, reorder by Pedogenon class
  branch.centroids.ord <- branch.centroids.ord %>% arrange(., Centroid)
  #branch.centroids.ord <- branch.centroids.ord[order(branch.centroids.ord$Centroid),]
  
  ### Create palette for leaflet
  #pal <- branch.centroids.ord$colors
  ### Project the map into the leaflet projection
  if (need.proj == TRUE) {
    study.area.map <- projectRaster(study.area.map, crs=CRS("+init=EPSG:3857"), method = "ngb")
  } else if (need.proj == FALSE) {
    study.area.map <- study.area.map
    }
  
  #study.area.map <- projectRaster(study.area.map, crs=CRS("+init=EPSG:3857"), method = "ngb")
  
  binpal <- colorBin(palette = branch.centroids.ord$colors,
                     bins = c(branch.centroids.ord$Centroid,
                              tail(branch.centroids.ord$Centroid,1)+1),
                     na.color = "transparent")
  
  map.out <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
     #Base groups
    addTiles(group="OSM (default)") %>%
    #addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>% # , group = "World Imagery"
    #addProviderTiles("OpenTopoMap", group = "Topo Map") %>%
    addRasterImage(study.area.map, opacity = 1, colors=binpal, project=FALSE, 
                   layerId = "values", maxBytes = 300000000, group="Pedogenons") # %>%
    #fitBounds(lng1=140, lat1=-38, lng2=154, lat2=-28) %>%
    # leafem::addMouseCoordinates() %>%
    # addLayersControl(
    #   baseGroups = c("OSM (default)","World Imagery", "Topo Map"),
    #   overlayGroups = c("Pedogenons"),
    #   options = layersControlOptions(collapsed = FALSE)
    # )
  
  #mapshot(map.out, file = paste0(OutDir,"/Map_",legend.name,".pdf"), remove_url = FALSE)
  output <- list("hc" = hc, "branch.centroids.ord" = branch.centroids.ord,
                 "legend.plot" =legend.plot,  "map.out" = map.out)
  
  return(output)
  
}


### Function to map the Pedogenons present in a study area, their distribution across all NSW
### It works with the output from the function viz.map.legend.pal.study.area
### Input:
### study.area.Geno.out - Output from viz.map.legend.pal.study.area
### LARGE.Geno.map - Map for the large study area (e.g., NSW) with Pedogenon classes
### Output:
### $map.out - leaflet map
### $kmap - masks the Pedogenon classes not present in the study area
pedogenons.inStudy.area.func <- function(LARGE.Geno.map, study.area.Geno.out, need.proj) {
  
  ### Mask all Pedogenons not present in the study area
  pedogenons.present <-  study.area.Geno.out$branch.centroids.ord$Centroid ### the Pedogenon classes present in the study area
  pedogenons.present <- as.numeric(unlist(pedogenons.present))
  kmap <- trim(calc(LARGE.Geno.map, fun= function(x) {ifelse(x %in% pedogenons.present, x, NA)}))
  
  if (need.proj == TRUE) {
    kmap <- projectRaster(kmap, crs=CRS("+init=EPSG:3857"), method = "ngb")
  } else if (need.proj == FALSE) {
    kmap <- kmap
    }
  
  ### Create palette for leaflet
  ### Reorder by Pedogenon number
  study.area.Geno.out$branch.centroids.ord <- study.area.Geno.out$branch.centroids.ord %>%
     filter(., Centroid %in% pedogenons.present) %>% arrange(., Centroid)
   
  binpal <- colorBin(palette = study.area.Geno.out$branch.centroids.ord$colors,
                     bins = c(study.area.Geno.out$branch.centroids.ord$Centroid,
                              tail(study.area.Geno.out$branch.centroids.ord$Centroid,1)+1),
                     na.color = "transparent")
  
  map.out <- leaflet() %>%
    # Base groups
    addTiles(group="OSM (default)") %>%
    addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>% # , group = "World Imagery"
    addProviderTiles("OpenTopoMap", group = "Topo Map") %>%
    addProviderTiles("Stamen.TonerLite", group = "Stamen.TonerLite") %>%
    addRasterImage(kmap, opacity = 1, colors=binpal, project=FALSE, 
                   maxBytes = 300000000, group="Pedogenons")  %>%
    #fitBounds(lng1=140, lat1=-38, lng2=154, lat2=-28) %>%
    # leafem::addMouseCoordinates() %>%
    addLayersControl(
       baseGroups = c("OSM (default)","World Imagery", "Topo Map","Stamen.TonerLite"),
       overlayGroups = c("Pedogenons"),
       options = layersControlOptions(collapsed = FALSE))

  output <- list("map.out" = map.out, "kmap" = kmap)
  return(output)
  
}


### Variation of previous function. Mapping only those Pedogenons in the study area, but the surface has to be bigger than a certain value
### It works with the output from the function viz.map.legend.pal.study.area
### Input:
### study.area.Geno.out - Output from viz.map.legend.pal.study.area
### LARGE.Geno.map - Map for the whole study area (e.g., NSW) with Pedogenon classes
### study.area.df - table with the area and Pedogenons present in the study area, output from study.pedogenon.area.func
### min.area - minimum area for a Pedogenon in order to be included in the map
### Output:
### $map.out - leaflet map
### $kmap - masks the Pedogenon classes not present in the study area 
### $dendro.larger.pedogenons -  dendrogram with the larger classes in color, and the others in grey
pedogenons.inStudy.area.bigger.func <- function(LARGE.Geno.map, study.area.Geno.out, study.area.df, min.area,need.proj) {
  
  ### Subset those with an area larger than a certain value
  pedogenons.present  <- study.area.df %>% filter(., Study_area_km2 >= min.area) %>% dplyr::select(., Pedogenon)
  pedogenons.present <- as.numeric(unlist(pedogenons.present))
  ### Mask all Pedogenons not present in the study area
  kmap <- calc(LARGE.Geno.map, fun = function(x){ifelse((x %in% pedogenons.present), yes = x, no =NA)})
  if (need.proj == TRUE) {
    kmap <- projectRaster(kmap, crs=CRS("+init=EPSG:3857"), method = "ngb")
  } else if (need.proj == FALSE) {
    kmap <- kmap
  }
  # kmap <- projectRaster(kmap, crs=CRS("+init=EPSG:3857"), method = "ngb")
  
  ### Put Color only the main (larger than min.area) Pedogenons
    ### Extract the membership from the tree
  order.desired <- study.area.Geno.out$hc %>% as.dendrogram(.) %>% labels %>% as.numeric()
  reorder_idx <- match(order.desired, study.area.Geno.out$branch.centroids.ord$Centroid) # Saving indices for how to reorder `branch.centroids$Centroid` to match `hc.labels`
  study.area.Geno.out$branch.centroids.ord <- study.area.Geno.out$branch.centroids.ord[reorder_idx,]
  
  # study.area.Geno.out$branch.centroids.ord <- study.area.Geno.out$branch.centroids.ord %>%
  #  right_join(tibble(Centroid = order.desired), by = "Centroid")
  
  ### To put in bold
  study.area.Geno.out$branch.centroids.ord$colors <- ifelse(
    study.area.Geno.out$branch.centroids.ord$Centroid %in% pedogenons.present,
    study.area.Geno.out$branch.centroids.ord$colors,
    "gray85")
  
  study.area.Geno.out$branch.centroids.ord$cex.label <- ifelse(
    study.area.Geno.out$branch.centroids.ord$Centroid %in% pedogenons.present,
    1,
    0.25)
  
  dendro.larger.pedogenons <- study.area.Geno.out$hc %>% as.dendrogram(.) %>% 
    set("labels_col", study.area.Geno.out$branch.centroids.ord$colors) %>% 
    set("branches_k_color", study.area.Geno.out$branch.centroids.ord$colors) %>%
    set("labels_cex", study.area.Geno.out$branch.centroids.ord$cex.label)
  
  #plot(dendro.larger.pedogenons)
  
  ### Create palette for leaflet
  
  ### Reorder by Pedogenon number
  # centroid.leaflet.pal <- study.area.Geno.out$branch.centroids.ord %>%
  #   filter(., Centroid %in% pedogenons.present)  %>% arrange(., Centroid)
  #pal <- centroid.leaflet.pal$colors ### Extract the colors from previous output
  # binpal <- colorBin(palette = centroid.leaflet.pal$colors,
  #                    bins = c(centroid.leaflet.pal$Centroid,
  #                             tail(centroid.leaflet.pal$Centroid,1)+1),
  #                    na.color = "transparent")
  
  study.area.Geno.out$branch.centroids.ord <- study.area.Geno.out$branch.centroids.ord %>%
    filter(., Centroid %in% pedogenons.present) %>% arrange(., Centroid)
  
  binpal <- colorBin(palette = study.area.Geno.out$branch.centroids.ord$colors,
                     bins = c(study.area.Geno.out$branch.centroids.ord$Centroid,
                              tail(study.area.Geno.out$branch.centroids.ord$Centroid,1)+1),
                     na.color = "transparent")
  
  map.out <- leaflet() %>%
    # Base groups
    addTiles(group="OSM (default)") %>%
    addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>% # , group = "World Imagery"
    addProviderTiles("OpenTopoMap", group = "Topo Map") %>%
    addRasterImage(kmap, opacity = 1,  project=FALSE, colors=binpal,
                   maxBytes = 300000000, group="Pedogenons") %>%
    #fitBounds(lng1=140, lat1=-38, lng2=154, lat2=-28) %>%
    leafem::addMouseCoordinates() %>%
    addLayersControl(
      baseGroups = c("OSM (default)","World Imagery", "Topo Map"),
      overlayGroups = c("Pedogenons"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  output <- list("map.out" = map.out, "kmap" = kmap, 
                 "dendro.larger.pedogenons"=dendro.larger.pedogenons)
  return(output)
  
}


###End of script
