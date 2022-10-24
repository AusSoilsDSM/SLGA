## TERN Landscapes
# Soil Color Mapping of Australia 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# Created: 7.4.21
# Modified: 12.4.21

# CODE PURPOSE
# Fiddle around with moist color preds to make them appear dry ie increase value by 1 or 2 or maybe combinations of the two
##

#libraries
library(raster);library(RColorBrewer);library(munsell);library(scales)

# root directories
data.root<- "/datasets/work/af-tern-mal-deb/work//projects/ternlandscapes_2019/soilColour/spatialPredictions/"
data.root2<- "/datasets/work/af-tern-mal-deb/work//projects/ternlandscapes_2019/soilColour/data/"
out.root<- "/datasets/work/af-tern-mal-deb/work//projects/ternlandscapes_2019/soilColour/outputs/"

##data sets
#reference soil color
ref.colors<- read.csv(file = paste0(data.root2,"soilColorRef_updated.csv"))


# surface red and nir rasters
nir.raster<- raster(paste0(data.root,"NIR_median.tif"))
nir.raster


red.raster<- raster(paste0(data.root,"type1_surface_R.tif"))
red.raster

green.raster<- raster(paste0(data.root,"type1_surface_G.tif"))
green.raster

blue.raster<- raster(paste0(data.root,"type1_surface_B.tif"))
blue.raster


comb.stack<- stack(nir.raster,red.raster,green.raster, blue.raster)


# Points
p1<- sampleRandom(x = comb.stack, 500000)
p1<- as.data.frame(p1)
names(p1)

names(ref.colors)
p1_ref_col<- ref.colors[0,c(1:6,13:15)]
p1_ref_col_1<- ref.colors[0,c(1:6,13:15)]
p1_ref_col_2<- ref.colors[0,c(1:6,13:15)]
p1_ref_col_3<- ref.colors[0,c(1:6,13:15)]

# Go through each row
for (j in 1:nrow(p1)){
  
  # disitance calcs
  d1<-sqrt((p1$type1_surface_R[j] -  ref.colors$R)^2 + (p1$type1_surface_G[j] -  ref.colors$G)^2 + (p1$type1_surface_B[j] -  ref.colors$B)^2)
  d1.sel<- which(d1 == min(d1))
  min(d1)
  p1_ref_col[j,]<- ref.colors[d1.sel,c(1:6,13:15)]
  
  ### add 1 to value
  p1_ref_col_1[j,1:6]<- ref.colors[d1.sel,c(1:6)]
  p1_ref_col_1$Value[j]<- p1_ref_col_1$Value[j] +1
  d2<-sqrt((p1_ref_col_1$Angle[j] -  ref.colors$Angle)^2 + (p1_ref_col_1$HUE1[j] -  ref.colors$HUE1)^2 + (p1_ref_col_1$Value[j] -  ref.colors$Value)^2 + (p1_ref_col_1$Chroma[j] -  ref.colors$Chroma)^2)
  d2.sel<- which(d2 == min(d2))
  p1_ref_col_1[j,7:9]<- ref.colors[d2.sel,c(13:15)]
  
  ### add 2 to value
  p1_ref_col_2[j,1:6]<- ref.colors[d1.sel,c(1:6)]
  p1_ref_col_2$Value[j]<- p1_ref_col_2$Value[j] +2
  d3<-sqrt((p1_ref_col_2$Angle[j] -  ref.colors$Angle)^2 + (p1_ref_col_2$HUE1[j] -  ref.colors$HUE1)^2 + (p1_ref_col_2$Value[j] -  ref.colors$Value)^2 + (p1_ref_col_2$Chroma[j] -  ref.colors$Chroma)^2)
  d3.sel<- which(d3 == min(d3))
  p1_ref_col_2[j,7:9]<- ref.colors[d3.sel,c(13:15)]
  
  ### add 1 or 2 to value
  p1_ref_col_3[j,1:6]<- ref.colors[d1.sel,c(1:6)]
  samp<- sample(1:2,1)
  p1_ref_col_3$Value[j]<- p1_ref_col_3$Value[j] + samp
  d4<-sqrt((p1_ref_col_3$Angle[j] -  ref.colors$Angle)^2 + (p1_ref_col_3$HUE1[j] -  ref.colors$HUE1)^2 + (p1_ref_col_3$Value[j] -  ref.colors$Value)^2 + (p1_ref_col_3$Chroma[j] -  ref.colors$Chroma)^2)
  d4.sel<- which(d4 == min(d4))
  p1_ref_col_3[j,7:9]<- ref.colors[d4.sel,c(13:15)]}


names(p1_ref_col)
p1_ref_col_3[232384,]<- NA

px<- p1[1:232384,]

px<- cbind(px, p1_ref_col_1)
names(px)[11:13]<- c("R_1", "G_1", "B_1")
px$type1_surface_R<- px$type1_surface_R/255
px$R_1<-  px$R_1/255

px<- cbind(px,p1_ref_col_2)
names(px)[20:22]<- c("R_2", "G_2", "B_2")
px$R_2<-  px$R_2/255

px<- cbind(px,p1_ref_col_3)
names(px)[29:31]<- c("R_3", "G_3", "B_3")
px$R_3<-  px$R_3/255


px<- px[1:232000,]



### Groupings
x1<- rep(seq(0,1,by = 0.05),21)
y1<- rep(seq(0,1,by = 0.05),21)
y1<- y1[order(y1)]
groups<- cbind(x1,y1)
groups<- as.data.frame(groups)
groups$id<- 1:nrow(groups)
groups$count_A<- NA
groups$count_1<- NA
groups$count_2<- NA
groups$count_3<- NA

px$group_A<- NA
px$group_1<- NA
px$group_2<- NA
px$group_3<- NA

px$ndvi_A<- (px$NIR_median - px$type1_surface_R)/(px$NIR_median+px$type1_surface_R)
px$ndvi_1<- (px$NIR_median - px$R_1)/(px$NIR_median+px$R_1)
px$ndvi_2<- (px$NIR_median - px$R_2)/(px$NIR_median+px$R_2)
px$ndvi_3<- (px$NIR_median - px$R_3)/(px$NIR_median+px$R_3)

summary(px$ndvi_A)
hist(px$ndvi_A)

summary(px$ndvi_1)
hist(px$ndvi_1)

summary(px$ndvi_2)
hist(px$ndvi_2)

summary(px$ndvi_3)
hist(px$ndvi_3)

for(i in 1:nrow(px)){
  
  # grouping
  dists_A<- sqrt((groups[,1] -px$NIR_median[i])^2 + (groups[,2] -px$type1_surface_R[i])^2)
  px$group_A[i]<- which(dists_A == min(dists_A))
  
  dists_1<- sqrt((groups[,1] -px$NIR_median[i])^2 + (groups[,2] -px$R_1[i])^2)
  px$group_1[i]<- which(dists_1 == min(dists_1))
  
  dists_2<- sqrt((groups[,1] -px$NIR_median[i])^2 + (groups[,2] -px$R_2[i])^2)
  px$group_2[i]<- which(dists_2 == min(dists_2))
  
  dists_3<- sqrt((groups[,1] -px$NIR_median[i])^2 + (groups[,2] -px$R_3[i])^2)
  px$group_3[i]<- which(dists_3 == min(dists_3))
  print(i)
}

names(groups)

uniq.grps_A<- unique(px$group_A)
length(uniq.grps_A)
for(i in 1:length(uniq.grps_A)){
  sel<- uniq.grps_A[i]
  groups[which(groups$id == sel), "count_A"]<- length(which(px$group_A == sel))
}

uniq.grps_1<- unique(px$group_1)
length(uniq.grps_1)
for(i in 1:length(uniq.grps_1)){
  sel<- uniq.grps_1[i]
  groups[which(groups$id == sel), "count_1"]<- length(which(px$group_1 == sel))
}

uniq.grps_2<- unique(px$group_2)
length(uniq.grps_2)
for(i in 1:length(uniq.grps_2)){
  sel<- uniq.grps_2[i]
  groups[which(groups$id == sel), "count_2"]<- length(which(px$group_2 == sel))
}

uniq.grps_3<- unique(px$group_3)
length(uniq.grps_3)
for(i in 1:length(uniq.grps_3)){
  sel<- uniq.grps_3[i]
  groups[which(groups$id == sel), "count_3"]<- length(which(px$group_3 == sel))
}


### Polygon plotting [original]
head(groups)

# group up the variable importances 
avgs<- 232000/nrow(groups)
grp_class<- c(0,1000,2500,5000,10000)
grp_class/232000

groups$class_A<- NA
for (j in 1:nrow(groups)){
  if(is.na(groups$count_A[j]) == TRUE){groups$class_A[j]<- 1} else {
    if(groups$count_A[j] >= grp_class[1] && groups$count_A[j] <= grp_class[2]){groups$class_A[j]<- 1}
    if(groups$count_A[j] > grp_class[2] && groups$count_A[j] <= grp_class[3]){groups$class_A[j]<- 2}
    if(groups$count_A[j] > grp_class[3] && groups$count_A[j] <= grp_class[4]){groups$class_A[j]<- 3}
    if(groups$count_A[j] > grp_class[4] && groups$count_A[j] <= grp_class[5]){groups$class_A[j]<- 4}
    if(groups$count_A[j] > grp_class[5]){groups$class_A[j]<- 5}}}


## plotting
cols <- brewer.pal(n = 4, name = "YlOrRd")
cols<- c("#f5f5f5",cols)
cols


##blank plot to be a template
tiff(file=paste0(out.root,"red_nir_orig_ndvi_isoline.tif"),width=15,height=15,units="cm",res=300,pointsize=8)

xp<- seq(0,1,0.1)
yp<- seq(0,1,0.1)
plot(xp,yp,xlim= c(0,1), ylim= c(0,1), type= "n",axes=F,ylab="NIR", xlab= "RED",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans", main= "Field Moist Colour and Dry ground NIR")
axis(side=2,at=seq(0,1,0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(0,1,0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")

### make polygons and add them to the plot
for (j in 1:nrow(groups)){
  sel<- groups[j,]
  
  # polygon shape
  xx<- c(sel$y1,sel$y1,sel$y1+0.05,sel$y1+0.05)
  yy<- c(sel$x1,sel$x1+0.05,sel$x1+0.05,sel$x1)
  
  #polygon cols
  if(sel$class_A == 1){polygon (xx,yy, lty=1, lwd=0.1,col=cols[1], border="white")}
  if(sel$class_A == 2){polygon (xx,yy, lty=1, lwd=0.1,col=cols[2], border="white")}
  if(sel$class_A == 3){polygon (xx,yy, lty=1, lwd=0.1,col=cols[3], border="white")}
  if(sel$class_A == 4){polygon (xx,yy, lty=1, lwd=0.1,col=cols[4], border="white")}
  if(sel$class_A == 5){polygon (xx,yy, lty=1, lwd=0.1,col=cols[5], border="white")}}


# ndvi lines
##ndvi isolines
isolines<- read.csv(file = paste0(data.root2,"ndvi_isolines.csv"))
isolines<- isolines/100
for (j in 2:ncol(isolines)){
  lines(isolines[,1],isolines[,j], col= "light grey")}

abline(0,1,col="black")

legend("bottomright", inset=.02, title="Proportion of pixels",
       c("0-0.004%","0.004-0.01%","0.01-0.02%", "0.02-0.04%", ">0.04%"), fill=cols, horiz=F, cex=1.1)

text(x = 0.11, 0.99, "0.8")
text(x = 0.25, 0.99, "0.6")
text(x = 0.42, 0.99, "0.4")
text(x = 0.65, 0.99, "0.2")
text(x = 0.99, 0.65, "-0.2")
text(x = 0.99, 0.42, "-0.4")
text(x = 0.60, 0.16, "-0.6")
text(x = 0.60, 0.07, "-0.8")
text(x = 0.99, 0.99, "1")

dev.off()

### Polygon plotting [value +1]
head(groups)

# group up the variable importances 
grp_class<- c(0,1000,2500,5000,10000)

groups$class_1<- NA
for (j in 1:nrow(groups)){
  if(is.na(groups$count_1[j]) == TRUE){groups$class_1[j]<- 1} else {
    if(groups$count_1[j] >= grp_class[1] && groups$count_1[j] <= grp_class[2]){groups$class_1[j]<- 1}
    if(groups$count_1[j] > grp_class[2] && groups$count_1[j] <= grp_class[3]){groups$class_1[j]<- 2}
    if(groups$count_1[j] > grp_class[3] && groups$count_1[j] <= grp_class[4]){groups$class_1[j]<- 3}
    if(groups$count_1[j] > grp_class[4] && groups$count_1[j] <= grp_class[5]){groups$class_1[j]<- 4}
    if(groups$count_1[j] > grp_class[5]){groups$class_1[j]<- 5}}}


## plotting
cols <- brewer.pal(n = 4, name = "YlOrRd")
cols<- c("#f5f5f5",cols)
cols


##blank plot to be a template
tiff(file=paste0(out.root,"red_nir_val1_ndvi_isoline.tif"),width=15,height=15,units="cm",res=300,pointsize=8)

xp<- seq(0,1,0.1)
yp<- seq(0,1,0.1)
plot(xp,yp,xlim= c(0,1), ylim= c(0,1), type= "n",axes=F,ylab="NIR", xlab= "RED",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans", main= "Field Moist Colour (value +1) and Dry ground NIR")
axis(side=2,at=seq(0,1,0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(0,1,0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")

### make polygons and add them to the plot
for (j in 1:nrow(groups)){
  sel<- groups[j,]
  
  # polygon shape
  xx<- c(sel$y1,sel$y1,sel$y1+0.05,sel$y1+0.05)
  yy<- c(sel$x1,sel$x1+0.05,sel$x1+0.05,sel$x1)
  
  #polygon cols
  if(sel$class_1 == 1){polygon (xx,yy, lty=1, lwd=0.1,col=cols[1], border="white")}
  if(sel$class_1 == 2){polygon (xx,yy, lty=1, lwd=0.1,col=cols[2], border="white")}
  if(sel$class_1 == 3){polygon (xx,yy, lty=1, lwd=0.1,col=cols[3], border="white")}
  if(sel$class_1 == 4){polygon (xx,yy, lty=1, lwd=0.1,col=cols[4], border="white")}
  if(sel$class_1 == 5){polygon (xx,yy, lty=1, lwd=0.1,col=cols[5], border="white")}}


# ndvi lines
##ndvi isolines
isolines<- read.csv(file = paste0(data.root2,"ndvi_isolines.csv"))
isolines<- isolines/100
for (j in 2:ncol(isolines)){
  lines(isolines[,1],isolines[,j], col= "light grey")}

abline(0,1,col="black")

legend("bottomright", inset=.02, title="Proportion of pixels",
       c("0-0.004%","0.004-0.01%","0.01-0.02%", "0.02-0.04%", ">0.04%"), fill=cols, horiz=F, cex=1.1)

text(x = 0.11, 0.99, "0.8")
text(x = 0.25, 0.99, "0.6")
text(x = 0.42, 0.99, "0.4")
text(x = 0.65, 0.99, "0.2")
text(x = 0.99, 0.65, "-0.2")
text(x = 0.99, 0.42, "-0.4")
text(x = 0.65, 0.16, "-0.6")
text(x = 0.65, 0.07, "-0.8")
text(x = 0.99, 0.99, "1")

dev.off()



### Polygon plotting [value +2]
head(groups)

# group up the variable importances 
grp_class<- c(0,1000,2500,5000,10000)

groups$class_2<- NA
for (j in 1:nrow(groups)){
  if(is.na(groups$count_2[j]) == TRUE){groups$class_2[j]<- 1} else {
    if(groups$count_2[j] >= grp_class[1] && groups$count_2[j] <= grp_class[2]){groups$class_2[j]<- 1}
    if(groups$count_2[j] > grp_class[2] && groups$count_2[j] <= grp_class[3]){groups$class_2[j]<- 2}
    if(groups$count_2[j] > grp_class[3] && groups$count_2[j] <= grp_class[4]){groups$class_2[j]<- 3}
    if(groups$count_2[j] > grp_class[4] && groups$count_2[j] <= grp_class[5]){groups$class_2[j]<- 4}
    if(groups$count_2[j] > grp_class[5]){groups$class_2[j]<- 5}}}


## plotting
cols <- brewer.pal(n = 4, name = "YlOrRd")
cols<- c("#f5f5f5",cols)
cols


##blank plot to be a template
tiff(file=paste0(out.root,"red_nir_val2_ndvi_isoline.tif"),width=15,height=15,units="cm",res=300,pointsize=8)

xp<- seq(0,1,0.1)
yp<- seq(0,1,0.1)
plot(xp,yp,xlim= c(0,1), ylim= c(0,1), type= "n",axes=F,ylab="NIR", xlab= "RED",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans", main= "Field Moist Colour (value +1) and Dry ground NIR")
axis(side=2,at=seq(0,1,0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(0,1,0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")

### make polygons and add them to the plot
for (j in 1:nrow(groups)){
  sel<- groups[j,]
  
  # polygon shape
  xx<- c(sel$y1,sel$y1,sel$y1+0.05,sel$y1+0.05)
  yy<- c(sel$x1,sel$x1+0.05,sel$x1+0.05,sel$x1)
  
  #polygon cols
  if(sel$class_2 == 1){polygon (xx,yy, lty=1, lwd=0.1,col=cols[1], border="white")}
  if(sel$class_2 == 2){polygon (xx,yy, lty=1, lwd=0.1,col=cols[2], border="white")}
  if(sel$class_2 == 3){polygon (xx,yy, lty=1, lwd=0.1,col=cols[3], border="white")}
  if(sel$class_2 == 4){polygon (xx,yy, lty=1, lwd=0.1,col=cols[4], border="white")}
  if(sel$class_2 == 5){polygon (xx,yy, lty=1, lwd=0.1,col=cols[5], border="white")}}


# ndvi lines
##ndvi isolines
isolines<- read.csv(file = paste0(data.root2,"ndvi_isolines.csv"))
isolines<- isolines/100
for (j in 2:ncol(isolines)){
  lines(isolines[,1],isolines[,j], col= "light grey")}

abline(0,1,col="black")

legend("bottomright", inset=.02, title="Proportion of pixels",
       c("0-0.004%","0.004-0.01%","0.01-0.02%", "0.02-0.04%", ">0.04%"), fill=cols, horiz=F, cex=1.1)

text(x = 0.11, 0.99, "0.8")
text(x = 0.25, 0.99, "0.6")
text(x = 0.42, 0.99, "0.4")
text(x = 0.65, 0.99, "0.2")
text(x = 0.99, 0.65, "-0.2")
text(x = 0.99, 0.42, "-0.4")
text(x = 0.65, 0.16, "-0.6")
text(x = 0.65, 0.07, "-0.8")
text(x = 0.99, 0.99, "1")

dev.off()


### Polygon plotting [value + 1 or 2]
head(groups)

# group up the variable importances 
grp_class<- c(0,1000,2500,5000,10000)

groups$class_3<- NA
for (j in 1:nrow(groups)){
  if(is.na(groups$count_3[j]) == TRUE){groups$class_3[j]<- 1} else {
    if(groups$count_3[j] >= grp_class[1] && groups$count_3[j] <= grp_class[2]){groups$class_3[j]<- 1}
    if(groups$count_3[j] > grp_class[2] && groups$count_3[j] <= grp_class[3]){groups$class_3[j]<- 2}
    if(groups$count_3[j] > grp_class[3] && groups$count_3[j] <= grp_class[4]){groups$class_3[j]<- 3}
    if(groups$count_3[j] > grp_class[4] && groups$count_3[j] <= grp_class[5]){groups$class_3[j]<- 4}
    if(groups$count_3[j] > grp_class[5]){groups$class_3[j]<- 5}}}


## plotting
cols <- brewer.pal(n = 4, name = "YlOrRd")
cols<- c("#f5f5f5",cols)
cols


##blank plot to be a template
tiff(file=paste0(out.root,"red_nir_val3_ndvi_isoline.tif"),width=15,height=15,units="cm",res=300,pointsize=8)

xp<- seq(0,1,0.1)
yp<- seq(0,1,0.1)
plot(xp,yp,xlim= c(0,1), ylim= c(0,1), type= "n",axes=F,ylab="NIR", xlab= "RED",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans", main= "Field Moist Colour (value +1) and Dry ground NIR")
axis(side=2,at=seq(0,1,0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(0,1,0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")

### make polygons and add them to the plot
for (j in 1:nrow(groups)){
  sel<- groups[j,]
  
  # polygon shape
  xx<- c(sel$y1,sel$y1,sel$y1+0.05,sel$y1+0.05)
  yy<- c(sel$x1,sel$x1+0.05,sel$x1+0.05,sel$x1)
  
  #polygon cols
  if(sel$class_3 == 1){polygon (xx,yy, lty=1, lwd=0.1,col=cols[1], border="white")}
  if(sel$class_3 == 2){polygon (xx,yy, lty=1, lwd=0.1,col=cols[2], border="white")}
  if(sel$class_3 == 3){polygon (xx,yy, lty=1, lwd=0.1,col=cols[3], border="white")}
  if(sel$class_3 == 4){polygon (xx,yy, lty=1, lwd=0.1,col=cols[4], border="white")}
  if(sel$class_3 == 5){polygon (xx,yy, lty=1, lwd=0.1,col=cols[5], border="white")}}


# ndvi lines
##ndvi isolines
isolines<- read.csv(file = paste0(data.root2,"ndvi_isolines.csv"))
isolines<- isolines/100
for (j in 2:ncol(isolines)){
  lines(isolines[,1],isolines[,j], col= "light grey")}

abline(0,1,col="black")



text(x = 0.11, 0.99, "0.8")
text(x = 0.25, 0.99, "0.6")
text(x = 0.42, 0.99, "0.4")
text(x = 0.65, 0.99, "0.2")
text(x = 0.99, 0.65, "-0.2")
text(x = 0.99, 0.42, "-0.4")
text(x = 0.60, 0.16, "-0.6")
text(x = 0.60, 0.07, "-0.8")
text(x = 0.99, 0.99, "1")

dev.off()



### plots of R vs NIR with points colored with munsel colours
tiff(file=paste0(out.root,"red_nir_soilcolour_isoline.tif"),width=15,height=15,units="cm",res=300,pointsize=8)
xp<- seq(0,1,0.1)
yp<- seq(0,1,0.1)
plot(xp,yp,xlim= c(0,1), ylim= c(0,1), type= "n",axes=F,ylab="NIR", xlab= "RED",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans", main= "Relation of soil colour with R and NIR")
axis(side=2,at=seq(0,1,0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(0,1,0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")

names(px)
pz_col<- px[,1:10]

isolines<- read.csv(file = paste0(data.root2,"ndvi_isolines.csv"))
isolines<- isolines/100
for (j in 2:ncol(isolines)){
  lines(isolines[,1],isolines[,j], col= "light grey")}

abline(0,1,col="black")
text(x = 0.11, 0.99, "0.8")
text(x = 0.25, 0.99, "0.6")
text(x = 0.42, 0.99, "0.4")
text(x = 0.65, 0.99, "0.2")
text(x = 0.99, 0.65, "-0.2")
text(x = 0.99, 0.42, "-0.4")
text(x = 0.99, 0.25, "-0.6")
text(x = 0.99, 0.11, "-0.8")
text(x = 0.99, 0.99, "1")

for (i in 1:nrow(pz_col)){
  sel<- pz_col[i, ]
  col_str<- paste0(format(round(sel$HUE1, 2), nsmall = 0),sel$HUE2, " ",sel$Value,"/",sel$Chroma)
  sel_hexcol<- mnsl2hex(col_str)
  points(x = sel$type1_surface_R, y = sel$NIR_median, col= sel_hexcol,pch=16,cex=0.5)
  #points(x = sel$type1_surface_R, y = sel$NIR_median,pch=16,cex=0.5,alpha(colour = sel_hexcol, 0.5))
}

dev.off()


