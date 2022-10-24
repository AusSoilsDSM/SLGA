## TERN Landscapes
# Soil Color Mapping of Australia 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# Created: 7.4.21
# Modified: 7.4.21

# CODE PURPOSE
# Develop up a soil line plot through comparison of Red and NIR predicitons
##

library(raster);library(hexbin);library(RColorBrewer);library(quantreg)

# root directories
data.root<- "/datasets/work/af-tern-mal-deb/work//projects/ternlandscapes_2019/soilColour/spatialPredictions/"
data.root2<- "/datasets/work/af-tern-mal-deb/work//projects/ternlandscapes_2019/soilColour/data/"




# surface red and nir rasters
nir.raster<- raster(paste0(data.root,"NIR_median.tif"))
nir.raster


red.raster<- raster(paste0(data.root,"type1_surface_R.tif"))
red.raster


comb.stack<- stack(nir.raster,red.raster)


# Points
p1<- sampleRandom(x = comb.stack, 500000)
p1<- as.data.frame(p1)
names(p1)
p1$rcov<- p1$type1_surface_R /255
saveRDS(object = p1, file = paste0(data.root2, "raster_random_sample_preds_color.rds"))

x1<- rep(seq(0,1,by = 0.05),20)
y1<- rep(seq(0,1,by = 0.05),21)
y1<- y1[order(y1)]
groups<- cbind(x1,y1)
groups<- as.data.frame(groups)
groups$count<- 0
groups$id<- 1:nrow(groups)

p1$group<- NA

for(i in 1:nrow(p1)){
  dists<- sqrt((groups[,1] -p1$NIR_median[i])^2 + (groups[,2] -p1$rcov[i])^2)
  p1$group[i]<- which(dists == min(dists))
}

uniq.grps<- unique(p1$group)
for(i in 1:length(uniq.grps)){
  sel<- uniq.grps[i]
  groups[which(groups$id == sel), 3]<- length(which(p1$group == sel))
}

avgs<- 500000/nrow(groups)
p1$count<- NA
for (i in 1:length(uniq.grps)){
  sz<- uniq.grps[i]
  tos<- which(p1$group == sz)
  p1$count[tos]<- groups[sz,3]
}

which(is.na(p1$count))

p1$weight<- p1$count/avgs
lmx<- lm(p1$NIR_median ~ p1$rcov)
lmxw<- lm(p1$NIR_median ~ p1$rcov, weights = p1$weight)
lmxw
lmx

bin<-hexbin(x = p1$rcov, y = p1$NIR_median, xbins=50)
bin@count[which(bin@count<500)]<- 0
plot(bin, main=""  , legend=1 ) 
hexVP.abline(hexViewport(bin), coefficients(lmx)[1], coefficients(lmx)[2],col="green")
hexVP.abline(hexViewport(bin), 0, 1,col="red")
hexVP.abline(hexViewport(bin), coefficients(lmxw)[1], coefficients(lmxw)[2],col="blue")

#quantile regression
multi_rqfit <- rq(p1$NIR_median ~ p1$rcov, tau = seq(0, 1, by = 0.1))
coefficients(multi_rqfit)
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,1], coefficients(multi_rqfit)[2,1],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,2], coefficients(multi_rqfit)[2,2],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,3], coefficients(multi_rqfit)[2,3],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,4], coefficients(multi_rqfit)[2,4],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,5], coefficients(multi_rqfit)[2,5],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,6], coefficients(multi_rqfit)[2,6],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,7], coefficients(multi_rqfit)[2,7],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,8], coefficients(multi_rqfit)[2,8],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,9], coefficients(multi_rqfit)[2,9],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,10], coefficients(multi_rqfit)[2,10],col="grey")


### Weed out very low frequency points
g1<- which(groups$count >= 5000)
selg<- groups[g1,]
selg

p2<- p1[p1$group %in% selg$id,]
p2$weight<- p2$count/avgs
lmx_1<- lm(p2$NIR_median ~ p2$rcov)
lmxw_1<- lm(p2$NIR_median ~ p2$rcov, weights = p2$weight)
lmxw_1
lmx_1

bin<-hexbin(x = p2$rcov, y = p2$NIR_median, xbins=50)
bin@count[which(bin@count<500)]<- 0
plot(bin, main=""  , legend=1 ) 
hexVP.abline(hexViewport(bin), coefficients(lmx_1)[1], coefficients(lmx_1)[2],col="green")
hexVP.abline(hexViewport(bin), 0, 1,col="red")
hexVP.abline(hexViewport(bin), coefficients(lmxw_1)[1], coefficients(lmxw_1)[2],col="blue")

#quantile regression
multi_rqfit <- rq(p2$NIR_median ~ p2$rcov, tau = seq(0, 1, by = 0.1))
coefficients(multi_rqfit)
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,1], coefficients(multi_rqfit)[2,1],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,2], coefficients(multi_rqfit)[2,2],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,3], coefficients(multi_rqfit)[2,3],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,4], coefficients(multi_rqfit)[2,4],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,5], coefficients(multi_rqfit)[2,5],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,6], coefficients(multi_rqfit)[2,6],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,7], coefficients(multi_rqfit)[2,7],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,8], coefficients(multi_rqfit)[2,8],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,9], coefficients(multi_rqfit)[2,9],col="grey")
hexVP.abline(hexViewport(bin), coefficients(multi_rqfit)[1,10], coefficients(multi_rqfit)[2,10],col="grey")


# ndvi
p1$ndvi<- (p1$NIR_median - p1$rcov)/(p1$NIR_median+p1$rcov)
hist(p1$ndvi)

# ndvi
p2$ndvi<- (p2$NIR_median - p2$rcov)/(p2$NIR_median+p2$rcov)
hist(p2$ndvi)




### Polygon plotting
head(groups)

# group up the variable importances 
grp_class<- c(0,1000,2500,5000,10000)

groups$class<- NA
for (j in 1:nrow(groups)){
  if(is.na(groups$count[j]) == TRUE){groups$class[j]<- NA} else {
    if(groups$count[j] >= grp_class[1] && groups$count[j] <= grp_class[2]){groups$class[j]<- 1}
    if(groups$count[j] > grp_class[2] && groups$count[j] <= grp_class[3]){groups$class[j]<- 2}
    if(groups$count[j] > grp_class[3] && groups$count[j] <= grp_class[4]){groups$class[j]<- 3}
    if(groups$count[j] > grp_class[4] && groups$count[j] <= grp_class[5]){groups$class[j]<- 4}
    if(groups$count[j] > grp_class[5]){groups$class[j]<- 5}}}


## plotting
cols <- brewer.pal(n = 4, name = "YlOrRd")
cols<- c("#f5f5f5",cols)
cols


##blank plot to be a template
xp<- seq(0,1,0.1)
yp<- seq(0,1,0.1)
plot(xp,yp,xlim= c(0,1), ylim= c(0,1), type= "n",axes=F,ylab="NIR", xlab= "RED",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(0,1,0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(0,1,0.1),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")

### make polygons and add them to the plot
for (j in 1:nrow(groups)){
  sel<- groups[j,]
  
  # polygon shape
  xx<- c(sel$y1,sel$y1,sel$y1+0.05,sel$y1+0.05)
  yy<- c(sel$x1,sel$x1+0.05,sel$x1+0.05,sel$x1)
  
  #polygon cols
  if(sel$class == 1){polygon (xx,yy, lty=1, lwd=0.1,col=cols[1], border="white")}
  if(sel$class == 2){polygon (xx,yy, lty=1, lwd=0.1,col=cols[2], border="white")}
  if(sel$class == 3){polygon (xx,yy, lty=1, lwd=0.1,col=cols[3], border="white")}
  if(sel$class == 4){polygon (xx,yy, lty=1, lwd=0.1,col=cols[4], border="white")}
  if(sel$class == 5){polygon (xx,yy, lty=1, lwd=0.1,col=cols[5], border="white")}}


# ndvi lines
##ndvi isolines
isolines<- read.csv(file = paste0(data.root2,"ndvi_isolines.csv"))
isolines<- isolines/100
for (j in 2:ncol(isolines)){
  lines(isolines[,1],isolines[,j], col= "light grey")}

abline(0,1,col="black")

legend("bottomright", inset=.02, title="Proportion of pixels",
       c("0-0.002%","0.002-0.005%","0.005-0.01%", "0.01-0.02%", ">0.02%"), fill=cols, horiz=F, cex=1.1)

text(x = 0.11, 0.99, "0.8")
text(x = 0.25, 0.99, "0.6")
text(x = 0.42, 0.99, "0.4")
text(x = 0.65, 0.99, "0.2")
text(x = 0.99, 0.65, "-0.2")
text(x = 0.99, 0.42, "-0.4")
text(x = 0.65, 0.16, "-0.6")
text(x = 0.65, 0.07, "-0.8")
text(x = 0.99, 0.99, "1")





