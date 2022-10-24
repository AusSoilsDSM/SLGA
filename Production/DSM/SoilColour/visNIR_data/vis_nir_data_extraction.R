library(sp);library(rgdal);library(ithir)



## spectra library
spec.dat<-readRDS("Z:/projects/openSpecs_2019/data/aus_soilspecarchive_stage1.rds")
names(spec.dat)

#remove nas
# remove cases with missing spectra data
ms<- which(!complete.cases(spec.dat[,which(names(spec.dat)=="453"):which(names(spec.dat)=="2500")]))
ms
spec.dat<- spec.dat[-ms,]

# soil color data
# lab data
soil.dat<- readRDS("Z:/projects/ternlandscapes_2019/soilColour/data/processed_1/combined_soil_color_out_process1_topsoils_2ndround_matched.rds")


## surface soil vis-NIR data
surf.dat<- readRDS("Z:/projects/ternlandscapes_2019/soilColour/data/visNIR/aus_visNIR_locs_subset_updated.rds")
str(surf.dat)
surf.dat$NIR<- NA
surf.dat$R<- NA
surf.dat$G<- NA
surf.dat$B<- NA




for (i in 1:nrow(surf.dat)){
  sel.pt<- t(as.matrix(as.numeric(surf.dat[i,3:2])))
  all.pts<- as.matrix(soil.dat[,17:18])
  dists<- spDistsN1(all.pts, sel.pt, longlat = TRUE)
  b1<- which(dists== min(dists))
  sel.dist<- dists[b1]
  if(sel.dist >=5){next}else {
  surf.dat[i,7:9]<- soil.dat[b1,39:41]
  print(c(i,sel.dist))}}
  

# remove points with no legacy soil color data
rmn<- which(is.na(surf.dat$R))
surf.dat<- surf.dat[-rmn,]
surf.dat$visNIR_R<- NA
surf.dat$visNIR_G<- NA
surf.dat$visNIR_B<- NA
surf.dat$visNIR_NIR<- NA





# go through each selction and pull out the spectrum and save to table
for (i in 1:nrow(surf.dat)){
  spec.id<- surf.dat$mySpecID[i]
  sel<- which(spec.dat$specID == spec.id)
  surf.dat$visNIR_R[i]<- mean(c(as.matrix(spec.dat[sel,which(names(spec.dat)=="600"):which(names(spec.dat)=="690")])))
  surf.dat$visNIR_G[i]<- mean(c(as.matrix(spec.dat[sel,which(names(spec.dat)=="520"):which(names(spec.dat)=="599")])))
  surf.dat$visNIR_B[i]<- mean(c(as.matrix(spec.dat[sel,which(names(spec.dat)=="450"):which(names(spec.dat)=="519")])))
  surf.dat$visNIR_NIR[i]<- mean(c(as.matrix(spec.dat[sel,which(names(spec.dat)=="840"):which(names(spec.dat)=="880")])))
  
  print(i)}

# Get rid of outliers
centers<- c(mean(surf.dat$visNIR_R),mean(surf.dat$visNIR_G),mean(surf.dat$visNIR_B),mean(surf.dat$visNIR_NIR))
centers
cov.mat<- cov(surf.dat[,10:13])

# create an empty matrix
chiMat <- matrix(NA, ncol = 3, nrow = nrow(surf.dat))


chiMat[,1] <- sqrt(mahalanobis(surf.dat[,10:13],
                               centers,
                               cov.mat))
# Chi square distribution of the distances
chiMat[, 2] <- pchisq(c(chiMat[, 1]), df = 4)

plot(chiMat[, 1],chiMat[, 2], xlab= "distance", ylab="cumulative probability")

# which spectra are outliers
pcrit<- 0.90 # remove the furtherest 1% from the data
for (i in 1:nrow(chiMat)){
  if (chiMat[i,2] >= pcrit)
  {chiMat[i,3]<-0}else{chiMat[i,3]<-1}}


par(mfrow=c(1,1))
plot(chiMat[,1],chiMat[,2], xlab= "distance", ylab="cumlative prob")
points(chiMat[which(chiMat[,3]==0),1:2],pch='X',col='red')
abline(v=qchisq(pcrit,5),col="green")


##Also remove the spectra outliers from the original data
new.surf.dat<- surf.dat[chiMat[,3] == 1,]

m1<- mean(new.surf.dat$R - new.surf.dat$visNIR_R)
m1
m2<- mean(new.surf.dat$G - new.surf.dat$visNIR_G)
m2
m3<- mean(new.surf.dat$B - new.surf.dat$visNIR_B)
m3

# find an optimal multiplier to correct vis NIR data to match observations
mults<- seq(1,4,0.01)
mults
out.mat<- matrix(NA, nrow= length(mults), ncol= 5)

names(new.surf.dat)
for (i in 1:nrow(out.mat)){
  out.mat[i,1]<- mults[i]
  new.pred<- new.surf.dat[,10:12] * mults[i]
  #check
  r.mat<- as.matrix(goof(observed = new.surf.dat$R, predicted = new.pred$visNIR_R))
  out.mat[i,2]<- r.mat[1,4]
  g.mat<- as.matrix(goof(observed = new.surf.dat$G, predicted = new.pred$visNIR_G))
  out.mat[i,3]<- g.mat[1,4]
  b.mat<- as.matrix(goof(observed = new.surf.dat$B, predicted = new.pred$visNIR_B))
  out.mat[i,4]<- b.mat[1,4]
  # take the mean
  out.mat[i,5]<- mean(r.mat[1,4],g.mat[1,4],b.mat[1,4])
  
}

min(out.mat[,5])
rms<- which(out.mat[,5]== min(out.mat[,5]))
opt.mult<- out.mat[rms,1]
opt.mult

names(new.surf.dat)
new.surf.dat$pred_R<- NA
new.surf.dat$pred_G<- NA
new.surf.dat$pred_B<- NA
new.surf.dat$pred_NIR<- NA

for (i in 1:nrow(new.surf.dat)){
  new.surf.dat[i,14:17]<- new.surf.dat[i,10:13]* opt.mult
  print(i)}

goof(observed = new.surf.dat$R,predicted = new.surf.dat$visNIR_R)
goof(observed = new.surf.dat$R,predicted = new.surf.dat$pred_R)

goof(observed = new.surf.dat$G,predicted = new.surf.dat$visNIR_G)
goof(observed = new.surf.dat$G,predicted = new.surf.dat$pred_G)

goof(observed = new.surf.dat$B,predicted = new.surf.dat$visNIR_B)
goof(observed = new.surf.dat$B,predicted = new.surf.dat$pred_B)


hist(new.surf.dat$pred_NIR)
rms<- which(new.surf.dat$pred_NIR>0.95)
out.surf.dat<- new.surf.dat[-rms,]
hist(out.surf.dat$pred_NIR)


saveRDS(out.surf.dat, file = "Z:/projects/ternlandscapes_2019/soilColour/data/processed_1/soilcolor_visnir_obs.rds")




