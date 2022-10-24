### Soil color 
## Distinguishing topsoils and subsoils
library(sp);library(rgdal)

root<- "Z:/projects/ternlandscapes_2019/soilColour/data/processed_1/"
dat<- readRDS(paste0(root,"combined_soil_color_out_process1_2ndround.rds"))
dat$UpperDepth<- as.numeric(dat$UpperDepth)
dat$LowerDepth<- as.numeric(dat$LowerDepth)
names(dat)
dat<- dat[complete.cases(dat[,7:8]),]

dat.top<- dat[0,]
dat.sub<- dat[0,]

uniq.site<- unique(dat$uniqcoord)

for (i in 1:length(uniq.site)){
  sub<- dat[dat$uniqcoord == uniq.site[i],]
  
  #topsoil
  if(sub$UpperDepth[1]==0){dat.top<- rbind(dat.top,sub[1,])}
  
  # subsoil
  depths<- as.numeric(sub$LowerDepth) - as.numeric(sub$UpperDepth)
  if(max(depths)>=0.3){
    rms<- which(depths == max(depths))
    sel<- sub[rms,]
    dat.sub<- rbind(dat.sub,sel)}
  print(i)
}

# australia boundary file
bound<- readOGR("Z:/datasets/national/Australia Topo/60803_shp/framework/aus10fgd_bmal.shp")
bound@bbox

xx<- dat.top[dat.top$Longitude >= bound@bbox[1,1] & dat.top$Longitude <= bound@bbox[1,2],]
xx<- xx[xx$Latitude >= bound@bbox[2,1] & xx$Latitude <= bound@bbox[2,2],]
plot(xx$Longitude,xx$Latitude)
dat.top<- xx

xx<- dat.sub[dat.sub$Longitude >= bound@bbox[1,1] & dat.sub$Longitude <= bound@bbox[1,2],]
xx<- xx[xx$Latitude >= bound@bbox[2,1] & xx$Latitude <= bound@bbox[2,2],]
plot(xx$Longitude,xx$Latitude)
dat.sub<- xx

## SAVE
saveRDS(dat.top, paste0(root,"combined_soil_color_out_process1_2ndround_topsoils.rds"))
saveRDS(dat.sub, paste0(root,"combined_soil_color_out_process1_2ndround_subsoils.rds"))


