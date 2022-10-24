### matching soil color ref data to observations
root<- "Z:/projects/ternlandscapes_2019/soilColour/data/processed_1/"
dat.top<-readRDS(paste0(root,"combined_soil_color_out_process1_2ndround_topsoils.rds"))
dat.sub<- readRDS(paste0(root,"combined_soil_color_out_process1_2ndround_subsoils.rds"))

dat.ref<- read.csv("Z:/projects/ternlandscapes_2019/soilColour/data/soilColorRef_updated.csv")
names(dat.ref)


# top soil data frame
out.mat<- matrix(NA, nrow=nrow(dat.top), ncol=ncol(dat.ref))
out.mat<- as.data.frame(out.mat)
names(out.mat)<- names(dat.ref)
dat.top<- cbind(dat.top,out.mat)

# sub soil data frame
out.mat<- matrix(NA, nrow=nrow(dat.sub), ncol=ncol(dat.ref))
out.mat<- as.data.frame(out.mat)
names(out.mat)<- names(dat.ref)
dat.sub<- cbind(dat.sub,out.mat)

## SUBSOIL
for (i in 1:nrow(dat.sub)){
  sub<- dat.sub[i,]
  mtch<- length(which(dat.ref$HUE2 == sub$HVC_ANGLE & dat.ref$HUE1 == sub$HVC_HUE & dat.ref$Value == sub$HVC_VALUE & dat.ref$Chroma == sub$HVC_CHROMA))
  if(mtch == 0){
    sub$HVC_CHROMA<- sub$HVC_CHROMA + 1
    mtch<- length(which(dat.ref$HUE2 == sub$HVC_ANGLE & dat.ref$HUE1 == sub$HVC_HUE & dat.ref$Value == sub$HVC_VALUE & dat.ref$Chroma == sub$HVC_CHROMA))
    if(mtch == 0){next} else {
      sel<- which(dat.ref$HUE2 == sub$HVC_ANGLE & dat.ref$HUE1 == sub$HVC_HUE & dat.ref$Value == sub$HVC_VALUE & dat.ref$Chroma == sub$HVC_CHROMA)
      dat.sub[i,24:41]<- dat.ref[sel,]}} else {
        sel<- which(dat.ref$HUE2 == sub$HVC_ANGLE & dat.ref$HUE1 == sub$HVC_HUE & dat.ref$Value == sub$HVC_VALUE & dat.ref$Chroma == sub$HVC_CHROMA)
        dat.sub[i,24:41]<- dat.ref[sel,]}
  print(i)
}

## TOPSOIL
for (i in 1:nrow(dat.top)){
  sub<- dat.top[i,]
  mtch<- length(which(dat.ref$HUE2 == sub$HVC_ANGLE & dat.ref$HUE1 == sub$HVC_HUE & dat.ref$Value == sub$HVC_VALUE & dat.ref$Chroma == sub$HVC_CHROMA))
  if(mtch == 0){
    sub$HVC_CHROMA<- sub$HVC_CHROMA + 1
    mtch<- length(which(dat.ref$HUE2 == sub$HVC_ANGLE & dat.ref$HUE1 == sub$HVC_HUE & dat.ref$Value == sub$HVC_VALUE & dat.ref$Chroma == sub$HVC_CHROMA))
    if(mtch == 0){next} else {
      sel<- which(dat.ref$HUE2 == sub$HVC_ANGLE & dat.ref$HUE1 == sub$HVC_HUE & dat.ref$Value == sub$HVC_VALUE & dat.ref$Chroma == sub$HVC_CHROMA)
      dat.top[i,24:41]<- dat.ref[sel,]}} else {
        sel<- which(dat.ref$HUE2 == sub$HVC_ANGLE & dat.ref$HUE1 == sub$HVC_HUE & dat.ref$Value == sub$HVC_VALUE & dat.ref$Chroma == sub$HVC_CHROMA)
        dat.top[i,24:41]<- dat.ref[sel,]}
  print(i)
}

dat.sub<- dat.sub[complete.cases(dat.sub$Angle),]
dat.top<- dat.top[complete.cases(dat.top$Angle),]
dat.top$concat<- paste0(dat.top$HUE1,dat.top$HUE2, dat.top$HVC_VALUE,dat.top$HVC_CHROMA)
length(unique(dat.top$concat))
dat.sub$concat<- paste0(dat.sub$HUE1,dat.sub$HUE2, dat.sub$HVC_VALUE,dat.sub$HVC_CHROMA)
length(unique(dat.sub$concat))

dat.sub[88,]



## SAVE
saveRDS(dat.top, paste0(root,"combined_soil_color_out_process1_topsoils_2ndround_matched.rds"))
saveRDS(dat.sub, paste0(root,"combined_soil_color_out_process1_subsoils_2ndround_matched.rds"))
write.csv(dat.top, paste0(root,"combined_soil_color_out_process1_topsoils_2ndround_matched.csv"),row.names = F)
write.csv(dat.sub, paste0(root,"combined_soil_color_out_process1_subsoils_2ndround_matched.csv"),row.names = F)
