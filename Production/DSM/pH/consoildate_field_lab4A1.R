### TERN LANDSCAPES 
# Soil pH 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 7.5.21
# modified: 7.5.21

# CODE PURPOSE
# join field and lab data and where colocations, preference lab data remove field data


# root directory
root.lab<- "Z:/projects/ternlandscapes_2019/soil_pH/data/curated_lab/"
root.field<- "Z:/projects/ternlandscapes_2019/soil_pH/data/fieldObs/curated_field/"
root.out<- "Z:/projects/ternlandscapes_2019/soil_pH/data/curated_all/"

# FILES
# FIELD DATA
field.dat<- readRDS(paste0(root.field, "field_ph_data_splined_dsm_ARD.rds"))
names(field.dat)
field.dat$type<- "F"
# rounding to nearest pH unit
field.dat$`0-5 cm`<- round(field.dat$`0-5 cm`/0.5)*0.5 
field.dat$`5-15 cm`<- round(field.dat$`5-15 cm`/0.5)*0.5 
field.dat$`15-30 cm`<- round(field.dat$`15-30 cm`/0.5)*0.5 
field.dat$`30-60 cm`<- round(field.dat$`30-60 cm`/0.5)*0.5 
field.dat$`60-100 cm`<- round(field.dat$`60-100 cm`/0.5)*0.5 
field.dat$`100-200 cm`<- round(field.dat$`100-200 cm`/0.5)*0.5 

# LAB DATA
# 4A1
lab.4a1<- readRDS(paste0(root.lab, "4A1/lab_ph_4A1_data_splined_dsm_ARD.rds"))
names(lab.4a1)
lab.4a1$type<- "L"


joined.data<- rbind(field.dat, lab.4a1)

# unique locations
un.locs<- unique(joined.data$nloc_fid)
length(unique(joined.data$nloc_fid))

consolidated.data<- joined.data[0,]


for (i in 1:length(unique(joined.data$nloc_fid))){
  # slect unique 
  sel<- un.locs[i]
  theones<- which(joined.data$nloc_fid == sel)
  subs<- joined.data[theones,]
  if(nrow(subs)>1){
    le<- length(which(subs$type == "L"))
    if(le != 0){
      tkt<- which(subs$type == "L")
      consolidated.data[i,]<- subs[tkt,]} else {
        consolidated.data[i,]<- subs[1,]}} else {
          consolidated.data[i,]<- subs[1,]}
  print(i)}
    
  
summary(as.factor(consolidated.data$type))

# save data
saveRDS(object = consolidated.data, file =paste0(root.out,"lab_4A1_field_splined_dsm_ARD.rds"))
