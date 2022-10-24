### TERN LANDSCAPES 
# Soil pH 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 23.8.22
# modified: 23.8.22

# CODE PURPOSE
# join field and lab data and where colocations, preference lab data remove field data


# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
root.lab<- paste0(g.root, "data/curated_lab/")
root.field<- paste0(g.root, "data/fieldObs/curated_field/")
root.out<- paste0(g.root,"data/curated_all/")

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
# 4B1
lab.4b1<- readRDS(paste0(root.lab, "4B1/lab_ph_4B1_data_splined_dsm_ARD.rds"))

# 4B2
lab.4b2<- readRDS(paste0(root.lab, "4B2/lab_ph_4B2_data_splined_dsm_ARD.rds"))
## combine 4B1 and 4B2
lab.4b<- rbind(lab.4b1,lab.4b2)
lab.4b$uniqloc<- paste0(lab.4b$Longitude, "_",lab.4b$Latitude)
lab.4b<- lab.4b[-which(duplicated(lab.4b$uniqloc)),]
lab.4b<- lab.4b[,1:15]
names(lab.4b)
lab.4b$type<- "L"

joined.data<- rbind(field.dat, lab.4b)

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
saveRDS(object = consolidated.data, file =paste0(root.out,"lab_4B_field_splined_dsm_ARD.rds"))
