### TERN LANDSCAPES 
# Soil pH 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 26.2.20
# modified: 26.02.20

# CODE PURPOSE
# Check for co-located pH across different methods and seek to develop tranfers between the methods
# possible scenarios:
# convert field estimates to 4A1
# convert field estimates to 4B1/4B2
# convert 4NR to 4A1
# convert 4NR to 4B1/4B2
# 
# 
# CURRENT WORK IS PREDOMINANTLY FOR 4A1 - 4NR

library(sp)

# get mode of a dataset
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# root directory
root.lab<- "Z:/projects/ternlandscapes_2019/soil_pH/data/curated_lab/"
root.field<- "Z:/projects/ternlandscapes_2019/soil_pH/data/fieldObs/curated_field/"
root.out<- "Z:/projects/ternlandscapes_2019/soil_pH/data/4nr_2_4a1_dists/"

# FILES
# FIELD DATA
field.dat<- readRDS(paste0(root.field, "field_ph_data_splined_dsm_ARD.rds"))

# LAB DATA
# 4A1
lab.4a1<- readRDS(paste0(root.lab, "4A1/lab_ph_4A1_data_splined_dsm_ARD.rds"))

# 4B1
lab.4b1<- readRDS(paste0(root.lab, "4B1/lab_ph_4B1_data_splined_dsm_ARD.rds"))

# 4B2
lab.4b2<- readRDS(paste0(root.lab, "4B2/lab_ph_4B2_data_splined_dsm_ARD.rds"))

# 4NR
lab.4NR<- readRDS(paste0(root.lab, "4_NR/lab_ph_4NR_data_splined_dsm_ARD.rds"))


# Check 1. lab 4nr and 4a
lab.4nr.ch1<- lab.4NR[0,]
lab.4a1.ch1<- lab.4a1[0,]

cnt<- 1
for (i in 1:nrow(lab.4a1)){
  dists<- sp::spDistsN1(pts = as.matrix(lab.4NR[,4:5]),pt = as.matrix(lab.4a1[i,4:5]),longlat = T)
  min.dist.loc<- which(dists == min(dists))
  min.dist<- min(dists)
  if (min.dist <= 0.002){
    lab.4nr.ch1[cnt,]<- lab.4NR[min.dist.loc,]
    lab.4a1.ch1[cnt,]<- lab.4a1[i,]
    cnt<- cnt+1
    print("MATCH!!")}
  print(i)
}



names(lab.4nr.ch1)
#lab.4nr.ch1[which(lab.4nr.ch1$`0-5 cm` == -9999),10]<- NA
lab.4nr.ch1[which(lab.4nr.ch1$`5-15 cm` == -9999),10]<- NA
lab.4nr.ch1[which(lab.4nr.ch1$`15-30 cm` == -9999),11]<- NA
lab.4nr.ch1[which(lab.4nr.ch1$`30-60 cm` == -9999),12]<- NA
lab.4nr.ch1[which(lab.4nr.ch1$`60-100 cm` == -9999),13]<- NA
lab.4nr.ch1[which(lab.4nr.ch1$`100-200 cm` == -9999),14]<- NA


for (i in 9:14){
  field_1D<- lab.4nr.ch1[,i]
  lab_1D<- lab.4a1.ch1[,i]
  J1D<- cbind(field_1D,lab_1D)
  assign(paste0("d",i), J1D)}
all.dat<- rbind(d9,d10,d11,d12,d13,d14)
all.dat<- all.dat[complete.cases(all.dat),]
  

rms<- c(which(all.dat[,1]<0),which(all.dat[,2]<0))
rms<- unique(rms)
all.dat<- all.dat[-rms,]
plot(all.dat[,2],all.dat[,1])
lmf<- lm(all.dat[,2]~all.dat[,1])
summary(lmf)
abline(lmf)
coefficients(lmf)




