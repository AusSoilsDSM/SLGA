### TERN LANDSCAPES 
# Soil pH 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 7.5.21
# modified: 7.5.21

# CODE PURPOSE
# Attribute numberic estimates of 4a1 to field pH in data.
# idea is to do the imputation and then begin the subsetting business for the cross-validation
# We have distributions for each field ph from 3.5 to 10.5 and these are sampled 

# root directory
data.root<- "Z:/projects/ternlandscapes_2019/soil_pH/data/curated_all/"
dists.root<- "Z:/projects/ternlandscapes_2019/soil_pH/data/field_2_4a1_dists/"



# site data
site.dat<- readRDS(paste0(data.root,"tern_soilpH4a1_siteDat_covariates_20210705_CALVALDAT_ARD.rds"))

## distribtions
dist_35<- readRDS(file = paste0(dists.root,"dists_35_field_2_4a1.rds"))
dist_40<- readRDS(file = paste0(dists.root,"dists_40_field_2_4a1.rds"))
dist_45<- readRDS(file = paste0(dists.root,"dists_45_field_2_4a1.rds"))
dist_50<- readRDS(file = paste0(dists.root,"dists_50_field_2_4a1.rds"))
dist_55<- readRDS(file = paste0(dists.root,"dists_55_field_2_4a1.rds"))
dist_60<- readRDS(file = paste0(dists.root,"dists_60_field_2_4a1.rds"))
dist_65<- readRDS(file = paste0(dists.root,"dists_65_field_2_4a1.rds"))
dist_70<- readRDS(file = paste0(dists.root,"dists_70_field_2_4a1.rds"))
dist_75<- readRDS(file = paste0(dists.root,"dists_75_field_2_4a1.rds"))
dist_80<- readRDS(file = paste0(dists.root,"dists_80_field_2_4a1.rds"))
dist_85<- readRDS(file = paste0(dists.root,"dists_85_field_2_4a1.rds"))
dist_90<- readRDS(file = paste0(dists.root,"dists_90_field_2_4a1.rds"))
dist_95<- readRDS(file = paste0(dists.root,"dists_95_field_2_4a1.rds"))
dist_100<- readRDS(file = paste0(dists.root,"dists_100_field_2_4a1.rds"))
dist_105<- readRDS(file = paste0(dists.root,"dists_105_field_2_4a1.rds"))


names(site.dat)
vect<- c(3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5)
cnt<- 9 # columns with the first soil depth [0-5cm]
for (i in 1:6){
  for (j in 1:length(vect)){
    len<- length(which(site.dat$type == "F" & site.dat[,cnt] == vect[j]))
    if(len != 0){
      chg<- which(site.dat$type == "F" & site.dat[,cnt] == vect[j])
      
      # take a sample from a required distribution
      if(vect[j] == 3.5){
        sel.samp<- sample(dist_35,size = len, replace = T)}
      if(vect[j] == 4){
        sel.samp<- sample(dist_40,size = len, replace = T)}
      if(vect[j] == 4.5){
        sel.samp<- sample(dist_45,size = len, replace = T)}
      if(vect[j] == 5){
        sel.samp<- sample(dist_50,size = len, replace = T)}
      if(vect[j] == 5.5){
        sel.samp<- sample(dist_55,size = len, replace = T)}
      if(vect[j] == 6){
        sel.samp<- sample(dist_60,size = len, replace = T)}
      if(vect[j] == 6.5){
        sel.samp<- sample(dist_65,size = len, replace = T)}
      if(vect[j] == 7){
        sel.samp<- sample(dist_70,size = len, replace = T)}
      if(vect[j] == 7.5){
        sel.samp<- sample(dist_75,size = len, replace = T)}
      if(vect[j] == 8){
        sel.samp<- sample(dist_80,size = len, replace = T)}
      if(vect[j] == 8.5){
        sel.samp<- sample(dist_85,size = len, replace = T)}
      if(vect[j] == 9){
        sel.samp<- sample(dist_90,size = len, replace = T)}
      if(vect[j] == 9.5){
        sel.samp<- sample(dist_95,size = len, replace = T)}
      if(vect[j] == 10){
        sel.samp<- sample(dist_100,size = len, replace = T)}
      if(vect[j] == 10.5){
        sel.samp<- sample(dist_105,size = len, replace = T)}
      
      # change the values
      site.dat[chg,cnt]<- sel.samp}}
  cnt<- cnt + 1}

# empirical disits for each depth
sel.dat<- site.dat$X0.5.cm
hist(sel.dat)

sel.dat<- site.dat$X5.15.cm
rms<- which(sel.dat == -9999)
sel.dat<- sel.dat[-rms]
hist(sel.dat)

sel.dat<- site.dat$X15.30.cm
rms<- which(sel.dat == -9999)
sel.dat<- sel.dat[-rms]
hist(sel.dat)

sel.dat<- site.dat$X30.60.cm
rms<- which(sel.dat < 0)
sel.dat<- sel.dat[-rms]
hist(sel.dat)

sel.dat<- site.dat$X60.100.cm
rms<- which(sel.dat < 0)
sel.dat<- sel.dat[-rms]
hist(sel.dat)

sel.dat<- site.dat$X100.200.cm
rms<- which(sel.dat < 0)
sel.dat<- sel.dat[-rms]
hist(sel.dat)
      


      
      
 

           
