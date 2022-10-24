### TERN LANDSCAPES 
# Soil pH model model fitting
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 18.5.21
# modified: 18.5.21

# CODE PURPOSE
# # Apply model fits to all available data [excluding external data.
# need to estimate model residuals
# depth 2

# fixed parameters
vart<- "pH"
depth<- "d2"
colsel<- 10
paramsf<- 1

# root directory
data.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/data/curated_all/"
dists.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/data/field_2_4a1_dists/"
params.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/data/ranger_model_hyperparams/"
model.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/models/"
funcs.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/miscell/"
slurm.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/model_fitting/slurm/"
r.code<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/model_fitting/"

# libraries
library(caret);library(ranger);library(raster);library(rgdal);library(sp);library(MASS)
source(paste0(funcs.out,"goof.R"))


#data
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
dist_105<- readRDS(file = paste0(dists.root,"dists_105_field_2_4a1.rds"))# root directory


empty.mat<- matrix(NA, nrow= nrow(site.dat),ncol=50)

# 50 realisations
for (zz in 1:50){
  site.dat<- readRDS(paste0(data.root,"tern_soilpH4a1_siteDat_covariates_20210705_CALVALDAT_ARD.rds"))
  
  #trim down data
  names(site.dat)
  site.dat<- site.dat[,c(1:8,colsel,16,17:55)] # change column selection for target variable
  site.dat<- site.dat[complete.cases(site.dat[,c(9,11:49)]),]
  site.dat$target<- site.dat[,9]
  
  
  ##########################
  # convert field to lab
  names(site.dat)
  vect<- c(3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5)
  cnt<- 9 # target variable
  
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
      site.dat[chg,50]<- sel.samp}}
  ######################
  
  # clean up and re-appoint data
  names(site.dat)
  site.dat<- site.dat[,c(1:9,50,10:49)]
  lns<- length(which(site.dat$target < 0))
  lns
  if (lns != 0){site.dat<- site.dat[-which(site.dat$target < 0),]}
  hist(site.dat$target)
  site.dat$Relief_geomorphons<- as.factor(site.dat$Relief_geomorphons)
  empty.mat[,zz]<- site.dat$target}

site.dat$targetAVG<- rowMeans(empty.mat)
site.dat<- site.dat[,c(1:10,51,11:50)]
names(site.dat)  
site.dat<- site.dat[,c(1:9,11:51)]  
saveRDS(object = site.dat, file = paste0(data.root,"tern_soilpH4a1_siteDat_covariates_CALVALDAT_SimulationAverages_d2.rds"))

