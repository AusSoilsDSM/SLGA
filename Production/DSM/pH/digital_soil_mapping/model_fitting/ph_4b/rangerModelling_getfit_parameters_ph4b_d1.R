### TERN LANDSCAPES 
# Soil pH model hyperparam optimisation
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 2.9.22
# modified: 2.9.22

# CODE PURPOSE
# Figure out the hyperparameters to use 
# Depth 1 [0-5cm]
# Code involves the quant of field data to a 'lab' estimate based 
# on the empirical relations between field and lab measures


# root directory
gen.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/"
data.root<- paste0(gen.root,"data/curated_all/")
dists.root<- paste0(gen.root, "data/field_2_4B_dists/")
params.out<- paste0(gen.root, "data/ranger_model_hyperparams/")
model.out<- paste0(gen.root, "models/ph_4b")
slurm.root<- paste0(gen.root, "rcode/digital_soil_mapping/model_fitting/ph_4b/slurm/")
r.code<- paste0(gen.root, "rcode/digital_soil_mapping/model_fitting/")


# libraries
library(caret);library(ranger);library(raster);library(rgdal);library(sp)


#data
# site data
site.dat<- readRDS(paste0(data.root,"tern_soilpH4B_siteDat_covariates_20223008_CALVALDAT_ARD.rds"))
names(site.dat)

## distribtions
dist_35<- readRDS(file = paste0(dists.root,"dists_35_field_2_4b.rds"))
dist_40<- readRDS(file = paste0(dists.root,"dists_40_field_2_4b.rds"))
dist_45<- readRDS(file = paste0(dists.root,"dists_45_field_2_4b.rds"))
dist_50<- readRDS(file = paste0(dists.root,"dists_50_field_2_4b.rds"))
dist_55<- readRDS(file = paste0(dists.root,"dists_55_field_2_4b.rds"))
dist_60<- readRDS(file = paste0(dists.root,"dists_60_field_2_4b.rds"))
dist_65<- readRDS(file = paste0(dists.root,"dists_65_field_2_4b.rds"))
dist_70<- readRDS(file = paste0(dists.root,"dists_70_field_2_4b.rds"))
dist_75<- readRDS(file = paste0(dists.root,"dists_75_field_2_4b.rds"))
dist_80<- readRDS(file = paste0(dists.root,"dists_80_field_2_4b.rds"))
dist_85<- readRDS(file = paste0(dists.root,"dists_85_field_2_4b.rds"))
dist_90<- readRDS(file = paste0(dists.root,"dists_90_field_2_4b.rds"))
dist_95<- readRDS(file = paste0(dists.root,"dists_95_field_2_4b.rds"))
dist_100<- readRDS(file = paste0(dists.root,"dists_100_field_2_4b.rds"))
dist_105<- readRDS(file = paste0(dists.root,"dists_105_field_2_4b.rds"))


# select variable
# lab dat
names(site.dat)
sub.site.dat<- site.dat[,c(1:8,9,16,17:55)] # change column selection for target variable
sub.site.dat<- sub.site.dat[complete.cases(sub.site.dat[,c(9,11:49)]),]
sub.site.dat$target<- sub.site.dat[,9] # make new column to put simulated values 


##########################
# convert field to lab
names(sub.site.dat)
vect<- c(3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5)
cnt<- 9 # target variable

for (j in 1:length(vect)){
  len<- length(which(sub.site.dat$type == "F" & sub.site.dat[,cnt] == vect[j]))
    if(len != 0){
      chg<- which(sub.site.dat$type == "F" & sub.site.dat[,cnt] == vect[j])
      
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
      sub.site.dat[chg,50]<- sel.samp}}
######################


# clean up and re-appoint data
names(sub.site.dat)
sub.site.dat<- sub.site.dat[,c(1:9,50,10:49)]
lns<- length(which(sub.site.dat$target < 0))
lns
if (lns != 0){sub.site.dat<- sub.site.dat[-which(sub.site.dat$target < 0),]}
hist(sub.site.dat$target)
sub.site.dat$Relief_geomorphons<- as.factor(sub.site.dat$Relief_geomorphons)



## Ranger model with cross validation
names(sub.site.dat)

#tgrid <- expand.grid(
#  .mtry = 7:17,
#  .splitrule= c("variance", "extratrees"),
#  .min.node.size = c(1:10)
#)

ranger.model<-train(x= sub.site.dat[,12:49], y= sub.site.dat$target,method = "ranger",trControl = trainControl(method = "oob"), num.trees =  500)  
summary(ranger.model)
ranger.model

## capture output
var_nm1<- paste0(params.out,"rangerModel_params_ph4b_d1.txt")

out1<- capture.output(summary(ranger.model))
out1<- paste0(out1,collapse = "\r\n")
cat(out1, file = var_nm1, sep=",", append = T)

out2<- capture.output(ranger.model)
out2<- paste0(out2,collapse = "\r\n")
cat(out2, file = var_nm1, sep=",", append = T)




