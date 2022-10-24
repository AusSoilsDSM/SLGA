### TERN LANDSCAPES 
# Soil pH 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 7.5.21
# modified: 7.5.21

# CODE PURPOSE
# extract out an external validation set
# 3000 sites with lab data
# 7000 sites with field data


# root directory
data.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/data/curated_all/"




# site data
site.dat<- readRDS(paste0(data.root,"tern_soilpH4a1_siteDat_covariates_20210705.rds"))
names(site.dat)
summary(site.dat$type)

lab.ids<- which(site.dat$type == "L")
field.ids<- which(site.dat$type != "L")

set.seed(12031980)
r.samp.lab<- sample(1:length(lab.ids),size = 3000, replace = F)
r.samp.field<- sample(1:length(field.ids),size = 7000, replace = F)

sel.labs<- lab.ids[r.samp.lab]
sel.labs

sel.field<- field.ids[r.samp.field]
sel.field

all.sel<- c(sel.labs,sel.field)

ext.data<- site.dat[all.sel,]

therest.dat<- site.dat[-all.sel,]


#save object
saveRDS(ext.data, file = paste0(data.root,"tern_soilpH4a1_siteDat_covariates_20210705_EXTERNAL_ARD.rds"))
saveRDS(therest.dat, file = paste0(data.root,"tern_soilpH4a1_siteDat_covariates_20210705_CALVALDAT_ARD.rds"))





