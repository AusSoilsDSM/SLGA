### TERN LANDSCAPES 
# Soil CEC
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 8.12.21
# modified: 28.01.22

# CODE PURPOSE
# Select test dataset for validation work [cec]

# root directory
data.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/model_outs/"
val.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/digitalsoilmapping/test/"
cal.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/digitalsoilmapping/calibration/"


# libraries
library(caret);library(ranger);library(raster);library(rgdal);library(sp)


#data
# site data
site.dat<- readRDS(paste0(data.root,"tern_cec_siteDat_covariates_20220127.rds"))
names(site.dat)

site.dat$avgCEC<- rowMeans(site.dat[,16:65])
site.dat$log_avgCEC<- log(site.dat$avgCEC + 0.1)
as.factor(site.dat$upper)
site.dat$depth<- NA
site.dat$depth[which(site.dat$upper == 0)]<- 1
site.dat$depth[which(site.dat$upper == 5)]<- 2
site.dat$depth[which(site.dat$upper == 15)]<- 3
site.dat$depth[which(site.dat$upper == 30)]<- 4
site.dat$depth[which(site.dat$upper == 60)]<- 5
site.dat$depth[which(site.dat$upper == 100)]<- 6

# change covariate names
oldcov.names<- names(site.dat)[66:103]
oldcov.names
newcov.names<- substr(names(site.dat)[66:103],start = 1,stop = (nchar(names(site.dat)[66:103])-4))
newcov.names
names(site.dat)[66:103]<- newcov.names
names(site.dat)

# select a test data set
training <- sample(nrow(site.dat), 5000, replace = FALSE)
training

calval.sitedat<- site.dat[-training,]
testdat.sitedat<- site.dat[training,]

# save outputs
saveRDS(object = calval.sitedat,file = paste0(cal.root,"tern_cec_siteDat_covariates_CALVAL_updated.rds"))
saveRDS(object = testdat.sitedat,file = paste0(val.root,"tern_cec_siteDat_covariates_TEST_updated.rds"))
