
library(terra)
library(ranger)
library(hexbin)
library(Cubist)
library(stringr)
library(ggplot2)
library(sf)

terraOptions(progress = 1)

scriptDir <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/Functions'
#covDir = '/datasets/work/lw-soildatarepo/work/http/Products/TERN/Covariates/Mosaics/90m_PCA'
covDir = '/datasets/work/lw-soildatarepo/work/http/Products/TERN/Covariates/Mosaics/90m'
rootDir <- '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/MeasuredTextures/V2/Parsimonious'
workDir <- paste0(rootDir, '/Explore')
if(!dir.exists(workDir)){dir.create(workDir)}


source(paste0(scriptDir, '/GeneralUtils.R'))
source(paste0(scriptDir, '/ModelUtils.R'))
source(paste0(scriptDir, '/RandomForestUtils_V2.R'))

numBoots=50


inDF <- readRDS( paste0(rootDir,'/allDrills', '.rds'))
pts <- st_as_sf(inDF, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
plot(st_geometry(pts))
head(inDF)
colnames(inDF)


a1 <- inDF$DUL_005-inDF$DLL_005
hist(a1)
summary(a1)
boxplot(a1)
a3 <- inDF$DUL_060-inDF$DLL_060
hist(a3)
summary(a3)
a5<- inDF$DUL_100-inDF$DLL_100
hist(a5)
summary(a5)
a6<- inDF$DUL_200-inDF$DLL_200
hist(a6)
summary(a6)

rl1 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/L15/L15_000_005_EV_N_P_AU_TRN_N_20210614.tif')
rl2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/L15/L15_005_015_EV_N_P_AU_TRN_N_20210614.tif')
rl3 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/L15/L15_015_030_EV_N_P_AU_TRN_N_20210614.tif')
rl4 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/L15/L15_030_060_EV_N_P_AU_TRN_N_20210614.tif')
rl5 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/L15/L15_060_100_EV_N_P_AU_TRN_N_20210614.tif')
rl6 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/L15/L15_100_200_EV_N_P_AU_TRN_N_20210614.tif')

ru1 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DUL/DUL_000_005_EV_N_P_AU_TRN_N_20210614.tif')
ru2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DUL/DUL_005_015_EV_N_P_AU_TRN_N_20210614.tif')
ru3 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DUL/DUL_015_030_EV_N_P_AU_TRN_N_20210614.tif')
ru4 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DUL/DUL_030_060_EV_N_P_AU_TRN_N_20210614.tif')
ru5 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DUL/DUL_060_100_EV_N_P_AU_TRN_N_20210614.tif')
ru6 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DUL/DUL_100_200_EV_N_P_AU_TRN_N_20210614.tif')


ar1 <- ru1 - rl1
summary(ar1)
hist(ar1)

ar5 <- ru5 - rl5
summary(ar5)
hist(ar1)


rawData <- read.csv('/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/SiteData/SDF_All_Props_Clean_With_PTFS_Ordered.csv')
rdif <- rawData$Vol.DUL - rawData$Vol.DLL                  
hist(rdif)
summary(rdif)
hist(rawData$Vol.DLL)
hist(rawData$Vol.DUL)

spData <- read.csv('/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/SiteData/SDF_All_Props_Clean_With_PTFS_Splined.csv')
spdif <- spData$DUL_005 - spData$DLL_005                 
hist(spdif)
summary(spdif)
boxplot(spdif)

spdif2 <- spData$DUL_100 - spData$DLL_100
summary(spdif2)
hist(spdif2)

library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl==4]
x2 <- mtcars$mpg[mtcars$cyl==6]
x3 <- mtcars$mpg[mtcars$cyl==8]
vioplot(spdif, rdif, names = c('splined', 'raw'), col = c('blue', 'green'))

