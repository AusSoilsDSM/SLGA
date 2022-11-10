#################################################################################
# Objective: Coarse fragments (% volume) at 90 m resolution
# Date: 09/06/2022
# Author: Mercedes Roman
# Project: SOC fraction maps for TERN
### map coarse fragments - probability maps - at 90

### Note: In this script, as an example we map the depth 5-15 cm. Doing it with my desktop, not with HPC.

### Load packages
library(sp)
library(raster)
library(rgdal)
library(ranger)
library(parallel)
library(doParallel)
library(foreach)

### Bring the data and set parameters

### Depth we are predicting for
GSM.layer <- 2
depths <-  c("0_5", "5_15", "15_30")
depth <- depths[[GSM.layer]]

# path to the covariates raster
InputDir <-"R:/PRJ-SoilBiodSOC2019/Covariates/Australia90m_filled/"
OutDir <- paste0("R:/PRJ-SoilBiodSOC2019/SOCfractions/Coarse_Fragments/", depth,"/")

# list all the raster files available
setwd(InputDir)
# load all the rasters
list_ras <- list.files(pattern="tif$")
covariates.stack <- stack(paste0(InputDir,list_ras))
print(covariates.stack)

### Create tiles
# dimensions of the stack: 
# nrow = 40800, 
# ncol = 49200, 
# ncell = 2007360000, 
# make blocks
bs <- blockSize(covariates.stack,minblocks = 100, minrows = 4) ### 10200 tiles

########################################################################################################

### Load the models for Coarse fragments class probability ------------------------------
### Load the models for Coarse fragments class probability
load(paste0("R:/PRJ-SoilBiodSOC2019/SOCfractions/Input/CF_rf_models/cf_rf_probs_",depth,".RData"))
CFvol.model <- cf_rf_probs_5_15
rm(cf_rf_probs_5_15)

# Covariates --------------------------------------------------------------
covar.names <- c("Clim_ADM", "Clim_EPA", "Clim_EPI", "Clim_EPX", 
                 "Clim_Prescott", "Clim_PTA", "Clim_PTI", "Clim_PTS1", "Clim_PTS2", 
                 "Clim_PTX", "Clim_RSM", "Clim_TNM", "Clim_TRA", "Clim_TXM", "Clim_WDA", 
                 "NDVI_mean_Q1", "NDVI_mean_Q2", "NDVI_mean_Q3", "NDVI_mean_Q4", 
                 "PM_Gravity", "PM_radmap_v4_2019_filtered_dose_GAPFilled", "PM_radmap_v4_2019_filtered_pctk_GAPFilled", 
                 "PM_radmap_v4_2019_filtered_ppmt_GAPFilled", "PM_radmap_v4_2019_filtered_ppmu_GAPFilled", 
                 "PM_radmap_v4_2019_ratio_tk_GAPFilled", "PM_radmap_v4_2019_ratio_u2t_GAPFilled", 
                 "PM_radmap_v4_2019_ratio_uk_GAPFilled", "PM_radmap_v4_2019_ratio_ut_GAPFilled", 
                 "PM_Weathering_Index", "relief_dems_3s_mosaic1", "Relief_mrrtf_3s", 
                 "relief_mrvbf_3s_mosaic", "relief_plan_curvature_3s", "relief_profile_curvature_3", 
                 "relief_roughness", "relief_slope_perc", "relief_twi_3s", 
                 "Veg_FC_Max_BS", "Veg_FC_Max_NPV", "Veg_FC_Max_PV", "Veg_FC_Mean_BS", 
                 "Veg_FC_Mean_NPV", "Veg_FC_Mean_PV", "Veg_FC_Min_BS", "Veg_FC_Min_NPV", 
                 "Veg_FC_Min_PV", "Veg_FC_SD_BS", "Veg_FC_SD_NPV", "Veg_FC_SD_PV", 
                 "Veg_FPAR_Max", "Veg_FPAR_Mean", "Veg_FPAR_Median", "Veg_FPAR_Min", 
                 "Veg_LandCoverTrend_evi_mean", "Veg_Persistant_green_Veg")
covar.depth.specific.list <- list(c("clay_0_5", "sand_0_5"),
                                  c("clay_5_15", "sand_5_15"),
                                  c("clay_15_30", "sand_15_30"))
covar.depth <- covar.depth.specific.list[[GSM.layer]]

#############################################################################################################

### Start the mapping

cf.classes.levels <- c("X1p","X2p","X3p","X4p","X5p","X6p")
cl <- makeCluster(detectCores()-2)   ### Create cluster
registerDoParallel(cl)
getDoParWorkers()


foreach(i=1:bs$n, .packages=c("raster","ranger"),
        .export = c("bs","covariates.stack", "CFvol.model", "OutDir", "cf.classes.levels")) %dopar% {
          ### Get one tile of the raster stack
          tile <- crop(covariates.stack, extent(covariates.stack, bs$row[[i]], bs$row[[i]]+bs$nrows[[i]], 1, ncol(covariates.stack)))
          
          ### Predict coarse fragments class probability
          model=CFvol.model
          pred <- raster::predict(object = tile, 
                                  model = model, 
                                  index=1:6,
                                  fun = function(model, ...) predict(model, ...)$predictions)
          writeRaster(pred, bylayer=TRUE, suffix=cf.classes.levels,
                      filename= paste0(OutDir,"cfvol",".",depth,".tile",i,".tif"),
                      na.rm=T, inf.rm=T,
                      format="GTiff", overwrite=TRUE)
          gc()
        }

stopCluster(cl)

#### Mosaic by cf class
for(var in 1:length(cf.classes.levels)){
  
  print(cf.classes.levels[[var]])
  tiles.files <- list.files(pattern=cf.classes.levels[[var]])
  rast.list <- list()
  
  for(i in 1:length(tiles.files)) { 
    tile_temp <- raster(paste0(OutDir, tiles.files[i]))
    rast.list[[i]] <- tile_temp
  }
  
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  
  whole.map <- do.call(mosaic, rast.list)
  names(whole.map) <- names(cf.classes.levels[[var]])
  print(whole.map)
  writeRaster(whole.map, filename= paste0(OutDir,"cf_vol_",cf.classes.levels[[var]],"_",depth,".tif"),
              na.rm=T,inf.rm=T, format="GTiff", overwrite=TRUE)
  
}

### End of the script for this depth :)