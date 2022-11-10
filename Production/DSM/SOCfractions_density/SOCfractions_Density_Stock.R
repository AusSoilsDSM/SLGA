#################################################################################
# Objective: Expected values of SOC fraction densities
# Date: 19/07/2022
# Author: Mercedes Roman
# Project: SOC fractions maps for TERN

library(sp)
library(raster)
library(rgdal)
library(ranger)
library(compositions)
library(parallel)
library(doParallel)
library(foreach)
library(rasterVis)

### Depth we are predicting for and parameters to calculate SOC fraction stocks
#GSM.layer <- 1
depths <-  c("0_5", "5_15", "15_30")
#depth <- depths[[GSM.layer]]
OutDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/SOCfr_density/"

### Bulk density
BdDir <- "R:/PRJ-SoilBiodSOC2019/Covariates/Australia90m/"
BD_0_5 <- raster(paste0(BdDir, "BDW_000_005_EV.tif"))
BD_5_15 <- raster(paste0(BdDir, "BDW_005_015_EV.tif"))
BD_15_30 <- raster(paste0(BdDir, "BDW_015_030_EV.tif"))
BD.list <- list(BD_0_5,BD_5_15,BD_15_30)

### TOC concentration
#TOCdir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/TOC/"
TOCdir <- "C:/Users/mrom8073/Dropbox (Sydney Uni)/SOC_mapping/maps_predicted/"
TOC_0_5 <- raster(paste0(TOCdir,"TOC_0_5_pred.tif"))
TOC_5_15 <- raster(paste0(TOCdir,"TOC_5_15_pred.tif"))
TOC_15_30 <- raster(paste0(TOCdir,"TOC_15_30_pred.tif"))
TOC.list <- list(TOC_0_5,TOC_5_15,TOC_15_30)

### SOC fractions
SOCfrDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/SOCfractions/"

### Coarse fragments
cfDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/Coarse_Fragments/Mosaics/"

### Calculate the SOC fraction densities for each depth

for(i in 1:length(depths)){
  print(i)
 #i <- 3
  depth <- depths[[i]]
  
  ### Load SOC fractions(%)
  setwd(paste0(SOCfrDir,depth,"/"))
  HOC <- raster(list.files(pattern="HOC.m"))
  POC <- raster(list.files(pattern="POC.m"))
  ROC <- raster(list.files(pattern="ROC.m"))
  
  ### Load CF (Class probability)
  setwd(paste0(cfDir,depth,"/"))
  CF1 <- raster(list.files(pattern="X1"))
  CF2 <- raster(list.files(pattern="X2"))
  CF3 <- raster(list.files(pattern="X3"))
  CF4 <- raster(list.files(pattern="X4"))
  CF5 <- raster(list.files(pattern="X5"))
  CF6 <- raster(list.files(pattern="X6"))
  
  ### Average CF - multiplying the class probability by the midpoint (% volume) of the class
  CFs <- stack(CF1, CF2, CF3, CF4, CF5, CF6)
  CFf <- function(s){sum((1*s[1]),(6*s[2]),(15*s[3]),(35*s[4]),(70*s[5]),(95*s[6]),na.rm=TRUE)}
  ff <- function(x) calc(x, CFf)
  
  setwd(OutDir)
  beginCluster(7)
  CF <- clusterR(CFs, ff, export = list("CFf","OutDir"),
                 filename =paste0(OutDir,"CF.",depth,".tif"),
                 format = "GTiff",na.rm=T, inf.rm=T, progress = "text", overwrite = T)
  endCluster()
  
  setwd(OutDir)
  CF <- raster(paste0(OutDir,"CF.",depth,".tif"))
  
  if(i==3){
    
    ### Extend the rasters to the same extent as in other depths
    
    ### Take a reference raster
    #extent(TOC);extent(CF);extent(HOC);res(CF);res(HOC);res(TOC);res(BD) ## xmax is different
    
    ### i use gdalwarp because it is faster
    setwd(paste0(SOCfrDir,depth,"/"))
    # library(gdalUtils)
    # gdalwarp(srcfile = paste0(SOCfrDir,depth,"/","pred.HOC.m.15_30.tif"),
    #            dstfile = gsub(".tif","_e.tif", paste0(SOCfrDir,depth,"/","pred.HOC.m.15_30.tif")),
    #            s_srs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0', 
    #            t_srs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
    #            te = c(112.9996, -44.00042, 153.9996, -10.00042),
    #            tr=c(0.0008333333, 0.0008333333), ## 90m in decimal degrees
    #            r="bilinear", ## Continuous variable
    #            overwrite=TRUE,
    #            verbose=TRUE)
    # gdalwarp(srcfile = paste0(SOCfrDir,depth,"/","pred.POC.m.15_30.tif"),
    #          dstfile = gsub(".tif","_e.tif", paste0(SOCfrDir,depth,"/","pred.POC.m.15_30.tif")),
    #          s_srs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0', 
    #          t_srs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
    #          te = c(112.9996, -44.00042, 153.9996, -10.00042),
    #          tr=c(0.0008333333, 0.0008333333), ## 90m in decimal degrees
    #          r="bilinear", ## Continuous variable
    #          overwrite=TRUE,
    #          verbose=TRUE)
    # gdalwarp(srcfile = paste0(SOCfrDir,depth,"/","pred.ROC.m.15_30.tif"),
    #          dstfile = gsub(".tif","_e.tif", paste0(SOCfrDir,depth,"/","pred.ROC.m.15_30.tif")),
    #          s_srs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0', 
    #          t_srs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
    #          te = c(112.9996, -44.00042, 153.9996, -10.00042),
    #          tr=c(0.0008333333, 0.0008333333), ## 90m in decimal degrees
    #          r="bilinear", ## Continuous variable
    #          overwrite=TRUE,
    #          verbose=TRUE)
    
    HOC <- raster(paste0(SOCfrDir,depth,"/pred.HOC.m.15_30_e.tif"))
    POC <- raster(paste0(SOCfrDir,depth,"/pred.POC.m.15_30_e.tif"))
    ROC <- raster(paste0(SOCfrDir,depth,"/pred.ROC.m.15_30_e.tif"))
  }
  
  ### TOC
  TOC <- TOC.list[[i]]
  
  ### BD
  BD <- BD.list[[i]]
  
  HOCs <- stack(HOC, TOC, BD, CF)
  POCs <- stack(POC, TOC, BD, CF)
  ROCs <- stack(ROC, TOC, BD, CF)
  
  
  #### Double check for mistakes in the calculation!
  
  ###   SOC fraction density(mg C-SOC fraction/ cm3) = 
  ###   SOC fraction (%/100) x TOC content (%)(0.1 mg C/g<2mm soil) x bulk density (g soil/cm3 soil) x
  ###   gravel correction (1-CF/100) (g<2mm/g soil) 
  
  ### We would have to multiply TOC by 10 to pass to mgC/cm3
  
  SOCd <- function(s){(s[1]/100)*s[2]*10*s[3]*(1-(s[4]/100))}
  ff <- function(x) calc(x, SOCd)
  
  beginCluster(7)
  HOCd <- clusterR(HOCs, ff, export = list("SOCd","OutDir"),
                 filename = paste0(OutDir,"HOCd.",depth,".tif"),
                 format = "GTiff",na.rm=T, inf.rm=T, progress = "text", overwrite = T)
  endCluster()
  plot(HOCd)
  
  beginCluster(7)
  POCd <- clusterR(POCs, ff, export = list("SOCd","OutDir"),
                   filename = paste0(OutDir,"POCd.",depth,".tif"),
                   format = "GTiff",na.rm=T, inf.rm=T, progress = "text", overwrite = T)
  endCluster()
  plot(POCd)
  
  beginCluster(7)
  ROCd <- clusterR(ROCs, ff, export = list("SOCd","OutDir"),
                   filename = paste0(OutDir,"ROCd.",depth,".tif"),
                   format = "GTiff",na.rm=T, inf.rm=T, progress = "text", overwrite = T)
  endCluster()
  plot(ROCd)
  
}

#plot(ROCd);plot(POCd); plot(HOCd)


### Calculate SOC fraction stock up to 30 cm

#crs(soil.thickness) <- "+proj=longlat +datum=WGS84 +no_defs"
# library(gdalUtils)
# gdalwarp(srcfile = "R:/PRJ-SoilBiodSOC2019/SoilProperties/soilthickness/median_SD.tif",
#          dstfile = "R:/PRJ-SoilBiodSOC2019/SoilProperties/soilthickness/median_SD_e.tif",
#          s_srs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
#          t_srs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
#          te = c(112.9996, -44.00042, 153.9996, -10.00042),
#          tr=c(0.0008333333, 0.0008333333), ## 90m in decimal degrees
#          r="bilinear", ## Continuous variable
#          overwrite=TRUE,
#          verbose=TRUE)

soil.thickness <- raster("R:/PRJ-SoilBiodSOC2019/SoilProperties/soilthickness/median_SD_e.tif")

conv <- function(s){s*100}
ff <- function(x) calc(x, conv)

beginCluster(7)
soil.thickness.cm <- clusterR(soil.thickness, ff, export = list("conv","OutDir"),
                 filename = paste0(OutDir,"median_SD_cm.tif"),
                 format = "GTiff",na.rm=T, inf.rm=T, progress = "text", overwrite = T)
endCluster()

soil.thickness.cm <- raster(paste0(OutDir,"median_SD_cm.tif"))

par(mfrow=c(1,1))
plot(soil.thickness.cm)

### soil thickness is in cm
### And now, to convert into Mg C/ha I need to multiply x 0.1
### SOC stock (Mg C/ha) = SOC density (mg C/cm3) x thickness (cm) x conversion factor (0.1 Mg C/mg C x cm2/ha)
### Coarse fragments is in % (divide by 100)

setwd(OutDir)
### Create stack
HOC.0_5 <- raster(paste0(OutDir,"HOCd.0_5.tif"))
HOC.5_15 <- raster(paste0(OutDir,"HOCd.5_15.tif"))
HOC.15_30 <- raster(paste0(OutDir,"HOCd.15_30.tif"))

POC.0_5 <- raster(paste0(OutDir,"POCd.0_5.tif"))
POC.5_15 <- raster(paste0(OutDir,"POCd.5_15.tif"))
POC.15_30 <- raster(paste0(OutDir,"POCd.15_30.tif"))

ROC.0_5 <- raster(paste0(OutDir,"ROCd.0_5.tif"))
ROC.5_15 <- raster(paste0(OutDir,"ROCd.5_15.tif"))
ROC.15_30 <- raster(paste0(OutDir,"ROCd.15_30.tif"))

### Add to the stack
HOC.s <- stack(soil.thickness.cm,HOC.0_5,HOC.5_15,HOC.15_30)
POC.s <- stack(soil.thickness.cm,POC.0_5,POC.5_15,POC.15_30)
ROC.s <- stack(soil.thickness.cm,ROC.0_5,ROC.5_15,ROC.15_30)

SOC_stock_f <- function(s) {  
  ### s is a raster stack with SOC fraction density by depth layer, and soil depth
  ### First, let's create an empty vector and fill the values
  totalSOC <- rep(NA, length(s[1]))
  totalSOC[is.na(s[1]) | is.na(s[2]) & is.na(s[3]) & is.na(s[4])] <- NA ### If there is a missing value for soil depth, or all SOC layers, assign NA
  totalSOC[s[1] <= 5]                <- s[1]*s[2]*0.1                                            
  totalSOC[s[1] > 5  & s[1] <= 15]   <- sum((5*s[2]),((s[1]-5)*s[3]), na.rm=T)*0.1
  totalSOC[s[1] > 15 & s[1] <= 30]   <- sum((5*s[2]),(10*s[3]),((s[1]-15)*s[4]), na.rm=TRUE)*0.1
  totalSOC[s[1] > 30]                <- sum((5*s[2]),(10*s[3]),(15*s[4]), na.rm=TRUE)*0.1
  totalSOC
}

### As it is, we could apply it like

setwd(OutDir)
ff <- function(x) calc(x, SOC_stock_f)
beginCluster(7)
HOC_0_30 <- clusterR(HOC.s, ff, export = list("SOC_stock_f"),
                     filename = "HOC_0_30.tif", format = "GTiff",
                     na.rm=T, inf.rm=T, progress = "text", overwrite = T)
endCluster()

beginCluster(7)
POC_0_30 <- clusterR(POC.s, ff, export = list("SOC_stock_f"),
                     filename = "POC_0_30.tif", format = "GTiff",
                     na.rm=T, inf.rm=T, progress = "text", overwrite = T)
endCluster()

beginCluster(7)
ROC_0_30 <- clusterR(ROC.s, ff, export = list("SOC_stock_f"),
                     filename = "ROC_0_30.tif", format = "GTiff",
                     na.rm=T, inf.rm=T, progress = "text", overwrite = T)
endCluster()

par(mfrow=c(1,3))
plot(HOC_0_30)
plot(POC_0_30)
plot(ROC_0_30)

### Calculate bulk SOC stock
setwd(OutDir)
CF <-  stack(list.files(pattern="CF")[c(1,3,2)])
 
SOC.s <- stack(soil.thickness.cm,
               TOC_0_5, TOC_5_15, TOC_15_30,
               BD_0_5, BD_5_15, BD_15_30,
               CF)

SOC_stock_f <- function(s) {  ### s is a raster stack with SOC fraction density by depth layer, and soil depth
  ### First, let's create an empty vector and fill the values
  totalSOC <- rep(NA, length(s[1]))

  ### Calculate SOC density
  TOCd1 <- s[2]*s[5]*(1-(s[8]/100)) ### SOCd of 0-5 cm
  TOCd2 <- s[3]*s[6]*(1-(s[9]/100)) ### SOCd of 0-5 cm
  TOCd3 <- s[4]*s[7]*(1-(s[10]/100)) ### SOCd of 0-5 cm

  totalSOC[is.na(s[1]) | is.na(s[2]) & is.na(s[3]) & is.na(s[4])] <- NA ### If there is a missing value for soil depth, or all TOC layers, assign NA
  totalSOC[s[1] <= 5]                <- s[1]*TOCd1
  totalSOC[s[1] > 5  & s[1] <= 15]   <- sum((5*TOCd1),((s[1]-5)*TOCd2), na.rm=T)
  totalSOC[s[1] > 15 & s[1] <= 30]   <- sum((5*TOCd1),(10*TOCd2),((s[1]-15)*TOCd3), na.rm=TRUE)
  totalSOC[s[1] > 30]                <- sum((5*TOCd1),(10*TOCd2),(15*TOCd3), na.rm=TRUE)
  totalSOC
}

ff <- function(x) calc(x, SOC_stock_f)
beginCluster(7)
SOC_0_30 <- clusterR(SOC.s, ff, export = list("SOC_stock_f"),
                     filename = "SOC_0_30.tif", format = "GTiff",
                     na.rm=T, inf.rm=T, progress = "text", overwrite = T)
endCluster()

par(mfrow=c(1,1))
plot(SOC_0_30)


# plot TOC maps -----------------------------------------------------------

soc <- stack(TOC_0_5, TOC_5_15, TOC_15_30)

levelplot(soc, 
          margin=FALSE,
          layout=c(3,1),
          names.attr = c("0-5 cm","5-15 cm","15-30 cm"),
          par.strip.text=list(cex=1.2, lines=1, col='black'),# suppress marginal graphics
          colorkey=list(
            space='right',                   # plot legend at the right
            labels=list(at=round(seq(0,20, by=5)), font=4)  # legend ticks and labels
          ),
          par.settings=list(
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
         # zscaleLog=TRUE,
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=rev(viridis(100)),      # colour ramp
          at=seq(0, 20, len=100)
)


levelplot(soc, 
          margin=FALSE,
          layout=c(3,1),
          names.attr = c("0-5 cm","5-15 cm","15-30 cm"),
          par.strip.text=list(cex=1.2, lines=1, col='black'),# suppress marginal graphics
          # colorkey=list(
          #   space='right',                   # plot legend at the right
          #   labels=list(at=round(seq(0,20, by=5)), font=4)  # legend ticks and labels
          # ),
          par.settings=list(
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
          zscaleLog=TRUE,
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=rev(viridis(100))#,      # colour ramp
          #at=seq(0, 20, len=100)
)


### End of the script