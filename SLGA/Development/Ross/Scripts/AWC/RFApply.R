
#################################
###  Author : Ross Searle         
###  Date : Wed Oct 07 09:57:32 2020                      
###  Project : TERN Landscapes
###  Purpose : Mucking around to work a processess to implement the pedotransfers for AWC generated by Sanji
#################################

library(raster)
library(stringr)
library(rgdal)
library(tictoc)
library(ranger)

machineName <- as.character(Sys.info()['nodename'])

if(machineName=='TERNSOIL2'){
  malRoot <- '//fs1-cbr.nexus.csiro.au/{af-tern-mal-deb}/work/projects/ternlandscapes_2019/soiltexture/predictions/tiles'
  slgaRoot <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work/External/National_digital_soil_property_maps'
  modelRoot <- '//fs1-cbr.nexus.csiro.au/{af-digiscapesm}/work'
  outRoot <- ''
}else{
  malRoot <- '/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/predictions/tiles'
  slgaRoot <- '/datasets/work/af-digiscapesm/work/External/National_digital_soil_property_maps'
  outRoot <- '/scratch1/sea084/SLGA/AWCRF'
  modelRoot <- '/datasets/work/af-digiscapesm/work/PTF analysis/Trained_Models'
}

tic()
args = commandArgs(trailingOnly=TRUE)
k = as.numeric(args[1])
#k=153
print(paste0("Processing iteraration = ", k))

atts <- c('DUL', 'DLL')

root.tiles <-  paste0(malRoot)
source.dirs <- list.files(path = root.tiles, full.names = T)

slgaD <- c('000_005','005_015','015_030','030_060','060_100','100_200')

tilePath <- source.dirs[k]
print(tilePath)
tileNum <- basename(tilePath)
print(paste0('Tile Number - ', tileNum))
outPath <- paste0(outRoot, '/',tileNum)
if(!dir.exists(outPath)){dir.create(outPath)}

for (i in 1:6) {
  print(paste0('Generating depth layer ', i))
  
  outfile <- paste0(outPath, '/RF_', 'DUL','_', i, '.tif')
  
  #if(!file.exists(outfile)) {
  
      r1Path <- paste0(malRoot,'/', tileNum, '/pred_clay_mean_d', i, '.tif' )
      tClay <-  raster(r1Path)
      tSand <-  raster(paste0(malRoot,'/', tileNum, '/pred_sand_mean_d', i, '.tif' ))
      tSilt <-  raster(paste0(malRoot,'/', tileNum, '/pred_silt_compos_mean_d', i, '.tif' ))
      bd <- raster(paste0(slgaRoot,'/Bulk-Density/BDW_',slgaD[[i]] ,'_EV_N_P_AU_NAT_C_20140801.tif' ))
      oc <- raster(paste0(slgaRoot,'/SOC/SOC_',slgaD[[i]] ,'_EV_N_P_AU_NAT_C_20140801.tif' ))
      
      tbd <- crop(bd, tClay)
      toc <- crop(oc, tClay)
      
      df <- data.frame(all_BD=tbd[], all_OC=toc[], all_clay=tClay[], all_silt=tSilt[], all_sand=tSand[])
      idxsNas <- which(!complete.cases(df))
      idxsNotNas <- which(complete.cases(df))
      
      if(length(idxsNotNas) > 0){
      
            for (j in 1:length(atts)) {
              
                att <- atts[j]
                outfile <- paste0(outPath, '/RF_', att,'_', i, '.tif')
                model <- readRDS(paste0(modelRoot, '/RF_VOL_', att, '.rds'))
                
                p1 <- predict(model, df[idxsNotNas,])
                v1<- numeric(length=nrow(df))
                v1[]<-NA
                v1[idxsNotNas] <- p1$predictions
                rpDUL <- raster(tClay)
                rpDUL[] <- v1
                writeRaster(rpDUL, outfile, overwrite=T)
                print(outfile)
                
            }
      }else{
        print("No data in this tile.")
      }
        
  # }else{
  #   print("Files exist")
  # }
    
}

print(paste0('Finished Successfully'))

