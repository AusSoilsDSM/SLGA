#################################################################################

# Objective: Mosaic tiles into a single map 
# Date: 13/10/2021
# Author: Mercedes Roman
# Project: SOC fraction maps for TERN
# Mosaic the tiles into a single raster layer - by job array - find which ones are missing

### Notes: these functions need to be adapted for each particular case, 
### depending on the organization of your directories and name of the files, etc.

### Load packages
library(sp)
library(raster)
library(rgdal)
library(fs)

### change to your directory
InputDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/Coarse_Fragments/0_5/"
setwd(InputDir)

### Depth we are predicting for, as well as job array
depth <- "0_5"
job_arrays <- c("j1", "j2", "j3", "j4", "j5", "j6", "j7", "j8", "j9", "j10", "j11")


### function to identify if any job/tile is missing
### Because we cannot list too many files, I recommend to check in batches of 500
### Initial: is the first tile / job to check, e.g., 501
### Final: is the last one of that batch, e.g., 1000
### from: directory where the files are stored
### variable.name: part of the variable we are searching for
### This function searches in the directory in RDS and returns which files from the desired ones are not there
### And example would be "pred.HOC.m.951.rds" "pred.HOC.m.952.rds" 
# 
which_jobs_to_rerun <- function(initial, final, variable_name, from, depth){
  setwd(from)
  jobs_num <- c(initial:final)
  desired_files <- paste0(variable_name,jobs_num,".",depth,".tif")
  ### Is any file missing?
  ### collapse files
  desired_pattern <- paste0(desired_files, collapse="|")
  do_exist <- list.files(pattern=desired_pattern)
  missing_jobs <- setdiff(desired_files,do_exist)
  return(missing_jobs)
}

### A variation, just for coarse fragments

which_jobs_to_rerun <- function(initial, final, from){
  setwd(from)
  jobs_num <- c(initial:final)
  desired_files <- paste0("cfvol.5_15.tile",jobs_num,"_X1p.tif")
  ### Is any file missing?
  ### collapse files
  desired_pattern <- paste0(desired_files, collapse="|")
  do_exist <- list.files(pattern=desired_pattern)
  missing_jobs <- setdiff(desired_files,do_exist)
  return(missing_jobs)
}

# ### Examples
j6_0_5_missing1 <- which_jobs_to_rerun(initial = 5001,final = 5500, variable_name = "pred.HOC.m.",depth=depths[[d]],
                                       from = InputDir)

###########################################################################################################################
initials1 <-   c(1,1001,2001,3001,4001,5001,6001,7001,8001,9001,10001)
finals1   <- c(500,1500,2500,3500,4500,5500,6500,7500,8500,9500,10100)
initials2 <- c(501,1501,2501,3501,4501,5501,6501,7501,8501,9501,10101)
finals2   <-c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,10200)

#variable_name <- "cfvol.5_15.tile"

missingjobs.list <- list()

for(j in 1:length(job_arrays)){
print(j)
  #InputDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/Coarse_Fragments/5_15/5_15/"
  missingjobs1 <- which_jobs_to_rerun(initial = initials1[[j]], final = finals1[[j]], from = InputDir)
  missingjobs2 <- which_jobs_to_rerun(initial = initials2[[j]], final = finals2[[j]], from = InputDir)
  missingjobs <- c(missingjobs1, missingjobs2)
  print(missingjobs)
  missingjobs.list[[j]] <- missingjobs
}

missingjobs <- unlist(missingjobs.list)


# Function to mosaic by subtile (divide the job array in 2 parts) ---------

### Because we cannot open too many files at once, I will do it in batches of 500
### Initial: is the first tile, e.g., 501
### Final: is the last one of that batch, e.g., 1000
### InputDir: directory where the rds files are stored
### OutDir: Directory to store the mosaic tiles
### variable_name: SOC fraction variable we will mosaic
mosaic_subtile <- function(InputDir, OutDir, variable_name,depth, initial1, final1, initial2, final2, rastname){
  
  ### 1. find the files
  setwd(InputDir)
  jobs_num <- c(initial1:final1)
  desired_files <- paste0(variable_name,jobs_num,".",depth,".tif")
  ### collapse files
  desired_pattern <- paste0(desired_files, collapse="|")
  ### List files in the Input Directory
  files_exist <- list.files(pattern=desired_pattern)
  
  ### Files missing here?
  missing_jobs <- setdiff(desired_files,files_exist)
  if(length(missing_jobs)==0){
    print("No tiles missing")
  } else {
    stop(paste0("Some tiles missing: ",missing_jobs))
  }
  
  ### 2. Mosaic this subtile
  ### Read the tif files and place in a list
  rast.list <- list()
  for(i in 1:length(files_exist)) { 
    tile_temp <- raster(paste0(InputDir, files_exist[i]))
    rast.list[[i]] <- tile_temp
  }
  
  # 3. Use do.call on the list of raster objects
  # names(rast.list)[1:2] <- c('x', 'y')
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  tile1 <- do.call(mosaic, rast.list)
  
  ### Do again with the second
  ### 1. find the files
  jobs_num <- c(initial2:final2)
  desired_files <- paste0(variable_name,jobs_num,".",depth,".tif")
  ### collapse files
  desired_pattern <- paste0(desired_files, collapse="|")
  ### List files in the Input Directory
  files_exist <- list.files(pattern=desired_pattern)
  
  ### Files missing here?
  missing_jobs <- setdiff(desired_files,files_exist)
  if(length(missing_jobs)==0){
    print("No tiles missing")
  } else {
    stop(paste0("Some tiles missing: ",missing_jobs))
  }
  
  ### 2. Mosaic this subtile
  ### Read the rds files and place in a list
  rast.list <- list()
  for(i in 1:length(files_exist)) { 
    tile_temp <- raster(paste0(InputDir, files_exist[i]))
    rast.list[[i]] <- tile_temp
  }
  
  # 3. Use do.call on the list of raster objects
  # names(rast.list)[1:2] <- c('x', 'y')
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  tile2 <- do.call(mosaic, rast.list)
  
  wholeTile <- mosaic(tile1, tile2, fun=mean)
  plot(wholeTile)
  names(wholeTile) <- rastname
  writeRaster(wholeTile, filename =paste0(OutDir,rastname,".tif"), overwrite = TRUE)
}


### run it in for loop for the 9 variables and the 11 arrays
job_arrays <- c("j1", "j2", "j3", "j4", "j5", "j6", "j7", "j8", "j9", "j10", "j11")
#job_arrays <- c("j5", "j6", "j7", "j8", "j9", "j10", "j11")
### Variable names
SOCfractions.names <- c("pred.HOC.m.","pred.POC.m.","pred.ROC.m.",
                        "pred.HOC.05p.","pred.POC.05p.","pred.ROC.05p.",
                        "pred.HOC.95p.","pred.POC.95p.","pred.ROC.95p." )
SOCfractions.names.out <- c("pred.HOC.m.","pred.POC.m.","pred.ROC.m.",
                            "pred.HOC.05p.","pred.POC.05p.","pred.ROC.05p.",
                            "pred.HOC.95p.","pred.POC.95p.","pred.ROC.95p." )
### Subtile numbers
initials1 <-   c(1,1001,2001,3001,4001,5001,6001,7001,8001,9001,10001)
finals1   <- c(500,1500,2500,3500,4500,5500,6500,7500,8500,9500,10100)
initials2 <- c(501,1501,2501,3501,4501,5501,6501,7501,8501,9501,10101)
finals2   <-c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,10200)

library(parallel)
library(doParallel)
library(foreach)

#for(a in 4:length(job_arrays)){
### InputDir <-  paste0("R:/PRJ-SoilBiodSOC2019/SOCfractions/Output_Artemis/HPR/",depths[[d]],"/",job_arrays[[a]],"/")
### Output directory for tile mosaic
### OutDir <- paste0("R:/PRJ-SoilBiodSOC2019/SOCfractions/Mosaic/",depths[[d]],"/")
### print(paste0("working on tile ",job_arrays[[a]]))
# foreach(a=6:length(job_arrays), .packages=c("raster","rgdal"),
#           .export = c("SOCfractions.names", "depths","d","mosaic_subtile",
#                       "initials1","initials2","finals1","finals2","SOCfractions.names.out")) %:%

for(a in 1:length(job_arrays)){
  print(paste0("working on tile ",job_arrays[[a]]))
  InputDir <-  paste0("R:/PRJ-SoilBiodSOC2019/SOCfractions/Output_Artemis/HPR/",depths[[d]],"/",job_arrays[[a]],"/")
  print(InputDir)
  setwd(InputDir)
  cl <- makeCluster(5)    ### Create cluster
  registerDoParallel(cl)
  foreach(i=1:length(SOCfractions.names), .packages=c("raster","rgdal")) %dopar% {
    # .export = c("SOCfractions.names", "depths","d","mosaic_subtile",
    #             "initials1","initials2","finals1","finals2","SOCfractions.names.out")) 
    #for(i in 1:length(SOCfractions.names)){
    #print(SOCfractions.names[[i]])
    InputDir <-  paste0("R:/PRJ-SoilBiodSOC2019/SOCfractions/Output_Artemis/HPR/",depths[[d]],"/",job_arrays[[a]],"/")
    #print(InputDir)
    #setwd(InputDir)
    OutDir <- paste0("R:/PRJ-SoilBiodSOC2019/SOCfractions/Mosaic/",depths[[d]],"/")
    #print(paste0("working on tile ",job_arrays[[a]]))
    mosaic_subtile(InputDir=paste0("R:/PRJ-SoilBiodSOC2019/SOCfractions/Output_Artemis/HPR/",depths[[d]],"/",job_arrays[[a]],"/"),
                   OutDir = OutDir, variable_name=SOCfractions.names[[i]],
                   depth=depths[[d]],
                   initial1 = initials1[[a]],final1 = finals1[[a]],
                   initial2 = initials2[[a]],final2 = finals2[[a]],
                   rastname = paste0(SOCfractions.names.out[[i]], depths[[d]],".",job_arrays[[a]]))
  }
  stopCluster(cl)
  gc()
}


### Adapted for coarse fragments
### Mosaic function
mosaic_subtile <- function(InputDir, OutDir, depth, initial1, final1, initial2, final2, suffix, job_array){
  
  ### 1. find the files
  setwd(InputDir)
  jobs_num <- c(initial1:final1)
  desired_files <- paste0("cfvol.",depth,".tile",jobs_num,"_",suffix,".tif")
  ### collapse files
  desired_pattern <- paste0(desired_files, collapse="|")
  ### List files in the Input Directory
  files_exist <- list.files(pattern=desired_pattern)
  
  ### Files missing here?
  missing_jobs <- setdiff(desired_files,files_exist)
  if(length(missing_jobs)==0){
    print("No tiles missing")
  } else {
    stop(paste0("Some tiles missing: ",missing_jobs))
  }
  
  ### 2. Mosaic this subtile
  ### Read the tif files and place in a list
  rast.list <- list()
  for(i in 1:length(files_exist)) { 
    tile_temp <- raster(paste0(InputDir, files_exist[i]))
    rast.list[[i]] <- tile_temp
  }
  
  # 3. Use do.call on the list of raster objects
  # names(rast.list)[1:2] <- c('x', 'y')
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  tile1 <- do.call(mosaic, rast.list)
  
  ### Do again with the second
  ### 1. find the files
  jobs_num <- c(initial2:final2)
  desired_files <- paste0("cfvol.",depth,".tile",jobs_num,"_",suffix,".tif")
  ### collapse files
  desired_pattern <- paste0(desired_files, collapse="|")
  ### List files in the Input Directory
  files_exist <- list.files(pattern=desired_pattern)
  
  ### Files missing here?
  missing_jobs <- setdiff(desired_files,files_exist)
  if(length(missing_jobs)==0){
    print("No tiles missing")
  } else {
    stop(paste0("Some tiles missing: ",missing_jobs))
  }
  
  ### 2. Mosaic this subtile
  ### Read the rds files and place in a list
  rast.list <- list()
  for(i in 1:length(files_exist)) { 
    tile_temp <- raster(paste0(InputDir, files_exist[i]))
    rast.list[[i]] <- tile_temp
  }
  
  # 3. Use do.call on the list of raster objects
  # names(rast.list)[1:2] <- c('x', 'y')
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  tile2 <- do.call(mosaic, rast.list)
  
  wholeTile <- mosaic(tile1, tile2, fun=mean)
  plot(wholeTile)
  rastname <- paste0("cfvol.",depth,".",suffix,".",job_array,".tif")
  names(wholeTile) <- rastname
  writeRaster(wholeTile, filename =paste0(OutDir,rastname,".tif"), overwrite = TRUE)
}

### run it in for loop for the 9 variables and the 11 arrays
job_arrays <- c("j1", "j2", "j3", "j4", "j5", "j6", "j7", "j8", "j9", "j10", "j11")
sufixes <- c("X1p", "X2p", "X3p", "X4p", "X5p","X6p")
depth <- "0_5"
### Subtile numbers
initials1 <-   c(1,1001,2001,3001,4001,5001,6001,7001,8001,9001,10001)
finals1   <- c(500,1500,2500,3500,4500,5500,6500,7500,8500,9500,10100)
initials2 <- c(501,1501,2501,3501,4501,5501,6501,7501,8501,9501,10101)
finals2   <-c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,10200)

InputDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/Coarse_Fragments/0_5/"
OutDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/Coarse_Fragments/Mosaics/0_5/"

library(parallel)
library(doParallel)
library(foreach)

### Loop through 2 vectors. 1.sufixes and 2. job arrays

for(a in 1:length(job_arrays)){
  print(paste0("working on tile ",job_arrays[[a]]))
  
  cl <- makeCluster(7)    ### Create cluster
  registerDoParallel(cl)
  foreach(class =1:length(sufixes), .packages=c("raster","rgdal"), 
          .export = c("job_arrays", "depth","mosaic_subtile",
          "initials1","initials2","finals1","finals2","sufixes")) %dopar% {
    InputDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/Coarse_Fragments/0_5/"
    OutDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/Coarse_Fragments/Mosaics/0_5/"
    setwd(InputDir)
 
    mosaic_subtile(InputDir=InputDir,
                   OutDir = OutDir,
                   depth = depth,
                   initial1 = initials1[[a]],final1 = finals1[[a]],
                   initial2 = initials2[[a]],final2 = finals2[[a]],
                   suffix =sufixes[[class]],
                   job_array = job_arrays[[a]])
  }
  stopCluster(cl)
  gc()
}


# Mosaic layer ------------------------------------------------------------

### Now that it is finished, let's make the mosaic of all these
InputDir <- OutDir
setwd(InputDir)
depth <- depths[[d]]
SOCfractions.names <- c("pred.HOC.m.","pred.POC.m.","pred.ROC.m.",
                        "pred.HOC.05p.","pred.POC.05p.","pred.ROC.05p.",
                        "pred.HOC.95p.","pred.POC.95p.","pred.ROC.95p." )

for(j in 1:length(SOCfractions.names)){
  #j <- 2
  print(SOCfractions.names[[j]])
  setwd(InputDir)
  rastname <- paste0(SOCfractions.names[[j]], depth)
  print(rastname)
  ### List the files for that variable and place in a list
  tiles.files <- list.files(pattern=SOCfractions.names[[j]])
  print(tiles.files)
  rast.list <- list()
  for(i in 1:length(tiles.files)) { 
    tile_temp <- raster(paste0(InputDir, tiles.files[i]))
    rast.list[[i]] <- tile_temp
  }
  # 3. Use do.call on the list of raster objects
  # names(rast.list)[1:2] <- c('x', 'y')
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  whole.map <- do.call(mosaic, rast.list)
  
  plot(whole.map)
  names(whole.map) <- rastname
  writeRaster(whole.map, filename =paste0(OutDir,rastname,".tif"), overwrite = TRUE)
  gc()
}


### Again, for coarse fragments

mosaic_subtile <- function(InputDir, OutDir, depth, suffix){
  
  ### 1. find the files
  setwd(InputDir)
  ### List files in the Input Directory
  files.tiles <- list.files(pattern=paste0(depth,".",suffix))
  
  ### 2. Mosaic this subtile
  ### Read the tif files and place in a list
  rast.list <- list()
  for(i in 1:length(files.tiles)) { 
    tile_temp <- raster(paste0(InputDir, files.tiles[i]))
    rast.list[[i]] <- tile_temp
  }
  
  # 3. Use do.call on the list of raster objects
  # names(rast.list)[1:2] <- c('x', 'y')
  rast.list$fun <- mean
  rast.list$na.rm <- TRUE
  mapo <- do.call(mosaic, rast.list)
  
  plot(mapo)
  rastname <- paste0("cfvol.",depth,".",suffix,".tif")
  names(mapo) <- rastname
  writeRaster(mapo, filename =paste0(OutDir,rastname,".tif"), overwrite = TRUE)
}

### In this layer
sufixes <- c("X1p", "X2p", "X3p", "X4p", "X5p","X6p")
depth <- "0_5"
InputDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/Coarse_Fragments/Mosaics/0_5/"
OutDir <- "R:/PRJ-SoilBiodSOC2019/SOCfractions/Coarse_Fragments/Mosaics/0_5/"

for(m in 1:length(sufixes)){
  print(m)
  mosaic_subtile(InputDir = InputDir,OutDir = OutDir,depth = depth,suffix = sufixes[[m]])
}

### end of the script