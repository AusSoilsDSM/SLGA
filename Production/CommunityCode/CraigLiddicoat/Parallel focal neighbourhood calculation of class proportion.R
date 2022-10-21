###########################################
#
# R script - supplementary material for the Manuscript titled:
# "Landscape biodiversity correlates with respiratory health in Australia"
#
# By Craig Liddicoat, Peng Bi, Michelle Waycott, John Glover, Andrew Lowe, Philip Weinstein
# Aug 2017
#
# >> R script for submission to High performance computing cluster
#    This used Tizard HPC on eRSA (http://support.ersa.edu.au/hpc/tizard-user-guide.html)
#
# I. Performs focal neighbourhood calculation of
#    class proportion and 
#    produces a histogram of raster values
#
#    *** CHECK & UPDATE CAT#, ref, lkup ***
# 
###########################################
### UPDATE MAIN INPUTS HERE ###############
###########################################

## working directory
work_dir <- "/scratch/ERSA00233/env_landuse_abares"    # CHECK & UPDATE !!!!!!!
setwd(work_dir)

## reference file: this is raster operated on in this script!

#  polygon-based thematic/categorical mapping has previously been converted to grid (Geotiff) format
#  by nearest neighbour resampling, based on a standard grid system

ref_file_name  <- "ABARES2014_landuse_18CLASS_250m_INT2S_NDVminus32767_WGS84_FINAL.tif"     # CHECK & UPDATE !!!!!!!
ref_short_name <- "landuse2014"                                                             # CHECK & UPDATE !!!!!!!
NAvalue.ref    <- -32767

## lookup table for class names = lkup$short_name   

CAT <- 1                    #  !!!!!! REPEAT THIS FOR OTHER CLASS NO's -- CHECK & UPDATE 'CAT' & 'lkup'   !!!!!!

lkup <- as.data.frame(read.table(paste(work_dir,"/lkup_ABARES2014_landuse_18CLASS_cleaned_FINAL.txt",sep=""),sep=",",header=TRUE))
lkup$VALUE <- as.numeric(lkup$VALUE)              # ensure numeric
lkup$short_name <- as.character(lkup$short_name)  # ensure character

## keep record of raster classes, saved with output
write.table(lkup,file=paste(ref_short_name,"__lkup_table.txt",sep=""),sep=",",col.names=TRUE,row.names=FALSE)


## decide chunksize = how many blocks that operations will be split up into?

CHUNKS <- 0.5e+06

###########################################
###########################################

## set library path - so spatial (GDAL, proj4) and other R libraries can be found on the HPC

.libPaths(new=c("/opt/shared/R/3.1.2/lib64",
                "/opt/shared/R/3.1.2/lib64/R",
                "/opt/shared/R/3.1.2/lib64/R/lib",
                "/opt/shared/R/3.1.2/lib64/R/library",
                "/usr/lib",
                "/home/users/ERSA00233/new_r_library/library",
                "/home/users/ERSA00233/gdal_library/bin",
                "/home/users/ERSA00233/gdal_library/lib",
                "/home/users/ERSA00233/gdal_library/include",
                "/home/users/ERSA00233/gdal_library/share/gdal",
                "/home/users/ERSA00233/proj4_library",
                "/home/users/ERSA00233/proj4_library/bin",
                "/home/users/ERSA00233/proj4_library/lib",
                "/home/users/ERSA00233/proj4_library/include",
                "/home/users/ERSA00233/proj4_library/share"))


## load required packages

library(raster) # also loads sp
library(rgdal)
library(moments)
library(lattice)
library(doParallel)
library(foreach)
library(ggplot2)


## store and set defaults
RO.default<-rasterOptions() # datatype: FLT8S, chunksize: 1e+07, maxmemory: 1e+08
rasterOptions(datatype="FLT4S",progress="text",chunksize=1e+08,maxmemory=1e+09,tmpdir="/scratch/ERSA00233/temp_rasters") # maxmemory = max no of cells to read into memory

par.default <- par()

# WGS84 projection:
#WGS84<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# No need to set projection if contained in .tfw world file, with .tif raster


#### Set raster of interest

r<- raster(paste(ref_file_name))
NAvalue(r) <- NAvalue.ref

# For standard grid system used in this project 3km radius has 12*cell dimension
(rad <- 12*res(r)[1] ) # 0.028188   radius of circle in CRS units

# set number of blocks for parallel operations
# CHUNKS value is set at top of this R script

(BS<-blockSize(r,chunksize=CHUNKS)) # 


## check levels in raster ?
##lcov.lev<-as.numeric(levels(as.factor(as.numeric(sampleRandom(r,size=500000,na.rm=T)))))

lcov.lev<-lkup$VALUE

lcov.lev
print(paste(ref_file_name))
print(paste("length of lcov.lev is: ",length(lcov.lev),sep=""))
print(paste("min value lcov.lev is: ",min(lcov.lev),sep=""))
print(paste("max value lcov.lev is: ",max(lcov.lev),sep=""))



#### Now calculate Focal Proportions of each class ??


## Define Focal calculation in parallel
# - - - - - - - - - - - - - - - - - 
## Define function to get FOCAL FILTER values:
# Call: get_focal_proportions( grid.orig = r , chunks = CHUNKS, radius = rad, classes = lcov.lev ,class_no = i )

get_focal_proportions <- function(grid.orig , chunks, radius, classes, class_no )  {    
  
  bs<-blockSize(grid.orig , chunksize=chunks)
  out<-grid.orig
  dy.tot<-ymax(out)-ymin(out) # total distance in y direction across tile
  dy.cell<-dy.tot/nrow(out) # 'height' of each cell in y direction
  
  buffer <- radius
  
  # non-buffered block extent
  block.orig.ext<-c(xmin(out),xmax(out),ymax(out)-((bs$row[j]-1) + bs$nrows[j])*dy.cell, ymax(out)-(bs$row[j]-1)*dy.cell) # Extent of each block
  
  # buffered block extents
  if (j == 1) {
    # top-most block:  {ymin - buffer} 
    block.focal.ext <- c(xmin(out),xmax(out),ymax(out)-((bs$row[j]-1) + bs$nrows[j])*dy.cell - buffer, ymax(out)-(bs$row[j]-1)*dy.cell) # Extent of each block
    
  } else if (j == bs$n) {
    # lowest block:  {ymax + buffer} 
    block.focal.ext <- c(xmin(out),xmax(out),ymax(out)-((bs$row[j]-1) + bs$nrows[j])*dy.cell, ymax(out)-(bs$row[j]-1)*dy.cell + buffer)  # Extent of each block
    
  } else 
    # all other blocks in the middle: do both   {ymin - buffer}  and   {ymax + buffer}
    block.focal.ext <- c(xmin(out),xmax(out),ymax(out)-((bs$row[j]-1) + bs$nrows[j])*dy.cell - buffer, ymax(out)-(bs$row[j]-1)*dy.cell + buffer) # Extent of each block
  
  # crop to buffered block extent
  grid.focal.ext <- crop(grid.orig , block.focal.ext)
  
  # focal calculation

  wt.mat <- focalWeight(x=grid.focal.ext , d=radius, type='circle' ) # d= radius of circle in CRS units

  wt.mat.multiplier <- 1/length(wt.mat[wt.mat>0])
  
  focal.calc <- focal(grid.focal.ext, w=wt.mat,fun =  function(x, ...){ length(which( na.omit(x[x>0]) == as.numeric(classes[class_no])*wt.mat.multiplier ))/length( na.omit(x[x>0])) }, pad=TRUE,padValue=NA )
  
  # crop focal calc raster back to non-buffered block extent
  focal.calc <- crop(focal.calc,block.orig.ext)
  
  # mask using original grid and non-buffered block extent
  focal.calc <- mask(focal.calc, mask = crop(grid.orig,block.orig.ext) )
  
  # get block values
  new_v <- getValuesBlock(focal.calc,row=1,nrows=bs$nrows[j])
  
  return(new_v)
  
  # clean up
  rm(focal.calc , grid.focal.ext, new_v)
}
# - - - - - - - - - - - - - - - - - 




### call Focal Proportions calc 

for (i in CAT:CAT) {                # CAT is the class no. set at top of script

  out<-r
  # CHECK & UPDATE ???
  out<-writeStart(out,filename=paste(ref_short_name,"_focal_prop_of_class_",lcov.lev[i],"_",lkup$short_name[i],sep=""),
                  format="GTiff",options="TFW=YES",datatype='FLT4S',NAflag= -99999,overwrite=T)
  
  # perform raster calculation/prediction blockwise in parallel 
  cl<-makeForkCluster(nnodes = 16)  # number of nodes based on HPC
  registerDoParallel(cl)
  
  # 'new_value' list {= raster calculations} created automatically by foreach() function
  #  use same 'CHUNKS' value as previous
  
  new_value <- foreach(j=1:BS$n, .packages=c('raster')) %dopar% 
    get_focal_proportions( grid.orig = r , chunks = CHUNKS, radius = rad, classes = lcov.lev ,class_no = i  ) 
  
  stopCluster(cl)
  
  # write to new raster by blocks
  for (j in 1:BS$n) {
    out<-writeValues(out,new_value[[j]],start=BS$row[j])
  }
  
  out<-writeStop(out)
  
  rm(r,new_value) # clean-up
  
  # plot histogram
  sample.cov <-  sampleRandom(out,size=100000,na.rm=T)
  kt <- kurtosis(sample.cov)
  kt <- round(kt,2)
  histplot<-qplot(sample.cov,geom="histogram",main=paste(ref_short_name,"_focal_prop_class_",lcov.lev[i],sep=""),
              xlab= paste("kurtosis: ",kt,sep=""),cex.main=0.8,cex.lab=0.8,cex.axis=0.8)
  ggsave(histplot,file=paste(ref_short_name,"_focal_prop_class_",lcov.lev[i],"_HIST.png",sep=""),width=12,height=12,units='cm',path=work_dir)
  
  # clean-up
  rm(sample.cov,histplot,out)
  
}



# END 

###################################
