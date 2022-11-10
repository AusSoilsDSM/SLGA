library(raster)
library(rasterVis)
library(viridis)
library(viridisLite)
library(rgdal)
library(sp)
library(raster)
library(rgdal)
library(ranger)
library(compositions)
library(parallel)
library(doParallel)
library(foreach)
library(rasterVis)



# 0-5 cm ------------------------------------------------------------------

inputDir <-"R:/PRJ-SoilBiodSOC2019/SOCfractions/Coarse_Fragments/Mosaics/0_5/"
setwd(inputDir)

### List files
CFfiles<- list.files('./', pattern=".tif")
# load all the rasters
CF <- stack(paste0(inputDir, CFfiles))

levelplot(CF,
          names.attr = c("Very few (< 2 %)","Few (2-10 %)","Common (10-20 %)",
                         "Many (20-50 %)","Abundant (50-90 %)","Very abundant (> 90%)"),
          layout=c(3,2),
          par.strip.text=list(cex=0.9, lines=1, col='black'),# suppress marginal graphics
          colorkey=list(
            space='right',                   # plot legend at the right
            labels=list(round(seq(0,1, by=0.2),digits=1), font=4)  # legend ticks and labels           
          ),    
          par.settings=list(
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=rev(magma(100)),      # colour ramp
          at=seq(0,1,len=100)
)

### Calculate most probable class

#dominantCF <- function(s){which.max(s)}

setwd(inputDir)

which.max2 <- function(x, ...) ifelse( length(x) ==sum(is.na(x) ), NA, which.max(x))
ff <- function(x) calc(x, which.max2)

beginCluster(7)
CFmost <- clusterR(CF, ff, export = list("which.max2"),
                 filename = "CF_class_0_5.tif",
                 format = "GTiff",na.rm=T, inf.rm=T, progress = "text", overwrite = T)
endCluster()

CFmost <- ratify(CFmost)
rat <- levels(CFmost)[[1]]
rat$CF_class <- c("Very few (< 2 %)","Few (2-10 %)","Common (10-20 %)","Many (20-50 %)","Abundant (50-90 %)","Very abundant (> 90%)")
levels(CFmost) <- rat

levelplot(CFmost,
          # names.attr = c("Very few (< 2 %)","Few (2-10 %)","Common (10-20 %)",
          #                "Many (20-50 %)","Abundant (50-90 %)","Very abundant (> 90%)"),
          par.strip.text=list(cex=0.9, lines=1, col='black'),# suppress marginal graphics
          par.settings=list(
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=rev(magma(6))      # colour ramp
)

plot(CFmost, col=rev(magma(6)))




# 5-15 cm ------------------------------------------------------------------

inputDir <-"R:/PRJ-SoilBiodSOC2019/SOCfractions/Coarse_Fragments/Mosaics/5_15/"
setwd(inputDir)

### List files
CFfiles<- list.files('./', pattern=".tif")
# load all the rasters
CF <- stack(paste0(inputDir, CFfiles))

levelplot(CF,
          names.attr = c("Very few (< 2 %)","Few (2-10 %)","Common (10-20 %)",
                         "Many (20-50 %)","Abundant (50-90 %)","Very abundant (> 90%)"),
          layout=c(3,2),
          par.strip.text=list(cex=0.9, lines=1, col='black'),# suppress marginal graphics
          colorkey=list(
            space='right',                   # plot legend at the right
            labels=list(round(seq(0,1, by=0.2),digits=1), font=4)  # legend ticks and labels           
          ),    
          par.settings=list(
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=rev(magma(100)),      # colour ramp
          at=seq(0,1,len=100)
)

### Calculate most probable class
setwd(inputDir)

which.max2 <- function(x, ...) ifelse( length(x) ==sum(is.na(x) ), NA, which.max(x))
ff <- function(x) calc(x, which.max2)

beginCluster(7)
CFmost <- clusterR(CF, ff, export = list("which.max2"),
                   filename = "CF_class_5_15.tif",
                   format = "GTiff",na.rm=T, inf.rm=T, progress = "text", overwrite = T)
endCluster()

CFmost <- ratify(CFmost)
rat <- levels(CFmost)[[1]]
rat$CF_class <- c("Very few (< 2 %)","Few (2-10 %)","Common (10-20 %)","Many (20-50 %)","Abundant (50-90 %)","Very abundant (> 90%)")
levels(CFmost) <- rat

levelplot(CFmost,
          # names.attr = c("Very few (< 2 %)","Few (2-10 %)","Common (10-20 %)",
          #                "Many (20-50 %)","Abundant (50-90 %)","Very abundant (> 90%)"),
          par.strip.text=list(cex=0.9, lines=1, col='black'),# suppress marginal graphics
          par.settings=list(
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=rev(magma(6))      # colour ramp
)

# 15-30 cm ------------------------------------------------------------------

rm(list=ls())
inputDir <-"R:/PRJ-SoilBiodSOC2019/SOCfractions/Coarse_Fragments/Mosaics/15_30/"
setwd(inputDir)

### List files
CFfiles<- list.files('./', pattern=".tif")
# load all the rasters
CF <- stack(paste0(inputDir, CFfiles))

levelplot(CF,
          names.attr = c("Very few (< 2 %)","Few (2-10 %)","Common (10-20 %)",
                         "Many (20-50 %)","Abundant (50-90 %)","Very abundant (> 90%)"),
          layout=c(3,2),
          par.strip.text=list(cex=0.9, lines=1, col='black'),# suppress marginal graphics
          colorkey=list(
            space='right',                   # plot legend at the right
            labels=list(round(seq(0,1, by=0.2),digits=1), font=4)  # legend ticks and labels           
          ),    
          par.settings=list(
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=rev(magma(100)),      # colour ramp
          at=seq(0,1,len=100)
)

### Calculate most probable class
setwd(inputDir)

which.max2 <- function(x, ...) ifelse( length(x) ==sum(is.na(x) ), NA, which.max(x))
ff <- function(x) calc(x, which.max2)

beginCluster(7)
CFmost <- clusterR(CF, ff, export = list("which.max2"),
                   filename = "CF_class_15_30.tif",
                   format = "GTiff",na.rm=T, inf.rm=T, progress = "text", overwrite = T)
endCluster()

CFmost <- ratify(CFmost)
rat <- levels(CFmost)[[1]]
rat$CF_class <- c("Very few (< 2 %)","Few (2-10 %)","Common (10-20 %)","Many (20-50 %)","Abundant (50-90 %)","Very abundant (> 90%)")
levels(CFmost) <- rat

levelplot(CFmost,
          # names.attr = c("Very few (< 2 %)","Few (2-10 %)","Common (10-20 %)",
          #                "Many (20-50 %)","Abundant (50-90 %)","Very abundant (> 90%)"),
          par.strip.text=list(cex=0.9, lines=1, col='black'),# suppress marginal graphics
          par.settings=list(
            strip.background=list(col='transparent'),
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=rev(magma(6))      # colour ramp
)

# End of this script