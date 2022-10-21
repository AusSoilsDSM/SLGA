######## About this Script #######################################################################
###
###  Author : Ross Searle         
###  Date : Mon Apr 4 11:18:04 2022                      
###  Project : TERN Landscapes
###  Purpose : Massage the raw SLGA rasters to get them ready for publication
###  
###############################################################################################

library(stringr)

source('/datasets/work/af-digiscapesm/work/Ross/SLGA/PrivateInfo.R')
source(paste0('/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/HPCUtils.R'))


debugPath <- '/datasets/work/af-digiscapesm/work/Ross/HPCout'
workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Production/NationalMaps/DataPublishing'


att='CLY'
att='SLT'
att='SND'
dataDir <- '/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/predictions/SLGA_ready'
adjustVal=100
makeCOG = 'T'
fillHoles = 'T'
numDecimals=0

dataDir <- '/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/predictions/SLGA_ready/90m'
att <- 'PHW'
adjustVal=1
makeCOG = 'T'
fillHoles = 'T'
numDecimals=1

att <- 'DUL'
att <- 'L15'
dataDir <- paste0('/scratch1/', ident, '/AWCv3/Parsimonious/Mosaics')
adjustVal=1
fillHoles = 'F'
numDecimals=0

depth=''

att <- 'DES'
dataDir <- paste0('/datasets/work/af-tern-mal-deb/work/projects/soilDepth_2019/soilDepth/SLGAReady')
adjustVal=1
makeCOG = 'T'
fillHoles = 'T'
numDecimals=2

att <- 'SOC'
dataDir <- paste0('/datasets/work/af-digiscapesm/work/Ross/TERN/SOCV2/SLGAReady')
adjustVal=1
makeCOG = 'T'
fillHoles = 'T'
numDecimals=2

att = 'COL'
adjustVal=1
makeCOG = 'T'
fillHoles = 'T'
dataDir <- paste0('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/spatialPredictions/SLGAReady')


att = 'AWC'
adjustVal=1
makeCOG = 'T'

fillHoles = 'T'
dataDir <- paste0('/scratch1/', ident, '/awc')
numDecimals=0

fillHoles = 'T'
dataDir <- paste0('/scratch1/', ident, '/awc')



att <- 'AVP'
dataDir <- paste0('/datasets/work/af-digiscapesm/work/Peter/PMap/SLGAReady')
adjustVal=1
makeCOG = 'T'
fillHoles = 'T'
numDecimals=0
minVal=0


att <- 'CEC'
dataDir <- paste0('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/predictions/90m/SLGAReady')
adjustVal=1
makeCOG = 'T'
fillHoles = 'T'
numDecimals=0
minVal=NULL

att <- 'AWC'
dataDir <- paste0('/scratch1/', ident, '/AWCPreds')
adjustVal=1
makeCOG = 'T'
fillHoles = 'T'
numDecimals=0
minVal=0

att <- 'CFP'
dataDir <-  paste0('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/SOC_fractions/SLGAReady/Coarse_Fragments_Probabilities')
adjustVal=1  # 1 to ignore
makeCOG = 'T'
fillHoles = 'F'
numDecimals=2
minVal=-1  # -1 to ignore
isInt = F
resample = F
applyMask = T

att <- 'CFD'
dataDir <-  paste0('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/SOC_fractions/SLGAReady/Coarse_Fragments_DominantClass')
adjustVal=1  # 1 to ignore
makeCOG = 'T'
fillHoles = 'F'
numDecimals=-1
minVal=-1  # -1 to ignore
isInt = T
resample = F
applyMask = T

# r1 <- raster::raster('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif')
# r2 <- raster::raster('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/SOC_fractions/SLGAReady/Proportion_SOC/SOF_000_005_05_N_P_AU_TRN_N_20221006_Proportion_MAOC.tif')
# raster::compareRaster(r1, r2)

att <- 'SOF_Proportions'
dataDir <-  paste0('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/SOC_fractions/SLGAReady/Proportion_SOC')
adjustVal=1  # 1 to ignore
makeCOG = 'T'
fillHoles = 'F'
numDecimals=2
minVal=-1  # -1 to ignore
isInt = F
resample = F
applyMask = T

att <- 'SOF_Stocks'
dataDir <-  paste0('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/SOC_fractions/SLGAReady/SOCfractions_stock')
adjustVal=1  # 1 to ignore
makeCOG = 'T'
fillHoles = 'F'
numDecimals=1
minVal=-1  # -1 to ignore
isInt = F
resample = F
applyMask = T

att <- 'SOF_Fractions_Density'
dataDir <-  paste0('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/SOC_fractions/SLGAReady/SOCfractions_density')
adjustVal=1  # 1 to ignore
makeCOG = 'T'
fillHoles = 'F'
numDecimals=1
minVal=-1  # -1 to ignore
isInt = F
resample = T
applyMask = T

r1 <- raster::raster('/datasets/work/lw-soildatarepo/work/http/Products/TERN/Covariates/Mosaics/30m/Relief_dem1sv1_0.tif')
r2 <- raster::raster('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/30m/SOC/SOC_000_005_05_N_P_AU_TRN_N_20220727_30m.tif')
raster::compareRaster(r1, r2)

depth=''


att <- 'SOC30m'
dataDir <-  paste0('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/30m/SOC')
adjustVal=1  # 1 to ignore
makeCOG = 'T'
fillHoles = 'F'
numDecimals=2
minVal=0  # -1 to ignore
isInt = F
resample = F
applyMask = T

 r1 <- raster::raster('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif')
 r2 <- raster::raster('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/Soil_Microbial_Biodiversity/SLGAReady/NMDS_Bacteria_3_Bacteria_pred.tif')
 raster::compareRaster(r1, r2)

att <- 'BioDiv'
dataDir <-  paste0('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/Soil_Microbial_Biodiversity/SLGAReady')
adjustVal=1  # 1 to ignore
makeCOG = 'T'
fillHoles = 'F'
numDecimals=2
minVal=-1  # -1 to ignore
isInt = F
resample = F
applyMask = F

projcode='OD-214808'
jobName='publishSLGA'
args=paste0(att, ' ', dataDir, ' ', resample, ' ', adjustVal, ' ', fillHoles, ' ', numDecimals, ' ', makeCOG, ' ', minVal, ' ', isInt, ' ', applyMask)
print(paste0(jobName, ' : ', args))
jobID <- sendJob(jobName=jobName, att=att, depth='', projCode=projcode,  workingDir=workingDir, wallTime='00:30:00', memoryGB='100GB', jobStartIteration=1, jobEndIteration=6, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)


showVerboseJobInfo(ident,debugPath, 10, 'ALL')
showQ(ident)

showFailedJobNos(jobID)
jobName='MakeCOGS_SLGA'
args=''
print(paste0(jobName, ' : ', args))
jobID <- sendJob(jobName=jobName, att=att, depth=depth,  workingDir=workingDir, wallTime='01:30:00', memoryGB='100GB', jobStartIteration=1, jobEndIteration=195, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)



showVerboseJobInfo(ident,debugPath, 10, 'ALL')
showQ(ident)

showFailedJobs(jobID)

showDebugFile(jobName = jobName, type='error', iteration =1)
showDebugFile(jobName = jobName, type='out', iteration = 1)



showVerboseJobInfo(ident,debugPath, 20, 'ALL', fromTime = '2022-06-27T17:00:00')
showJobInfo(ident, 20, 'ALL')

showVerboseJobInfo(ident,debugPath, 20, 'ALL', fromTime = '2022-02-22T12:22:00')
showJobInfo(ident, 20, 'ALL')



monitorJob(jobID, debugPath)
showCPUs2(ident)
showCPUs(ident=ident)
showCPUs(jobID=jobID)
showQ(ident)
showQforJob(jobID)
showDetails(jobID)
showNonCompletedJobs(jobID)
showFailedJobs(jobID)
nsj <- showNonSuccessfullJobs(jobName, debugPath)


showJobLog(debugPath)
tail(showJobLog(debugPath), 100)
showFailedJobs(jobID)
showFailedJobNos(jobID)


showJobInfo(ident, 30, 'RUNNING')
showJobInfo(ident, 10, 'ALL')
showJobInfo(ident, 20, 'FAILED')
showJobInfo(ident, 20, 'CANCELLED')
showJobInfo(ident, 20, 'TIMEOUT')


showDebugFile(jobName = jobName, type='error', iteration =1322)
showDebugFile(jobName = jobName, type='out', iteration = 1018)


cancelJob(jobID)


head(showAllUsers(), 20)
HPCLoad()

