######## About this Script #######################################################################
###
###  Author : Ross Searle         
###  Date : Mon Apr 4 11:18:04 2022                      
###  Project : TERN Landscapes
###  Purpose : Massage the raw SLGA rasters to get them ready for publication
###  
###############################################################################################

library(stringr)

source('c:/PrivateInfo/PrivateInfo.R')
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
dataDir <- paste0('/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/MeasuredTextures/SLGAReady/', att)
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

fillHoles = 'F'
dataDir <- paste0('/scratch1/', ident, '/awc')



jobName='publishSLGA'
args=paste0(att, ' ', dataDir, ' ', adjustVal, ' ', fillHoles, ' ', numDecimals, ' ', makeCOG)
print(paste0(jobName, ' : ', args))
jobID <- sendJob(jobName=jobName, att=att, depth=depth,  workingDir=workingDir, wallTime='01:30:00', memoryGB='100GB', jobStartIteration=1, jobEndIteration=1, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)


showVerboseJobInfo(ident,debugPath, 10, 'ALL')
showQ(ident)

showFailedJobs(4744007)

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
cancelJob(6197494)

head(showAllUsers(), 20)
HPCLoad()

