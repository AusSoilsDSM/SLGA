
######## About this Script #######################################################################
###
###  Author : Ross Searle         
###  Date : Wed Jul 29 11:18:04 2020                      
###  Project : TERN Landscapes
###  Purpose : This script controls and monitors processing on the HPC
###  
###############################################################################################

library(raster)
library(stringr)
library(rgdal)

source(paste0('/datasets/work/af-tern-mal-deb/work/projects/r_codes_2019/HPC_codes/HPCUtils.R'))


debugPath <- '/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/rcode/slurm/spatialprediction/HPCout'


workingDir<- '/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/rcode/digitalsoilmapping/spatialPrediction'

## Inital job run
jobID <- sendJob(jobName=jobName, 
                 workingDir=workingDir, 
                 wallTime='3:00:00', memoryGB='8GB', 
                 jobStartIteration=1, jobEndIteration=2172, 
                 debugPath=debugPath,jobFileName='nir_spatial_all', deleteDebugFiles=T)


# clean up the missed ones for whatever reason
files<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/rcode/slurm/spatialprediction/NIR/"
files.short<- list.files(path = files)
file.short.spl<- strsplit(files.short,"_")

sca.num<- as.numeric(sapply(file.short.spl, `[`, 2))
sca.num<- sca.num[order(sca.num)]
sca.num
xx<- 1:2172
setdiff(xx, sca.num) 


its<- c(299 ,300,608,612,613,772,852,853,882,887,963,1016,1017,1126,1171,1173,2050)

for (ww in 1:length(its)){
  jobName='nir_spatial_all'
  jobID <- sendJob(jobName=jobName, workingDir=workingDir, wallTime='3:00:00', memoryGB='8GB', jobStartIteration=its[ww], jobEndIteration=its[ww], debugPath=debugPath,jobFileName='nir_spatial_all', deleteDebugFiles=F)}


monitorJob(jobID, debugPath) 
showCPUs(ident=ident)
showCPUs(jobID=jobID)
showQ(ident)
showQforJob(jobID)
showDetails(jobID)
showNonCompletedJobs(jobID)
showFailedJobs(jobID)
showNonSuccessfullJobs(jobName, debugPath)

showJobLog(debugPath)
showFailedJobs(jobID)
showFailedJobNos(jobID)


showJobInfo('sea084', 10, 'RUNNING')
showJobInfo('sea084', 10, 'ALL')
showJobInfo('sea084', 20, 'FAILED')
showJobInfo('sea084', 20, 'CANCELLED')
showJobInfo('sea084', 20, 'TIMEOUT')

showDebugFile(jobName = 'mosLinMaps90mTiles', type='error', iteration = 1)
showDebugFile(jobName = jobName, type='out', iteration = 1)

cancelJob(jobID)
cancelJob('47124435')

jobID<-'47124728'

showAllUsers()
