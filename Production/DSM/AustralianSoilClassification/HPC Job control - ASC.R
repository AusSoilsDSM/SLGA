
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

source('c:/PrivateInfo/PrivateInfo.R')
source(paste0('/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/HPCUtils.R'))


debugPath <- '/datasets/work/af-digiscapesm/work/Ross/HPCout'
# workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/AWC/APSOIL_RF'
# rootDir2 <- '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/ApsoilRF'
workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Production/NationalMaps/AustralianSoilClassificationRoss'

mf <- paste0('RFmodel_ASCOnly.rds')
#rootDir2 <- '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/ApsoilRFSelCovs'


#jobEndIteration=2040

att='ASC'
att='ASCOnly'

depth=''

jobName='doPredictions'
args=paste0(att, ' ', mf, ' 20 F')
print(args)
jobID <- sendJobNoDepths(jobName=jobName, att=att, workingDir=workingDir, wallTime='01:30:00', memoryGB='20GB', jobStartIteration=1, jobEndIteration=2040, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

#####  fills in missing files after run above if neeeds be
jobName='doPredictions'
chks<- 2040
args=paste0(att, ' ', mf, ' 20 F')
print(args)
pb <- txtProgressBar(min=0, max=chks, style=3)
for (i in 1:chks) {
  setTxtProgressBar(pb, i)
  f <- paste0(workingDir, '/Maps/', att, '/Chunks/AllCellVals_', i, '.rds')
  if(!file.exists(f)){
    print(paste0('Missing - ', f))
    jobID <- sendJobNoDepths(jobName=jobName, att=att, workingDir=workingDir, wallTime='01:30:00', memoryGB='16GB', jobStartIteration=i, jobEndIteration=i, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=F)
    }
}
close(pb)



#########   Bootstrap predictions   ##########

att = 'ASCOnly'
jobName='doPredictionsBootstrap'
args=paste0(att, ' 20 F')
print(args)
jobID <- sendJobNoDepths(jobName=jobName, att=att, workingDir=workingDir, wallTime='10:00:00', memoryGB='16GB', jobStartIteration=1, jobEndIteration=2040, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)



showJobInfo(ident, 10, 'ALL')
showVerboseJobInfo(ident,debugPath, 10, 'ALL')
showVerboseJobInfo(ident,debugPath, 100, 'ALL', fromTime = '2021-05-12T12:22:00')

showVerboseJobInfo(ident,debugPath, 200, 'RUNNING')

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

showFailedJobs('52188662')

showNonCompletedJobs('53046746')

showJobLog(debugPath)
tail(showJobLog(debugPath), 100)
showFailedJobs(jobID)
showFailedJobNos(jobID)

showQforJob('53299636')

showJobInfo(ident, 30, 'RUNNING')
showJobInfo(ident, 10, 'ALL')
showJobInfo(ident, 20, 'FAILED')
showJobInfo(ident, 20, 'CANCELLED')
showJobInfo(ident, 20, 'TIMEOUT')


showDebugFile(jobName = jobName, type='error', iteration =1)
showDebugFile(jobName = jobName, type='out', iteration = 1018)


jobID='53283428'
cancelJob('53800649')

 cancelJob(jobID)

jobID<-'47159401'

head(showAllUsers(), 10)
HPCLoad()
