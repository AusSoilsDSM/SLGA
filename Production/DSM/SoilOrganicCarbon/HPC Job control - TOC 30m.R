
######## About this Script #######################################################################
###
###  Author : Ross Searle         
###  Date : Thurs Sept 8 9:18:04 2022                      
###  Project : TERN Landscapes
###  Purpose : This script controls and monitors processing on the HPC
###  
###############################################################################################


library(stringr)


source(paste0('/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/HPCUtils.R'))
source('PrivateInfo.R')

debugPath <- '/datasets/work/af-digiscapesm/work/Ross/HPCout'
workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/TOC30m'

grpSize=4

depth=''
jobName='HPCApplyRFModelToTile_TOC'
args=paste0(grpSize)
print(paste0(jobName, ' : ', args))
#n=ceiling(2172/grpSize) # 90m Tiles
n=ceiling(18359/grpSize)
projcode='OD-214808'

jobID <- sendJob(jobName=jobName, att='TOC', depth=depth, projCode = projcode, workingDir=workingDir, wallTime='03:00:00', memoryGB='50GB', jobStartIteration=1, jobEndIteration=n, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)
showVerboseJobInfo(ident,debugPath, 20, 'ALL', fromTime = '2022-09-12T10:00:00')

jobName='makeMosaics'
args=''
jobID <- sendJob(jobName=jobName, att='TOC', depth=depth, projCode = projcode, workingDir=workingDir, wallTime='05:00:00', memoryGB='50GB', jobStartIteration=1, jobEndIteration=18, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)
showVerboseJobInfo(ident,debugPath, 20, 'ALL', fromTime = '2022-09-12T10:00:00')


jobName='makePatchedCOGs'
args=''
jobID <- sendJob(jobName=jobName, att='TOC', depth=depth, projCode = projcode, workingDir=workingDir, wallTime='05:00:00', memoryGB='50GB', jobStartIteration=1, jobEndIteration=4, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)
showVerboseJobInfo(ident,debugPath, 20, 'ALL', fromTime = '2022-09-10T06:55:00')


activeJobCount(ident)
showQ(ident)
showQforJob(9902902)

showNonCompletedJobs(jobID)
showFailedJobNos(jobID)
showFailedJobNos(9902902)
showDebugFile(jobName = jobName, type='error', iteration =5)
showDebugFile(jobName = jobName, type='out', iteration = 1018)

showCPUs2(ident)

head(showAllUsers(), 20)
HPCLoad()


cancelJob(jobID)
          
monitorJob(jobID, debugPath)
showCPUs2(ident)
showCPUs(ident=ident)
showCPUs(jobID=jobID)
showQ(ident)
showQforJob(jobID)
showDetails(jobID)
tail(showNonCompletedJobs(jobID))
showFailedJobs(jobID)
nsj <- showNonSuccessfullJobs(jobName, debugPath)


