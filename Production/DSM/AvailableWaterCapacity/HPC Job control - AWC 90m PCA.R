
######## About this Script #######################################################################
###
###  Author : Ross Searle         
###  Date : Wed Jul 29 11:18:04 2020                      
###  Project : TERN Landscapes
###  Purpose : This script controls and monitors processing on the HPC
###  
###############################################################################################


library(stringr)


source(paste0('/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/HPCUtils.R'))
source('c:/PrivateInfo/PrivateInfo.R')


debugPath <- '/datasets/work/af-digiscapesm/work/Ross/HPCout'
workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/AWC/MeasuredTextures/V2'
fls <- list.files(workingDir)

#jobEndIteration=218

att='AWCPCA'
depth=''

########  Drill the covariates
jobName='HPCDrillCovariates'
args=paste0(att)
print(paste0(jobName, ' : ', args))
jobID <- sendJob(jobName=jobName, att=att, depth=depth,  workingDir=workingDir, wallTime='00:10:00', memoryGB='5GB', jobStartIteration=1, jobEndIteration=218, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)



# #########  Generate the models
# jobName='RFProcessingBootstrappingHPC'
# args=paste0(att)
# print(paste0(jobName, ' : ', args))
# jobID <- sendJob(jobName=jobName, att=att, depth=depth,  workingDir=workingDir, wallTime='00:60:00', memoryGB='30GB', jobStartIteration=1, jobEndIteration=50, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)



covs = 'Parsimonious'
product = 'AWC'
grpSize=4
att='DUL'
att='DLL'
#covFilt = 'isParsimonious'
depth = '005'
depth = '015'
depth = '030'
depth = '060'
depth = '100'
depth = '200'

jobName='HPCApplyRFModelToTile'
args=paste0(att, ' ',product, ' ', depth, ' ', covs, ' ', grpSize)
print(paste0(jobName, ' : ', args))
n=ceiling(2172/grpSize)
jobID <- sendJob(jobName=jobName, att=att, depth=depth,  workingDir=workingDir, wallTime='04:00:00', memoryGB='10GB', jobStartIteration=1, jobEndIteration=n, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

showVerboseJobInfo(ident,debugPath, 25, 'ALL', fromTime = '2022-06-21T18:15:00')
showQ(ident)

activeJobCount(ident)
showCPUs2(ident)


showFailedJobNos(jobID)
showDebugFile(jobName = jobName, type='error', iteration =5)
showDebugFile(jobName = jobName, type='out', iteration = 1018)
#cancelJob(jobID)
cancelJob(7130467 )



##########   Make the mosaics from the tiles for all products   #########

jobName='makeMosaics'
att='DUL'
att='DLL'
#method='PCA'
method='Parsimonious'
depth=''

args=paste0(att, ' ', method)
print(paste0(jobName, ' : ', args))
jobID <- sendJob(jobName=jobName, att=att, depth=depth,  workingDir=workingDir, wallTime='01:30:00', memoryGB='100GB', jobStartIteration=2, jobEndIteration=4, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

showVerboseJobInfo(ident,debugPath, 10, 'ALL', fromTime = '2022-06-21T09:00:00')

adjustVal=1
makeCOG = 'T'
dataDirRoot <- paste0('/scratch2/', ident)
numDecimals=0
depth=''


#args=paste0(att, ' ', method, ' ', dataDirRoot, ' ', adjustVal, ' ', numDecimals, ' ', makeCOG)


head(showAllUsers(), 20)

showVerboseJobInfo(ident,debugPath, 10, 'ALL')
showVerboseJobInfo(ident,debugPath, 10, 'ALL', fromTime = '2022-02-28T12:22:00')

showVerboseJobInfo(ident,debugPath, 200, 'RUNNING')


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

showFailedJobs('3890830')

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
cancelJob('53189218')

 cancelJob(jobID)

jobID<-'47159401'

head(showAllUsers(), 20)
HPCLoad()
