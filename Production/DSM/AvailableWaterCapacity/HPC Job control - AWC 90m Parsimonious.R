
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
source('PrivateInfo')

debugPath <- '/datasets/work/af-digiscapesm/work/Ross/HPCout'
workingDir<- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/AWC/MeasuredTextures/V2'
fls <- list.files(workingDir)

#jobEndIteration=218

att='AWCParsimonious'
depth=''
method='Parsimonious'

########  Drill the covariates
jobName='HPCDrillCovariates'
args=paste0(method)
print(paste0(jobName, ' : ', args))
jobID <- sendJob(jobName=jobName, att=att, depth=depth,  workingDir=workingDir, wallTime='00:10:00', memoryGB='5GB', jobStartIteration=1, jobEndIteration=218, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

showVerboseJobInfo(ident,debugPath, 10, 'ALL')
# #########  Generate the models
# jobName='RFProcessingBootstrappingHPC'
# args=paste0(att)
# print(paste0(jobName, ' : ', args))
# jobID <- sendJob(jobName=jobName, att=att, depth=depth,  workingDir=workingDir, wallTime='00:60:00', memoryGB='30GB', jobStartIteration=1, jobEndIteration=50, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)



covs = 'Parsimonious'
product = 'AWCv3'
grpSize=4
att='DLL'
att='DUL'
#covFilt = 'isParsimonious'
depth = '005'
depth = '015'
depth = '030'
depth = '060'
depth = '100'
depth = '200'

jobName='HPCApplyRFModelToTile_V3_2'
args=paste0(att, ' ',product, ' ', depth, ' ', covs, ' ', grpSize)
print(paste0(jobName, ' : ', args))
n=ceiling(2172/grpSize)

jobID <- sendJob(jobName=jobName, att=att, depth=depth,  workingDir=workingDir, wallTime='10:00:00', memoryGB='15GB', jobStartIteration=1, jobEndIteration=n, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)


showVerboseJobInfo(ident,debugPath, 20, 'ALL', fromTime = '2022-08-24T07:00:00')

activeJobCount(ident)
showQ(ident)

showFailedJobNos(jobID)
showFailedJobNos(9342360)
showDebugFile(jobName = jobName, type='error', iteration =5)
showDebugFile(jobName = jobName, type='out', iteration = 1018)

showCPUs2(ident)
#cancelJob(jobID)
#cancelJob(6347492)

att='Pmap'
depth=''
workingDir <- '/datasets/work/af-digiscapesm/work/Ross/temp'
jobName='Mapper'
args=paste0('Bob')
print(paste0(jobName, ' : ', args))
n=2172

jobID <- sendJob(jobName=jobName, att=att, depth=depth,  workingDir=workingDir, wallTime='06:00:00', memoryGB='15GB', jobStartIteration=1, jobEndIteration=n, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)



##########   Make the mosaics from the tiles for all products   #########

jobName='makeMosaics'
att='DUL'
att='DLL'
method='Parsimonious'


adjustVal=1
makeCOG = 'T'

numDecimals=0
depth=''
projcode='OD-214808'

#args=paste0(att, ' ', method, ' ', dataDirRoot, ' ', adjustVal, ' ', numDecimals, ' ', makeCOG)

args=paste0(att, ' ', method)
print(paste0(jobName, ' : ', args))
jobID <- sendJob(jobName=jobName, att=att, depth=depth, projCode=projcode,  workingDir=workingDir, wallTime='01:00:00', memoryGB='100GB', jobStartIteration=1, jobEndIteration=24, limit='', arguments=args, debugPath=debugPath, deleteDebugFiles=T)

head(showAllUsers(), 20)

showVerboseJobInfo(ident,debugPath, 20, 'ALL')
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
