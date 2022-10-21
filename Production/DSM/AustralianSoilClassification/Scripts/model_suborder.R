##########################################################################
###       Packages
##########################################################################

library(raster)
library(ranger)
library(rgdal)

##########################################################################
###       Inputs
##########################################################################

#General directory
root.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry'
model.directory = paste0(root.directory, '/Models')


#Training data location
training.filename = 'ASC_ORD'      ## ".shp" extension not needed
training.fieldname = 'O_ASC_ORD'
training.fieldname.sub = 'O_ASC_SUBORD'
training.id = "Observation_ID" 

order.num = 14

##########################################################################
###       Initialise
##########################################################################

#Covariate data
input  = readRDS(paste0(root.directory, "/CovariateTraining.rds"))

#Suborder data
soil.data  = readRDS(paste0(root.directory, "/SoilTypes.rds"))

#Original order levels
order.original = readRDS(paste0(root.directory, '/OriginalFactors.rds'))

order.train = readRDS(paste0(root.directory, "/Test_ASC_ORD.rds"))

##########################################################################
###       Merge Data
##########################################################################

#Merge data
input = merge(input, soil.data[,c(4,19)], by.x = 'SID', by.y = training.id)
input = input[,c(1:4,dim(input)[2],5:(dim(input)[2]-1))] #putting suborder next to order

#Remove empty columns
input = input[!is.na(input$O_ASC_SUBORD),]

##########################################################################
###       Correct Soil Types
##########################################################################

soil.suborders = c('HR','HS','HT','HU','HV','HW','HX',                              #ANTHROPOSILS
                   'EL','FJ','CV','DA','FB','CQ','BD',                              #CALCAROSOLS
                   'AA','AB','AC','AD','AE',                                        #CHROMOSOLS
                   'AA','AB','AC','AD','AE',                                        #DERMOSOLS
                   'AA','AB','AC','AD','AE',                                        #FERROSOLS
                   'EW','BT','CS','EG','ED','DT',                                   #HYDROSOLS
                   'AA','AB','AC','AD','AE',                                        #KANDOSOLS
                   'AA','AB','AC','AD','AE',                                        #KUROSOLS
                   'BW','CE','EH',                                                  #ORGANOSOLS
                   'AL','EJ','AM',                                                  #PODOSOLS
                   'FJ','CS','EL','HG','AO','GV','ER','HH','CY',                    #RUDOSOLS
                   'AA','AB','AC','AD','AE',                                        #SODOSOLS
                   'BF','BE','IL','IM','AW','CY','GZ','IN','IO','IP','IQ','IR',     #TENOSOLS
                   'AA','AB','AC','AD','AE'                                         #vERTOSOLS
                   )

soil.suborders = unique(soil.suborders)

#Remove incorrect suborders
input = input[which(input$O_ASC_SUBORD %in% soil.suborders),]

##########################################################################
###       Remove incorrect Combinations
##########################################################################

qqq = list()
qqq[[1]] = c('AA','AB','AC','AD','AE')#vERTOSOLS
qqq[[2]] = c('AA','AB','AC','AD','AE')#SODOSOLS
qqq[[3]] = c('AA','AB','AC','AD','AE')#DERMOSOLS
qqq[[4]] = c('AA','AB','AC','AD','AE')#CHROMOSOLS
qqq[[5]] = c('AA','AB','AC','AD','AE')#FERROSOLS
qqq[[6]] = c('AA','AB','AC','AD','AE')#KUROSOLS
qqq[[7]] = c('BF','BE','IL','IM','AW','CY','GZ','IN','IO','IP','IQ','IR')#TENOSOLS
qqq[[8]] = c('AA','AB','AC','AD','AE') #KANDOSOLS
qqq[[9]] = c('EW','BT','CS','EG','ED','DT')#HYDROSOLS
qqq[[10]] = c('AL','EJ','AM')#PODOSOLS
qqq[[11]] = c('FJ','CS','EL','HG','AO','GV','ER','HH','CY')#RUDOSOLS
qqq[[12]] = c('EL','FJ','CV','DA','FB','CQ','BD')#CALCAROSOLS
qqq[[13]] = c('BW','CE','EH')#ORGANOSOLS
qqq[[14]] = c('HR','HS','HT','HU','HV','HW','HX')#ANTHROPOSILS

correct.id = c()
for (i in 1:order.num){
  correct = which(input[input[,4] == i,5] %in% qqq[[i]])
  correct.id = c(correct.id,as.character(input[input[,4] == i,1][correct]))
}
saveRDS(correct.id, paste0(root.directory, '/ValidSuborderIputs.rds') )

input = input[which(input[,1] %in% correct.id),]

##########################################################################
###       Change Factor Levels
##########################################################################

#Save original levels
suborder.original = unique(input[[training.fieldname.sub]])
saveRDS(suborder.original, (paste0(root.directory, "/OriginalSubOrders.rds")))

#Change to number order
for (i in 1:length(suborder.original)){
  input[[training.fieldname.sub]][input[[training.fieldname.sub]] == suborder.original[i]] = i
}

test = input[which(order.train[,1] %in% input[,1]),]

input = input[-which(order.train[,1] %in% input[,1]),]



#input[[training.fieldname.sub]] = as.factor(soil.data[[training.fieldname.sub]])
saveRDS(input, paste0(root.directory, '/CovariateTraining_Suborder.rds'))
saveRDS(test, paste0(root.directory, '/CovariateTest_Suborder.rds'))

##########################################################################
###       Implement Algorithms
##########################################################################
                            
for (i in 1:length(order.original)){
  
  data = input[complete.cases(input) & input[[training.fieldname]] == i,]
  
  model.suborder.ranger = ranger(as.factor(data[[training.fieldname.sub]]) ~., data = data[,-(1:5)], write.forest = TRUE)
  
  #Save model
  saveRDS(model.suborder.ranger, paste0(model.directory, "/Ranger.suborder_", i,".rds"))
  
}





