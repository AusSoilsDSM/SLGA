library(raster)
library(ranger)
library(rgdal)
library(ithir)

##########################################################################
###       Inputs
##########################################################################

#General directory
root.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry'
model.directory = paste0(root.directory, '/Models')
test.file = paste0(root.directory, '/Test_ASC_ORD.rds')
test.suborder.file = paste0(root.directory, '/CovariateTest_Suborder.rds')

order.num = 14

##########################################################################
###       Initialise
##########################################################################

#Model
model.ranger  = readRDS(paste0(model.directory, "/Ranger.rds"))

#Test data
test = readRDS(test.file)

#Split data
input = test[,-(1:4)]
test.true = test[,4]

test.suborder = readRDS(test.suborder.file)
test.suborder = test.suborder[complete.cases(test.suborder),]
input.suborder = test.suborder[,c(-(1:3), -5)]
test.suborder.true = test.suborder[,5]

##########################################################################
###       Apply the model spatially
##########################################################################

#Order
complete.index = complete.cases(input)
predicted = predict(model.ranger, data = input[complete.index,])$predictions
goof_o = goofcat(observed = test.true[complete.index], predicted = predicted)
saveRDS(goof_o, paste0(root.directory, '/Output/Goof/Goof_order.rds'))


#Suborder alone
predicted.sub = rep(NA, length(test.suborder.true))
for (i in 1:order.num){
  model.sub = readRDS(paste0(model.directory, "/Ranger.suborder_", i,".rds"))
  predicted.sub[input.suborder[,1] == i] = predict(model.sub, data = input.suborder[input.suborder[,1] == i,-1])$predictions
}
goof_sub = goofcat(observed = test.suborder.true[complete.cases(input.suborder)], predicted = predicted.sub)
saveRDS(goof_sub, paste0(root.directory, '/Output/Goof/Goof_suborder.rds'))

#Both order amd sub order
predicted.ord = predict(model.ranger, data = input.suborder[,-1])$predictions

predicted.sub2 = rep(NA, length(test.suborder.true))
for (i in 1:order.num){
  model.sub = readRDS(paste0(model.directory, "/Ranger.suborder_", i,".rds"))
  predicted.sub2[predicted.ord == i] = predict(model.sub, data = input.suborder[predicted.ord == i,-1])$predictions
}
predicted.both = 100*as.numeric(levels(predicted.ord))[predicted.ord] + predicted.sub2
predicted.both.true = 100*as.numeric(levels(input.suborder[,1]))[input.suborder[,1]] +  as.numeric(test.suborder.true)
goof_both = goofcat(observed = predicted.both.true, predicted = predicted.both)
saveRDS(goof_both, paste0(root.directory, '/Output/Goof/Goof_both.rds'))


#Order with two guess
complete.index = complete.cases(input)
predicted = predict(model.ranger, data = input[complete.index,])$predictions

pred.all = predict(model.ranger, data = input[complete.index,], predict.all = T)$predictions
#Second Prediction
second.pred = apply(pred.all, 1, function(x){
  if (sum(is.na(x)) > 0){return(NA)
  }else {return(sort(x,partial=order.num-1)[order.num-1][[1]])}
})

replace.index = !(test.true[complete.index] == predicted)

predicted[replace.index] = second.pred[replace.index]

goof_second = goofcat(observed = test.true[complete.index], predicted = predicted)
saveRDS(goof_o, paste0(root.directory, '/Output/Goof/Goof_order_second.rds')) 
