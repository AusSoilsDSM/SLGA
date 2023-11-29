### TERN Landscapes 
# Total P
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 20.11.23
# modified: 20.11.23

# CODE PURPOSE
# Determination of ranger model hyperparmeters
##


# root
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_P/"
model.out<- paste0(g.root, "models/hyperparameter/")


# libraries
library(caret);library(ranger)


#data
# site data
site.dat<- readRDS(paste0(g.root,"data/ross/model_ready/tern_totalP_labelled_covariates_CAL.rds"))
names(site.dat)


model.frame<- site.dat[,c(3,6:45)]

## Ranger model with cross validation
names(model.frame)


ranger.model<-train(x= model.frame[,2:41], y= model.frame$target,method = "ranger",trControl = trainControl(method = "oob"), num.trees =  500)  
summary(ranger.model)
ranger.model

## capture output
var_nm1<- paste0(model.out,"rangerModel_params_totalP.txt")

out1<- capture.output(summary(ranger.model))
out1<- paste0(out1,collapse = "\r\n")
cat(out1, file = var_nm1, sep=",", append = T)

out2<- capture.output(ranger.model)
out2<- paste0(out2,collapse = "\r\n")
cat(out2, file = var_nm1, sep=",", append = T)

# END



