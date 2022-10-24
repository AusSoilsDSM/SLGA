## Soil color external validation work
library(ithir);library(caret);library(ranger);library(nnet);library(MASS)

# root
root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/processed_1/"
root.models<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/models/model_fitting/GRA/"
root.data<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/"

## location of models
models<- list.files(path = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/models/model_fitting/", pattern = ".rds", full.names = T,recursive = T)
models<- models[-c(1:6)]
models

## SOIL SURFACE
# external validation data
surf.ext.day<- readRDS(paste0(root, "tern_soilcolor_siteDat_covariates_surface_valset.rds"))
surf.ext.day$L<- droplevels(surf.ext.day$L)

# L MODEL
l.model.surface<- readRDS(models[2])

pred.Lt<- predict(l.model.surface,newdata = surf.ext.day)
goofcat(observed = surf.ext.day$L, predicted = pred.Lt)
surf.ext.day$L_norm<- pred.Lt

# a MODEL
a.model.surface<- readRDS(models[16])
a.model.surface

pred.At<- predict(a.model.surface,newdata = surf.ext.day)
goof(observed = surf.ext.day$A, predicted = pred.At,plot.it = T)
surf.ext.day$a_norm<- pred.At

# b MODEL
b.model.surface<- readRDS(models[24])
b.model.surface

pred.Bt<- predict(b.model.surface,newdata = surf.ext.day)
goof(observed = surf.ext.day$B, predicted = pred.Bt, plot.it = T)
surf.ext.day$b_norm<- pred.Bt


# CASCADE MODEL

# L MODEL
l.model.surface.CAS<- readRDS(models[10])
summary(l.model.surface.CAS)
varImp(l.model.surface.CAS)

pred.Lt.CASS<- predict(l.model.surface.CAS,newdata = surf.ext.day)
goofcat(observed = surf.ext.day$L, predicted = pred.Lt.CASS)
surf.ext.day$L_cass<- pred.Lt.CASS

# a MODEL
a.model.surface<- readRDS(models[16])
a.model.surface

pred.At<- predict(a.model.surface,newdata = surf.ext.day)
goof(observed = surf.ext.day$A, predicted = pred.At)
surf.ext.day$a_cass<- pred.At

# b MODEL
b.model.surface.CASS<- readRDS(models[8])
b.model.surface.CASS
varImp(b.model.surface.CASS)

pred.Bt.CASS<- predict(b.model.surface.CASS,newdata = surf.ext.day)
goof(observed = surf.ext.day$B, predicted = pred.Bt.CASS,plot.it = T)
surf.ext.day$b_cass<- pred.Bt.CASS



## MODEL AVERAGING [GRA METHOD]
gra.data<- readRDS(paste0(root, "tern_soilcolor_siteDat_covariates_subsoil_MAset.rds"))
gra.data$L<- gra.data$L

# L MODEL #1
l.model_1.surface<- readRDS(models[2])
gra.data$model_1_L<- as.numeric(as.character(predict(l.model_1.surface,newdata = gra.data)))
surf.ext.day$model_1_L<- as.numeric(as.character(predict(l.model_1.surface,newdata = surf.ext.day)))

l.model_2.surface<- readRDS(models[10])
gra.data$model_2_L<- as.numeric(as.character(predict(l.model_2.surface,newdata = gra.data)))
surf.ext.day$model_2_L<- as.numeric(as.character(predict(l.model_2.surface,newdata = surf.ext.day)))


l.model_3.surface<- readRDS(models[12])
gra.data$model_3_L<- as.numeric(as.character(predict(l.model_3.surface,newdata = gra.data)))
surf.ext.day$model_3_L<- as.numeric(as.character(predict(l.model_3.surface,newdata = surf.ext.day)))


l.model_4.surface<- readRDS(models[14])
gra.data$model_4_L<- as.numeric(as.character(predict(l.model_4.surface,newdata = gra.data)))
surf.ext.day$model_4_L<- as.numeric(as.character(predict(l.model_4.surface,newdata = surf.ext.day)))

str(gra.data)
test <- multinom(L ~ model_1_L+ model_3_L+ model_3_L + model_4_L, data = gra.data)
summary(test)
saveRDS(object = test, file = paste0(root.models, "gra_surface_L.rds"))

## Model summaries
summary(test)
surf.ext.day$MA_L<- predict(test, newdata = surf.ext.day)
goofcat(observed = surf.ext.day$L, predicted = surf.ext.day$MA_L)


# A MODEL #1
a.model_1.surface<- readRDS(models[16])
gra.data$model_1_A<- predict(a.model_1.surface,newdata = gra.data)
goof(observed = gra.data$A,predicted = gra.data$model_1_A,plot.it = T)
surf.ext.day$model_1_A<- predict(a.model_1.surface,newdata = surf.ext.day)

a.model_2.surface<- readRDS(models[18])
gra.data$model_2_A<- predict(a.model_2.surface,newdata = gra.data)
goof(observed = gra.data$A,predicted = gra.data$model_2_A,plot.it = T)
surf.ext.day$model_2_A<- predict(a.model_2.surface,newdata = surf.ext.day)


a.model_3.surface<- readRDS(models[20])
gra.data$model_3_A<- predict(a.model_3.surface,newdata = gra.data)
goof(observed = gra.data$A,predicted = gra.data$model_3_A,plot.it = T)
surf.ext.day$model_3_A<- predict(a.model_3.surface,newdata = surf.ext.day)


a.model_4.surface<- readRDS(models[22])
gra.data$model_4_A<- predict(a.model_4.surface,newdata = gra.data)
goof(observed = gra.data$A,predicted = gra.data$model_4_A,plot.it = T)
surf.ext.day$model_4_A<- predict(a.model_4.surface,newdata = surf.ext.day)


test <- lm(A~model_1_A + model_2_A + model_3_A + model_4_A, data = gra.data)
saveRDS(object = test, file = paste0(root.models, "gra_surface_A.rds"))

## Model summaries
summary(test)
surf.ext.day$MA_A<- predict(test, newdata = surf.ext.day)
goof(observed = surf.ext.day$A, predicted = surf.ext.day$MA_A, plot.it = T)


# B MODEL #1
b.model_1.surface<- readRDS(models[24])
gra.data$model_1_B<- predict(b.model_1.surface,newdata = gra.data)
goof(observed = gra.data$B,predicted = gra.data$model_1_B,plot.it = T)
surf.ext.day$model_1_B<- predict(b.model_1.surface,newdata = surf.ext.day)

b.model_2.surface<- readRDS(models[4])
gra.data$model_2_B<- predict(b.model_2.surface,newdata = gra.data)
goof(observed = gra.data$B,predicted = gra.data$model_2_B,plot.it = T)
surf.ext.day$model_2_B<- predict(b.model_2.surface,newdata = surf.ext.day)


b.model_3.surface<- readRDS(models[6])
gra.data$model_3_B<- predict(b.model_3.surface,newdata = gra.data)
goof(observed = gra.data$B,predicted = gra.data$model_3_B,plot.it = T)
surf.ext.day$model_3_B<- predict(b.model_3.surface,newdata = surf.ext.day)


b.model_4.surface<- readRDS(models[8])
gra.data$model_4_B<- predict(b.model_4.surface,newdata = gra.data)
goof(observed = gra.data$B,predicted = gra.data$model_4_B,plot.it = T)
surf.ext.day$model_4_B<- predict(b.model_4.surface,newdata = surf.ext.day)


test <- lm(B~model_1_B + model_2_B + model_3_B + model_4_B, data = gra.data)
saveRDS(object = test, file = paste0(root.models, "gra_surface_B.rds"))

## Model summaries
summary(test)
surf.ext.day$MA_B<- predict(test, newdata = surf.ext.day)
goof(observed = surf.ext.day$B, predicted = surf.ext.day$MA_B, plot.it = T)




## REFERENCE DATA
ref.data<- read.csv(paste0(root.data,"soilColorRef_updated.csv"))

surf.ext.day$L_norm<- as.numeric(as.character(surf.ext.day$L_norm))
surf.ext.day$L_cass<- as.numeric(as.character(surf.ext.day$L_cass))
surf.ext.day$MA_L<- as.numeric(as.character(surf.ext.day$MA_L))

d1<-sqrt((surf.ext.day$L_norm[1] -  ref.data$L.)^2 + (surf.ext.day$a_norm[1] -  ref.data$a.)^2 + (surf.ext.day$b_norm[1] -  ref.data$b.)^2)
d2<-sqrt((surf.ext.day$L_cass[1] -  ref.data$L.)^2 + (surf.ext.day$a_cass[1] -  ref.data$a.)^2 + (surf.ext.day$b_cass[1] -  ref.data$b.)^2)
d3<-sqrt((surf.ext.day$MA_L[1] -  ref.data$L.)^2 + (surf.ext.day$MA_A[1] -  ref.data$a.)^2 + (surf.ext.day$MA_B[1] -  ref.data$b.)^2)
which(surf.ext.day$L[1] == ref.data$L. & surf.ext.day$A[1] == ref.data$a. & surf.ext.day$B[1] == ref.data$b.)

which(d1 == min(d1))
which(d2 == min(d2))
which(d3 == min(d3))

ref.data[740,]
names(surf.ext.day)
surf.ext.day[1,c("L","A","B")]
surf.ext.day[1,c("L_norm","a_norm","b_norm")]
surf.ext.day[1,c("L_cass","a_cass","b_cass")]
surf.ext.day[1,c("MA_L","MA_A","MA_B")]

names(surf.ext.day)
# add some new columns
surf.ext.day$L_norm_near<- NA
surf.ext.day$a_norm_near<- NA
surf.ext.day$b_norm_near<- NA

surf.ext.day$L_cass_near<- NA
surf.ext.day$a_cass_near<- NA
surf.ext.day$b_cass_near<- NA

surf.ext.day$MA_L_near<- NA
surf.ext.day$MA_A_near<- NA
surf.ext.day$MA_B_near<- NA

surf.ext.day$norm_neigh<- NA
surf.ext.day$cass_neigh<- NA
surf.ext.day$MA_neigh<- NA


# cycle through each site and do the distance calcs

for (i in 1:nrow(surf.ext.day)){
  d1<-sqrt((surf.ext.day$L_norm[i] -  ref.data$L.)^2 + (surf.ext.day$a_norm[i] -  ref.data$a.)^2 + (surf.ext.day$b_norm[i] -  ref.data$b.)^2)
  d2<-sqrt((surf.ext.day$L_cass[i] -  ref.data$L.)^2 + (surf.ext.day$a_cass[i] -  ref.data$a.)^2 + (surf.ext.day$b_cass[i] -  ref.data$b.)^2)
  d3<-sqrt((surf.ext.day$MA_L[i] -  ref.data$L.)^2 + (surf.ext.day$MA_A[i] -  ref.data$a.)^2 + (surf.ext.day$MA_B[i] -  ref.data$b.)^2)
  obs.col<- which(surf.ext.day$L[i] == ref.data$L. & surf.ext.day$A[i] == ref.data$a. & surf.ext.day$B[i] == ref.data$b.)
  
  d1.min<-which(d1 == min(d1))
  d2.min<- which(d2 == min(d2))
  d3.min<- which(d3 == min(d3))
  
  # put nearest into the data frame
  surf.ext.day[i,c("L_norm_near", "a_norm_near", "b_norm_near")]<- ref.data[d1.min,10:12]
  surf.ext.day[i,c("L_cass_near", "a_cass_near", "b_cass_near")]<- ref.data[d2.min,10:12]
  surf.ext.day[i,c("MA_L_near", "MA_A_near", "MA_B_near")]<- ref.data[d3.min,10:12]
  
  or.d1<- order(d1)
  or.d2<- order(d2)
  or.d3<- order(d3)
  
  surf.ext.day$norm_neigh[i]<- which(or.d1 == obs.col)
  surf.ext.day$cass_neigh[i]<- which(or.d2 == obs.col)
  surf.ext.day$MA_neigh[i]<- which(or.d3 == obs.col)
  
  print(i)}


# Export data
saveRDS(object = surf.ext.day, file = paste0(root, "surface_externalVAL_outs.rds"))
write.csv(x = surf.ext.day,file = paste0(root, "surface_externalVAL_outs.csv"),row.names = F ) 

hist(surf.ext.day$norm_neigh)  
hist(surf.ext.day$cass_neigh)  
hist(surf.ext.day$MA_neigh)    
surf.ext.day$MA_neigh

