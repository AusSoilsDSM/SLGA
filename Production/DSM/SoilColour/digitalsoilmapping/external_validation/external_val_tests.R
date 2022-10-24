## Soil color external validation work
library(ithir);library(caret);library(ranger);library(nnet);library(MASS)

# root
root<- "Z:/projects/ternlandscapes_2019/soilColour/data/processed_1/"

## location of models
models<- list.files(path = "Z:/projects/ternlandscapes_2019/soilColour/models/model_fitting/", pattern = ".rds", full.names = T,recursive = T)

## SOIL SURFACE
# external validation data
surf.ext.day<- readRDS(paste0(root, "tern_soilcolor_siteDat_covariates_surface_valset.rds"))

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
goof(observed = surf.ext.day$B, predicted = pred.Bt)
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
goof(observed = surf.ext.day$B, predicted = pred.Bt.CASS)
surf.ext.day$b_cass<- pred.Bt.CASS


## MODEL AVERAGING [GRA METHOD]
gra.data<- readRDS(paste0(root, "tern_soilcolor_siteDat_covariates_subsoil_MAset.rds"))
# L MODEL #1
l.model_1.surface<- readRDS(models[2])
gra.data$model_1_L<- predict(l.model_1.surface,newdata = gra.data)

l.model_2.surface<- readRDS(models[10])
gra.data$model_2_L<- predict(l.model_2.surface,newdata = gra.data)

l.model_3.surface<- readRDS(models[12])
gra.data$model_3_L<- predict(l.model_3.surface,newdata = gra.data)

l.model_4.surface<- readRDS(models[14])
gra.data$model_4_L<- predict(l.model_4.surface,newdata = gra.data)

test <- multinom(droplevels(L)~model_1_L+model_3_L+model_3_L+model_4_L, data = gra.data)

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
surf.ext.day$model_2_A<- predict(a.model_2.surface,newdata = surf.ext.day)


a.model_3.surface<- readRDS(models[20])
gra.data$model_3_A<- predict(a.model_3.surface,newdata = gra.data)
surf.ext.day$model_3_A<- predict(a.model_3.surface,newdata = surf.ext.day)


a.model_4.surface<- readRDS(models[22])
gra.data$model_4_A<- predict(a.model_4.surface,newdata = gra.data)
surf.ext.day$model_4_A<- predict(a.model_4.surface,newdata = surf.ext.day)


test <- lm(A~model_1_A+model_2_A+model_3_A+model_4_A, data = gra.data)

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


test <- lm(B~model_1_B+model_2_B+model_3_B+model_4_B, data = gra.data)

## Model summaries
summary(test)
surf.ext.day$MA_B<- predict(test, newdata = surf.ext.day)
goof(observed = surf.ext.day$B, predicted = surf.ext.day$MA_B, plot.it = T)








