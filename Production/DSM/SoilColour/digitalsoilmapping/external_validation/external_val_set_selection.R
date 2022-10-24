## Soil color model fitting
# Selection of external validation dataset


root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/processed_1/"

  
data<- readRDS(paste0(root,"tern_soilcolor_siteDat_2ndround_covariates_20200908.rds"))
names(data)



# general processing
data<- data[complete.cases(data[,44:82]),]
data$L.<- as.factor(data$L.)
data$relief_geomorphons<- as.factor(data$relief_geomorphons)
which(is.na(data[,33:35]))
# re-arrange columns
data<- data[,c(4,43,17:22,33:35,44:82)]
names(data)[9:11]<- c("L", "A", "B")
rms<- which(data$L == "10.63")
rms
data<- data[-rms,]
rms<- which(data$L == "91.08")
rms
data<- data[-rms,]
data$L<- droplevels(data$L)
data$L
# split into top and subsoils
top.data<- data[data$level == "top",]
sub.data<- data[data$level == "sub",]

summary(top.data$L)
summary(sub.data$L)
#### Selection of external validation data
#surface
nrow(top.data)
training <- sample(nrow(top.data), 30000,replace = F)
val.top<- top.data[training,]
top.data<- top.data[-training,]
saveRDS(top.data, file = paste0(root,"tern_soilcolor_siteDat_covariates_surface_calset.rds"))
training <- sample(nrow(val.top), 10000,replace = F)
MA.top<- val.top[training,]
saveRDS(MA.top, file = paste0(root,"tern_soilcolor_siteDat_covariates_surface_MAset.rds"))
val.top<- val.top[-training,]
saveRDS(val.top, file = paste0(root,"tern_soilcolor_siteDat_covariates_surface_valset.rds"))

#subsoil
nrow(sub.data)
training <- sample(nrow(sub.data), 30000,replace = F)
val.top<- sub.data[training,]
sub.data<- sub.data[-training,]
saveRDS(sub.data, file = paste0(root,"tern_soilcolor_siteDat_covariates_subsoil_calset.rds"))
training <- sample(nrow(val.top), 10000,replace = F)
MA.top<- val.top[training,]
saveRDS(MA.top, file = paste0(root,"tern_soilcolor_siteDat_covariates_subsoil_MAset.rds"))
val.top<- val.top[-training,]
saveRDS(val.top, file = paste0(root,"tern_soilcolor_siteDat_covariates_subsoil_valset.rds"))


#END


