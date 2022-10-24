### TERN LANDSCAPES 
# Soil pH model model fitting
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 12.5.21
# modified: 27.5.21

# CODE PURPOSE
# Assess the external validation predictions and come up with depth specific MSE estimates for the uncertainty analysis
library(ithir);library(MASS)

model.out<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/models/"

# External validation
# depth 1
root.in<- paste0(model.out,"d1/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
temp.file<- temp.file[0,]
for(i in 1:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- rbind(temp.file,in.dat)
}
obs.dat<- temp.file$target
pred.dat<- temp.file$prediction
goof.out.d1<- goof(observed = obs.dat, predicted = pred.dat)
goof.out.d1$depth<- "d1"
goof.out.d1

# depth 2
root.in<- paste0(model.out,"d2/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
temp.file<- temp.file[0,]
for(i in 1:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- rbind(temp.file,in.dat)
}
obs.dat<- temp.file$target
pred.dat<- temp.file$prediction
goof.out.d2<- goof(observed = obs.dat, predicted = pred.dat,plot.it = F)
goof.out.d2$depth<- "d2"
goof.out.d2


# depth 3
root.in<- paste0(model.out,"d3/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
temp.file<- temp.file[0,]
for(i in 1:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- rbind(temp.file,in.dat)
}
obs.dat<- temp.file$target
pred.dat<- temp.file$prediction
goof.out.d3<- goof(observed = obs.dat, predicted = pred.dat,plot.it = F)
goof.out.d3$depth<- "d3"
goof.out.d3

# depth 4
root.in<- paste0(model.out,"d4/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
temp.file<- temp.file[0,]
for(i in 1:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- rbind(temp.file,in.dat)
}
obs.dat<- temp.file$target
pred.dat<- temp.file$prediction
goof.out.d4<- goof(observed = obs.dat, predicted = pred.dat,plot.it = F)
goof.out.d4$depth<- "d4"
goof.out.d4

# depth 5
root.in<- paste0(model.out,"d5/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
temp.file<- temp.file[0,]
for(i in 1:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- rbind(temp.file,in.dat)
}
obs.dat<- temp.file$target
pred.dat<- temp.file$prediction
goof.out.d5<- goof(observed = obs.dat, predicted = pred.dat,plot.it = F)
goof.out.d5$depth<- "d5"
goof.out.d5

# depth 6
root.in<- paste0(model.out,"d6/data_obs_preds/ext/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
temp.file<- temp.file[0,]
for(i in 1:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- rbind(temp.file,in.dat)
}
obs.dat<- temp.file$target
pred.dat<- temp.file$prediction
goof.out.d6<- goof(observed = obs.dat, predicted = pred.dat,plot.it = F)
goof.out.d6$depth<- "d6"
goof.out.d6


# combine goof outputs and save
all.goof.EXT<- rbind(goof.out.d1,goof.out.d2,
                 goof.out.d3,goof.out.d4,
                 goof.out.d5,goof.out.d6)
all.goof.EXT
write.csv(x = all.goof.EXT, file = paste0(model.out,"ranger_EXT_diogs_pH_summary_alldepths.csv") ,row.names = F)


# Validation validation
# depth 1
root.in<- paste0(model.out,"d1/data_obs_preds/val/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
temp.file<- temp.file[0,]
for(i in 1:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- rbind(temp.file,in.dat)
}
obs.dat<- temp.file$target
pred.dat<- temp.file$prediction
goof.out.d1<- goof(observed = obs.dat, predicted = pred.dat)
goof.out.d1$depth<- "d1"
goof.out.d1

# depth 2
root.in<- paste0(model.out,"d2/data_obs_preds/val/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
temp.file<- temp.file[0,]
for(i in 1:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- rbind(temp.file,in.dat)
}
obs.dat<- temp.file$target
pred.dat<- temp.file$prediction
goof.out.d2<- goof(observed = obs.dat, predicted = pred.dat,plot.it = F)
goof.out.d2$depth<- "d2"
goof.out.d2


# depth 3
root.in<- paste0(model.out,"d3/data_obs_preds/val/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
temp.file<- temp.file[0,]
for(i in 1:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- rbind(temp.file,in.dat)
}
obs.dat<- temp.file$target
pred.dat<- temp.file$prediction
goof.out.d3<- goof(observed = obs.dat, predicted = pred.dat,plot.it = F)
goof.out.d3$depth<- "d3"
goof.out.d3

# depth 4
root.in<- paste0(model.out,"d4/data_obs_preds/val/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
temp.file<- temp.file[0,]
for(i in 1:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- rbind(temp.file,in.dat)
}
obs.dat<- temp.file$target
pred.dat<- temp.file$prediction
goof.out.d4<- goof(observed = obs.dat, predicted = pred.dat,plot.it = F)
goof.out.d4$depth<- "d4"
goof.out.d4

# depth 5
root.in<- paste0(model.out,"d5/data_obs_preds/val/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
temp.file<- temp.file[0,]
for(i in 1:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- rbind(temp.file,in.dat)
}
obs.dat<- temp.file$target
pred.dat<- temp.file$prediction
goof.out.d5<- goof(observed = obs.dat, predicted = pred.dat,plot.it = F)
goof.out.d5$depth<- "d5"
goof.out.d5

# depth 6
root.in<- paste0(model.out,"d6/data_obs_preds/val/")
files<- list.files(root.in, full.names = T)
temp.file<- read.table(files[1], sep=",", header = T)
temp.file<- temp.file[0,]
for(i in 1:length(files)){
  in.dat<- read.table(files[i], sep=",", header = T)
  temp.file<- rbind(temp.file,in.dat)
}
obs.dat<- temp.file$target
pred.dat<- temp.file$prediction
goof.out.d6<- goof(observed = obs.dat, predicted = pred.dat,plot.it = F)
goof.out.d6$depth<- "d6"
goof.out.d6


# combine goof outputs and save
all.goof.VAL<- rbind(goof.out.d1,goof.out.d2,
                     goof.out.d3,goof.out.d4,
                     goof.out.d5,goof.out.d6)
write.csv(x = all.goof.VAL, file = paste0(model.out,"ranger_VAL_diogs_pH_summary_alldepths.csv") ,row.names = F)


