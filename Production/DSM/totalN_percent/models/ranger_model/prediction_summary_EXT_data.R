### TERN LANDSCAPES 
# Total N
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 9.11.23
# modified: 9.11.23

# CODE PURPOSE
# Get a prediction summary for the external data from the model iterations

# root directory
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/total_N/"
data.root<- paste0(g.root, "data/ross/model_ready/")
model.out<- paste0(g.root, "models/ranger_models/")
funcs.out<- paste0(g.root, "rcode/miscell/")

# list files of model preds on external data
ext.preds<- list.files(path = paste0(model.out,"data_obs_preds/ext/"), pattern = ".txt", full.names = T)

# read in a file to use for a template
temp.file<- read.table(file = ext.preds[1],header = T, sep=",")

for (i in 2:length(ext.preds)){
  # read in file
  read.in<- read.table(file = ext.preds[i],header = T, sep=",")
  # bind prediciton column
  temp.file<- cbind(temp.file, read.in$prediction)}

# calulate prediciton median
names(temp.file)
pred.mat_ext_rm<- rowMeans(temp.file[,7:106],na.rm = T)

# save result
out.dat<- cbind(temp.file[,1:6],pred.mat_ext_rm)
names(out.dat)[7]<- "pred_avg"  
write.csv(x = out.dat, file = paste0(model.out,"data_obs_preds/ext/ranger_EXT_preds_totalN_depth_",depth,"_summary.csv"), row.names = F)




