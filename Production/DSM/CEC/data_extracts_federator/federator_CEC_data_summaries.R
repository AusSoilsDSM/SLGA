### TERN Landscapes 
# Cation Exchange Capacity
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.10.21
# modified: 24.10.21

# CODE PURPOSE
# Data summaries of CEC and individual cations for each data source
##

library(stringr)

cations.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/lab_method_codes/CATIONS"
cec.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/lab_method_codes/"
file.outs<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/data/lab_method_codes/summaries/"


cations.short<- list.dirs(cations.root,recursive = F, full.names = F)
cations.long<- list.dirs(cations.root,recursive = F, full.names = T)
cations.short

cec.long<- list.dirs(cec.root,recursive = F, full.names = T)[c(1:17)]
cec.long
cec.short<- list.dirs(cec.root,recursive = F, full.names = F)[c(1:17)]
cec.short



## go through the individual cation folder first

for (i in 1:length(cations.short)){
  sel.fol<- cations.long[i]
  ind.fol.long<- list.dirs(sel.fol, recursive = F, full.names = T)
  ind.fol.short<- list.dirs(sel.fol, recursive = F, full.names = F)
  ind.files<- list.files(path = ind.fol.long, pattern = "summary", full.names = T)
  
  # get a template together
  template.dat<- read.csv(file = ind.files[1])
  fs_split<- strsplit(template.dat[,], ":")
  datasetz<- data.frame(dataset= sapply(fs_split, `[`, 1))
  
  temp.mat<- matrix(NA, nrow= nrow(datasetz), ncol= length(ind.files))
  for (j in 1:length(ind.files)){
    temp.dat<- read.csv(file= ind.files[j])
    fj_split<- strsplit(temp.dat[,], ":")
    nums<- as.numeric(sapply(fj_split, `[`, 2))
    temp.mat[,j]<- nums}
  temp.mat<- as.data.frame(temp.mat)
  names(temp.mat)<- ind.fol.short
  datasetz<- cbind(datasetz,temp.mat)
  
  # check if there is CEC data for the method
  
  lg<- length(which(str_detect(cec.short,cations.short[i])))
  print(lg)
  
  if (lg >= 1){
    
    cec.fol<- cec.long[which(str_detect(cec.short,cations.short[i]))]
    cec.file<- list.files(path = cec.fol, pattern = "summary", full.names = T)
    for (x in 1:length(cec.file)){
      cec.dat<- read.csv(file = cec.file[x])
      fr_split<- strsplit(cec.dat[,], ":")
      num.cec<- as.numeric(sapply(fr_split, `[`, 2))
      datasetz$cec<- num.cec
      names(datasetz)[ncol(datasetz)] <- cec.short[which(str_detect(cec.short,cations.short[i]))][x]}
    
    # write file to folder
    sel.sh<- cations.short[i]
    write.csv(x = datasetz, file = paste0(file.outs,sel.sh, "_dataset_summaries.csv"))
    # remove folder from selection
    rms<- which(str_detect(cec.short,cations.short[i]))
    cec.short<- cec.short[-rms]
    cec.long<- cec.long[-rms]
    } else {
      print("!!!!!!!")
      sel.sh<- cations.short[i]
      write.csv(x = datasetz, file = paste0(file.outs,sel.sh, "_dataset_summaries.csv"))}
      
} 


# work with where there is only cec data
cec.short
cec.long  


## go through the individual cation folder first

for (i in 1:length(cec.short)){
  sel.fol<- cec.long[i]
  ind.files<- list.files(path = sel.fol, pattern = "summary", full.names = T)
  
  # get a template together
  template.dat<- read.csv(file = ind.files[1])
  fs_split<- strsplit(template.dat[,], ":")
  datasetz<- data.frame(dataset= sapply(fs_split, `[`, 1), count = as.numeric(sapply(fs_split, `[`, 2)) )
  names(datasetz)[2]<- cec.short[i]
  # write file to folder
  sel.sh<- cec.short[i]
  write.csv(x = datasetz, file = paste0(file.outs,sel.sh, "_dataset_summaries.csv"))}
    