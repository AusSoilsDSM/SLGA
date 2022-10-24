## Data Mongering: TERN soil pH
## Lab data
## Step 2: combine data by methods and datasource. Then look for replicates

root<- "Z:/projects/ternlandscapes_2019/soil_pH/data/"
root.out<- "Z:/projects/ternlandscapes_2019/soil_pH/data/curated_step2/"

#DATA
lab.methods<- read.csv(file = paste0(root, "available_lab_methods.csv"))
sel.methods<- lab.methods[c(9,11:17,19:20),]
sel.methods


files<- list.files(path = paste0(root, "curated_step1"), pattern = "LAB", full.names = T)
files
files.short<- list.files(path = paste0(root, "curated_step1"), pattern = "LAB", full.names = F)
files.short

# empty 
empty_list <- vector(mode = "list", length = length(files))

for (i in 1:length(files)){
  dat<- readRDS(files[i])
  empty_list[[i]]<- dat
}


# Create a set of data.frames to collect the different methods
meth1<- c("4_NR")
meth2<- c("4A_C_2.5")
meth3<- c("4B_C_2.5")
meth4<- c("4B1", "4B2")
meth5<- c("4C1", "4C2")
meth6<- c("4A1", "4A1_MQ01", "4A1_MQ02")
meths<- list(meth1,meth2,meth3,meth4,meth5, meth6)

meth1_dat<- empty_list[[1]][0,]
meth2_dat<- empty_list[[1]][0,]
meth3_dat<- empty_list[[1]][0,]
meth4_dat<- empty_list[[1]][0,]
meth5_dat<- empty_list[[1]][0,]
meth6_dat<- empty_list[[1]][0,]
meth_dat<- list(meth1_dat,meth2_dat,meth3_dat,meth4_dat,meth5_dat,meth6_dat)

#### Combine data sources by each method

for (i in 1:6){

  # select the method
  sel.meth<- meths[[i]]
  
  # go through each file and extract the necessary data and compile into new list
  for (j in 1:length(empty_list)){
    dat<- empty_list[[j]]
    
    # select the data according to method
    sub.dat<- dat[dat$ObservedProperty %in% sel.meth,] 
    if (nrow(sub.dat) == 0){next}
    
    # place into new data
    meth_dat[[i]]<- rbind(meth_dat[[i]], sub.dat)}}


### Go through each method and look for replicated sites. Remove replicates 
temp.dat<- meth_dat[[1]]
names(temp.dat)
temp.dat$nloc_fid<- NA
rmeth1_dat<- temp.dat[0,]
rmeth2_dat<- temp.dat[0,]
rmeth3_dat<- temp.dat[0,]
rmeth4_dat<- temp.dat[0,]
rmeth5_dat<- temp.dat[0,]
rmeth6_dat<- temp.dat[0,]
rmeth_dat<- list(rmeth1_dat,rmeth2_dat,rmeth3_dat,rmeth4_dat,rmeth5_dat,rmeth6_dat)

for (i in 1:length(meth_dat)){
  dat<- meth_dat[[i]]
  
  # unique number of locations
  uniq.locs<- unique(dat$uniqLOC)
  length(uniq.locs)
  for (j in 1:length(uniq.locs)){
    sub.dat<- dat[dat$uniqLOC == uniq.locs[j],]
    # split to handle replicates
    spl.sub.dat<- split(sub.dat,sub.dat$Dataset)
    sel.dat<- spl.sub.dat[[1]]
    sel.dat$nloc_fid<- j
    sel.dat<- sel.dat[order(sel.dat$UpperDepth),]
    # add to new frame
    rmeth_dat[[i]]<- rbind(rmeth_dat[[i]],sel.dat)
    print(j)
  }
}


### export data out into files
for (i in 1:length(rmeth_dat)){
  dat<- rmeth_dat[[i]]
  nm1<- paste0(root.out,"meth_",i,"_soil_LAB_pH_data_2020-07-15_curated_LAB_step2.rds")
  saveRDS(object = dat, file = nm1)}

## END
  



