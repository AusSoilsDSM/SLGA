### TERN Landscapes 
# Cation Exchange Capacity
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 12.11.21
# modified: 12.11.21

# CODE PURPOSE
# Compile all CEC data from all methods into a single file
##

# root
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/"
in.root<- paste0(g.root,"data/lab_method_codes/")
data.out<- paste0(g.root,"data/data_curation/")


# method codes
method_codes<- c("15_NR", "15_NR_CEC","15A1_CEC", "15A2_CEC","15A3_CEC", "15B1_CEC", "15B2_CEC", "15B3_CEC",  
                 "15C1_CEC", "15C2_CEC", "15C2_CEC_M", "15D1_CEC", "15D2_CEC",
                 "15E1_CEC", "15E2_CEC", "15E3_CEC" , "15K1")



# get a template file together
temp.dat<- readRDS(file = paste0(in.root,method_codes[1],"/NatSoil_soil_LAB_CEC_data_YY_2021-11-08.rds"))
temp.dat<- temp.dat[0,]

# go through each folder and file and compile data

for (i in 1:length(method_codes)){
  new.path<- paste0(in.root, method_codes[i],"/")
  lg<- length(list.files(path = new.path,pattern = ".rds",full.names = T))
  if (lg == 0){next} else {
    files<- list.files(path = new.path,pattern = ".rds",full.names = T)
    for (j  in 1:length(files)){
      read.dat<- readRDS(file = files[j])
      temp.dat<- rbind(temp.dat,read.dat)}}
    print(i)}

temp.dat$uloc_id<- paste0(temp.dat$Longitude,"_",temp.dat$Latitude)
length(unique(temp.dat$uloc_id))

plot(temp.dat$Longitude,temp.dat$Latitude)


summary(as.factor(temp.dat$Dataset))

# save output
saveRDS(object = temp.dat, file = paste0(data.out,"_1_tern_cec_allmethods_combined_raw.rds"))

