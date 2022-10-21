## Workflow for process lab PSA data
## TERN Landscapes work
## PROCESS 2: 
# Combine the data sets from process 1 and look for any doubled up cores and remove the offending ones

# data sets
files<- list.files(path = "Z:/projects/ternlandscapes_2019/soiltexture/data/process1/",
                   pattern = "_lab_PSA_data.rds", full.names = T, recursive = F)
files
length(files)
nms<- c("dat1", "dat2", "dat3","dat4", "dat5", "dat6","dat7", "dat8", "dat9", "dat10", "dat11")

for (i in 1:length(files)){
  assign(nms[i], readRDS(files[i]))
}

## DATA
# dat1: lawson grain
# dat2: natGeochem
# dat3: NatSoil
# dat4: NSW Gov
# dat5: NT Gov
# dat6: QLD Gov
# dat7: SA Gov
# dat8: SCARP
# dat9: Tas Gov
# dat10: Vic Gov
# dat11: WA Gov

# fiddle around with the data
dat2$Observation_ID<- as.factor(dat2$Observation_ID)



# combine
comb.dat<- rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9, dat10, dat11)


# unique observation IDs
uniqs<- unique(comb.dat$Observation_ID)
length(uniqs)     

top.dat<- comb.dat[comb.dat$UpperDepth == 0,]
names(top.dat)

# replicated sites
reps<- which(duplicated(top.dat[,7:8]))
reps

# remove the replicated sites
ntop.dat<- top.dat[-reps,]
which(is.na(ntop.dat$DataStore))
ntop.dat<- ntop.dat[-7646,]




# Go through each ntop.dat line (site) and pull the data from comb.dat to store in new data frame
ncomb.dat<- comb.dat[0,]

cnt<- 1

#for (i in 1:159){
for (i in 1:nrow(ntop.dat)){
  # observation
  lab<- ntop.dat[i, "Observation_ID"]
  
  
  # get observation from comb.dat
  temp.dat<- comb.dat[comb.dat$Observation_ID == lab,]
  temp.dat$FID<- paste0(temp.dat$Dataset, "_", temp.dat$Provider, "_", temp.dat$Observation_ID)
  temp.dat<- temp.dat[!temp.dat$FID == "NA_NA_NA",]
  
  ulab<- unique(temp.dat$FID)
  ulab
  if(length(ulab) > 1){
    stemp.dat<- split(temp.dat,temp.dat$FID)[[1]] # take the first entry
    ncomb.dat[cnt:(cnt + nrow(stemp.dat)-1),]<- stemp.dat[,1:14]
    cnt<- cnt + nrow(stemp.dat)} else {
      ncomb.dat[cnt:(cnt + nrow(temp.dat)-1),]<- temp.dat[,1:14]
      cnt<- cnt + nrow(temp.dat)}
  print(c(i,cnt))
}

# save output
saveRDS(ncomb.dat, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/process2/combined_lab_PSA_data.rds")


