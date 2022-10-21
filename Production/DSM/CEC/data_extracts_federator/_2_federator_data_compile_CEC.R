### TERN Landscapes 
# Cation Exchange Capacity
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 12.11.21
# modified: 12.11.21

# CODE PURPOSE
# Search for replicated soil profiles and remove from available database
# replications are where there are more than one profile at a location. This doubling up is the result of same 
# data being found across different databases.
##

# root
g.root<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_CEC/"
in.root<- paste0(g.root,"data/data_curation/")
data.out<- paste0(g.root,"data/data_curation/")


avail.data<- readRDS(paste0(in.root,"_1_tern_cec_allmethods_combined_raw.rds"))
summary(as.factor(avail.data$Dataset))
avail.data$myFID<- NA

# unique observation IDs
uniqs<- unique(avail.data$uloc_id)
length(uniqs)     

top.dat<- avail.data[avail.data$UpperDepth == 0,]
names(top.dat)

# replicated sites [spatially]
reps<- which(duplicated(top.dat$uloc_id))
reps

# remove the replicated sites
ntop.dat<- top.dat[-reps,]
which(is.na(ntop.dat$DataStore))
ntop.dat<- ntop.dat[-84,]

plot(ntop.dat$Longitude,ntop.dat$Latitude)


# Go through each ntop.dat line (site) and pull the data from comb.dat to store in new data frame
ncomb.dat<- avail.data[0,]

cnt<- 1

names(avail.data)
for (i in 1:nrow(ntop.dat)){
  # observation
  lab<- ntop.dat[i, "uloc_id"]
  
  
  # get observation from comb.dat
  temp.dat<- avail.data[avail.data$uloc_id == lab,]
  temp.dat$FID<- paste0(temp.dat$Dataset, "_", temp.dat$Provider, "_", temp.dat$Location_ID)
  temp.dat<- temp.dat[!temp.dat$FID == "NA_NA_NA",]
  
  ulab<- unique(temp.dat$FID)
  ulab
  if(length(ulab) > 1){
    stemp.dat<- split(temp.dat,temp.dat$FID)[[1]] # take the first entry
    stemp.dat<- stemp.dat[order(stemp.dat$UpperDepth),]
    stemp.dat$myFID<- i
    ncomb.dat[cnt:(cnt + nrow(stemp.dat)-1),]<- stemp.dat[,1:22]
    cnt<- cnt + nrow(stemp.dat)} else {
      temp.dat<- temp.dat[order(temp.dat$UpperDepth),]
      temp.dat$myFID<- i
      ncomb.dat[cnt:(cnt + nrow(temp.dat)-1),]<- temp.dat[,1:22]
      cnt<- cnt + nrow(temp.dat)}
  print(c(i,cnt))
}


summary(as.factor(ncomb.dat$Dataset))
# save output
saveRDS(ncomb.dat, file = paste0(data.out,"_2_tern_cec_allmethods_combined_replicatesremoved.rds"))



  
 
