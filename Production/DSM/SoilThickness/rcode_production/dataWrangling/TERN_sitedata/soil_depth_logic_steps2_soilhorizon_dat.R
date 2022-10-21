## soil observations Australia


## Logic for assessing soil depth from TERN data set


# load in site observation data from step 1 which has site obs linked with horizon data

## New dataset with locations and horizons
dat<- read.table("/home/brendo1001/mywork/dropboxShare/2018/soilDepth/data/dbo_Horzons_loc_sub.txt", header = T,sep=",")
str(dat)


# number of unique soil profiles
profs<- length(unique(dat$myFID))
profFID<- unique(dat$myFID)



##MUDDLES
# horizon descriptors
summary(dat$h_desig_master)
unique(dat$h_desig_master)
head(dat[dat$h_desig_master=="C1",])
dat[dat$myFID==1361,]

head(dat[dat$h_desig_master=="Cr",])
dat[dat$myFID==131745,]

head(dat[dat$h_desig_master=="C/B",])
dat[dat$myFID==55587,]

head(dat[dat$h_desig_master=="2C",])
dat[dat$myFID==1367,]

head(dat[dat$h_desig_master=="AC",])
dat[dat$myFID==368,]

head(dat[dat$h_desig_master=="2C1",])
dat[dat$myFID==1646,]

head(dat[dat$h_desig_master=="C3",])
dat[dat$myFID==9490,]

head(dat[dat$h_desig_master=="CKM",])
dat[dat$myFID==131857,]

head(dat[dat$h_desig_master=="Other",])
dat[dat$myFID==58849,]


# empty data frame to put outputs
sdat<- dat[1,]
colsd<- ncol(sdat)
sdat$HD<- NA # indicator of whether profile has horizon descriptors
sdat$censored<- NA #inidcator of whther data is censored
sdat$maxD<- NA # the observed maximum soil depth
sdat$horizDesc<- NA
sdat<- sdat[0,]
sdat

#Possible Horizon desriptors to indicate presence of lithic contact
posHor<- unique(c(grep('C', unique(dat$h_desig_master), value=TRUE),
                  grep('R', unique(dat$h_desig_master), value=TRUE)))
unique(grep('BC', unique(dat$h_desig_master), value=TRUE, fixed = T))

posHor



for (i in 1:profs){
  sel<- profFID[i] # soil profile number
  
  
  # extract the soil profile
  selDat<- dat[dat$myFID==sel,]
  
  # Put some data into the the output frame
  sdat[i,1:colsd]<- selDat[1,] # first row of the selected data into the output frame
  
  
  
  # TEST 1: Does the profile have horizon descriptors
  test1<- as.numeric(selDat$h_desig_master) # gives a 1 for each entry where there is no information
  if(sum(test1)/length(test1) == 1){
    sdat$HD[i]<- 0
    sdat$maxD[i]<- max(selDat$h_lower_depth)
    sdat$censored[i]<- 1} else {
      sdat$HD[i]<- 1
      # check to see if there is an AC horizon of any description
      test2<- as.character(selDat$h_desig_master)
      test2_2<- which(test2 %in% posHor) # horizons that meet criteria
      if (length(test2_2)==0){  # horizon descriptor not a lithic one
        sdat$censored[i]<- 1 #censored
        if(max(selDat$h_lower_depth, na.rm = T) == -Inf){sdat$maxD[i]<- -99999
        sdat$horizDesc[i]<- -99999}else{
        sdat$maxD[i]<- max(selDat$h_lower_depth, na.rm = T) #max depth
        xx1<- which(selDat$h_lower_depth== max(selDat$h_lower_depth, na.rm = T))
        sdat$horizDesc[i]<- as.character(selDat$h_desig_master[xx1])}} else {
          sdat$censored[i]<- 0 # not censored
          sdat$horizDesc[i]<- as.character(selDat$h_desig_master[test2_2[1]]) #horizon descriptor
          sdat$maxD[i]<- min(selDat$h_upper_depth[test2_2[1]],selDat$h_lower_depth[test2_2[1]],
                             selDat$h_lower_depth[test2_2[1]-1], na.rm = T) #max depth 
        }}}
          
    names(sdat)
    sdat<- sdat[,c(1,3:8,15:16,18:21)]
    write.table(sdat, file = "/home/brendo1001/mywork/dropboxShare/2018/soilDepth/data/dbo_Horzons_loc_sub_step2_Out.txt",sep = ",",row.names = F, col.names = T)
          
          
          
      