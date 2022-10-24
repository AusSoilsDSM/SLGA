### TERN LANDSCAPES 
# Soil pH 4B1
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 26.2.20
# modified: 26.02.20

# CODE PURPOSE
# Data Mongering: TERN soil pH
## labMeasurements
## Step 2: spline fitting
##

# root directory
root<- "Z:/projects/ternlandscapes_2019/soil_pH/data/curated_lab/4B2/"


# files
lab.dat<- readRDS(paste0(root, "curated_step1_labpH_4B2.rds"))


## some basic data curation
lab.dat$Value<- as.numeric(lab.dat$Value)
#remove missing data for values
if(length(which(is.na(lab.dat$Value))) != 0){
  lab.dat<- lab.dat[-which(is.na(lab.dat$Value)),]}
# check for unrealistic values
if(length(which(lab.dat$Value >=12)) != 0){
  lab.dat<- lab.dat[-which(lab.dat$Value >=12),]}
if(length(which(lab.dat$Value <=2)) != 0){
  lab.dat<- lab.dat[-which(lab.dat$Value <=2),]}

# fix the depths
lab.dat[,"UpperDepth"]<-as.numeric(lab.dat$UpperDepth) *100
lab.dat[,"LowerDepth"]<-as.numeric(lab.dat$LowerDepth) *100
lab.dat<- lab.dat[-which(is.na(lab.dat$UpperDepth)),]
lab.dat<- lab.dat[-which(is.na(lab.dat$LowerDepth)),]

# remove missing data for depths
if(length(which(is.na(lab.dat$UpperDepth))) != 0){
  lab.dat<- lab.dat[-which(is.na(lab.dat$UpperDepth)),]}
if(length(which(is.na(lab.dat$LowerDepth))) != 0){
  lab.dat<- lab.dat[-which(is.na(lab.dat$LowerDepth)),]}

# spline nitty gritty
min(lab.dat$Value);max(lab.dat$Value)

#Spline parameters
lam<-  0.01
d<- c(0,5,15,30,60,100,200)
vlow <- 2
vhigh<- 12
show.progress<- TRUE

names(lab.dat)
d.dat<- as.data.frame(lab.dat[,c(16,10,11,14)])
number.profiles<- length(unique(d.dat[,1]))
number.profiles
profile.names<- unique(d.dat[,1])
d.dat$removals<- NA

mxd<- max(d)
mxd

# Matrix in which the averaged values of the spline are fitted. The depths are specified in the (d) object
yave<- matrix(NA,ncol=length(d)+1,nrow = number.profiles)


#combined data frame for observations and spline predictions
dave<- d.dat[1,1:4]
dave$predicted<- 0
dave$FID<- 0
dave$rmse<-0
dave$rmseiqr<- 0
dave<- dave[0,]
dave  


## Fit splines profile by profile:            
cnt<- 1
#for(st in 1:1000) {
for(st in 1:number.profiles) {
  
  
  subs<-d.dat[d.dat[,1] == st,1:4]  # subset the profile required
  subs<- subs[order(subs$UpperDepth),]
  subs
  
  
  ## Checks
  
  # there is no data?
  if(nrow(subs) == 0){
    next}
  
  # If upper and lower depths are the same
  if(sum(subs[,2]==subs[,3]) != 0){
    rm<- which(subs[,2]==subs[,3])
    subs$LowerDepth[rm]<- subs$LowerDepth[rm] + 1
    if (length(which(rm==1)) == 1){
      rm1m<- rm[-1]
      subs$UpperDepth[rm1m]<- subs$UpperDepth[rm1m] - 1}
  }
  
  # if first upper depth obs is not 0 but less or equal to 5cm adjust to 0
  if(subs$UpperDepth[1]<= 5){subs$UpperDepth[1]<- 0}
  
  
  # top depth does not equal zero (catches the other profiles)
  if (subs[1,2] != 0){
    d.dat[d.dat[,1] == st, "removals"]<- -9999
    next}
  
  # if there are two values of 0 chop off the fist value
  if (nrow(subs) >= 2){
    if (sum(as.numeric(subs[1:2,2] == 0)) == 2){
      subs<- subs[-1,]}}
  
  # duplicated depths
  ld1<- sum(as.numeric(duplicated(subs$UpperDepth)))
  if(ld1 != 0){
    subs<- subs[-which(duplicated(subs$UpperDepth)),]}
  
  # make sure the upper or lower depths are unique
  if(length(unique(subs[,2])) != nrow(subs)){
    d.dat[d.dat[,1] == st, "removals"]<- -9999
    next}
  
  # overlapping layers
  if (nrow(subs) > 1){
    ctab<- c()
    ctab[1]<- 1
    for (zz in 2:nrow(subs)){
      ctab[zz]<- as.numeric(subs$UpperDepth[zz] >= subs$LowerDepth[zz-1])}
    esso<- length(which(ctab==0))
    if(esso != 0){
      rmz<- which(ctab==0)
      subs<- subs[-rmz,]}}
  
  
  # If maximum depth is >7m
  #if (max(subs[,3]) > 700){
  #  d.dat[d.dat$myIdent == st, "removals"]<- -9999
  #  next}
  
  
  
  # Passed checks
  subs<-as.matrix(subs)
  #meth.sel<-d.dat[d.dat[,1] == st,5][1]
  #mat_id[st]<- subs[1,1]
  
  # manipulate the profile data to the required form
  ir<- c(1:nrow(subs))
  ir<-as.matrix(t(ir))
  u<- as.numeric(subs[ir,2])
  u<-as.matrix(t(u))   # upper 
  v<- as.numeric(subs[ir,3])
  v<-as.matrix(t(v))   # lower
  y<- as.numeric(subs[ir,4])
  y<-as.matrix(t(y))   # concentration 
  n<- length(y);       # number of observations in the profile
  d<- t(d)
  
  
  ############################################################################################################################################################
  ### routine for handling profiles with one observation
  if (n == 1){
    dave[cnt:(cnt-1+nrow(subs)),1:4]<- subs
    dave[cnt:(cnt-1+nrow(subs)),5]<- y
    dave[cnt:(cnt-1+nrow(subs)),6]<- st
    dave[cnt:(cnt-1+nrow(subs)),7]<- 0
    dave[cnt:(cnt-1+nrow(subs)),8]<- 0
    xfit<- as.matrix(t(seq(1,mxd,1))) # spline will be interpolated onto these depths (1cm res)
    nj<- max(v)
    if (nj > mxd){
      nj<- mxd}
    yfit<- xfit
    yfit[,1:nj]<- y   # values extrapolated onto yfit
    if (nj < mxd){
      yfit[,(nj+1):mxd]=-9999}
    #m_fyfit[st,]<- yfit
    
    # Averages of the spline at specified depths
    nd<- length(d) -1  # number of depth intervals
    dl<-d+1     #  increase d by 1
    yave[st,(length(d)+1)]<- st
    for (cj in 1:nd) { 
      xd1<- dl[cj]
      xd2<- dl[cj+1]-1
      if (nj>=xd1 & nj<=xd2){
        xd2<- nj-1
        yave[st,cj]<- mean(yfit[,xd1:xd2])} else {
          yave[st,cj]<- mean(yfit[,xd1:xd2])}   # average of the spline at the specified depth intervals
      yave[st,cj+1]<- max(v) #maximum soil depth
      #meth[st]<- meth.sel
      } 
    cnt<- cnt+nrow(subs)} else {
      
      # End of single observation profile routine
      ###############################################################################################################################################################
      
      ## Start of routine for fitting spline to profiles with multiple observations         
      
      
      ###############################################################################################################################################################
      dave[cnt:(cnt-1+nrow(subs)),1:4]<- subs
      dave[cnt:(cnt-1+nrow(subs)),6]<- st
      ## ESTIMATION OF SPLINE PARAMETERS
      np1 <- n+1  # number of interval boundaries
      nm1 <- n-1
      delta <- v-u  # depths of each layer
      del <- c(u[2:n],u[n])-v   # del is (u1-v0,u2-v1, ...)
      
      ## create the (n-1)x(n-1) matrix r; first create r with 1's on the diagonal and upper diagonal, and 0's elsewhere
      r <- matrix(0,ncol=nm1,nrow=nm1)
      
      for(dig in 1:nm1){
        r[dig,dig]<-1}
      
      for(udig in 1:nm1-1){
        r[udig,udig+1]<-1}
      
      ## then create a diagonal matrix d2 of differences to premultiply the current r
      d2 <- matrix(0, ncol=nm1, nrow=nm1)
      diag(d2) <- delta[2:n]  # delta = depth of each layer
      
      ## then premultiply and add the transpose; this gives half of r
      r <- d2 %*% r
      r <- r + t(r)
      
      ## then create a new diagonal matrix for differences to add to the diagonal
      d1 <- matrix(0, ncol=nm1, nrow=nm1)
      diag(d1) <- delta[1:nm1]  # delta = depth of each layer
      
      d3 <- matrix(0, ncol=nm1, nrow=nm1)
      diag(d3) <- del[1:nm1]  # del =  differences
      
      r <- r+2*d1 + 6*d3
      
      ## create the (n-1)xn matrix q
      q <- matrix(0,ncol=n,nrow=n)
      
      for (dig in 1:n){
        q[dig,dig]<- -1}
      
      for (udig in 1:n-1){
        q[udig,udig+1]<-1}
      
      q <- q[1:nm1,1:n]
      dim.mat <- matrix(q[],ncol=length(1:n),nrow=length(1:nm1))
      
      ## inverse of r
      rinv <- try(solve(r), TRUE)
      
      ## Note: in same cases this will fail due to singular matrix problems, hence you need to check if the object is meaningfull:
      if(is.matrix(rinv)){
        ## identity matrix i
        ind <- diag(n)
        
        ## create the matrix coefficent z
        pr.mat <- matrix(0,ncol=length(1:nm1),nrow=length(1:n))
        pr.mat[] <- 6*n*lam
        fdub <- pr.mat*t(dim.mat)%*%rinv
        z <- fdub%*%dim.mat+ind
        
        ## solve for the fitted layer means
        sbar <- solve(z,t(y))
        dave[cnt:(cnt-1+nrow(subs)),5]<- sbar
        #cnt<- cnt+nrow(subs)
        
        
        ## calculate the fitted value at the knots
        b <- 6*rinv%*%dim.mat%*% sbar
        b0 <- rbind(0,b) # add a row to top = 0
        b1 <- rbind(b,0) # add a row to bottom = 0
        gamma <- (b1-b0) / t(2*delta)
        alfa <- sbar-b0 * t(delta) / 2-gamma * t(delta)^2/3
        
        ## END ESTIMATION OF SPLINE PARAMETERS
        ###############################################################################################################################################################
        
        
        ## fit the spline 
        xfit<- as.matrix(t(seq(1,mxd,1))) ## spline will be interpolated onto these depths (1cm res)
        nj<- max(v)
        if (nj > mxd){
          nj<- mxd}
        yfit<- xfit
        for (k in 1:nj){
          xd<-xfit[k]
          if (xd< u[1]){
            p<- alfa[1]} else {
              for (its in 1:n) {
                if(its < n){
                  tf2=as.numeric(xd>v[its] & xd<u[its+1])} else {
                    tf2<-0}
                if (xd>=u[its] & xd<=v[its]){
                  p=alfa[its]+b0[its]*(xd-u[its])+gamma[its]*(xd-u[its])^2} else if (tf2){
                    phi=alfa[its+1]-b1[its]*(u[its+1]-v[its])
                    p=phi+b1[its]*(xd-v[its])}}}
          yfit[k]=p }
        if (nj < mxd){
          yfit[,(nj+1):mxd]=-9999}
        
        yfit[which(yfit > vhigh)] <- vhigh
        
        # handle 9999s
        nines<- which(yfit == -9999)
        yfit[which(yfit < vlow)]  <-vlow
        yfit[nines]<- -9999
        #m_fyfit[st,]<- yfit
        
        ## Averages of the spline at specified depths
        nd<- length(d)-1  # number of depth intervals
        dl<-d+1     #  increase d by 1
        yave[st,(length(d)+1)]<- st
        for (cj in 1:nd) { 
          xd1<- dl[cj]
          xd2<- dl[cj+1]-1
          if (nj>=xd1 & nj<=xd2){
            xd2<- nj-1
            yave[st,cj]<- mean(yfit[,xd1:xd2])} else {
              yave[st,cj]<- mean(yfit[,xd1:xd2])}   # average of the spline at the specified depth intervals
          yave[st,cj+1]<- max(v) #maximum soil depth 
          #meth[st]<- meth.sel
          } 
        
        ## CALCULATION OF THE ERROR BETWEEN OBSERVED AND FITTED VALUES
        rmse <- sqrt(sum((t(y)-sbar)^2)/n)
        rmseiqr <- rmse/IQR(y)
        dave[cnt:(cnt-1+nrow(subs)),7]<- rmse
        dave[cnt:(cnt-1+nrow(subs)),8]<- rmseiqr
        cnt<- cnt+nrow(subs)}
      
    }
  print(st)
}



## asthetics for output 
## yave
dodgones<- which(!complete.cases(yave))

yave<- as.data.frame(yave)
yave<- yave[complete.cases(yave),]


jmat<- matrix(NA,ncol=1,nrow=length(d))
for (i in 1:length(d)-1) {
  a1<-paste(d[i],d[i+1],sep="-")
  a1<-paste(a1,"cm",sep=" ")
  jmat[i]<- a1}
jmat[length(d)]<- "soil depth"
for (jj in 1:length(jmat)){
  names(yave)[jj]<- jmat[jj] 
}

names(yave)[ncol(yave)]<- "myID"
names(yave)


# Export data 
# Values for the specified depths
write.csv(yave, paste0(root,"splined_lab_4B2.csv"),row.names = F)
saveRDS(yave,paste0(root,"splined_lab_4B2.rds"))

# Error checking between obs and spline predictions
write.csv(dave, paste0(root,"splined_lab_4B2_errors.csv"), row.names = F)
saveRDS(dave,paste0(root,"splined_lab_4B2_errors.rds"))

# Original dataset but flagged for data to remove
write.csv(d.dat, paste0(root,"splined_lab_4B2_origdat.csv"),row.names = F)
saveRDS(d.dat,paste0(root,"splined_lab_4B2_origdat.rds"))



# Join the spline estimates to the label data
yave$Dataset<- NA
yave$Observation_ID<- NA
yave$Longitude<- NA
yave$Latitude<- NA
yave$ObservedProperty<- NA
yave$nloc_fid<- NA
yave$SampleDate<- NA


unID<- unique(yave$myID)

for (i in 1:length(unID)){
  subs<- lab.dat[lab.dat$myLABS == unID[i],]
  subz<- subs[1,]
  # add info
  yave$Dataset[i]<- subz$Dataset
  yave$Observation_ID[i]<- subz$Location_ID
  yave$Longitude[i]<- subz$Longitude
  yave$Latitude[i]<- subz$Latitude
  yave$ObservedProperty[i]<- subz$ObservedProperty
  yave$nloc_fid[i]<- subz$uniqLoc
  yave$SampleDate[i]<- subz$SampleDate
  
}

names(yave)
yave<- yave[,c(8:15,1:7)]

# Original dataset but flagged for data to remove
write.csv(yave, paste0(root, "lab_ph_4B2_data_splined_dsm_ARD.csv"),row.names = F)
saveRDS(yave,paste0(root, "lab_ph_4B2_data_splined_dsm_ARD.rds"))


plot(yave$Longitude, yave$Latitude)
