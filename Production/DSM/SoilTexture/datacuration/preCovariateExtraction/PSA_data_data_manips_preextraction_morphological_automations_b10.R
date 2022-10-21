## TERN soil texture
library(parallel);library(doParallel);library(compositions)

### The following procedure is going to need to be done in an auotmated fashion. One simulation event at a time.

## morphological data
morph.files<- list.files(path = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/process4/morph_sims",full.names = T)
morph.files

# begin parallel cluster and register it with foreach
cpus<- 5
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)


oper1<- foreach(i=46:50, .packages = c("compositions")) %dopar% {
#for (i in 1:length(morph.files)){
  lab.dat<- readRDS(morph.files[i])
  
  lab.dat$UpperDepth<- as.numeric(lab.dat$UpperDepth)
  lab.dat$LowerDepth<- as.numeric(lab.dat$LowerDepth)
  
  lab.dat[,"UpperDepth"]<- lab.dat[,"UpperDepth"] * 100
  lab.dat[,"LowerDepth"]<- lab.dat[,"LowerDepth"] * 100
  
  which(is.na(lab.dat$UpperDepth))
  lab.dat<- lab.dat[-which(is.na(lab.dat$UpperDepth)),]
  which(is.na(lab.dat$LowerDepth))
  lab.dat<- lab.dat[-which(is.na(lab.dat$LowerDepth)),]
  
  ### compositional data
  #soil texture data
  names(lab.dat)
  
  #columns for compositions
  lab.dat$comp1_clay<- NA
  lab.dat$comp2_sand<- NA
  lab.dat$comp3_silt<- NA
  
  # cycle through each row and estimate the ILR composiiton
  for (j in 1:nrow(lab.dat)){
    it<- ilr(lab.dat[j,c(15,17,16)])
    itc<- c(it)
    itc
    ilrInv(itc)
    lab.dat[j,19:20]<- itc
    print(j)}
  
  ## internal check
  #lab.dat<- lab.dat[which(!is.na(lab.dat$comp1_clay)),]
  
  ## spline estimates: compositional clay
  
  bounds<- c(min(lab.dat$comp1_clay),max(lab.dat$comp1_clay))
  bounds
  #Spline parameters
  lam<-  0.01
  d<- c(0,5,15,30,60,100,200)
  vlow <- bounds[1]-1
  vhigh<-  bounds[2] +1
  show.progress<- TRUE
  
  names(lab.dat)
  d.dat<- as.data.frame(lab.dat[,c(1,11,12,19)])
  number.profiles<- length(unique(d.dat[,1]))
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
    
    sts<- 17892 + st
    subs<-d.dat[d.dat[,1] == sts,1:4]  # subset the profile required
    subs<- subs[order(subs$UpperDepth),]
    subs
    
    ## Checks
    if(nrow(subs) == 0){
      next}
    
    if (subs[1,2] != 0){
      d.dat[d.dat[,1] == st, "removals"]<- -9999
      next}
    
    # if there are two values of 0 chop off the fist value
    if (nrow(subs) >= 2){
      if (sum(as.numeric(subs[1:2,2] == 0)) == 2){
        subs<- subs[-1,]}}
    
    # If maximum depth is >7m
    if (max(subs[,3]) > 700){
      d.dat[d.dat$myIdent == st, "removals"]<- -9999
      next}
    
    # If maximum carbon is >10%
    #if (max(subs[,4]) > 10){
    #  d.dat[d.dat$myIdent == st, "removals"]<- -9999
    #  next}
    
    subs
    
    # Passed checks
    subs<-as.matrix(subs)
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
      dave[cnt:(cnt-1+nrow(subs)),6]<- sts
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
      yave[st,(length(d)+1)]<- sts
      for (cj in 1:nd) { 
        xd1<- dl[cj]
        xd2<- dl[cj+1]-1
        if (nj>=xd1 & nj<=xd2){
          xd2<- nj-1
          yave[st,cj]<- mean(yfit[,xd1:xd2])} else {
            yave[st,cj]<- mean(yfit[,xd1:xd2])}   # average of the spline at the specified depth intervals
        yave[st,cj+1]<- max(v)} #maximum soil depth
      cnt<- cnt+nrow(subs)} else {
        
        # End of single observation profile routine
        ###############################################################################################################################################################
        
        ## Start of routine for fitting spline to profiles with multiple observations         
        
        
        ###############################################################################################################################################################
        dave[cnt:(cnt-1+nrow(subs)),1:4]<- subs
        dave[cnt:(cnt-1+nrow(subs)),6]<- sts
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
          yave[st,(length(d)+1)]<- sts
          for (cj in 1:nd) { 
            xd1<- dl[cj]
            xd2<- dl[cj+1]-1
            if (nj>=xd1 & nj<=xd2){
              xd2<- nj-1
              yave[st,cj]<- mean(yfit[,xd1:xd2])} else {
                yave[st,cj]<- mean(yfit[,xd1:xd2])}   # average of the spline at the specified depth intervals
            yave[st,cj+1]<- max(v)} #maximum soil depth 
          
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
  yjve<- yave[!complete.cases(yave),] # dodgy ones
  yave<- as.data.frame(yave)
  yave<- yave[complete.cases(yave),]
  
  jmat<- matrix(NA,ncol=1,nrow=length(d))
  for (zz in 1:length(d)-1) {
    a1<-paste(d[zz],d[zz+1],sep="-")
    a1<-paste(a1,"cm",sep=" ")
    jmat[zz]<- a1}
  jmat[length(d)]<- "soil depth"
  for (jj in 1:length(jmat)){
    names(yave)[jj]<- jmat[jj] 
  }
  
  names(yave)[ncol(yave)]<- "myID"
  names(yave)
  
  
  # Export data 
  # Values for the specified depths
  yave.name<- paste0("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/","splined_morph_comp_clay_sim_",i, ".rds")
  saveRDS(yave,file = yave.name)
  
  # Error checking between obs and spline predictions
  dave.name<- paste0("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/","splined_morph_comp_clay_errors_sim_",i, ".rds")
  saveRDS(dave,file = dave.name)
  
  # Original dataset but flagged for data to remove
  ddat.name<- paste0("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/","splined_morph_comp_clay_origdat_sim_",i, ".rds")
  saveRDS(d.dat,file = ddat.name)
  
  
  ## spline estimates: compositional sand
  
  bounds<- c(min(lab.dat$comp2_sand),max(lab.dat$comp2_sand))
  bounds
  #Spline parameters
  lam<-  0.01
  d<- c(0,5,15,30,60,100,200)
  vlow <- bounds[1]-1
  vhigh<-  bounds[2] + 1
  show.progress<- TRUE
  
  names(lab.dat)
  d.dat<- as.data.frame(lab.dat[,c(1,11,12,20)])
  number.profiles<- length(unique(d.dat[,1]))
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
    
    sts<- 17892 + st
    subs<-d.dat[d.dat[,1] == sts,1:4]  # subset the profile required
    subs<- subs[order(subs$UpperDepth),]
    subs
    
    ## Checks
    if(nrow(subs) == 0){
      next}
    
    if (subs[1,2] != 0){
      d.dat[d.dat[,1] == st, "removals"]<- -9999
      next}
    
    # if there are two values of 0 chop off the fist value
    if (nrow(subs) >= 2){
      if (sum(as.numeric(subs[1:2,2] == 0)) == 2){
        subs<- subs[-1,]}}
    
    # If maximum depth is >7m
    if (max(subs[,3]) > 700){
      d.dat[d.dat$myIdent == st, "removals"]<- -9999
      next}
    
    # If maximum carbon is >10%
    #if (max(subs[,4]) > 10){
    #  d.dat[d.dat$myIdent == st, "removals"]<- -9999
    #  next}
    
    subs
    
    # Passed checks
    subs<-as.matrix(subs)
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
      dave[cnt:(cnt-1+nrow(subs)),6]<- sts
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
      yave[st,(length(d)+1)]<- sts
      for (cj in 1:nd) { 
        xd1<- dl[cj]
        xd2<- dl[cj+1]-1
        if (nj>=xd1 & nj<=xd2){
          xd2<- nj-1
          yave[st,cj]<- mean(yfit[,xd1:xd2])} else {
            yave[st,cj]<- mean(yfit[,xd1:xd2])}   # average of the spline at the specified depth intervals
        yave[st,cj+1]<- max(v)} #maximum soil depth
      cnt<- cnt+nrow(subs)} else {
        
        # End of single observation profile routine
        ###############################################################################################################################################################
        
        ## Start of routine for fitting spline to profiles with multiple observations         
        
        
        ###############################################################################################################################################################
        dave[cnt:(cnt-1+nrow(subs)),1:4]<- subs
        dave[cnt:(cnt-1+nrow(subs)),6]<- sts
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
          yave[st,(length(d)+1)]<- sts
          for (cj in 1:nd) { 
            xd1<- dl[cj]
            xd2<- dl[cj+1]-1
            if (nj>=xd1 & nj<=xd2){
              xd2<- nj-1
              yave[st,cj]<- mean(yfit[,xd1:xd2])} else {
                yave[st,cj]<- mean(yfit[,xd1:xd2])}   # average of the spline at the specified depth intervals
            yave[st,cj+1]<- max(v)} #maximum soil depth 
          
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
  yjve<- yave[!complete.cases(yave),] # dodgy ones
  yave<- as.data.frame(yave)
  yave<- yave[complete.cases(yave),]
  
  jmat<- matrix(NA,ncol=1,nrow=length(d))
  for (zz in 1:length(d)-1) {
    a1<-paste(d[zz],d[zz+1],sep="-")
    a1<-paste(a1,"cm",sep=" ")
    jmat[zz]<- a1}
  jmat[length(d)]<- "soil depth"
  for (jj in 1:length(jmat)){
    names(yave)[jj]<- jmat[jj] 
  }
  
  names(yave)[ncol(yave)]<- "myID"
  names(yave)
  
  
  # Export data 
  # Values for the specified depths
  yave.name<- paste0("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/","splined_morph_comp_sand_sim_",i, ".rds")
  saveRDS(yave,file = yave.name)
  
  # Error checking between obs and spline predictions
  dave.name<- paste0("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/","splined_morph_comp_sand_errors_sim_",i, ".rds")
  saveRDS(dave,file = dave.name)
  
  # Original dataset but flagged for data to remove
  ddat.name<- paste0("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/","splined_morph_comp_sand_origdat_sim_",i, ".rds")
  saveRDS(d.dat,file = ddat.name)
  
  
  
  
  
  ###########
  # Join the spline estimates to the label data
  lab.dat$Observation_ID<- as.character(lab.dat$Observation_ID)
  unique.ids<- unique(lab.dat$Observation_ID)
  clay.dat<- readRDS(file = paste0("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/","splined_morph_comp_clay_sim_",i, ".rds"))
  sand.dat<- readRDS(file = paste0("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/","splined_morph_comp_sand_sim_",i, ".rds"))
  names(lab.dat)
  lab.dat<- lab.dat[,c(1,2,6,9:12)]
  lab.dat<- lab.dat[which(lab.dat$num_horiz_ID == 1),]
  
  #clay 
  names(clay.dat)<- c("clay_0-5 cm","clay_5-15 cm","clay_15-30 cm","clay_30-60 cm","clay_60-100 cm","clay_100-200 cm","clay_soil depth","clay_myID")
  
  lab.dat<- lab.dat[match(clay.dat$clay_myID, lab.dat$num_obs_ID),]
  
  lab.dat<- cbind(lab.dat,clay.dat)
  which(lab.dat$num_obs_ID != lab.dat$clay_myID)
  
  #sand
  names(sand.dat)<- c("sand_0-5 cm","sand_5-15 cm","sand_15-30 cm","sand_30-60 cm","sand_60-100 cm","sand_100-200 cm","sand_soil depth","sand_myID")
  
  lab.dat<- lab.dat[match(sand.dat$sand_myID, lab.dat$num_obs_ID),]
  
  lab.dat<- cbind(lab.dat,sand.dat)
  which(lab.dat$num_obs_ID != lab.dat$sand_myID)
  
  
  # Original dataset but flagged for data to remove
  
  saveRDS(lab.dat,file = paste0("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/data/","morph_psa_data_splined_dsm_extract_prepared_sim_",i, ".rds"))
}
