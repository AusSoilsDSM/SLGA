## Soil texture lab data analysis
## Process 1
## TERN
## read in all lab datasets and make the outputs into sigle columns of sand/silt/clay instead of listing lab methods

# data sets
files<- list.files(path = "Z:/projects/ternlandscapes_2019/soiltexture/data/",
                   pattern = "_PSA_lab_data_2019-12-11_combi.csv", full.names = T, recursive = F)
files
length(files)
nms<- c("dat1", "dat2", "dat3","dat4", "dat5", "dat6","dat7", "dat8", "dat9",
        "dat10","dat11")

for (i in 1:length(files)){
  assign(nms[i], read.csv(files[i]))
}


# dat1: Lawson Grains
# dat2: NatGeochem
# dat3: Natsoil
# dat4: NSW Gov
# dat5: NT Gov
# dat6: QLD Gov
# dat7: SA Gov
# dat8: SCARP
# dat9: Tas Gov
# dat10: Vic Gov
# dat11: WA Gov



# DAT 9 [Tas gov]
proc<- dat9[-1,]
labs<- names(proc)[12:102]
vec<- c()
for (i in 1:91){
  sel.lab<- labs[i]
  vec[i]<- nrow(proc) - sum(as.numeric(is.na(proc[,sel.lab])))
  print(sum(as.numeric(is.na(proc[,sel.lab]))))
}
cbind(vec,labs)

# Main lab method groups
#G1<- c("P10_CF_C", "P10_CF_CS","P10_CF_FS","P10_CF_S","P10_CF_Z")
G3<- c("P10A1_C","P10A1_CS","P10A1_FS","P10_HYD_S","P10A1_Z")
G1<- c("P10_NR_C","P10_NR_CS","P10_NR_FS","P10_NR_S","P10_NR_Z")
G2<- c("P10_PB_C","P10_PB_CS","P10_PB_FS","P10_PB_S","P10_PB_Z")


## GROUP 1
# new data
proc_1<- proc[,1:10]
proc_1$clay<- NA
proc_1$silt<- NA
proc_1$sand<- NA

# work on each group sperately
proc_1<- cbind(proc_1,proc[,G1])
proc_1$chvec<- NA
for (j in 1:nrow(proc_1)){
  proc_1$chvec[j]<- sum(as.numeric(is.na(proc_1[j,14:18])))}

# remove instances of all missing values along vector
proc_1<- proc_1[proc_1$chvec != 5,]
proc_1$com<- NA
proc_1$methgroup<- NA

levs<- unique(proc_1$Observation_ID)
for (i in 1:length(levs)){
  subs<- proc_1[proc_1$Observation_ID == levs[i],] 
  subs$methgroup<- "G1"
  subs<- subs[order(subs$UpperDepth),]
  
  # check for coordinates
  scord<- sum(as.numeric(is.na(subs$Longitude)))
  if(scord != 0){
    subs[, "com"]<- -9999
    proc_1[proc_1$Observation_ID == levs[i],] <- subs
    next}
  
  for (j in 1:nrow(subs)){
    vals<- subs[j, 14:19]
    sna<- sum(is.na(vals))
    
    # if only no values are missing
    if(vals$chvec == 0){
      subs[j,"clay"]<- vals$P10_NR_C
      subs[j,"silt"]<- vals$P10_NR_Z
      subs[j,"sand"]<- vals$P10_NR_S 
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
    # if only one value is missing and it is sand 
    if(vals$chvec == 1 & is.na(vals$P10_NR_S)){
      subs[j,"clay"]<- vals$P10_NR_C
      subs[j,"silt"]<- vals$P10_NR_Z
      subs[j,"sand"]<- vals$P10_NR_CS + vals$P10_NR_FS
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
    # if we have data about all three fractions [clay, sand, silt]
    if(vals$chvec == 2 & !is.na(vals$P10_NR_C) & !is.na(vals$P10_NR_S) & !is.na(vals$P10_NR_Z) ){
      subs[j,"clay"]<- vals$P10_NR_C
      subs[j,"silt"]<- vals$P10_NR_Z
      subs[j,"sand"]<- vals$P10_NR_S 
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
    # if we have data about clay and sand fraction but not silt
    if(vals$chvec == 3 & !is.na(vals$P10_NR_C) & !is.na(vals$P10_NR_S) & is.na(vals$P10_NR_Z) ){
      subs[j,"clay"]<- vals$P10_NR_C
      subs[j,"silt"]<- 100 - vals$P10_NR_C - vals$P10_NR_S
      subs[j,"sand"]<- vals$P10_NR_S  
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
    # if we have data about clay and silt only
    if(vals$chvec == 3 & !is.na(vals$P10_NR_C) & is.na(vals$P10_NR_S) & !is.na(vals$P10_NR_Z) ){
      subs[j,"clay"]<- vals$P10_NR_C
      subs[j,"silt"]<- vals$P10_NR_Z
      subs[j,"sand"]<- 100 -  vals$P10_NR_C - vals$P10_NR_Z
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
  }
  
  # ones that slipped the net
  subs[which(is.na(subs$com)), "com"]<- -9999
  
  issu<- sum(subs$com == -9999)
  if(issu > 0){
    subs$com <- -9999
    proc_1[proc_1$Observation_ID == levs[i],] <- subs} else {
      proc_1[proc_1$Observation_ID == levs[i],] <- subs}
  print(i)
}

which(proc_1$com == -9999)
proc_z<- proc_1[-which(proc_1$com == -9999),] 
names(proc_z)
proc_z<- proc_z[,c(1:13,21)]


## GROUP 2
# new data
proc_1<- proc[,1:10]
proc_1$clay<- NA
proc_1$silt<- NA
proc_1$sand<- NA

# work on each group sperately
proc_1<- cbind(proc_1,proc[,G2])
proc_1$chvec<- NA
for (j in 1:nrow(proc_1)){
  proc_1$chvec[j]<- sum(as.numeric(is.na(proc_1[j,14:18])))}

# remove instances of all missing values along vector
proc_1<- proc_1[proc_1$chvec != 5,]
proc_1$com<- NA
proc_1$methgroup<- NA

levs<- unique(proc_1$Observation_ID)
for (i in 1:length(levs)){
  subs<- proc_1[proc_1$Observation_ID == levs[i],] 
  subs$methgroup<- "G2"
  subs<- subs[order(subs$UpperDepth),]
  
  # check for coordinates
  scord<- sum(as.numeric(is.na(subs$Longitude)))
  if(scord != 0){
    subs[, "com"]<- -9999
    proc_1[proc_1$Observation_ID == levs[i],] <- subs
    next}
  
  for (j in 1:nrow(subs)){
    vals<- subs[j, 14:19]
    sna<- sum(is.na(vals))
    
    # if only no values are missing
    if(vals$chvec == 0){
      subs[j,"clay"]<- vals$P10_PB_C
      subs[j,"silt"]<- vals$P10_PB_Z
      subs[j,"sand"]<- vals$P10_PB_S 
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
    # if only one value is missing and it is sand 
    if(vals$chvec == 1 & is.na(vals$P10_PB_S)){
      subs[j,"clay"]<- vals$P10_PB_C
      subs[j,"silt"]<- vals$P10_PB_Z
      subs[j,"sand"]<- vals$P10_PB_CS + vals$P10_PB_FS
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
    # if we have data about all three fractions [clay, sand, silt]
    if(vals$chvec == 2 & !is.na(vals$P10_PB_C) & !is.na(vals$P10_PB_S) & !is.na(vals$P10_PB_Z) ){
      subs[j,"clay"]<- vals$P10_PB_C
      subs[j,"silt"]<- vals$P10_PB_Z
      subs[j,"sand"]<- vals$P10_PB_S 
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
    # if we have data about clay and sand fraction but not silt
    if(vals$chvec == 3 & !is.na(vals$P10_PB_C) & !is.na(vals$P10_PB_S) & is.na(vals$P10_PB_Z) ){
      subs[j,"clay"]<- vals$P10_PB_C
      subs[j,"silt"]<- 100 - vals$P10_PB_C - vals$P10_PB_S
      subs[j,"sand"]<- vals$P10_PB_S  
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
    # if we have data about clay and silt only
    if(vals$chvec == 3 & !is.na(vals$P10_PB_C) & is.na(vals$P10_PB_S) & !is.na(vals$P10_PB_Z) ){
      subs[j,"clay"]<- vals$P10_PB_C
      subs[j,"silt"]<- vals$P10_PB_Z
      subs[j,"sand"]<- 100 -  vals$P10_PB_C - vals$P10_PB_Z
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
  }
  
  # ones that slipped the net
  subs[which(is.na(subs$com)), "com"]<- -9999
  
  issu<- sum(subs$com == -9999)
  if(issu > 0){
    subs$com <- -9999
    proc_1[proc_1$Observation_ID == levs[i],] <- subs} else {
      proc_1[proc_1$Observation_ID == levs[i],] <- subs}
  print(i)
}

which(proc_1$com == -9999)
proc_1<- proc_1[-which(proc_1$com == -9999),] 
proc_z<- rbind(proc_z, proc_1[,c(1:13,21)])


## GROUP 3
# new data
proc_1<- proc[,1:10]
proc_1$clay<- NA
proc_1$silt<- NA
proc_1$sand<- NA

# work on each group sperately
proc_1<- cbind(proc_1,proc[,G3])
proc_1$chvec<- NA
for (j in 1:nrow(proc_1)){
  proc_1$chvec[j]<- sum(as.numeric(is.na(proc_1[j,14:18])))}

# remove instances of all missing values along vector
proc_1<- proc_1[proc_1$chvec != 5,]
proc_1$com<- NA
proc_1$methgroup<- NA

levs<- unique(proc_1$Observation_ID)
for (i in 1:length(levs)){
  subs<- proc_1[proc_1$Observation_ID == levs[i],] 
  subs$methgroup<- "G3"
  subs<- subs[order(subs$UpperDepth),]
  
  
  # check for coordinates
  scord<- sum(as.numeric(is.na(subs$Longitude)))
  if(scord != 0){
    subs[, "com"]<- -9999
    proc_1[proc_1$Observation_ID == levs[i],] <- subs
    next}
  
  for (j in 1:nrow(subs)){
    vals<- subs[j, 14:19]
    sna<- sum(is.na(vals))
    
    # if only no values are missing
    if(vals$chvec == 0){
      subs[j,"clay"]<- vals$P10A1_C
      subs[j,"silt"]<- vals$P10A1_Z
      subs[j,"sand"]<- vals$P10_HYD_S 
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
    # if only one value is missing and it is sand 
    if(vals$chvec == 1 & is.na(vals$P10_HYD_S)){
      subs[j,"clay"]<- vals$P10A1_C
      subs[j,"silt"]<- vals$P10A1_Z
      subs[j,"sand"]<- vals$P10A1_CS + vals$P10A1_FS
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
    # if we have data about all three fractions [clay, sand, silt]
    if(vals$chvec == 2 & !is.na(vals$P10A1_C) & !is.na(vals$P10_HYD_S) & !is.na(vals$P10A1_Z) ){
      subs[j,"clay"]<- vals$P10A1_C
      subs[j,"silt"]<- vals$P10A1_Z
      subs[j,"sand"]<- vals$P10_HYD_S 
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
    # if we have data about clay and sand fraction but not silt
    if(vals$chvec == 3 & !is.na(vals$P10A1_C) & !is.na(vals$P10_HYD_S) & is.na(vals$P10A1_Z) ){
      subs[j,"clay"]<- vals$P10A1_C
      subs[j,"silt"]<- 100 - vals$P10A1_C - vals$P10_HYD_S
      subs[j,"sand"]<- vals$P10_HYD_S  
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
    # if we have data about clay and silt only
    if(vals$chvec == 3 & !is.na(vals$P10A1_C) & is.na(vals$P10_HYD_S) & !is.na(vals$P10A1_Z) ){
      subs[j,"clay"]<- vals$P10A1_C
      subs[j,"silt"]<- vals$P10A1_Z
      subs[j,"sand"]<- 100 -  vals$P10A1_C - vals$P10A1_Z
      sval<- sum(subs[j,11:13])
      # check to see if numbers make sense
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          subs[j, "com"]<- 1
          subs[j, 11:13] <- ((subs[j,11:13])/(sum(subs[j,11:13]))) * 100}
    }
    
  }
  
  # ones that slipped the net
  subs[which(is.na(subs$com)), "com"]<- -9999
  
  issu<- sum(subs$com == -9999)
  if(issu > 0){
    subs$com <- -9999
    proc_1[proc_1$Observation_ID == levs[i],] <- subs} else {
      proc_1[proc_1$Observation_ID == levs[i],] <- subs}
  print(i)
}

which(proc_1$com == -9999)
proc_1<- proc_1[-which(proc_1$com == -9999),] 
proc_z<- rbind(proc_z, proc_1[,c(1:13,21)])



# test of whether there are duplicated sites
sits<- proc_z[proc_z$UpperDepth==0,]
names(sits)
reps<- which(duplicated(sits[,7:8]))
reps
sits<- sits[-reps,]
#which(is.na(sits$DataStore))
#sits<- sits[-6279,]


# Go through each sits (site) and pull the data from proc_z to store in new data frame
nproc_z<- proc_z[0,]

cnt<- 1

#for (i in 1:159){
for (i in 1:nrow(sits)){
  # observation
  lab<- sits[i, "Observation_ID"]
  
  # get observation from comb.dat
  temp.dat<- proc_z[proc_z$Observation_ID == lab,]
  temp.dat$FID<- paste0(temp.dat$Dataset, "_", temp.dat$Provider, "_", temp.dat$Observation_ID)
  ulab<- unique(temp.dat$FID)
  if(length(ulab) > 1){
    stemp.dat<- split(temp.dat,temp.dat$FID)[[1]] # take the first entry
    stemp.dat<- stemp.dat[order(stemp.dat$UpperDepth),]
    nproc_z[cnt:(cnt + nrow(stemp.dat)-1),]<- stemp.dat[,1:14]
    cnt<- cnt + nrow(stemp.dat)} else {
      temp.dat[,1:14]<- temp.dat<- temp.dat[order(temp.dat$UpperDepth),1:14]
      nproc_z[cnt:(cnt + nrow(temp.dat)-1),]<- temp.dat[,1:14]
      cnt<- cnt + nrow(temp.dat)}
  print(c(i,cnt))
}

# save output
saveRDS(nproc_z, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/process1/tasGov_lab_PSA_data.rds")

