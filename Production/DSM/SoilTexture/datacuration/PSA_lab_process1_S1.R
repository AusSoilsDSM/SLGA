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


# DAT 1
proc<- dat1[-1,]
labs<- names(proc)[12:102]
vec<- c()
for (i in 1:91){
  sel.lab<- labs[i]
  vec[i]<- nrow(proc) - sum(as.numeric(is.na(proc[,sel.lab])))
  print(sum(as.numeric(is.na(proc[,sel.lab]))))
}
vec
labs[c(28,31,32)]
## [1] "P10_CF_C" "P10_CF_S" "P10_CF_Z"

# new data
proc_1<- proc[,1:10]
proc_1$clay<- NA
proc_1$silt<- NA
proc_1$sand<- NA

# move data
proc_1$clay<- proc[, "P10_CF_C"]
proc_1$sand<- proc[, "P10_CF_S"]
proc_1$silt<- proc[, "P10_CF_Z"]
proc_1$com<- NA
proc_1$methgroup<- NA

# fill holes and remove issues

levs<- unique(proc$Observation_ID)
for (i in 1:length(levs)){
  subs<- proc_1[proc_1$Observation_ID == levs[i],] 
  subs<- subs[order(subs$UpperDepth),]
  
  # check for coordinates
  scord<- sum(as.numeric(is.na(subs$Longitude)))
  if(scord != 0){
    subs[, "com"]<- -9999
    proc_1[proc_1$Observation_ID == levs[i],] <- subs
    next}
  
  for (j in 1:nrow(subs)){
    vals<- subs[j, 11:13]
    sna<- sum(is.na(vals))
    
    if (sna == 1){ # if there is a single missing value
      diff<- 100 - sum(vals[which(!is.na(vals))])
      if(diff > 50){
        vals[which(is.na(vals))] <- 100 - sum(vals[which(!is.na(vals))])
        subs[j, "com"]<- -9999
        subs[j, 11:13]<- vals} else {
          vals[which(is.na(vals))] <- 100 - sum(vals[which(!is.na(vals))])
          subs[j, "com"]<- 200
          subs[j, 11:13] <- vals}}
    
    if (sna == 2){ # if there are two missing value
      subs[j, "com"]<- -9999}
    
    if (sna == 3){ # if there are three missing value
      subs[j, "com"]<- -9999}
    
    if (sna == 0){ # if no missing values
      sval<- sum(vals)
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          vals<- (vals/sum(vals)) * 100
          subs[j, "com"]<- 1
          subs[j, 11:13] <- vals}}
    
  }
  
  issu<- sum(subs$com == -9999)
  if(issu > 0){
    subs$com <- -9999
    proc_1[proc_1$Observation_ID == levs[i],] <- subs} else {
      proc_1[proc_1$Observation_ID == levs[i],] <- subs}
}

proc<- proc_1[-which(proc_1$com == -9999),] 

dat1_1<- proc[,c(1:13,15)]

# save output
saveRDS(dat1_1, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/process1/lawsonGrain_lab_PSA_data.rds")






# DAT 2 [NatGeochem]
proc<- dat2[-1,]
labs<- names(proc)[12:102]
vec<- c()
for (i in 1:91){
  sel.lab<- labs[i]
  vec[i]<- nrow(proc) - sum(as.numeric(is.na(proc[,sel.lab])))
  print(sum(as.numeric(is.na(proc[,sel.lab]))))
}
vec
labs[c(42,45,46)]
## [1] "P10_NR_C" "P10_NR_S" "P10_NR_Z"

# new data
proc_1<- proc[,1:10]
proc_1$clay<- NA
proc_1$silt<- NA
proc_1$sand<- NA

# move data
proc_1$clay<- proc[, "P10_NR_C"]
proc_1$sand<- proc[, "P10_NR_S"]
proc_1$silt<- proc[, "P10_NR_Z"]
proc_1$com<- NA
proc_1$methgroup<- NA

# fill holes and remove issues

levs<- unique(proc$Observation_ID)
for (i in 1:length(levs)){
  subs<- proc_1[proc_1$Observation_ID == levs[i],] 
  
  # check for coordinates
  scord<- sum(as.numeric(is.na(subs$Longitude)))
  if(scord != 0){
    subs[, "com"]<- -9999
    proc_1[proc_1$Observation_ID == levs[i],] <- subs
    next}
  
  for (j in 1:nrow(subs)){
    vals<- subs[j, 11:13]
    sna<- sum(is.na(vals))
    
    if (sna == 1){ # if there is a single missing value
      diff<- 100 - sum(vals[which(!is.na(vals))])
      if(diff > 50){
        vals[which(is.na(vals))] <- 100 - sum(vals[which(!is.na(vals))])
        subs[j, "com"]<- -9999
        subs[j, 11:13]<- vals} else {
          vals[which(is.na(vals))] <- 100 - sum(vals[which(!is.na(vals))])
          subs[j, "com"]<- 200
          subs[j, 11:13] <- vals}}
    
    if (sna == 2){ # if there are two missing value
      subs[j, "com"]<- -9999}
    
    if (sna == 3){ # if there are three missing value
      subs[j, "com"]<- -9999}
    
    if (sna == 0){ # if no missing values
      sval<- sum(vals)
      if(sval < 90) {
        subs[j, "com"]<- -9999} else {
          vals<- (vals/sum(vals)) * 100
          subs[j, "com"]<- 1
          subs[j, 11:13] <- vals}}
    
  }
  
  issu<- sum(subs$com == -9999)
  if(issu > 0){
    subs$com <- -9999
    proc_1[proc_1$Observation_ID == levs[i],] <- subs} else {
      proc_1[proc_1$Observation_ID == levs[i],] <- subs}
}

proc<- proc_1[-which(proc_1$com == -9999),] 

dat2_1<- proc[,c(1:13,15)]

# save output
saveRDS(dat2_1, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/process1/natGeochemSoil_lab_PSA_data.rds")




