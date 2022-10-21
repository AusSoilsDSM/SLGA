## Workflow for process combining lab PSA data and morph PSA data
## TERN Landscapes work
## PROCESS 4: 
# For each profile of the morph data want to simulate plausible realities of the texture fractions

#libraries
library(parallel);library(doParallel);library(Compositional);library(stringr)

# morph data
morph.data<- readRDS(file = "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/ternlandscapes_2019/soiltexture/data/process3/combined_morph_PSA_data_labPSA_checks_removed.rds")
morph.data$simDAT_clay<- NA
morph.data$simDAT_silt<- NA
morph.data$simDAT_sand<- NA


# soil class data
sfiles<- list.files(path = "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/ternlandscapes_2019/soiltexture/data/",
                    pattern = "soil_class_data_2019-12-11.rds", full.names = T, recursive = F)
sfiles

nms<- c("sdat1", "sdat2", "sdat3","sdat4", "sdat5", "sdat6","sdat7", "sdat8", "sdat9")

for (i in 1:length(sfiles)){
  assign(nms[i], readRDS(sfiles[i]))
}

## DATA
# sdat1: natsoil
# sdat2: NSW gov
# sdat3: NT gov
# sdat4: QLD gov
# sdat5: SA gov
# sdat6: SCARP
# sdat7: Tas Govt
# sdat8: Vic gov
# sdat9: WA gov


## PERTINENT information
# soil texture class centroids
centroids<- read.csv("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/ternlandscapes_2019/soiltexture/data/texture_class_centroids.csv")

# corrections for sub-plastic soils
subplastics<- read.csv("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/ternlandscapes_2019/soiltexture/data/morph_lab_texturedata_subplastics_correction.csv")



# unique sites
uniq.s<- unique(morph.data$Observation_ID)
nosP<- length(unique(morph.data$Observation_ID))
nosP # 186121 profiles


# begin parallel cluster and register it with foreach
cpus<- 24
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)


oper1<- foreach(ii=1:50,.packages = c("Compositional", "stringr")) %dopar% {

#for (i in 1:100){
for (i in 1:nosP){
  # sub set
  subs.t<- morph.data[morph.data$Observation_ID == uniq.s[i], ]
  subs.t<- subs.t[order(subs.t$UpperDepth),]
  
  # soil data
  if (subs.t$Dataset[1] == "NatSoil"){sc.dat<- sdat1}
  if (subs.t$Dataset[1] == "NTGovernment"){sc.dat<- sdat3}
  if (subs.t$Dataset[1] == "QLDGovernment"){sc.dat<- sdat4}
  if (subs.t$Dataset[1] == "SAGovernment"){sc.dat<- sdat5}
  if (subs.t$Dataset[1] == "TasGovernment"){sc.dat<- sdat7}
  if (subs.t$Dataset[1] == "TERNSurveillance"){sc.dat<- NULL}
  if (subs.t$Dataset[1] == "VicGovernment"){sc.dat<- sdat8}
  if (subs.t$Dataset[1] == "WAGovernment"){sc.dat<- sdat9}
  
  subs.s<- sc.dat[sc.dat$Observation_ID == uniq.s[i],]
  subs.s<- subs.s[1,]
  
  # simulate a plausible value given the centroids
  unique.TG<- unique(subs.t$Value.x)
  if (any(unique.TG %in% centroids$levs == FALSE) == TRUE) {next} else {
    for (j in 1:length(unique.TG)){
      sel.TG<- unique.TG[j]
      nos1<- length(which(subs.t$Value.x == sel.TG))
      
      # If there is only once instance of a texture class within a profile
      if (nos1 == 1){
        stt<- centroids[which(as.character(centroids$levs) == as.character(sel.TG)),]
        # sample from distribution
        x <- rdiri(n = 10000, c(stt[,"clayF"]*100, stt[,"siltF"]*100, stt[,"sandF"]*100))
        x<- as.data.frame(x)
        names(x)<- c("clay", "silt", "sand")
        x<- x[order(x$clay),]
        
        # does this layer have a texture qualifier? (+ -)
        lnos<- which(subs.t$Value.x == sel.TG)
        # if a + qualifier we only want to take a sample from the upper part of the distribution
        if(subs.t$Value.y[lnos] == "+"){
          xup<- x[9001:10000,]
          sim1<- xup[sample(1:1000, 1),]
          subs.t[lnos,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1}
        
        # if a - qualifier we only want to take a sample from the lower part of the distribution
        if(subs.t$Value.y[lnos] == "-"){
          xdown<- x[1:1000,]
          sim1<- xdown[sample(1:1000, 1),]
          subs.t[lnos,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1} 
        
        # if there are no qualifiers then we just take a sample at random
        if(subs.t$Value.y[lnos] != "-" & subs.t$Value.y[lnos] != "+"){
          sim1<- x[sample(1:10000, 1),]
          subs.t[lnos,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1} 
        # end routine for single layers      
        }
      # where we have more than one instance of a texture class in a profile
      # assuming soil textures in the same class will be consistent 
      
      if (nos1 != 1){
        stt<- centroids[which(as.character(centroids$levs) == as.character(sel.TG)),]
        # sample from distribution
        x <- rdiri(n = 10000, c(stt[,"clayF"]*100, stt[,"siltF"]*100, stt[,"sandF"]*100))
        x<- as.data.frame(x)
        names(x)<- c("clay", "silt", "sand")
        x<- x[order(x$clay),]
              
        # does either layer have a texture qualifier? (+ -)
        lnos<- which(subs.t$Value.x == sel.TG)
              
        # select the first instance of given texture grade
        slnos<- lnos[1]
        # if a + qualifier we only want to take a sample from the upper part of the distribution
        if(subs.t$Value.y[slnos] == "+"){
          xup<- x[9001:10000,]
          sim1<- xup[sample(1:1000, 1),]
          subs.t[slnos,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1}
              
        # if a - qualifier we only want to take a sample from the lower part of the distribution
        if(subs.t$Value.y[slnos] == "-"){
          xdown<- x[1:1000,]
          sim1<- xdown[sample(1:1000, 1),]
          subs.t[slnos,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1} 
        
        # if there are no qualifiers then we just take a sample at random
        if(subs.t$Value.y[slnos] != "-" & subs.t$Value.y[slnos] != "+"){
          sim1<- x[sample(1:10000, 1),]
          subs.t[slnos,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1}
        
        # contraints (same texture in profile should have similar characterisitcs unless there are qualifiers)
        upb1<- sim1[,"clay"] + 0.02
        lob1<- sim1[,"clay"] - 0.02
        subs1<- x[x$clay >= lob1 & x$clay <= upb1 ,]
        
        for (k in 2:nos1){
          slnos1<- lnos[k]
          
          # if a + qualifier we only want to take a sample from the upper part of the distribution
          if(subs.t$Value.y[slnos1] == "+"){
            xup<- x[9001:10000,]
            sim1<- xup[sample(1:1000, 1),]
            subs.t[slnos1,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1}
                
          # if a - qualifier we only want to take a sample from the lower part of the distribution
          if(subs.t$Value.y[slnos1] == "-"){
            xdown<- x[1:1000,]
            sim1<- xdown[sample(1:1000, 1),]
            subs.t[slnos1,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1} 
          
          # where there is no qualier we can get the texture grades similar for the other ones of the same texture class
          if(subs.t$Value.y[slnos1] != "-" & subs.t$Value.y[slnos1] != "+"){
            sim1<- subs1[sample(1:nrow(subs1), 1),]
            subs.t[slnos1,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1}
        }
      }
    }
    
    if (subs.t$Dataset[1] == "TERNSurveillance"){
      morph.data[morph.data$Observation_ID == uniq.s[i], ]<- subs.t
      #print(i)
      next} else {
      # checks and changes for sub-plasticity
      krazIDs <- which(  (str_to_upper(subs.s$ObservedProperty) == 'O_ASC_ORD' & str_to_upper(subs.s$Value) == 'FE')
                             | (str_to_upper( subs.s$ObservedProperty) == 'O_GSG' & str_to_upper( subs.s$Value) == 'K'  )
                             | (str_to_upper( subs.s$ObservedProperty) == 'O_GSG' & str_to_upper( subs.s$Value) == 'E'   )
                             | (str_to_upper( subs.s$ObservedProperty) == 'O_PPF' & str_to_upper( subs.s$Value) == 'GN3.11'  )
                             | (str_to_upper( subs.s$ObservedProperty) == 'O_PPF' & str_to_upper( subs.s$Value) == 'GN3.12'  )
                             | (str_to_upper( subs.s$ObservedProperty) == 'O_PPF' & str_to_upper( subs.s$Value) == 'GN3.14'  )
                             | (str_to_upper( subs.s$ObservedProperty) == 'O_PPF' & str_to_upper( subs.s$Value) == 'GN3.10'  )
                             | (str_to_upper( subs.s$ObservedProperty) == 'O_PPF' & str_to_upper( subs.s$Value) == 'GN3.17'  ))
      
      if (length(krazIDs) >= 1){
        for (s in 1:nrow(subs.t)){
          kz1.sel<- subs.t[s,]
          lj<- as.character(kz1.sel$Value.x)
          # check to see if the texture class corresponds to one corrections should be made for
          if (lj %in% subplastics$Texture == TRUE){
            set.diff<- subplastics[which(lj %in% subplastics$Texture), "diff"]
            # add clay content simulated value
            kz1.sel$simDAT_clay<- kz1.sel$simDAT_clay + (set.diff/100)
            # re-allocate sand and silt fractions
            kz1.sel$simDAT_silt<- kz1.sel$simDAT_silt - (0.3 * (set.diff/100)) 
            kz1.sel$simDAT_sand<- kz1.sel$simDAT_sand - (0.7 * (set.diff/100))
            subs.t[s,] <- kz1.sel} else {
              next}
        }
      }
    }
    
    morph.data[morph.data$Observation_ID == uniq.s[i], ]<- subs.t
    #print(i)
  }
}


nas<- which(is.na(morph.data$simDAT_clay))
length(nas)
if (length(nas) == 0){
  dat.fin<- morph.data} else {
    dat.fin<- morph.data[-which(is.na(morph.data$simDAT_clay)),]}
nrow(dat.fin)
nrow(morph.data)


# save object
saveRDS(object = dat.fin, file = paste0("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/ternlandscapes_2019/soiltexture/data/process4/morph_sims/combined_morph_data_2020-01_sim_",ii,".rds"))
}

#stopCluster(cl)

