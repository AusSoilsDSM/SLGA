## Workflow for converting field soil textures to quantitative values.
## TERN Landscapes work
## PROCESS 1:
# Go through each data set and identify dodgt entries: missing coordinates, unspecified texture classes

library(Compositional);library(stringr)

# datasets
# morphological data
files<- list.files(path = "Z:/projects/ternlandscapes_2019/soiltexture/data/",
           pattern = "PSA_morph_data_2019-12-11", full.names = T, recursive = F)
# soil class data
sfiles<- list.files(path = "Z:/projects/ternlandscapes_2019/soiltexture/data/",
                    pattern = "soil_class_data_2019-12-11.rds", full.names = T, recursive = F)
sfiles


## PERTINENT information
# soil texture class centroids
centroids<- read.csv("Z:/projects/ternlandscapes_2019/soiltexture/data/texture_class_centroids.csv")

# corrections for sub-plastic soils
subplastics<- read.csv("Z:/projects/ternlandscapes_2019/soiltexture/data/morph_lab_texturedata_subplastics_correction.csv")


# selected datasets
# morphological data
files
dat<- readRDS(files[10])
head(dat)

# split into texture and qualifier sets then column combine or merge
dat.morph<- dat[dat$ObservedProperty == "H_TEXTURE",]
names(dat.morph)

dat.qual<- dat[dat$ObservedProperty == "H_TEXTURE_QUAL",] 

dat.merge<- merge(dat.morph,dat.qual, 
            by=c("DataStore","Dataset","Provider","Observation_ID","SampleID",
                 "SampleDate", "UpperDepth", "LowerDepth","PropertyType" ),all=T)
names(dat.merge)
#dat.merge<- dat.merge[,c(1,2,3,4,5,6,17,18, 7,8,11,20)] # NatSoil
#dat.merge<- dat.merge[,c(1,2,3,4,5,6,7,8, 10,11,13, 22)] # NT
#dat.merge<- dat.merge[,c(1,2,3,4,5,6,7,8, 10,11,13, 22)] # QLD
#dat.merge<- dat.merge[,c(1,2,3,4,5,6,7,8, 10,11,13, 22)] # SA
#dat.merge<- dat.merge[,c(1,2,3,4,5,6,7,8, 10,11,13, 22)] # TAS
#dat.merge<- dat.merge[,c(1,2,3,4,5,6,7,8, 10,11,13, 22)] # VIC
dat.merge<- dat.merge[,c(1,2,3,4,5,6,7,8, 10,11,13, 22)] # WA

# remove strange ones
#dat.merge<- dat.merge[-which(dat.merge$Observation_ID == "199_NC_C461_1"),]
#dat.merge<- dat.merge[-which(dat.merge$Observation_ID == "199_NC_C463_1"),]

# space for simulated data
dat.merge$simDAT_clay<- NA
dat.merge$simDAT_silt<- NA
dat.merge$simDAT_sand<- NA


# soil class data
sfiles
sc.dat<- readRDS(sfiles[9])
head(sc.dat)



# unique sites
uniq.s<- unique(dat.merge$Observation_ID)
nosP<- length(unique(dat.merge$Observation_ID))
nosP

for (i in 1:nosP){
  # sub set
  subs.t<- dat.merge[dat.merge$Observation_ID == uniq.s[i], ]
  subs.t<- subs.t[order(subs.t$UpperDepth),]
  subs.s<- sc.dat[sc.dat$Observation_ID == uniq.s[i],]
  subs.s<- subs.s[1,]
  
  
  # curate
  subs.t$Value.y<- as.character(subs.t$Value.y)
  subs.t[which(is.na(subs.t$Value.y)), "Value.y"]<- "XX"
  
  # remove from analysis is all coordinates are missing
  loc.nas<- sum(as.numeric(is.na(subs.t$Longitude.x)))
  if(loc.nas == nrow(subs.t)){next} else {
    if (loc.nas != nrow(subs.t) & loc.nas >= 1 ) {
      fil.long<- subs.t[which(!is.na(subs.t$Longitude.x)),"Longitude.x"]
      fil.lat<- subs.t[which(!is.na(subs.t$Latitude.x)),"Latitude.x"]
      subs.t$Longitude.x[which(is.na(subs.t$Longitude.x))]<- fil.long
      subs.t$Latitude.x[which(is.na(subs.t$Latitude.x))]<- fil.lat} else {
        
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
  dat.merge[dat.merge$Observation_ID == uniq.s[i], ]<- subs.t
  print(i)
  
}}}}


dat.fin<- dat.merge[-which(is.na(dat.merge$simDAT_clay)),]


# save object
files
saveRDS(object = dat.fin, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/process1/WAGovernment_PSA_morph_data_2019-12-11_process1.rds")



