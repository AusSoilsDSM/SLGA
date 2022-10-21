## texture anysis workflow for TERN Landscapes
library(Compositional)

## data to play with
soil.dat<- read.csv("Z:/projects/ternlandscapes_2019/soiltexture/morph_lab_texturedata_clay.csv")
soil.dat$concatLAB<- paste0(soil.dat$proj_code, "_", soil.dat$s_id)

# space for simulated data
soil.dat$simDAT_clay<- NA
soil.dat$simDAT_silt<- NA
soil.dat$simDAT_sand<- NA



# soil texture class centroids
centroids<- read.csv("Z:/projects/ternlandscapes_2019/soiltexture/data/texture_class_centroids.csv")

# corrections for sub-plastic soils
subplastics<- read.csv("Z:/projects/ternlandscapes_2019/soiltexture/morph_lab_texturedata_subplastics_correction.csv")



#site IDs
siteIDs<- unique(soil.dat$concatLAB)
length(siteIDs)


# cycle through profiles
#for (i in 1:1){
for (i in 1: length(siteIDs)){
  sel.profile<- soil.dat[soil.dat$concatLAB == siteIDs[i],]
  sel.profile$h_texture_qual<- as.character(sel.profile$h_texture_qual)
  sel.profile[which(is.na(sel.profile$h_texture_qual)), "h_texture_qual"]<- "XX"
  
  # simulate a plausible value given the centroids
  unique.TG<- unique(sel.profile$h_texture)
  
  for (j in 1:length(unique.TG)){
    sel.TG<- unique.TG[j]
    nos1<- length(which(sel.profile$h_texture == sel.TG))
    # If there is only once instance of a texture class within a profile
    if (nos1 == 1){
      stt<- centroids[which(as.character(centroids$levs) == as.character(sel.TG)),]
      # sample from distribution
      x <- rdiri(n = 10000, c(stt[,"clayF"]*100, stt[,"siltF"]*100, stt[,"sandF"]*100))
      x<- as.data.frame(x)
      names(x)<- c("clay", "silt", "sand")
      x<- x[order(x$clay),]
      
      # does this layer have a texture qualifier? (+ -)
      lnos<- which(sel.profile$h_texture == sel.TG)
      # if a + qualifier we only want to take a sample from the upper part of the distribution
      if(sel.profile$h_texture_qual[lnos] == "+"){
        xup<- x[9001:10000,]
        sim1<- xup[sample(1:1000, 1),]
        sel.profile[lnos,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1}
      # if a - qualifier we only want to take a sample from the lower part of the distribution
      if(sel.profile$h_texture_qual[lnos] == "-"){
          xdown<- x[1:1000,]
          sim1<- xdown[sample(1:1000, 1),]
          sel.profile[lnos,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1} 
      # if there are no qualifiers then we just take a sample at random
      if(sel.profile$h_texture_qual[lnos] != "-" & sel.profile$h_texture_qual[lnos] != "+"){
        sim1<- x[sample(1:10000, 1),]
        sel.profile[lnos,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1} 
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
      lnos<- which(sel.profile$h_texture == sel.TG)
      
      # select the first instance of given texture grade
      slnos<- lnos[1]
      # if a + qualifier we only want to take a sample from the upper part of the distribution
      if(sel.profile$h_texture_qual[slnos] == "+"){
        xup<- x[9001:10000,]
        sim1<- xup[sample(1:1000, 1),]
        sel.profile[slnos,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1}
      # if a - qualifier we only want to take a sample from the lower part of the distribution
      if(sel.profile$h_texture_qual[slnos] == "-"){
        xdown<- x[1:1000,]
        sim1<- xdown[sample(1:1000, 1),]
        sel.profile[slnos,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1} 
      # if there are no qualifiers then we just take a sample at random
      if(sel.profile$h_texture_qual[slnos] != "-" & sel.profile$h_texture_qual[slnos] != "+"){
        sim1<- x[sample(1:10000, 1),]
        sel.profile[slnos,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1}
      
      # contraints (same texture in profile should have similar characterisitcs unless there are qualifiers)
      upb1<- sim1[,"clay"] + 0.02
      lob1<- sim1[,"clay"] - 0.02
      subs1<- x[x$clay >= lob1 & x$clay <= upb1 ,]
      
      for (k in 2:nos1){
        slnos1<- lnos[k]
        # if a + qualifier we only want to take a sample from the upper part of the distribution
        if(sel.profile$h_texture_qual[slnos1] == "+"){
          xup<- x[9001:10000,]
          sim1<- xup[sample(1:1000, 1),]
          sel.profile[slnos1,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1}
        # if a - qualifier we only want to take a sample from the lower part of the distribution
        if(sel.profile$h_texture_qual[slnos1] == "-"){
          xdown<- x[1:1000,]
          sim1<- xdown[sample(1:1000, 1),]
          sel.profile[slnos1,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1} 
        # where there is no qualier we can get the texture grades similar for the other ones of the same texture class
        if(sel.profile$h_texture_qual[slnos1] != "-" & sel.profile$h_texture_qual[slnos1] != "+"){
          sim1<- subs1[sample(1:nrow(subs1), 1),]
          sel.profile[slnos1,c("simDAT_clay","simDAT_silt","simDAT_sand")]<- sim1}
        }
    }
  }
  # checks and changes for sub-plasticity
  krazIDs <- which(  (str_to_upper( sel.profile$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( sel.profile$o_asc_ord ) == 'FE'  )
                     | (str_to_upper( sel.profile$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( sel.profile$o_gsg) == 'K'  )
                     | (str_to_upper( sel.profile$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( sel.profile$o_gsg) == 'E'   )
                     | (str_to_upper( sel.profile$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( sel.profile$o_ppf) == 'GN3.11'  )
                     | (str_to_upper( sel.profile$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( sel.profile$o_ppf) == 'GN3.12'  )
                     | (str_to_upper( sel.profile$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( sel.profile$o_ppf) == 'GN3.14'  )
                     | (str_to_upper( sel.profile$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( sel.profile$o_ppf) == 'GN3.10'  )
                     | (str_to_upper( sel.profile$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( sel.profile$o_ppf) == 'GN3.17'  ))
  
  if (length(krazIDs) >= 1){
    for (s in 1:length(krazIDs)){
      kz1<- krazIDs[s]
      kz1.sel<- sel.profile[kz1,]
      lj<- as.character(kz1.sel$h_texture)
      # check to see if the texture class corresponds to one corrections should be made for
      if (lj %in% subplastics$Texture == TRUE){
      set.diff<- subplastics[which(lj %in% subplastics$Texture), "diff"]
      # add clay content simulated value
      kz1.sel$simDAT_clay<- kz1.sel$simDAT_clay + (set.diff/100)
      # re-allocate sand and silt fractions
      kz1.sel$simDAT_silt<- kz1.sel$simDAT_silt - (0.3 * (set.diff/100)) 
      kz1.sel$simDAT_sand<- kz1.sel$simDAT_sand - (0.7 * (set.diff/100))
      sel.profile[kz1,] <- kz1.sel} else {
        next}
      }
  }
  soil.dat[soil.dat$concatLAB == siteIDs[i],]<- sel.profile
  print(i)
}

 
  
  
  