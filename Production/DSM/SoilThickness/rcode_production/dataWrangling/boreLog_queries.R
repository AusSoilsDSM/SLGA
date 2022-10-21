library(tokenizers);library("RODBC");library(tcltk) #load package


## Read data base into R

#db<-file.path("X:/DepthData/National Bores.accdb") #connect database.
#Note the UNIX style slash (/). "\" is "escape character" so all "\"  you should replace either with "/" or "\\"
#channel<-odbcConnectAccess2007(db) #internal RODBC function
#data_boreLith<- sqlFetch(channel,"bore_lith") #read particular table from Access database file.
#data_boreLoc1<- sqlFetch(channel,"bore_locations")
#data_boreLoc2<- sqlFetch(channel,"bore_locations2")

#saveRDS(data_boreLith,file = "X:/DepthData/data_boreLith.rds")
#saveRDS(data_boreLoc1,file = "X:/DepthData/data_boreLoc1.rds")
#saveRDS(data_boreLoc2,file = "X:/DepthData/data_boreLoc2.rds")

data_boreLith<- readRDS("X:/DepthData/data_boreLith.rds")
data_boreLoc1<- readRDS("X:/DepthData/data_boreLoc1.rds")
data_boreLoc2<- readRDS("X:/DepthData/data_boreLoc2.rds")

# for lithology descriptions

# good words and 2 word phrases (add to these as seen fit)
gw<- c("sand", "sands",  "top", "topsoil", "clay","clays", "loam", "silt", "soil","soils", "silts" ,"subsoil", "overburden","surface",
       "colluvium","unconsolidated","unconsolidated sand", "unconsolidated clay", "unconsolidated silt", "marl", "caly", "lignite sand","earth")

# bad words and 2 word phrases
bw<- c("rock", "rocks", "lense", "lenses", "calcrete", "basalt", "stone", "sandstone","mudstone", "siltstone",
       "tufa","scoria","granite", "limestone", "lignite", "liginite", "gravel", "boulder", "claystone", "ironstone",
       "fossilized", "bluestone", "fossil","shale","salamander", "slate", "quartz", "bedrock", "schist", "cement", "basa","buckshot",
       "pebbles", "coal","metamorphic","hornfels","rhyodacite", "laterite","gneissic","ferricrete","conglomerate", "diorite","decomposed",
       "drift","boulders", "soapstone","stones", "consolidated", "flint", "sandy siltstone", "calcarenite", "grit", "gravels", "breccia",
       "brecciated", "rubble", "travertine", "marble", "shell", "sandstne","metamorphosed")


# Ideal lithologies
idLiths<- c("CLAY", "SAND", "TPSL", "SOIL", "SDCY", "SILT", "LOAM", "CLSD", "SLCY", "MARL", "SCLM", "CLLM", "CLYU", "SAPT",
            "SA", "CL", "CLST", "CLYW", "SDLC", "CL,SA", "SA,CL", "QUAR", "PEAT", "OI", "MUD")

# not ideal lithologies
nidLiths<- c("SHLE","SDSN","UNKN", "GRVL", "LMST", "SDST",  "BSLT", "ROCK", "GRNT","SLAT","SCHT",
             "QTZT", "SLTE","GNSS", "CLYS","SLSN","SLST","BIFM","CGLM","COAL","MDST","CALC", "CORL","MARB",
             "MDSN","LMSN", "CAAR","IRSN","DLOM","CLSN", "BLDR", "QRTZ", "ALVM", "STON", "DRFT", "WB", "SILC", "LS",
             "SS", "DCMP", "QRTT", "AMPH", "BNST", "BDRK", "SH", "LIGN", "IRSU", "FLNT", "GRIT", "GR", "BREC", "RUBL",
             "VLCC", "LGNT","GYST", "PRPR", "SPST", "GPSM","TRAV", "SHEL","WRCK")









### Fiddling around 
levels(data_boreLith$HydroCode)[1]


data_boreLoc1[which(data_boreLoc1$HydroCode == "104186"),]  # location
data_boreLith[which(data_boreLith$HydroCode == "104186"),]

# summaries
summary(data_boreLith$MajorLithC) 
summary(data_boreLith$Descriptio)
summary(data_boreLith$MajorLithC, maxsum = 2000)

summary(data_boreLith$MajorLithC)
head(data_boreLith[which(data_boreLith$MajorLithC== "SDMN"),c(1:5,10:14)])

#######



## table for outputs
outDat<- cbind(data_boreLoc1[1,c("HydroID", "HydroCode", "StateBoreI", "Latitude", "Longitude")],
      data_boreLith[1,c("BoreID", "HydroCode", "FromDepth", "ToDepth")])
outDat$notes<- NA
outDat$descriptions<- NA
outDat<- outDat[0,]
outDat
str(outDat)


### Start the text mining 

data_boreLith$BoreID<- as.factor(data_boreLith$BoreID)
cnt=1
total <- length(levels(data_boreLith$HydroCode))
pb <- tkProgressBar(title = "progress bar", min = 0, max = total , width = 300)

for (i in 208368:length(levels(data_boreLith$HydroCode))){ # for each level of the lithology hydrocode data
  
  ## Note Hydrocode seems like a good link between the lithology data and the location data.
  ## However it seems sometimes there may be more than 1 bore for a given hydrocode
  ## Subsequently we need to split by bore id
  ## Bore id from litholgy data sees to relate to HydroID from location data
  
  
  ## For each lithology hydrocode level
  nm1<- levels(data_boreLith$HydroCode)[i] 
  lith_datT<- as.data.frame(data_boreLith[which(data_boreLith$HydroCode == nm1),])
  lith_datT
  
  # check for 
  if (0 %in% lith_datT$FromDepth == FALSE){
    next}
  
  lith_datT$BoreID<- droplevels(lith_datT$BoreID)
  lithdat_sp<- split(lith_datT, lith_datT$BoreID)
  length(lith_dat_sp)
  
  for (el in 1:length(lith_dat_sp)){
    
    lith_dat<- lith_dat_sp[[el]]
    lith_dat
    
    if (0 %in% lith_dat$FromDepth == FALSE){
      next}
    
    ## if length of lith_dat_sp ==1
    # attribute some location info for the observation
    if (length(lith_dat_sp) == 1){
      nm2<- as.character(lith_dat$HydroCode)
      nm2<- nm2[!duplicated(nm2)]
      nm3<- as.character(lith_dat$BoreID)
      nm3<- nm3[!duplicated(nm3)]
      
      if (nrow(data_boreLoc1[which(data_boreLoc1$HydroCode == nm2),]) == 1){
        outDat[cnt,1:5]<-  data_boreLoc1[which(data_boreLoc1$HydroCode == nm2),c("HydroID", "HydroCode", "StateBoreI", "Latitude", "Longitude")]} 
      
      if (nrow(data_boreLoc1[which(data_boreLoc1$HydroCode == nm2),]) > 1){
        if(nrow(data_boreLoc1[which(data_boreLoc1$HydroID == nm3),]) == 0){
          next} else {
            outDat[cnt,1:5]<-  data_boreLoc1[which(data_boreLoc1$HydroID == nm3),c("HydroID", "HydroCode", "StateBoreI", "Latitude", "Longitude")]}} 
      
      if (nrow(data_boreLoc1[which(data_boreLoc1$HydroCode == nm2),]) == 0){
        if (nrow(data_boreLoc1[which(data_boreLoc1$HydroID == nm3),]) == 1){
          outDat[cnt,1:5]<-  data_boreLoc1[which(data_boreLoc1$HydroID == nm3),c("HydroID", "HydroCode", "StateBoreI", "Latitude", "Longitude")]} else {
            next}}} else {
              nm3<- as.character(lith_dat$BoreID)
              nm3<- nm3[!duplicated(nm3)]
              nm2<- as.character(lith_dat$HydroCode)
              nm2<- nm2[!duplicated(nm2)]
              if(nrow(data_boreLoc1[which(data_boreLoc1$HydroID == nm3),])== 0){
                next} else {
                  outDat[cnt,1:5]<-  data_boreLoc1[which(data_boreLoc1$HydroID == nm3),c("HydroID", "HydroCode", "StateBoreI", "Latitude", "Longitude")]}}
    #outDat
    
    ### do some cleaning up and housekeeping
    
    # remove duplicated rows
    lith_dat<- lith_dat[!duplicated(lith_dat$FromDepth),]
    
    # sort from shallowest  to deepest
    lith_dat<- lith_dat[order(lith_dat$FromDepth),] 
    lith_dat
    
    # remove layers where the upper depth is greater than 20m
    lith_dat<- lith_dat[!lith_dat$FromDepth>20, ]
    lith_dat
    
    ### assess for presence of soil material
    grade<- c()
    
    for (d in 1:nrow(lith_dat)){
      # if both lithology and description data are not available
      if(is.na(lith_dat$MajorLithC[d]) == TRUE & is.na(lith_dat$Descriptio[d]) == TRUE ){
        grade[d]<- -9999} 
      
      # if lithology data is not available, do text mining of description (any ambiguity is removed. Only sure things kept)
      if(is.na(lith_dat$MajorLithC[d]) == TRUE & is.na(lith_dat$Descriptio[d]) == FALSE){
        wds<- c(tokenize_skip_ngrams(tolower(as.character(lith_dat$Descriptio[d])), n = 2, k = 1))[[1]] # token word string into single and two word groups
        gwm<- match(gw, wds) # good word match
        bwm<- match(bw, wds) # bad word match
        # if there is a bad word match, if there is a good match, if there is no match 
        if(length(bwm[complete.cases(bwm)]) > 0 & length(gwm[complete.cases(gwm)]) > 0){
          grade[d]<- -9999} else if (length(gwm[complete.cases(gwm)]) > 0 & length(bwm[complete.cases(bwm)]) == 0 ){
            grade[d]<- 1} else if (length(gwm[complete.cases(gwm)]) == 0 & length(bwm[complete.cases(bwm)]) > 0) {
              grade[d]<- 500} else {grade[d]<- -9999}}
      
      # if lithology data is available, do text mining of lithology. (Have to also do a bad word match too as there might be some ambiguity)
      if(is.na(lith_dat$MajorLithC[d]) == FALSE){
        lwm<- match(idLiths,lith_dat$MajorLithC[d])
        nlwm<- match(nidLiths,lith_dat$MajorLithC[d])
        # also check descriptions
        wds2<- c(tokenize_skip_ngrams(tolower(as.character(lith_dat$Descriptio[d])), n = 2, k = 1))[[1]]
        gwm2<- match(gw, wds2) # good word match
        bwm2<- match(bw, wds2) # bad word match
        
        if(length(lwm[complete.cases(lwm)]) > 0 ){ # word match for good lithology
          grade[d]<- 1}
        if(length(lwm[complete.cases(lwm)]) == 0 ){ # no word match for good lithology
          if(length(nlwm[complete.cases(nlwm)]) > 0 & length(bwm2[complete.cases(bwm2)]) > 0 & length(gwm2[complete.cases(gwm2)]) == 0) {  # word match for bad lithology
            grade[d]<- 500}
          if(length(nlwm[complete.cases(nlwm)]) > 0 & length(bwm2[complete.cases(bwm2)]) > 0 & length(gwm2[complete.cases(gwm2)]) > 0) {  # word match for bad lithology
            grade[d]<- -9999}
          if(length(nlwm[complete.cases(nlwm)]) > 0 & length(bwm2[complete.cases(bwm2)]) == 0 & length(gwm2[complete.cases(gwm2)]) > 0) {  # word match for bad lithology
            grade[d]<- -9999}
          if(length(nlwm[complete.cases(nlwm)]) > 0 & length(bwm2[complete.cases(bwm2)]) == 0 & length(gwm2[complete.cases(gwm2)]) == 0) {  # word match for bad lithology
            grade[d]<- 500}
          if(length(nlwm[complete.cases(nlwm)]) == 0){ # no word match for bad litholgy
            if(length(bwm2[complete.cases(bwm2)]) > 0 & length(gwm2[complete.cases(gwm2)]) > 0){
              grade[d]<- -9999} # ambiguous lithology description
            if(length(bwm2[complete.cases(bwm2)]) > 0 & length(gwm2[complete.cases(gwm2)]) == 0){
              grade[d]<- 500} # description indicates rock
            if(length(bwm2[complete.cases(bwm2)]) == 0 & length(gwm2[complete.cases(gwm2)]) > 0){
              grade[d]<- 1}
            if(length(bwm2[complete.cases(bwm2)]) == 0 & length(gwm2[complete.cases(gwm2)]) == 0){
              grade[d]<- -9999}
            }}} # description indicates soil
      }
    
    
    ## figure out where the soil starts and stops
    lith_dat
    grade
    
    # if the profile has any ambiguity
    if (-9999 %in% grade == TRUE){
      grade<- -9999
      outDat[cnt, 6:9]<- NA
      outDat[cnt, 10]<- "ambig"
      cnt<- cnt +1}
    
    # if the top layer is a 'soil'
    if (grade[1]== 1){
      pp<- which(grade==1) # get the "soil"
      yy=c(1:100)
      yyy=match(yy,pp)
      yyyy<- which(is.na(yyy))[1]
      sel<- yyyy-1
      #save some outputs
      outDat[cnt, 6:9]<- lith_dat[sel,c("BoreID", "HydroCode", "FromDepth", "ToDepth")]
      outDat[cnt, 10]<- "soil limit"
      if(sel == nrow(lith_dat)){
        outDat[cnt, 11]<- "possibly right censored"} else {
          outDat[cnt, 11]<- "likely thickness"}
      cnt<- cnt + 1}
    
    # if the top layer is not a soil
    if (grade[1]== 500){
      #save some outputs
      outDat[cnt, 6:9]<- lith_dat[1,c("BoreID", "HydroCode","FromDepth", "ToDepth" )]
      outDat[cnt, 10]<- "rock limit"
      outDat[cnt, 11]<- as.character(lith_dat[1,"Descriptio"])
      cnt<- cnt + 1}
  }
  setTkProgressBar(pb, i, label=paste( round(i/total*100, 0),"% done"))
}

print(i)
write.table(outDat, file = "Y:/DepthData/bore_soil_depth_7_1.txt", row.names = F, col.names = T, sep=",")





