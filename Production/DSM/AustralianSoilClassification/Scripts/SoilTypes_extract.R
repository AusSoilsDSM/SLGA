##########################################################################
###       Packages
##########################################################################

library(jsonlite)
library(devtools)
#install_github("AusSoilsDSM/SoilDataFeder8R")
library(SoilDataFeder8R)
library(rgdal)

source('c:/PrivateInfo/PrivateInfo.R')

##########################################################################
###       Initialise
##########################################################################

avail.datasets<- fromJSON(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/DataSets?usr=", sdfUsr , "&key=", sdfKey))

lodfs <- vector("list", length(avail.datasets$DataSet))
for (i in 1:length(avail.datasets$DataSet)){
  nm1<- paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=O_PPF;O_GSG;O_ASC_ORD;O_ASC_SUBORD&DataSet=",avail.datasets$DataSet[i],"&usr=&key=")
  df <- fromJSON(nm1)
  print(head(df))
  lodfs[[i]] <- df
}

outDF = as.data.frame(data.table::rbindlist(lodfs))
odf <- outDF[outDF$ObservedProperty == 'O_ASC_ORD', ]
head(odf)


##########################################################################
###       Ross's Merge Function (from SoilDataFeder8r)
##########################################################################

#mergeObservedProperties
soilDF = outDF
### 
props <- unique(soilDF$ObservedProperty)

lodfs <- vector("list", length(props)-1)
outdf <-  soilDF[soilDF$ObservedProperty == props[1], ]
names(outdf)[names(outdf) == "Value"] <- props[1]
names(outdf)[names(outdf) == "Units"] <- paste0('Units_', props[1])
outdf <- outdf[, -10]
outdf <- as.data.frame(outdf, rownames(NULL))[ ,  c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate", "Longitude", "Latitude", "UpperDepth", "LowerDepth", props[1])]

for(i in 2:length(props)){
  # print(i)
  fdf <- as.data.frame(soilDF[soilDF$ObservedProperty == props[i], ])
  # outdf <- merge(outdf, lodfs[[i]], 
  #                by = c('DataStore','Dataset','Provider', 'Observation_ID', 'SampleID', 'SampleDate', 'Longitude', 'Latitude', 
  #                       'UpperDepth', 'LowerDepth', 'PropertyType',  'QualCollection', 'QualSpatialAgg'
  #                       , 'QualManagement', 'ExtractTime'), all = T, allow.cartesian=TRUE)
  
  
  jdf <- fdf[, c("DataStore", "Dataset", "Provider", "Observation_ID", "SampleID", "SampleDate", "Longitude", "Latitude", "UpperDepth", "LowerDepth", "Value")]
  mdf <- dplyr::full_join(outdf, jdf)
  names(mdf)[names(mdf) == "Value"] <- props[i]
  outdf <- mdf
}
###

##########################################################################
###       Trim and save
##########################################################################

#Relevant output from above is outdf!!!

outdf = outdf[, -which(colnames(outdf) %in% c("O_GSG", "O_PPF"))]
soilDF = outdf
soilDF = soilDF[-which(is.na(soilDF$O_ASC_ORD)),]


#create column of combined 
qqq = lapply(1:dim(soilDF)[1], function(i) {paste0(soilDF$O_ASC_ORD[i], '_', soilDF$O_ASC_SUBORD[i]) })
for (i in 1:dim(soilDF)[1]){soilDF$O_ASC_ORD_SUBORD[i] = qqq[[i]]}


saveRDS(soilDF, "//ternsoils/E/Harry/SoilTypes.rds")


bboxExt <- extent(covariates)
idxs <- which(soilDF$Longitude >= bboxExt@xmin & soilDF$Longitude <= bboxExt@xmax & soilDF$Latitude >= bboxExt@ymin & soilDF$Latitude <= bboxExt@ymax)
outdf <- soilDF[idxs, ]

coordinates(outdf)  <- ~Longitude+Latitude

#save as rds (for now)
saveRDS(outdf, "E:/Harry/NT_DSM_Training/SoilSiteData/Soil_Type.rds")


