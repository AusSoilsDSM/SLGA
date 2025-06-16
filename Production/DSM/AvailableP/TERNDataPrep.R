## Create predictor variables point dataset ###
##
## This script uses the TERN Soil Data Federator to create a point data set for model fitting (PData.csv)
## 
## Version 1.9 - 16/08/2022 - Fixed bugs in code and removed option to keep samples with depths > 0.3m
## Version 1.8 - 03/06/2022 - Added depth function for sites with only surface value
## Version 1.7 - 02/06/2022 - Added Bosch data from Jon Carter at DES
## Version 1.6 - 22/04/2022 - Add in option to retain P values from excluded sites with upper sample depths > 0.3 m
## Version 1.5 - 07/04/2022 - SDF changes Observation_ID to Location_ID, improved data clean up, BQ values not halved; migrate from sp to sf, remove duplicated sites in spline data
## Version 1.4 - 07/07/2021 - Add in extra PMAP data collected outside Queensland
## Version 1.3 - 02/07/2021 - Updated SoilDataFederator field names due to source changes 
## Version 1.2 - 03/08/2020 - Added extra Qld David Lawerance data
## Version 1.1 - 22/07/2020 - Added extra WA data
## Version 1.0 - 02/07/2020 - Original version - Author - P.R.Zund
##
## Follow these steps
## 1. Place the following files into working directory
##  - ALUMClassV8.csv
##  - clum_50m1220m.tif (Landuse data from ACLUMP)
##  - PData_SALI_060722.csv (Extra Qld SALI data)
##  - PMAP_NonQLD_sites.csv (Other PMAP data collected across Australia (not QLD) that is not in a state database)
##  - sa5_smpl_p_P_colwell_hm_220404.csv (Extra NSW data)
##  - Bosh_monitoring.csv (Extra data from John carter DAF data)
##  - DLData.csv (Extra data from David Lawerance DAF Qld)
##  - WA_data_20072022_PHCO3.csv from Dennis Van Gool (All Colwell P data held by DPIRD)
##
## 2. Set option to retain values at depths greater than 0.3 m regardless of land use by removing remarks on lines  238 - 239
##
## 3. Set the following variables
##
setwd("C:/Data/RWorking/SoilData") # Working directory
source("C:/Data/PrivateData.R") #Contains TERN Soil Data Federator User name
Property <- c("9B1","9B2", "9C1", "9C2") # Select what national lab data you want to retrieve (if more than one attribute separate by comma)
Bulk <- 2 # 2- The sample is from one location only eg a single soil core or pit; 1- Samples were combined/bulked in to one sample 0 - It is unknown if numerous samples were combined/bulked in to one sample   
ColQual <- 0 # 0 - all samples; 2 - all samples except those collected by untrained staff; 3 - all samples except those collected by untrained staff and Agronomists 
Location <- 0 # 0 - all samples;
Mgt <- 0 # 0 - all samples;
LUBuffer <- 100 # Landuse buffer in meters. Samples excluded if within excluded landuses
MaxDisturb <- 4 # Exclude samples from sites with a greater disturbance (Yellow Book disturbance p.128)
MaxP <- 1500 # Exclude samples with a greater Cowell P (mg / Kg)
MinP <- 7 # Retain samples with Cowell P less than
##
## Code starts here ##

#Libraries
library(jsonlite)
library(devtools)
#devtools::install_github("hrbrmstr/jsonview") #Must be connectted to private wireless network for this to work
library(jsonview)
library(plyr)
library(dplyr)
library(tidyr)
library(terra)
library(httr)

## Get national data (this step takes time, as it is requesting all of Aus data & it only works if not on dept network)
# This section of to extract SDF data written by Ross
usr <- sdfUsr
key <- sdfKey
# Get the available datasets
datasetsDF <- fromJSON(paste0('https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets?usr=', usr, '&key=', key))
write.csv(datasetsDF, 'c:/Temp/SDFdatasets.csv', row.names = F)
datasets <- datasetsDF$DataSet
p <- '9B2'
# Make an empty list to put individual results in for this property
res <- vector("list", length = length(datasets))
# Iterate Datasets
for (j in 1:length(datasets)) {
  d <- datasets[j]
  print(paste0(d, ' : ', p))
  url <-URLencode(paste0("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=", p ,"&DataSet=", d, "&format=json&usr=", usr, "&key=", key))
  resp <- GET(url, timeout(300))
  response <- content(resp, "text", encoding = 'UTF-8')
  odf <- fromJSON(response)
  
  # If there is data returned slot it into the list
  if(is.data.frame(odf)){
    if(nrow(odf)>0){
      res[[j]] <- odf
    }
  }
}

print("Getting national data, please wait")
soildat <- NA
for(i in 1:length(Property)) {
  url <- paste("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=", Property[i], "&usr=", User, "&key=", Key, sep = "")
  dat <- fromJSON(url)
  soildat <- rbind(soildat, dat)
  }
soildat <- subset(soildat, Longitude != 0) # Remove sites without coords (Mostly Qld obs)

## Get site disturbance info for each sample and add to soildat collation as a field
url <- paste("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=DIST_TYPE", "&usr=", User, "&key=", Key, sep = "")
disturbance <- fromJSON(url)
disturbance <- plyr::rename(disturbance, c(Value = "Dist_type"))

# Clean up data
disturbance <- subset(disturbance, Dist_type != "") # Null disturbance class
disturbance <- subset(disturbance, Latitude != 0) # Remove coords with 0
# Non-standard disturbance data conversion where possible
Dist_type <- unique(disturbance$Dist_type)
yb_dist_type <- c(7,5,4,6,1,2,3,0,8,NA,NA,1,1,1,0,0,0,1,NA,NA,2,1,1,NA,1,2,4,8,3,2,NA,NA,NA,3,4,4,4,8,6,NA,5,8,5,2,7,3,4,7,6,0,2,0,3,4,1,8,7,5,6)
dist_type_lookup <- data.frame(Dist_type, yb_dist_type)
disturbance <- join(disturbance, dist_type_lookup, by="Dist_type", type = "left")
disturbance <- subset(disturbance, !is.na(yb_dist_type)) # Null disturbance class
disturbance <- disturbance[,c(1:13, 15:21)] #remove old disturbance column
disturbance <- plyr::rename(disturbance, c(yb_dist_type="Dist_type"))

soildat <- join(soildat, disturbance, by = c("Location_ID"), type = "left", match = "first")
soildat <- soildat[,c(1:20, 39)]

## Get site ASC order info for each sample and add to soildat collation as a field
url <- paste("https://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=O_ASC_ORD", "&usr=", User, "&key=", Key, sep = "")
ASCOrder <- fromJSON(url)
ASCOrder <- plyr::rename(ASCOrder, c(Value = "O_ASC_ORD"))

# Data clean up
ASCOrder <- subset(ASCOrder, O_ASC_ORD != "") # Null disturbance class
ASCOrder <- subset(ASCOrder, Latitude != 0) # Remove coords with 0
# Non-standard ASC Order data conversion where possible
O_ASC_ORD <- c("VE","SO","DE","CH","FE","RU","KU","TE","KA","HY","PO","AN","CA","OR","Ferrosol","Podosol","Kurosol","Dermosol","Hydrosol","Chromosol","Kandosol","Vertosol","Tenosol","No Available Class","Class Undetermined","Sodosol","Rudosol","Organosol","Calcarosol","Anthroposol","RE")
asc_order_code <- c("VE","SO","DE","CH","FE","RU","KU","TE","KA","HY","PO","AN","CA","OR","FE","PO","KU","DE","HY","CH","KA","VE","TE","ZZ","YY","SO","RU","OR","CA","AN","RE")
ASC_lookup <- data.frame(O_ASC_ORD, asc_order_code)
ASCOrder <- join(ASCOrder, ASC_lookup, by="O_ASC_ORD", type = "left")
ASCOrder <- subset(ASCOrder, !is.na(asc_order_code)) # Null disturbance class
ASCOrder <- ASCOrder[,c(1:13, 15:21)] #remove old disturbance column
ASCOrder <- plyr::rename(ASCOrder, c(asc_order_code ="O_ASC_ORD"))

soildat <- join(soildat, ASCOrder, by = c("Location_ID"), type = "left", match = "first")
soildat <- soildat[,c(1:21, 40)]
save(soildat, file = "soildat.rds")
print("Data retrival complete")

## Clean and filter data
load("soildat.rds")

# Data quality filtering
#soildat <- soildat[,"QualCollection" >= ColQual]
#soildat <- soildat[,"QualSpatialAgg" >= Bulk]
#soildat <- soildat[,"QualManagement" >= Mgt]

# replace '-' with NA in Value column
soildat <- soildat %>% dplyr::mutate(Value = ifelse(Value == '-', NA, Value))

# delete sample if coords or Value is NA, Zero or have Latitude > -50 
soildat <- subset(soildat, !is.na(Longitude))
soildat <- subset(soildat, Longitude > 112)
soildat <- subset(soildat, Latitude > -44)
soildat <- subset(soildat, !is.na(Value))

## Add in missing QLD data
salidata <- read.csv("PData_SALI_060722.csv") #get data
# Clean up data
salidata <- subset(salidata, X != 0) #remove zero coords
salidata <- subset(salidata, VALUE > 0.49) # Well below quant, possibly incorrect units
salidata$VALUE[salidata$VALUE == 0.5] <- 1 # SALI numeric value for BQ results was half of BQ, that has now been changed to the BQ
# Add missing fields to match soildat
salidata$Location_ID <- paste("QLD", salidata$PROJECT_CODE, salidata$SITE_ID, salidata$OBS_NO, sep = "_")
salidata$DataStore <- "SALI"; salidata$Dataset <- "QLDGovernment"; salidata$Provider <- "QLDGovernment"
salidata$PropertyType <- "LaboratoryMeasurement"; salidata$Units <- "mg/kg"; salidata$QualCollection <- 3
salidata$QualSpatialAggregation <- 2; salidata$QualManagement <- 5; salidata$ExtractTime <- 06072022; salidata$QualSpatialAccuracy <- 0
salidata$Layer_ID <- NA
salidata <- plyr::rename(salidata, c(X = "Longitude", Y = "Latitude", VALUE = "Value", LAB_METH_CODE = "ObservedProperty", SAMPLEID = "SampleID", SAMPLEDATE = "SampleDate", UPPERDEPTH = "UpperDepth", LOWERDEPTH = "LowerDepth", DIST_TYPE = "Dist_type")) # adjust field names to match soildat
MissingQLDSites <- salidata[!(salidata$Location_ID %in% soildat$Location_ID),]
MissingQLDSites <- MissingQLDSites[,c(-3,-4,-5,-6,-7,-8,-16)]
soildat <- rbind(soildat, MissingQLDSites)

## Add in PMap data not in Queensland (SALI)
pmapdata <- read.csv("PMAP_NonQLD_sites.csv")
pmapdata$Location_ID <- paste("QLD", "PMAP", pmapdata$Site, pmapdata$Obs_id, sep = "_")
pmapdata$DataStore <- "EXCEL"; pmapdata$Dataset <- "QLDGovernment"; pmapdata$Provider <- "QLDGovernment"
pmapdata$PropertyType <- "LaboratoryMeasurement"; pmapdata$Units <- "mg/kg"; pmapdata$QualCollection <- 1
pmapdata$QualSpatialAggregation <- 2; pmapdata$QualManagement <- 2; pmapdata$ExtractTime <- date(); pmapdata$QualSpatialAccuracy <- 3
pmapdata$Dist_type <- NA; pmapdata$ObservedProperty <- "9B2"; pmapdata$O_ASC_ORD <- NA
pmapdata <- plyr::rename(pmapdata, c(Colwell = "Value", Sample_no = "SampleID", Date = "SampleDate", Horizon_no = "Layer_ID")) # adjust field names to match soildat
pmapdata <- pmapdata[,c(-1,-2,-3,-6,-10,-11,-12,-16,-17,-18,-19)]
pmapdata <- pmapdata[,c(9:12,1:3,5,4,6:7,13,21,8,14:17,19,18,20,22)]
soildat <- rbind(soildat, pmapdata)

## Add in extra NSW data (4/4/2022)
NSWdat <- read.csv("sa5_smpl_p_P_colwell_hm_220404.csv")
NSWdat$Location_ID <- paste("DPIE", NSWdat$Survey.number, NSWdat$Profile.ID, "1", sep = "_") # Concatenate Observation_ID
NSWdat <- plyr::rename(NSWdat, c(Boundupper = "UpperDepth", Boundlower = "LowerDepth",
                                 Samplenumber = "SampleID", N9B2 = "Value", Sampledate = "SampleDate")) # rename columns
NSWdat$DataStore <- "SALIS"; NSWdat$Dataset <- "NSWGovernment"; NSWdat$Provider <- "DPIE"; NSWdat$Layer_ID <- NA
NSWdat$PropertyType <- "LaboratoryMeasurement"; NSWdat$ObservedProperty <- "9B2";NSWdat$Units <- "mg/kg"
NSWdat$QualCollection <- 3; NSWdat$QualSpatialAggregation <- 2;NSWdat$QualManagement <- 5; NSWdat$QualSpatialAccuracy <- 3
NSWdat$ExtractTime <- "2022-04-04"; NSWdat$Dist_type <- NA; NSWdat$O_ASC_ORD <- NA
soildat <- rbind(soildat, NSWdat[,c(21:25,15,17,11,10,18:19,26:27,20,28:35)])

## Add in extra Qld data (Bosch & Kellner sites) from John Carter (5/4/2022)
Boschdat <- read.csv("Bosh_monitoring.csv")
Boschdat$Location_ID <- paste("DPI_Bosch", Boschdat$Site, "1", sep = "_") # Concatenate Observation_ID
Boschdat <- plyr::rename(Boschdat, c(LAT..dec. = "Latitude", LONG..dec. = "Longitude", ColwellP = "Value")) # rename columns
Boschdat$DataStore <- "EXCEL"; Boschdat$Dataset <- "QLDGovernment"; Boschdat$Provider <- "DES"; Boschdat$Layer_ID <- NA
Boschdat$SampleID <- 1; Boschdat$SampleDate <- "1989-00-00"; Boschdat$UpperDepth <- 0; Boschdat$LowerDepth <- 0.1
Boschdat$PropertyType <- "LaboratoryMeasurement"; Boschdat$ObservedProperty <- "9B2";Boschdat$Units <- "mg/kg"
Boschdat$QualCollection <- 3; Boschdat$QualSpatialAggregation <- 0;Boschdat$QualManagement <- 2; Boschdat$QualSpatialAccuracy <- 1
Boschdat$ExtractTime <- "2022-04-05"; Boschdat$Dist_type <- NA; Boschdat$O_ASC_ORD <- NA
soildat <- rbind(soildat, Boschdat[,c(5:11,3,2,12:15,4,16:23)])

## Add in WA P data that is not coming through the SDF
WAdat <- read.csv("WA_data_20072022_PHCO3.csv")
# Add missing fields to match soildat
WAdat$DataStore <- "WASDB"; WAdat$Dataset <- "WAGovernment"; WAdat$Provider <- "DennisVanGool"
WAdat$PropertyType <- "LaboratoryMeasurement"; WAdat$QualCollection <- 3
WAdat$QualSpatialAggregation <- 2; WAdat$QualManagement <- 5; WAdat$ExtractTime <- 20072022; WAdat$QualSpatialAccuracy <- 0
WAdat$Layer_ID <- NA; WAdat$Dist_type <- NA; WAdat$O_ASC_ORD <- NA
WAdat <- plyr::rename(WAdat, c(APSO = "Location_ID", LABSA_UNITS = "Units", O_LONGITUDE_GDA = "Longitude", O_LATITUDE_GDA = "Latitude", LAB_RESULT = "Value", LABM_CSIRO_CODE = "ObservedProperty", S_ID = "SampleID", S_DATE_DESC = "SampleDate", SAMP_UPPER_DEPTH = "UpperDepth", SAMP_LOWER_DEPTH = "LowerDepth")) # adjust field names to match soildat
MissingWASites <- WAdat[!(WAdat$Location_ID %in% soildat$Location_ID),]
MissingWASites <- MissingWASites[,c(-2,-3,-5,-12,-15,-16)]
soildat <- rbind(soildat, MissingWASites)

## Convert formatted values to numeric values in the Value field without losing the below quant info and the original formatted value
soildat$BQ <- NA
soildat$Formatted_Value <- soildat$Value
soildat$BQ <- ifelse(grepl('<', soildat$Value), "TRUE", "FALSE") 
soildat$Value <- as.numeric(gsub('<', '', soildat$Value)) # remove '<' from Values and convert Value from character to numeric  

## Convert Olsen P values to Cowell P (Moody_etal_2013)
olsen <- subset(soildat, ObservedProperty %in% c("9C1","9C2","9C2a") & Value > 1.0213 & O_ASC_ORD != "CA")
olsen$Value <- olsen$Value*2.869-2.93 # convert only values > 2.93/2.869 to avoid negative numbers
olsen$ObservedProperty <- "9B2c" # re-label converted values to 9B2c
alldata <- rbind(soildat,olsen)
soildat <- subset(alldata, !(ObservedProperty %in% c("9C1", "9C2", "9C2a"))) # remove non-converted Olsen values

# Round up below quant values (Colwell P < 1 mg / kg) and remove decimal places
soildat$Value <- ifelse(soildat$Value < 1, 1, soildat$Value)
soildat$Value <- round(soildat$Value, 0) # Round all P values to zero decimal places
print("Data cleaning, filtering and conversion complete")

# Exclude sample based on site disturbance (disturbance at time of sampling) 
ExcludedSitesDisturb <- subset(soildat, Value > MinP & Dist_type > MaxDisturb)

## For sites without a SALI disturbance type (Dist_type like NA) use ALUM land use data to determine if sample is to be excluded 
# Select those with no disturbance record
soildat2 <- subset(soildat, is.na(Dist_type))

# Add landuse info to soildat2
print("Checking against landuse data")
.rs.unloadPackage("tidyr")
library(raster)
#library(sp)
library(sf)
library(terra)
alum <- raster("clum_50m1220m.tif") # Landuse raster import crs = 3577 GDA94 Albers
spts <- st_as_sf(soildat2,coords = c("Longitude", "Latitude"), crs = 4326) #Step1. Convert points to a SpatialPoints sf object
sptsalbers <- st_transform(spts, crs = st_crs(3577)) #Step 2. Transform object from WGS84 (EPSG:4326) to GDA94 Albers (EPSG:3577)
#SampLUAlbers <- terra::extract(alum, sptsalbers, buffer = LUBuffer, fun=max, df = TRUE, small = TRUE, sp = TRUE) # Step 3. Determine landuse at each sample site with a buffer
SampLUAlbers <- terra::extract(alum, sptsalbers, fun=max, df = TRUE, small = TRUE, sp = TRUE) # Step 3. Determine landuse at each sample site
SampLU <- as.data.frame(SampLUAlbers) # Step 5. Convert from Spatial Points to a data frame
SampLU <- plyr::rename(SampLU, c(clum_50m1220m = "clum"))
SampLU <- join(SampLU, soildat2[, c("Location_ID","Longitude","Latitude")], by = "Location_ID", type = "left", match = "first") #Add geographic coordinates back in
save(SampLU, file = "SampLU.rds")
load("SampLU.rds")

# Prepare land use classification data
alumclass <- read.csv("ALUMClassV8.csv")
require(stringr)
alumclass$clum <- str_remove(alumclass$Tertiary, "[.]") # Convert Tertiary land use code format i.e. from 2.1.0 to 210
alumclass$clum <- str_remove(alumclass$clum, "[.]") # Convert Tertiary land use code format i.e. from 2.1.0 to 210

# Add Fertiliser attribute to sample landuse layer (SampLU) based on lookup table (alumclass)
fertareas <- join(SampLU, alumclass, by = "clum", type = "left", match = "all")

# Remove samples based on landuse (But leave in samples with low Cowell P values)
ExcludedSitesLU <- subset(fertareas, FertYN == 2 & Value > MinP )
print("Land use check complete")

## Combine excluded sites based on SALI Site Disturbance and Land use
ExcludedSites <- rbind(ExcludedSitesLU[,c(1:7,26:27,8:22)], ExcludedSitesDisturb[,1:24])

## Remove excluded sites from original list of sites
FinalSites <- soildat %>% anti_join(ExcludedSites)

# Add in extra Qld sites from David Lawrence from spreadsheet
dldata <- read.csv("DLData2.csv")
dldata$Location_ID <- paste("QLD_DL_", dldata$ID, "_1", sep = "") # Concatentate Observation_ID
dldata <- plyr::rename(dldata, c(X = "Longitude", Y = "Latitude")) # rename X and Y column
dldata0_10 <- dldata[,c(2:4,31)] # Dataset with 0 to 10cm Colwell P
dldata0_10$UpperDepth <- 0; dldata0_10$LowerDepth <- 0.1; dldata0_10$SampleID <- 1 # Add depth columns
dldata0_10 <- plyr::rename(dldata0_10, c(ColP0.10 = "Value")) # Rename value column
dldata10_30 <- dldata[,c(2:3,5,31)] # Dataset with 10 to 30cm Colwell P
dldata10_30$UpperDepth <- 0.1; dldata10_30$LowerDepth <- 0.3; dldata10_30$SampleID <- 2 # Add depth columns
dldata10_30 <- plyr::rename(dldata10_30, c(ColP10.30 = "Value")) # Rename value column
dldata <- na.omit(rbind(dldata0_10,dldata10_30)) # Combine surface and sub-surface samples
dldata$DataStore <- "Project"; dldata$Dataset <- "QLDGovernment"; dldata$Provider <- "DAF"
dldata$SampleDate <- NA; dldata$PropertyType <- "LaboratoryMeasurement"; dldata$ObservedProperty <- "9B2";
dldata$Units <- "mg/kg"; dldata$QualCollection <- 2; dldata$QualSpatialAggregation <- 0;
dldata$QualManagement <- 2; dldata$ExtractTime <- "2020-08-03T16:48:00"; dldata$Dist_type <- NA; dldata$O_ASC_ORD <- NA
dldata$BQ <- FALSE; dldata$Formatted_Value <- dldata$Value; dldata$QualSpatialAccuracy <- 3; dldata$Layer_ID <- 1
FinalSites <- rbind(FinalSites, dldata) # Add extra Qld sites to FinalSites
save(FinalSites, file = "FinalSites.rds")
load("FinalSites.rds")
write.csv(FinalSites,file = "FinalSitesDetails.csv",row.names = FALSE) # This file contains the original depth data before splining

## Apply the depth function for sites with only a surface P Value
# Select sites with only surface value. Do not include sites with single samples at other depths
count1 <- FinalSites %>% group_by(Location_ID) %>% tally() #Count number of depths per site
surfacedata <- merge(FinalSites, subset(count1,n==1), by = c("Location_ID"))
surfacedata <- subset(surfacedata, UpperDepth <= 0.05)
# Interpolate P at standard LRA sampling depths so we can compare with real standard sampling depth
intdepths <- c(0.25,0.55,0.85,1.15,1.45,2) #Interpolation depth
intupper <- c(0.2,0.5,0.8,1.1,1.4,1.9) #Theoretical upper depth of sample
intlower <- c(0.3,0.6,0.9,1.2,1.5,2) #Theoretical lower depth of sample
presult <- data.frame(Location_ID = NA, UpperDepth = NA, LowerDepth = NA, Value = NA)
for(p in 1:nrow(surfacedata)){
  ps <- surfacedata[p,14]
  ID <- surfacedata[p,1]
  inresult <- data.frame(Location_ID = NA, UpperDepth = NA, LowerDepth = NA, Value = NA)
  interresult <- data.frame(Location_ID = NA, UpperDepth = NA, LowerDepth = NA, Value = NA)
  for(d in 1:6){
    interresult$Value <- 1+ps*0.007698*intdepths[d]^-1.59 #Based on analysis of all profiles (n=997) with > 2 samples down the profile in Aust. Significant to 0.05 (95% two tail CI), pr = 0.0324, T-test = 4.11, se = 0.001785, ps range 1 to 321
    interresult$Location_ID <- ID; interresult$UpperDepth <- intupper[d]
    interresult$LowerDepth <- intlower[d]
    inresult <- rbind(inresult, interresult)
    }
 presult <- rbind(presult, inresult)
}
# Prepare interpolated P results to append to real data (FinalSites)
presult <- na.omit(presult) #Remove NA records
presult <- merge(presult,FinalSites[,c(1,8:9,19)]) # Add coord info
# Other required fields
presult$DataStore <- "None"; presult$Dataset <- "InterpolatedP"; presult$Provider <- "PZ"
presult$SampleDate <- NA; presult$PropertyType <- "LaboratoryMeasurement"; presult$ObservedProperty <- "9B2";
presult$Units <- "mg/kg"; presult$QualCollection <- NA; presult$QualSpatialAggregation <- NA;
presult$QualManagement <- NA; presult$ExtractTime <- date(); presult$Dist_type <- NA; presult$O_ASC_ORD <- NA
presult$BQ <- FALSE; presult$Formatted_Value <- presult$Value; presult$Layer_ID <- NA
presult$SampleID <- NA
# Add interp P to FinalSites
FinalSites <- rbind(FinalSites, presult) 

### Spline sites with more than one depth interval
#install_bitbucket("brendo1001/ithir/pkg")
library(ithir)
library(dplyr)
horizon <- FinalSites[,c("Location_ID", "UpperDepth", "LowerDepth", "Value")] # Select data to spline
count1 <- horizon %>% group_by(Location_ID) %>% tally() #Count number of depths per site
morethan1 <- subset(count1, n > 1) #Remove sites with only one analysed depth
horizon1 <- merge(horizon, morethan1, by = c("Location_ID"))
horizon1 <- aggregate(x=horizon1$Value,by=list(Location_ID=horizon1$Location_ID, UpperDepth=horizon1$UpperDepth, LowerDepth=horizon1$LowerDepth),FUN = min) #Remove duplicates
horizon1$UpperDepth <- horizon1$UpperDepth*100 #Convert depths from m to cm
horizon1$LowerDepth <- horizon1$LowerDepth*100
horizon1 <- na.omit(setNames(horizon1, c("Soil.ID", "Upper.Boundary", "Lower.Boundary", "Value"))) #adjusted column names and remove nulls (column names adjusted to ea_spline function requirement)
splineout <- ithir::ea_spline(horizon1, var.name = "Value", lam = 0.1, d = t(c(0,5,15,30,60,100,200)), vlow = 0, vhigh = MaxP, show.progress=TRUE)
harmondat <- splineout[["harmonised"]] #Harmonised data
harmondat <- setNames(harmondat, c("ID","0-5cm","5-15cm","15-30cm","30-60cm","60-100cm","100-200cm","SoilDepth")) #remove spaces from column names 

# Add in sites for which we have single depths and determine sample interval midpoint
only1 <- subset(count1, n < 2)
horizon2 <- join(horizon, only1, by = c("Location_ID"), type = "right")
horizon2$midpoint <- round((horizon2$UpperDepth + horizon2$LowerDepth)/2, 2)

# Add 0 - 0.05m data
horizon2_5 <- subset(horizon2, midpoint <= 0.05)
if(nrow(horizon2_5)>0){
horizon2_5 <- horizon2_5[,c("Location_ID", "Value")]
horizon2_5 <- setNames(horizon2_5, c("ID", "0-5cm"))
horizon2_5$"5-15cm" <- NA; horizon2_5$"15-30cm" <- NA; horizon2_5$"30-60cm" <- NA; horizon2_5$"60-100cm" <- NA; horizon2_5$"100-200cm" <- NA; horizon2_5$SoilDepth <- NA
harmondat <- rbind(harmondat, horizon2_5)}

# Add 0.5 - 0.15m data
horizon2_15 <- subset(horizon2, midpoint > 0.05 & midpoint <=0.1)
if(nrow(horizon2_15)>0){
  horizon2_15 <- horizon2_15[,c("Location_ID", "Value")]
  horizon2_15 <- setNames(horizon2_15, c("ID", "5-15cm"))
  horizon2_15$"0-5cm" <- NA; horizon2_15$"15-30cm" <- NA; horizon2_15$"30-60cm" <- NA; horizon2_15$"60-100cm" <- NA; horizon2_15$"100-200cm" <- NA; horizon2_15$"SoilDepth" <- NA
  harmondat <- rbind(harmondat, horizon2_15)}

# Add 0.15 - 0.3m data
horizon2_30 <- subset(horizon2, midpoint > 0.1 & midpoint <=0.25)
if(nrow(horizon2_30)>0){
horizon2_30 <- horizon2_30[,c("Location_ID", "Value")]
horizon2_30 <- setNames(horizon2_30, c("ID", "15-30cm"))
horizon2_30$"0-5cm" <- NA; horizon2_30$"5-15cm" <- NA; horizon2_30$"30-60cm" <- NA; horizon2_30$"60-100cm" <- NA; horizon2_30$"100-200cm" <- NA; horizon2_30$"SoilDepth" <- NA
harmondat <- rbind(harmondat, horizon2_30)}

# Add 0.3 - 0.6m data
horizon2_60 <- subset(horizon2, midpoint > 0.25 & midpoint <=0.55)
if(nrow(horizon2_60)>0){
horizon2_60 <- horizon2_60[,c("Location_ID", "Value")]
horizon2_60 <- setNames(horizon2_60, c("ID", "30-60cm"))
horizon2_60$"0-5cm" <- NA; horizon2_60$"5-15cm" <- NA; horizon2_60$"15-30cm" <- NA; horizon2_60$"60-100cm" <- NA; horizon2_60$"100-200cm" <- NA; horizon2_60$"SoilDepth" <- NA
harmondat <- rbind(harmondat, horizon2_60)}

# Add 0.6 - 1m data
horizon2_100 <- subset(horizon2, midpoint > 0.55 & midpoint <=0.95)
if(nrow(horizon2_100)>0){
horizon2_100 <- horizon2_100[,c("Location_ID", "Value")]
horizon2_100 <- setNames(horizon2_100, c("ID", "60-100cm"))
horizon2_100$"0-5cm" <- NA; horizon2_100$"5-15cm" <- NA; horizon2_100$"15-30cm" <- NA; horizon2_100$"30-60cm" <- NA; horizon2_100$"100-200cm" <- NA; horizon2_100$"SoilDepth" <- NA
harmondat <- rbind(harmondat, horizon2_100)}

# Add 1 - 2m data
horizon2_200 <- subset(horizon2, midpoint > 0.95 & midpoint <= 1.95)
if(nrow(horizon2_200)>0){
horizon2_200 <- horizon2_200[,c("Location_ID", "Value")]
horizon2_200 <- setNames(horizon2_200, c("ID", "100-200cm"))
horizon2_200$"0-5cm" <- NA; horizon2_200$"5-15cm" <- NA; horizon2_200$"15-30cm" <- NA; horizon2_200$"30-60cm" <- NA; horizon2_200$"60-100cm" <- NA; horizon2_200$"SoilDepth" <- NA 
harmondat <- rbind(harmondat, horizon2_200)}
print("Harmonization of depth data complete")

#Clean up after splining
harmondat[harmondat==-9999] <- NA #Replace No data Value '-9999' with NA
harmondat[harmondat<=1] <- 1 # convert values <= 0 to 1 mg / kg P

# Add coords back into data
harmondat <- plyr::rename(harmondat, c(ID = "Location_ID"))
splinedandsitedat <- join(harmondat, FinalSites, by = "Location_ID", type = "left", match = "first")

# Create a csv with disturbance details for analyst purposes
splined_with_disturb <- subset(splinedandsitedat, select = c("Location_ID", "Dist_type", "Longitude", "Latitude", "0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"))
modeldat <- subset(splinedandsitedat, select = c("Location_ID", "Longitude", "Latitude", "0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"))
#FinalSites <- FinalSites[!duplicated(FinalSites[,c("Longitude","Latitude")]),] # Remove sites which have differnet ID's but same location, only want one value per location. Current solution does not select a specfic 0-5 cm value, would prefer to select the small value of P
modeldat <- modeldat %>% 
  group_by(Longitude, Latitude) %>% 
  slice_min(`0-5cm`) %>% 
  ungroup() # Remove sites with >1 site on the same location, pick site with lowest 0-5cm value

# Retrieve QG training data points for Forage Report
qgdat <- subset(splinedandsitedat, Provider %in% c("QLDGovernment", "CSIRO"), select=c('Location_ID','Longitude','Latitude','0-5cm'))
qgdat <- qgdat %>% 
  group_by(Longitude, Latitude) %>% 
  slice_min(`0-5cm`) %>% 
  ungroup() # Remove sites with >1 site on the same location, pick site with lowest 0-5cm value
write.csv(qgdat[1:3],"SALISitesUsed.csv", row.names = FALSE)

# Get statistical summary data of final sites 
summarydat <- summary(modeldat[,c(4:9)])
write.csv(summarydat, "SummaryData.csv")

## Create input file for model  
sqrtdat <- cbind(modeldat[,c(2,3,1)], sqrt((modeldat[, 4:9]))) # Transform data using square root 
logdat <- cbind(modeldat[,c(2,3,1)], log((modeldat[, 4:9]+ 0.01))) # Transform data using square root + 0.01 (add 0.01 to prevent log(0) which is infinity) 
indat <- tibble::rowid_to_column(modeldat, "ID") # Create a numeric ID index from the row number
indat <- indat[,-2] # Delete old id column
indat <- plyr::rename(indat, c(Longitude = "X", Latitude = "Y"))
indat <- indat[, c(2,3,1,4:9)]
indat <- subset(indat, X != 0) # Remove sites with no coords

## Export data
write.csv(ExcludedSites,file = "ExcludedSites.csv",row.names = FALSE)
write.csv(splined_with_disturb,file = "FinalSites_with_disturbance_data.csv",row.names = FALSE)
write.csv(splinedandsitedat, file = "FinalSites_ColwellP.csv",row.names = FALSE)
write.csv(indat, "PData.csv", row.names = FALSE)
write.csv(indat[1:3], "PDataForForage.csv", row.names = FALSE)

print("End of processing")
#### End of script ####
