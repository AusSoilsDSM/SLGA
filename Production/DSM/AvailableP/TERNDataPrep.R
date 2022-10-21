## Create predictor variable point dataset ###
##
## This script uses the TERN Soil Data Federator to create a point data set for model fitting (PData.csv)
## Version 1.2 - 03/08/2020 - Extra Qld David Lawerance data added in
## Version 1.1 - 22/07/2020 - Extra WA data added in
## Version 1 - 02/07/2020 - Orginal version - Author - P.R.Zund
##
## Follow these steps
## 1. Place the following files into working directory
##  - ALUMClassV8.csv (Supplied with R script)
##  - clum_50m1218m.tif (Supplied with R script)
##  - salidata.csv (This is a temporary fix of Qld. data problems)
##  - ExcludedWASites2.csv (These are WA sites excluded using WA land use categories, SEL 1 or 2)
##  - IncludedWASites.csv (These are WA sites using WA land use categories SEL = 3 to 5)
##
## 2. Set the following variables
##
setwd("M:\\Projects\\PMap\\SoilData\\TERN") # Working directory
User <- "email" # TERN Soil Data Federator User name
Key <- "password" # TERN Soil Data Federator password
Property <- c("9B1","9B2", "9C1", "9C2") # Select what national lab data you want to retrieve (if more than one attribute separate by comma)
Bulk <- 2 # 1- bulk samples only; 2- Profile samples (0 - unknown sample aggregation info)   
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
library(httr)
library(htmltidy)
library(XML)
library(xml2)
library(devtools)
#devtools::install_github("hrbrmstr/jsonview")
library(jsonview)
library(plyr)
library(tidyr)

## Get national data (this step takes time, as it is requesting all of Aus data & it only works if not on dept network)
print("Getting national data, please wait")
soildat <- NA
for(i in 1:length(Property)) {
  url <- paste("http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=", Property[i], "&usr=", User, "&key=", Key, sep = "")
  dat <- fromJSON(url)
  soildat <- rbind(soildat, dat)
  }

## Get site disturbance info for each sample and add to soildat collation as a field
url <- paste("http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=DIST_TYPE", "&usr=", User, "&key=", Key, sep = "")
disturbance <- fromJSON(url)
disturbance <- plyr::rename(disturbance, c(Value = "Dist_type"))
soildat <- join(soildat, disturbance, by = c("Observation_ID"), type = "left", match = "first")
soildat <- soildat[,c(1:18, 30)]

## Get site ASC order info for each sample and add to soildat collation as a field
url <- paste("http://esoil.io/TERNLandscapes/SoilDataFederatoR/R/SoilDataAPI/SoilData?observedProperty=O_ASC_ORD", "&usr=", User, "&key=", Key, sep = "")
ASCOrder <- fromJSON(url)
ASCOrder <- plyr::rename(ASCOrder, c(Value = "O_ASC_ORD"))
soildat <- join(soildat, ASCOrder, by = c("Observation_ID"), type = "left", match = "first")
soildat <- soildat[,c(1:19, 31)]

print("Data retrival complete")

## Clean and filter data

# Data quality filtering
soildat <- soildat[,"QualCollection" >= ColQual]
soildat <- soildat[,"QualSpatialAgg" >= Bulk]
soildat <- soildat[,"QualManagement" >= Mgt]

# replace '-' with NA in Value column
soildat <- soildat %>% mutate(Value = ifelse(Value == '-', NA, Value))

# delete sample if coords are NA, Zero or have Latitude < -80 
soildat <- subset(soildat, !is.na(Longitude))
soildat <- subset(soildat, Longitude != 0)
soildat <- subset(soildat, Latitude > -50)

## Add in missing QLD data
salidata <- read.csv("salidata.csv") #get data
# add missing fields to match soildat
salidata$Observation_ID <- paste("QLD", salidata$PROJECT_CODE, salidata$SITE_ID, salidata$OBS_NO, sep = "_")
salidata$DataStore <- "SALI"; salidata$Dataset <- "QLDGovernment"; salidata$Provider <- "QLDGovernment"
salidata$PropertyType <- "LaboratoryMeasurement"; salidata$Units <- "mg/kg"; salidata$QualCollection <- 3
salidata$QualSpatialAgg <- 2; salidata$QualManagement <- 5; salidata$ExtractTime <- date()
salidata <- plyr::rename(salidata, c(X = "Longitude", Y = "Latitude", VALUE = "Value", OBSERVEDPROPERTY = "ObservedProperty", SAMPLEID = "SampleID", SAMPLEDATE = "SampleDate", UPPERDEPTH = "UpperDepth", LOWERDEPTH = "LowerDepth", DIST_TYPE = "Dist_type")) # adjust field names to match soildat
MissingQLDSites <- salidata[!(salidata$Observation_ID %in% soildat$Observation_ID),]
MissingQLDSites <- MissingQLDSites[,c(-3,-4,-7,-8)]
soildat <- rbind(soildat, MissingQLDSites)

# Remove WA sites where LU SEL = 1 or 2 as recorded in spreadsheet supplied by Dennis Vangool (2/7/2020)
wadata <- read.csv("ExcludedWASites2.csv") #get data
RemovedWASites <- wadata[(wadata$ID %in% soildat$Observation_ID),]
soildat <- soildat[!(soildat$Observation_ID %in% wadata$ID),]

# Convert formatted values to numeric values in the Value field without losing the below quant info and the orginal formatted value
soildat$BQ <- NA
soildat$Formatted_Value <- soildat$Value
soildat$BQ <- ifelse(grepl('<', soildat$Value), "TRUE", "FALSE") 
soildat$Value <- as.numeric(gsub('<', '', soildat$Value)) # remove '<' from Values and convert Value from character to numeric  
soildat <- soildat %>% mutate(Value = ifelse(BQ == TRUE, round(Value/2, 0), Value)) # halve results that are BQ and round to zero decimal places 

## Convert Olsen P values to Cowell P (Moody_etal_2013)
soildat$Value <- as.numeric(soildat$Value) # convert Value field from character to number
soildat <- soildat %>% mutate(Value = ifelse(ObservedProperty == "9C1" & Value > 1.0213 & O_ASC_ORD != "CA", Value*2.869-2.93, Value)) # convert only values > 2.93/2.869 to avoid negative numbers
soildat <- soildat %>% mutate(Value = ifelse(ObservedProperty == "9C2" & Value > 1.0213 & O_ASC_ORD != "CA", Value*2.869-2.93, Value)) # convert only values > 2.93/2.869 to avoid negative numbers
soildat <- soildat %>% mutate(Value = ifelse(ObservedProperty == "9C2a" & Value > 1.0213 & O_ASC_ORD != "CA", Value*2.869-2.93, Value)) # convert only values > 2.93/2.869 to avoid negative numbers
# re-label converted values to 9B2c
soildat <- soildat %>% mutate(ObservedProperty = ifelse(ObservedProperty == "9C1" & Value > 1.0213 & O_ASC_ORD != "CA", "9B2c", ObservedProperty)) # convert only values > 2.93/2.869 to avoid negative numbers
soildat <- soildat %>% mutate(ObservedProperty = ifelse(ObservedProperty == "9C2" & Value > 1.0213 & O_ASC_ORD != "CA", "9B2c", ObservedProperty)) # convert only values > 2.93/2.869 to avoid negative numbers
soildat <- soildat %>% mutate(ObservedProperty = ifelse(ObservedProperty == "9C2a" & Value > 1.0213 & O_ASC_ORD != "CA", "9B2c", ObservedProperty)) # convert only values > 2.93/2.869 to avoid negative numbers
soildat <- subset(soildat, ObservedProperty %in% c("9B1", "9B2", "9B2c", "9B-9C")) # remove non-converted Olsen values

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
library(sp)
alum <- raster("clum_50m1218m.tif") # Landuse raster import crs = 3577 GDA94 Albers
sp <- SpatialPointsDataFrame(soildat2[, c("Longitude", "Latitude")], soildat2, match.ID = TRUE, proj4string=CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")) # Step1. Convert points to a SpatialPoints object
spalbers <- spTransform(sp, CRSobj = "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") #Step 2. Transform object from WGS84 to GDA94 Albers (3577)
SampLUAlbers <- extract(alum, spalbers, buffer = LUBuffer, fun=max, df = TRUE, small = TRUE, sp = TRUE) # Step 3. Determine landuse at each sample site
#SampLUAlbers <- extract(alum, spalbers, df = TRUE, sp = TRUE) # Step 3. Determine landuse at each sample site
SampLU <- as.data.frame(SampLUAlbers) # Step 5. Convert from Spatial Points to a data frame
SampLU <- plyr::rename(SampLU, c(clum_50m1218m = "clum"))

# Prepare land use classification data
alumclass <- read.csv("ALUMClassV8.csv")
require(tidyverse)
alumclass$clum <- str_remove(alumclass$Tertiary, "[.]") # Convert Tertiary land use code format i.e. from 2.1.0 to 210
alumclass$clum <- str_remove(alumclass$clum, "[.]") # Convert Tertiary land use code format i.e. from 2.1.0 to 210

# Add Fertiliser attribute to sample landuse layer (SampLU) based on lookup table (alumclass)
fertareas <- join(SampLU, alumclass, by = "clum", type = "left", match = "all")

# Remove samples based on landuse (But leave in samples with low Cowell P values)
ExcludedSitesLU <- subset(fertareas, FertYN == 2 & Value > MinP )
print("Land use check complete")

## Add in excluded sites based on SALI Site Disturbance
ExcludedSites <- rbind(ExcludedSitesLU[,1:22], ExcludedSitesDisturb)

## Remove excluded sites from orginal list of sites
FinalSites <- soildat[!(soildat$Observation_ID %in% ExcludedSites$Observation_ID),] 

# Add in extra Qld sites from David Lawrence from spreadsheet
dldata <- read.csv("DLData.csv")
dldata$Observation_ID <- paste("QLD_DL_", dldata$ID, "_1", sep = "") # Concatentate Observation_ID
dldata <- plyr::rename(dldata, c(X = "Longitude", Y = "Latitude")) # rename X and Y column
dldata0_10 <- dldata[,c(2:3,13,40)] # Dataset with 0 to 10cm Colwell P
dldata0_10$UpperDepth <- 0; dldata0_10$LowerDepth <- 0.1; dldata0_10$SampleID <- 1 # Add depth columns
dldata0_10 <- plyr::rename(dldata0_10, c(ColP0.10 = "Value")) # Rename value column
dldata10_30 <- dldata[,c(2:3,14,40)] # Dataset with 0 to 10cm Colwell P
dldata10_30$UpperDepth <- 0.1; dldata10_30$LowerDepth <- 0.3; dldata10_30$SampleID <- 2 # Add depth columns
dldata10_30 <- plyr::rename(dldata10_30, c(ColP10.30 = "Value")) # Rename value column
dldata <- na.omit(rbind(dldata0_10,dldata10_30)) # Combine surface and sub-surface samples
dldata$DataStore <- "Project"; dldata$Dataset <- "QLDGovernment"; dldata$Provider <- "DAF"
dldata$SampleDate <- NA; dldata$PropertyType <- "LaboratoryMeasurement"; dldata$ObservedProperty <- "9B2";
dldata$Units <- "mg/kg"; dldata$QualCollection <- 2; dldata$QualSpatialAgg <- 0;
dldata$QualManagement <- 2; dldata$ExtractTime <- "2020-08-03T16:48:00"; dldata$Dist_type <- NA; dldata$O_ASC_ORD <- NA
dldata$BQ <- FALSE; dldata$Formatted_Value <- dldata$Value
FinalSites <- rbind(FinalSites, dldata) # Add extra Qld sites to FinalSites

# Add in extra WA sites from spreadsheet supplied by Dennis Vangool (Dennis Vangool) with LU SEL = 3 to 5
wadata <- read.csv("extra_sites_depths.csv") #get data
ExtraWASites <- wadata[!(wadata$ID %in% soildat$Observation_ID),] # Find sites not already in soildat
WASites <- ExtraWASites[c(1,4,12,13)] # get these fields - Observation_ID, SampleID, Longitude, Latitude
WASites$DataStore <- "ASRIS"; WASites$Dataset <- "WAGovernment"; WASites$Provider <- "WAGovernment"
WASites$SampleDate <- NA; WASites$UpperDepth <- ExtraWASites$SAMP_UPPER_DEPTH/100; WASites$LowerDepth <- ExtraWASites$SAMP_LOWER_DEPTH/100; 
WASites$PropertyType <- "LaboratoryMeasurement"; WASites$ObservedProperty <- ExtraWASites$Method; WASites$Value <- ExtraWASites$P_any_
WASites$Units <- "mg/kg"; WASites$QualCollection <- NA; WASites$QualSpatialAgg <- 0;
WASites$QualManagement <- 5; WASites$ExtractTime <- "2020-07-08T13:48:00"; WASites$Dist_type <- NA; WASites$O_ASC_ORD <- NA
WASites$BQ <- FALSE; WASites$Formatted_Value <- WASites$Value
# Convert Olsen P values to Cowell P (Moody_etal_2013)
WASites <- WASites %>% mutate(Value = ifelse(ObservedProperty == "9C2" & Value > 1.0213, Value*2.869-2.93, Value)) # convert only values > 2.93/2.869 to avoid negative numbers
WASites$ObservedProperty <- as.character(WASites$ObservedProperty) # re-label does converted to 9B2c
WASites <- WASites %>% mutate(ObservedProperty = ifelse(ObservedProperty == "9C2" & Value > 1.0213, "9B2c", ObservedProperty)) # convert only values > 2.93/2.869 to avoid negative numbers
WASites$Value <- round(WASites$Value, 0) # Round all P values to zero decimal places
WASites <- WASites[c(1,5:7,2,8,3,4,9:22)] # Reorder columns to match FinalSites df
WASites <- setNames(WASites,names(FinalSites)) # Rename columns to match FinalSites df
FinalSites <- rbind(FinalSites, WASites) # Add sites to FinalSites
write.csv(FinalSites,file = "FinalSites.csv",row.names = FALSE) # This file contains the orginal depth data before splining

### Spline sites with more than one depth interval
library(ithir)
library(tidyverse)
horizon <- FinalSites[,c("Observation_ID", "UpperDepth", "LowerDepth", "Value")] # Select data to spline
count1 <- horizon %>% group_by(Observation_ID) %>% tally() #Count number of depths per site
morethan1 <- subset(count1, n > 1) #Remove sites with only one analysed depth
horizon1 <- merge(horizon, morethan1, by = c("Observation_ID"))
horizon1$UpperDepth <- horizon1$UpperDepth*100 #Convert depths from m to cm
horizon1$LowerDepth <- horizon1$LowerDepth*100
horizon1 <- na.omit(setNames(horizon1, c("Soil.ID", "Upper.Boundary", "Lower.Boundary", "Value"))) #adjusted column names and remove nulls (column names adjusted to ea_spline function requirement)
splineout <- ithir::ea_spline(horizon1, var.name = "Value", lam = 0.1, d = t(c(0,5,15,30,60,100,200)), vlow = 0, vhigh = MaxP, show.progress=TRUE)
harmondat <- splineout[["harmonised"]] #Harmonised data
harmondat <- setNames(harmondat, c("ID","0-5cm","5-15cm","15-30cm","30-60cm","60-100cm","100-200cm","SoilDepth")) #remove spaces from column names 

# Add in sites for which we have single depths and determine sample interval midpoint
only1 <- subset(count1, n < 2)
horizon2 <- join(horizon, only1, by = c("Observation_ID"), type = "right")
horizon2$midpoint <- round((horizon2$UpperDepth + horizon2$LowerDepth)/2, 2)

# Add 0 - 0.05m data
horizon2_5 <- subset(horizon2, midpoint <= 0.05)
horizon2_5 <- horizon2_5[,c("Observation_ID", "Value")]
horizon2_5 <- setNames(horizon2_5, c("ID", "0-5cm"))
horizon2_5$"5-15cm" <- NA; horizon2_5$"15-30cm" <- NA; horizon2_5$"30-60cm" <- NA; horizon2_5$"60-100cm" <- NA; horizon2_5$"100-200cm" <- NA; horizon2_5$SoilDepth <- NA
harmondat <- rbind(harmondat, horizon2_5)

# Add 0.5 - 0.15m data
horizon2_15 <- subset(horizon2, midpoint > 0.05 & midpoint <=0.1)
horizon2_15 <- horizon2_15[,c("Observation_ID", "Value")]
horizon2_15 <- setNames(horizon2_15, c("ID", "5-15cm"))
horizon2_15$"0-5cm" <- NA; horizon2_15$"15-30cm" <- NA; horizon2_15$"30-60cm" <- NA; horizon2_15$"60-100cm" <- NA; horizon2_15$"100-200cm" <- NA; horizon2_15$"SoilDepth" <- NA
harmondat <- rbind(harmondat, horizon2_15)

# Add 0.15 - 0.3m data
horizon2_30 <- subset(horizon2, midpoint > 0.1 & midpoint <=0.25)
horizon2_30 <- horizon2_30[,c("Observation_ID", "Value")]
horizon2_30 <- setNames(horizon2_30, c("ID", "15-30cm"))
horizon2_30$"0-5cm" <- NA; horizon2_30$"5-15cm" <- NA; horizon2_30$"30-60cm" <- NA; horizon2_30$"60-100cm" <- NA; horizon2_30$"100-200cm" <- NA; horizon2_30$"SoilDepth" <- NA
harmondat <- rbind(harmondat, horizon2_30)

# Add 0.3 - 0.6m data
horizon2_60 <- subset(horizon2, midpoint > 0.25 & midpoint <=0.55)
horizon2_60 <- horizon2_60[,c("Observation_ID", "Value")]
horizon2_60 <- setNames(horizon2_60, c("ID", "30-60cm"))
horizon2_60$"0-5cm" <- NA; horizon2_60$"5-15cm" <- NA; horizon2_60$"15-30cm" <- NA; horizon2_60$"60-100cm" <- NA; horizon2_60$"100-200cm" <- NA; horizon2_60$"SoilDepth" <- NA
harmondat <- rbind(harmondat, horizon2_60)

# Add 0.6 - 1m data
horizon2_100 <- subset(horizon2, midpoint > 0.55 & midpoint <=0.95)
horizon2_100 <- horizon2_100[,c("Observation_ID", "Value")]
horizon2_100 <- setNames(horizon2_100, c("ID", "60-100cm"))
horizon2_100$"0-5cm" <- NA; horizon2_100$"5-15cm" <- NA; horizon2_100$"15-30cm" <- NA; horizon2_100$"30-60cm" <- NA; horizon2_100$"100-200cm" <- NA; horizon2_100$"SoilDepth" <- NA
harmondat <- rbind(harmondat, horizon2_100)

# Add 1 - 2m data
horizon2_200 <- subset(horizon2, midpoint > 0.95 & midpoint <= 1.95)
horizon2_200 <- horizon2_200[,c("Observation_ID", "Value")]
horizon2_200 <- setNames(horizon2_200, c("ID", "100-200cm"))
horizon2_200$"0-5cm" <- NA; horizon2_200$"5-15cm" <- NA; horizon2_200$"15-30cm" <- NA; horizon2_200$"30-60cm" <- NA; horizon2_200$"60-100cm" <- NA; horizon2_200$"SoilDepth" <- NA 
harmondat <- rbind(harmondat, horizon2_200)
print("Harmonization of depth data complete")

# Add coords back into data
harmondat <- plyr::rename(harmondat, c(ID = "Observation_ID"))
FinalSites <- join(harmondat, soildat, by = "Observation_ID", type = "left", match = "first")

# Create a csv with with disturbance details for anaylst purposes
FinalSitesDetails <- subset(FinalSites, select = c("Observation_ID", "Dist_type", "Longitude", "Latitude", "0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"))
FinalSites <- subset(FinalSites, select = c("Observation_ID", "Longitude", "Latitude", "0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"))
FinalSites <- FinalSites[!duplicated(FinalSites[,c("Longitude","Latitude")]),] # Remove sites which have differnet ID's but same location, only want one value per location. Current solution does not select a specfic 0-5 cm value, would prefer to select the small value of P

# Get statistical summary data of final sites 
#summarydat <- round(summary(FinalSites$),1)

## Create input file for Cubist model  
Cubistdat <- cbind(FinalSites[,1:3], log((FinalSites[, 4:9]+ 0.01))) # Transform data using natural log + 0.01 (add 0.01 to prevent log(0) which is infinity) 
Cubistdat <- tibble::rowid_to_column(Cubistdat, "ID") # Create a numeric ID idex from the row number
Cubistdat <- Cubistdat[,-2] # Delete old id column
Cubistdat <- plyr::rename(Cubistdat, c(Longitude = "X", Latitude = "Y"))
Cubistdat <- Cubistdat[, c(2,3,1,4:9)]
Cubistdat <- subset(Cubistdat, X != 0)

## Export data
write.csv(ExcludedSites,file = "ExcludedSites.csv",row.names = FALSE)
write.csv(FinalSitesDetails,file = "FinalSitesDetails.csv",row.names = FALSE)
write.csv(Cubistdat, "PData.csv", row.names = FALSE) #Transformed data
print("End of processing")
#### End of script ####
