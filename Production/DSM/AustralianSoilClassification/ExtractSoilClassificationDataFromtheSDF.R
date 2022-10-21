#################################
###  Author : Ross Searle         
###  Date : Thu Aug 05 12:50:21 2021                      
###  Project : TERN Landscapes
###  Purpose : Extract Soil Classification data from the SoilDataFederator to use in ASC modelling

#################################

library(raster)
library(stringr)
library(rgdal)
library(httr)
library(jsonlite)
library(raster)
library(sf)

### supply you own key here. You need to register to use the SoilDataFederator - https://shiny.esoil.io/SoilDataFederator/Pages/Register/
usr <- ''  
key <- ''

workDir <- 'Z:/Ross/TERN/AWC/ObsDataSDF'

# Get the available datasets
datasetsDF <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/DataSets')
datasets<-datasetsDF$DataSet

groups <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/PropertyGroups')

props <- fromJSON('http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties')

str(props)




sdfExtract <- function(att, props, datasets, outDir=NULL, usr='Demo', key='Demo'){
  
  # Iterate Properties
  cnt=0
  for(i in 1:length(props)){
    # Make an empty list to put individual results in for this property
    res <- vector("list", length = length(datasets))
    print(props[i])
    # Iterate Datasets
    for (j in 1:length(datasets)) {
      p <- props[i]
      d <- datasets[j]
      print(paste0(d, ' : ', p))
      url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/SoilData?observedProperty=", p ,"&DataSet=", d, "&format=json&usr=", usr, "&key=", key))
      resp <- GET(url, timeout(300))
      response <- content(resp, "text", encoding = 'UTF-8')
      odf <- fromJSON(response)
      
      # If there is data returned slot it into the list
      if(is.data.frame(odf)){
        if(nrow(odf)>0){
          res[[j]] <- odf
          cnt=cnt+1
        }
      }
    }
    # Merge the dataframes in the list into one dataframe
    outDF = as.data.frame(data.table::rbindlist(res, fill=T))
    write.csv(outDF, paste0(outDir, '/SDF_', att, '_', p, '.csv'))
    
  }
  return(paste0(cnt, ' datasets extracted'))
}


##### extract the Soil Classification data
url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/PropertyGroups"))
pgs <- fromJSON(url)
url <-URLencode(paste0("http://esoil.io/TERNLandscapes/SoilDataFederatoR/SoilDataAPI/Properties?PropertyGroup=observations"))
props <- fromJSON(url)
att='Classification'


props <- c('O_GSG', 'O_PPF', 'O_ASC_ORD')
#props <- c('O_ASC_ORD')

sdfExtract(att=att, props=props, datasets=datasets, outDir=workDir, usr=usr, key=key)


gsgDF <- read.csv(paste0(workDir, '/SDF_Classification_O_GSG.csv'), stringsAsFactors = F)
ppfDF <- read.csv(paste0(workDir, '/SDF_Classification_O_PPF.csv'), stringsAsFactors = F)
ASCDF <- read.csv(paste0(workDir, '/SDF_Classification_O_ASC_ORD.csv'), stringsAsFactors = F)

mdf1 <- merge(gsgDF, ppfDF, all=T, by=c('DataStore','Dataset','Provider','Location_ID','SampleDate','Longitude','Latitude','PropertyType'))
head(mdf1)
colnames(mdf1)
mdf2 <- mdf1[,c(1:8,15, 28, 17:20)]
head(mdf2)
colnames(mdf2) <- c('DataStore','Dataset','Provider','Location_ID','SampleDate','Longitude','Latitude','PropertyType',
                    'GSG', 'PPF',  'QualCollection', 'QualSpatialAggregation', 'QualManagement', 'QualSpatialAccuracy')
head(mdf2)

mdf3 <- merge(mdf2, ASCDF, all=T, by=c('DataStore','Dataset','Provider','Location_ID','SampleDate','Longitude','Latitude','PropertyType'))
head(mdf3)
colnames(mdf3)
mdf4 <- mdf3[,c(1:10,21)]
head(mdf4)
colnames(mdf4) <- c('DataStore','Dataset','Provider','Location_ID','SampleDate','Longitude','Latitude','PropertyType',
                    'GSG', 'PPF', 'ASC')

mdf5 <- mdf4[!duplicated(mdf4),]
write.csv(mdf5, paste0(workDir, '/SDF_All_Classification.csv'))
#mdf4[mdf4$Dataset=='VicGovernment', ]
cdf <- read.csv(paste0(workDir, '/SDF_All_Classification.csv'))
count(cdf[!is.na(cdf$ASC), ], 'Dataset')
