
#################################
###  Author : Ross Searle         
###  Date : Wed Oct 03 08:48:18 2018                      
###  Project :                 
#################################

library(XML)
library(xml2)
library(stringr)
library(jsonlite)
library(rCurl)
library(raster)
library(rgdal)

source('c:/PrivateInfo/PrivateInfo.R')

source(paste0('C:/Users/', ident, '/Dropbox/RossRCode/myFunctions/GeneralUtils.R'))
source(paste0('C:/Users/', ident, '/Dropbox/RossRCode/myFunctions/VectorUtils.R'))
source(paste0('C:/Users/', ident, '/Dropbox/RossRCode/myFunctions/DateTimeUtils.R'))


rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+08,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory



### Data obtained from http://www.ga.gov.au/scientific-topics/disciplines/geophysics/rock-properties


fileName <- 'c:/temp/ad.csv'

d <- download.file('http://www.ga.gov.au/geophysics-rockpropertypub-gws/ga_rock_properties_wfs/ows?service=WFS&version=2.0.0&request=GetFeature&typename=ga_rock_properties_wfs:scalar_results&outputformat=csv', destfile = fileName)

df <- read.csv(fileName, stringsAsFactors = F)
head(df)
str(df)
nrow(df)

unique(df$SAMPLINGFEATURETYPE)
unique(df$PROCESSTYPE)
unique(df$SAMPLETYPE)

outcrops <- df[df$SAMPLETYPE == 'outcrop sample', ]
nrow(outcrops)


head(outcrops)
uro <- unique(outcrops[,c('SAMPLE_LATITUDE', 'SAMPLE_LONGITUDE', 'PROPERTY', 'SAMPLINGDATE', 'LOCATIONACCURACY_M', 'LOCATIONMETHOD')])
nrow(uro)

spOutcrops <- outcrops
coordinates(spOutcrops) <- ~SAMPLE_LONGITUDE+SAMPLE_LATITUDE
crs(spOutcrops) <- CRS("+proj=longlat +datum=WGS84")
plot(spOutcrops)


spuro <- uro
spuro$rndDepth <- runif(nrow(spuro), min=0, max=5)
coordinates(spuro) <- ~SAMPLE_LONGITUDE+SAMPLE_LATITUDE
plot(spuro)
crs(spuro) <- CRS("+proj=longlat +datum=WGS84")
writeShapeFile(spuro, 'C:/Projects/TernLandscapes/Regolith/GARockDatabase/GA_Outcrops.shp')
write.csv(uro, 'C:/Projects/TernLandscapes/Regolith/GARockDatabase/GA_Outcrops.csv')


# not used as yet but need to check if Json downloads deliver more than 100k rows


library(RCurl)


d <- getURL('http://www.ga.gov.au/geophysics-rockpropertypub-gws/ga_rock_properties_wfs/ows?service=WFS&version=2.0.0&request=GetFeature&typename=ga_rock_properties_wfs:scalar_results&count=100s&outputformat=json')

fileName <- 
d <- download.file('http://www.ga.gov.au/geophysics-rockpropertypub-gws/ga_rock_properties_wfs/ows?service=WFS&version=2.0.0&request=GetFeature&typename=ga_rock_properties_wfs:scalar_results&outputformat=json', destfile = fileName)
df <- fromJSON(d)
df$features
dff <- df$features
nrow(dff)
