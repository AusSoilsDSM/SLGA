
library(jsonlite);library(httr);library(RCurl);library(curl)
#library(leaflet);library(magrittr);library(sp)


# Check what datasets are available
exp.dat<- fromJSON("http://asris-daas02/NT_Services/api/MorphResults?morphology_attribute=ph_value")
exp.dat

plot(exp.dat$o_longitude_GDA94, exp.dat$o_latitude_GDA94)


# export data
saveRDS(object = exp.dat, 
        file = "Z:/projects/ternlandscapes_2019/soil_pH/data/fieldObs/NTGovernment_soil_FIELD_pH_data_2021-01-04.rds")

dat1<- readRDS("Z:/projects/ternlandscapes_2019/soil_pH/data/fieldObs/NTGovernment_soil_FIELD_pH_data_2021-01-04.rds")
dat2<- readRDS("Z:/projects/ternlandscapes_2019/soil_pH/data/fieldObs/NTGovernment_soil_FIELD_pH_data_2020-12-23.rds")
