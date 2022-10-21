## soil observations Australia

## Steps of procedure

# load in site observation data
# load in continental boundary

# First steps: (r code)
# remove sites that do not have a spatial coordinate
# remove sites that are not within bounding box of boundary
# remove points that have duplicate coordinates (only need to keep 1 record of site cordinates )

# Second step
# perfomed externally
# join site data with horizon data

# Third step (r code)
# check whether newly joined data has any dublicate site locations 


library(sp);library(rgdal)

dat<- read.table("/home/brendo1001/mywork/dropboxShare/2018/soilDepth/data/dbo_OBSERVATIONS_sub.txt", header = T,sep=",")


# National boundary
ausBound<- readOGR("/home/brendo1001/mywork/redUSB/USYD/PRJ-SIA_dataware/datasets/TERN/Australia Topo/60803_shp/framework/aus10fgd_l.shp")
plot(ausBound)
ausBound@bbox[1,]




#spatialise

## locations without spatial data
names(dat)
dat<- dat[which(complete.cases(dat[,5:6])),] # keep only those points with coordinates

## locations within bounding box
dat<- dat[which(dat$o_longitude_GDA94 <= ausBound@bbox[1,2] & dat$o_longitude_GDA94 >= ausBound@bbox[1,1]),]
dat<- dat[which(dat$o_latitude_GDA94 <= ausBound@bbox[2,2] & dat$o_latitude_GDA94 >= ausBound@bbox[2,1]),]

## locations with the same coordinate (multiple site obs?)
# Note: this takes a while to run
zerodists<- zerodist(dat)
nrow(zerodists)
actual_ZD_sites<- unique(zerodists[,1])
#remove
dat_sub<- dat[-actual_ZD_sites,]
plot(dat_sub)

#check for zerodists
zerodists_sub<- zerodist(dat_sub)

### export
dat_sub<- as.data.frame(dat_sub)
write.table(dat_sub,"/home/brendo1001/mywork/dropboxShare/2018/soilDepth/data/dbo_OBSERVATIONS_sub_unique.txt", col.names = T, sep=",", row.names = F)

str(dat)



## New dataset with locations and horizons
dat<- read.table("/home/brendo1001/mywork/dropboxShare/2018/soilDepth/data/dbo_Horzons_loc_sub.txt", header = T,sep=",")
str(dat)


sub_dat<- dat[dat$myHoriz==1,] # first observation of each profile
names(sub_dat)
coordinates(sub_dat)<- ~ o_longitude_GDA94 + o_latitude_GDA94
#check for zerodists
zerodists_sub<- zerodist(sub_dat) 
# after the fact: it appears there are only unique site locations with horizon info in this data set. This is good
# move on!)


# Code END

# next step: logic for determining max soil depth for each profile. 

