### Combine soil archive data with soil spectral data
## attribute location data

## spectra library
spec.dat<-readRDS("Z:/projects/openSpecs_2019/data/aus_soilspecarchive_stage1.rds")
names(spec.dat)

#remove nas
# remove cases with missing spectra data
ms<- which(!complete.cases(spec.dat[,which(names(spec.dat)=="453"):which(names(spec.dat)=="2500")]))
ms
spec.dat<- spec.dat[-ms,]





## soil archive data
arch.dat<- read.csv("Z:/projects/openSpecs_2019/data/archiveData/BrendanMalone_datarequest.csv")
str(arch.dat)

# soil information where there is a spectra
arch.dat<- arch.dat[arch.dat$spec_id %in% spec.dat$specID,]

uniqs<- unique(arch.dat$spec_id)
uniqs

# pull up site data attributed to the spec id
sel.dat<- arch.dat[0,]
for (i in 1:length(uniqs)){
  suz<- uniqs[i]
  suz.dat<- arch.dat[which(arch.dat$spec_id == suz),]
  sel.dat[i,]<- suz.dat[1,]
  print(i)
}

sel.dat$mySpecID<- as.numeric(as.character(sel.dat$spec_id))
sel.dat<- sel.dat[order(sel.dat$mySpecID),]
sel.dat$lowerD<- as.numeric(as.character(sel.dat$samp_lower_depth))
top.sel.dat<- sel.dat[which(sel.dat$lowerD < 0.50),] # narrow down topsoils

# reduction
names(top.sel.dat)
rm.dat<- top.sel.dat[,c(15,8,9,10,11)]
nulls<- which(rm.dat$o_latitude_GDA94 == "NULL")
rm.dat<- rm.dat[-nulls,]
rm.dat<- rm.dat[complete.cases(rm.dat),]
rm.dat$newcol<- paste0(rm.dat$o_latitude_GDA94, "_", rm.dat$o_longitude_GDA94)
length(unique(rm.dat$newcol))

rm.dat$newcol<- as.factor(rm.dat$newcol)
rm.dat$Site<- as.numeric(rm.dat$newcol)
rm.dat<- rm.dat[order(rm.dat$Site),]
surf.dat<- rm.dat[which(rm.dat$samp_upper_depth == 0),] # get surface soils
surf.dat<- surf.dat[order(surf.dat$mySpecID),]
names(surf.dat)
surf.dat<- surf.dat[,c(1:5)]

saveRDS(surf.dat, file = "Z:/projects/ternlandscapes_2019/soilColour/data/visNIR/aus_visNIR_locs_subset_updated.rds")
write.csv(surf.dat,file = "Z:/projects/ternlandscapes_2019/soilColour/data/visNIR/aus_visNIR_locs_updated.csv",row.names = F )


plot(surf.dat$o_longitude_GDA94, surf.dat$o_longitude_GDA94)
