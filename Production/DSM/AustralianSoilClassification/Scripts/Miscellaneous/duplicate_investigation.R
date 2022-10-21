
soil = readRDS("//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry/SoilTypes.rds")

soilDF = soil[,4]
idx <- duplicated(soilDF) | duplicated(soilDF, fromLast = TRUE)
ot <- soil[idx, ]
sot <- ot[order(ot$Observation_ID, decreasing = FALSE),]
sot


#all duplicates
soilDF = input[,1]
idx <- duplicated(soilDF) | duplicated(soilDF, fromLast = TRUE)
ot <- input[idx, ]
sot <- ot[order(ot$SID, decreasing = FALSE),]
sot

#save as csv
doubles = unique(sot$Observation_ID)
d = soil[which(soil$Observation_ID %in% doubles),][,1:6]
write.table(d,"E:/Harry/NT_DSM_Training/Doubles.csv", sep=",", row.names=F)


#not identical rows
qqq = unique(sot)
soilDF = qqq[,1]
idx <- duplicated(soilDF) | duplicated(soilDF, fromLast = TRUE)
ot <- qqq[idx, ]
sot1 <- ot[order(ot$SID, decreasing = FALSE),]
sot1




#make a .shp fale
allpts2 = data[!complete.cases(data),]

coordinates(allpts2) <- ~ Easting + Northing
proj4string(allpts2) <- CRS("+proj=longlat +datum=WGS84")
writeOGR(allpts2, root.directory, "MissingData4", driver="ESRI Shapefile", overwrite_layer=TRUE)
cat(showWKT(proj4string(allpts2)),file=paste0(root.directory, "MissingData3.prj"))


