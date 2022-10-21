
library(terra)
terraOptions(progress=1)

source('c:/PrivateInfo/PrivateInfo.R')


r <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/CLY/CLY_000_005_EV_N_P_AU_TRN_N_20210902.tif')
rr <- round(r,digits=2)
rr
writeRaster(rr, paste0('/scratch1/', ident, '/CLY_000_005_EV_N_P_AU_TRN_N_20210902_2Decimals.tif', datatype='FLT4S', overwrite=T))
dataType(rr)


rr <- round(r,digits=0)
rr
writeRaster(rr, paste0('/scratch1/', ident, '/CLY_000_005_EV_N_P_AU_TRN_N_20210902_INTOnly.tif'), datatype='int1U', overwrite=T)


rrDec <- rast(paste0('/scratch1/', ident, '/CLY_000_005_EV_N_P_AU_TRN_N_20210902_2Decimals.tif'))
rrInt <- rast(paste0('/scratch1/', ident, '/CLY_000_005_EV_N_P_AU_TRN_N_20210902_INTOnly.tif'))


xy <- rbind(c(145, -27), c(146, -26))
p <- vect(xy, crs="+proj=longlat +datum=WGS84")

v<-extract(rrDec, xy)
v
is.integer(v$CLY_000_005_EV_N_P_AU_TRN_N_20210902[[1]][1])

v<-extract(rrInt, xy)
v
is.integer(v$CLY_000_005_EV_N_P_AU_TRN_N_20210902)

is.integer(rrDec)
is.integer(rrInt)

typeof(values(rrDec[1:10]))
