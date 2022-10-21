library(terra)

terraOptions(progress = 0, chunksize=1e+08,maxmemory=1e+09)

getChunkInfo <- function(chunkLines, tot){
  
  remainder <- tot%%chunkLines
  
  if(remainder > 0){
    chunks <- ceiling(tot/chunkLines)
    starts <- seq.int(from=1,length=chunks,by=chunkLines)
    nlines <- rep(chunkLines, length(starts))
    nlines[length(nlines)] <- remainder
    
  }else{
    chunks <- tot/chunkLines
    starts <- seq.int(from=1,length=chunks,by=chunkLines)
    nlines <- rep(chunkLines, length(starts))
  }
  
  return(list(chunks=length(starts), starts=starts, nlines=nlines))
}



rl1 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/L15/L15_000_005_EV_N_P_AU_TRN_N_20210614.tif')
rl2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/L15/L15_005_015_EV_N_P_AU_TRN_N_20210614.tif')
rl3 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/L15/L15_015_030_EV_N_P_AU_TRN_N_20210614.tif')
rl4 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/L15/L15_030_060_EV_N_P_AU_TRN_N_20210614.tif')
rl5 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/L15/L15_060_100_EV_N_P_AU_TRN_N_20210614.tif')
rl6 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/L15/L15_100_200_EV_N_P_AU_TRN_N_20210614.tif')

ru1 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DUL/DUL_000_005_EV_N_P_AU_TRN_N_20210614.tif')
ru2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DUL/DUL_005_015_EV_N_P_AU_TRN_N_20210614.tif')
ru3 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DUL/DUL_015_030_EV_N_P_AU_TRN_N_20210614.tif')
ru4 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DUL/DUL_030_060_EV_N_P_AU_TRN_N_20210614.tif')
ru5 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DUL/DUL_060_100_EV_N_P_AU_TRN_N_20210614.tif')
ru6 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DUL/DUL_100_200_EV_N_P_AU_TRN_N_20210614.tif')

rd <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DES/DES_000_200_EV_N_P_AU_TRN_C_20190901.tif') 


# if(d >= 0 & d < 0.05){
#   rout <- (u1-l1) * (d*1000)
# }else if(d >= 0.05 & d < 0.15){
#   rout <- ((u1-l1) * (0.05*1000)) + ((u2-l2) * ((d - 0.05) * 1000))
# }else if(d >= 0.15 & d < 0.30){
#   rout <- ((u1-l1) * (0.05*1000)) + ((u2-l2) * ((0.10) * 1000)) + ((u3-l3) * ((d - 0.15) * 1000))
# }else if(d >= 0.30 & d < 0.60){
#   rout <- ((u1-l1) * (0.05*1000)) + ((u2-l2) * ((0.10) * 1000)) + ((u3-l3) * ((0.15) * 1000))  + ((u4-l4) * ((d - 0.30) * 1000))
# }else if(d >= 0.60 & d < 1.0){
#   rout <- ((u1-l1) * (0.05*1000)) + ((u2-l2) * ((0.10) * 1000)) + ((u3-l3) * ((0.15) * 1000))  + ((u4-l4) * ((0.30) * 1000))  + ((u5-l5) * ((d - 0.60) * 1000))
# }else if(d >= 1.0 ){
#   rout <- ((u1-l1) * (0.05*1000)) + ((u2-l2) * ((0.10) * 1000)) + ((u3-l3) * ((0.15) * 1000))  + ((u4-l4) * ((0.30) * 1000))  + ((u5-l5) * ((0.40) * 1000)) + ((u6-l6) * ((d - 1.0) * 1000))
# }



chk <-	getChunkInfo(100, nrow(rd))

outRaster <- paste0('/scratch1/', ident, '/AWCV3_2004.tif')
outR <-rast(rd)
names(outR) <- 'Available Water Capacity to 2 metres'
bob <- writeStart(outR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")

readStart(rl1);readStart(rl2);readStart(rl3);readStart(rl4);readStart(rl5);readStart(rl6)
readStart(ru1);readStart(ru2);readStart(ru3);readStart(ru4);readStart(ru5);readStart(ru6)
readStart(rd);

for (i in 1:chk$chunks) {
  
  print(paste0('Chunk ', i, ' of ',chk$chunks ))
  csize<-chk$nlines[i] * ncol(rd)
  ov <- rep(NAflag(rd), csize )
  
  l1 <- as.numeric(readValues(rl1, row=chk$starts[i], nrows=chk$nlines[i]))
  l2 <- as.numeric(readValues(rl2, row=chk$starts[i], nrows=chk$nlines[i]))
  l3 <- as.numeric(readValues(rl3, row=chk$starts[i], nrows=chk$nlines[i]))
  l4 <- as.numeric(readValues(rl4, row=chk$starts[i], nrows=chk$nlines[i]))
  l5 <- as.numeric(readValues(rl5, row=chk$starts[i], nrows=chk$nlines[i]))
  l6 <- as.numeric(readValues(rl6, row=chk$starts[i], nrows=chk$nlines[i]))
  
  u1 <- as.numeric(readValues(ru1, row=chk$starts[i], nrows=chk$nlines[i]))
  u2 <- as.numeric(readValues(ru2, row=chk$starts[i], nrows=chk$nlines[i]))
  u3 <- as.numeric(readValues(ru3, row=chk$starts[i], nrows=chk$nlines[i]))
  u4 <- as.numeric(readValues(ru4, row=chk$starts[i], nrows=chk$nlines[i]))
  u5 <- as.numeric(readValues(ru5, row=chk$starts[i], nrows=chk$nlines[i]))
  u6 <- as.numeric(readValues(ru6, row=chk$starts[i], nrows=chk$nlines[i]))
  
  d <- as.numeric(readValues(rd, row=chk$starts[i], nrows=chk$nlines[i])) * 1000
  
  idxs <- which(d >= 0 & d < 50)
  ov[idxs] <- (u1[idxs]-l1[idxs])/100 * d[idxs]
  
  idxs <- which(d >= 50 & d < 150)
  ov[idxs] <- ((u1[idxs]-l1[idxs])/100 * 50) + ((u2[idxs]-l2[idxs])/100 * (d[idxs] - 50) )
  
  idxs <- which(d >= 150 & d < 300)
  ov[idxs] <- ((u1[idxs]-l1[idxs])/100 * 50) + ((u2[idxs]-l2[idxs])/100 * 100) + ((u3[idxs]-l3[idxs])/100 * (d[idxs] - 150))
  
  idxs <- which(d >= 300 & d < 600)
  ov[idxs] <- ((u1[idxs]-l1[idxs])/100 * 50) + ((u2[idxs]-l2[idxs])/100 * 100) + ((u3[idxs]-l3[idxs])/100 * 150)  + ((u4[idxs]-l4[idxs])/100 * (d[idxs] - 300))
  
  idxs <- which(d >= 600 & d < 1000)
  ov[idxs] <- ((u1[idxs]-l1[idxs])/100 * 50) + ((u2[idxs]-l2[idxs])/100 * 100) + ((u3[idxs]-l3[idxs])/100 * 150)  + ((u4[idxs]-l4[idxs])/100 * 300)  + ((u5[idxs]-l5[idxs])/100 * (d[idxs] - 600))
  
  idxs <- which(d >= 1000)
  ov[idxs] <- ((u1[idxs]-l1[idxs])/100 * 50) + ((u2[idxs]-l2[idxs])/100 * 100) + ((u3[idxs]-l3[idxs])/100 * 150)  + ((u4[idxs]-l4[idxs])/100 * 300)  + ((u5[idxs]-l5[idxs])/100 * 400) + ((u6[idxs]-l6[idxs])/100 * (d[idxs] - 1000))
  
  idxs <- which(ov > 300)
  if(length(idxs)>0){
    ov[idxs] <- 300
  }
  
  writeValues(outR, ov, chk$starts[i], chk$nlines[i])
}

writeStop(outR)


####   Total AWC to 100cm ###########
outRaster100 <- paste0('/scratch1/', ident, '/AWCV3_1004.tif')
outR100 <-rast(rd)
names(outR100) <- 'Available Water Capacity to 1 metre'
bob2 <- writeStart(outR100,filename=outRaster100,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
chk <-	getChunkInfo(100, nrow(rd))
readStart(rl1);readStart(rl2);readStart(rl3);readStart(rl4);readStart(rl5)
readStart(ru1);readStart(ru2);readStart(ru3);readStart(ru4);readStart(ru5)
readStart(rd);

for (i in 1:chk$chunks) {
  
  print(paste0('Chunk ', i, ' of ',chk$chunks ))
  csize<-chk$nlines[i] * ncol(rd)
  ov <- rep(NAflag(rd), csize )
  
  l1 <- as.numeric(readValues(rl1, row=chk$starts[i], nrows=chk$nlines[i]))
  l2 <- as.numeric(readValues(rl2, row=chk$starts[i], nrows=chk$nlines[i]))
  l3 <- as.numeric(readValues(rl3, row=chk$starts[i], nrows=chk$nlines[i]))
  l4 <- as.numeric(readValues(rl4, row=chk$starts[i], nrows=chk$nlines[i]))
  l5 <- as.numeric(readValues(rl5, row=chk$starts[i], nrows=chk$nlines[i]))

  u1 <- as.numeric(readValues(ru1, row=chk$starts[i], nrows=chk$nlines[i]))
  u2 <- as.numeric(readValues(ru2, row=chk$starts[i], nrows=chk$nlines[i]))
  u3 <- as.numeric(readValues(ru3, row=chk$starts[i], nrows=chk$nlines[i]))
  u4 <- as.numeric(readValues(ru4, row=chk$starts[i], nrows=chk$nlines[i]))
  u5 <- as.numeric(readValues(ru5, row=chk$starts[i], nrows=chk$nlines[i]))
  
  d <- as.numeric(readValues(rd, row=chk$starts[i], nrows=chk$nlines[i])) * 1000
  
  idxs <- which(d >= 0 & d < 50)
  ov[idxs] <- (u1[idxs]-l1[idxs])/100 * d[idxs]
  
  idxs <- which(d >= 50 & d < 150)
  ov[idxs] <- ((u1[idxs]-l1[idxs])/100 * 50) + ((u2[idxs]-l2[idxs])/100 * (d[idxs] - 50) )
  
  idxs <- which(d >= 150 & d < 300)
  ov[idxs] <- ((u1[idxs]-l1[idxs])/100 * 50) + ((u2[idxs]-l2[idxs])/100 * 100) + ((u3[idxs]-l3[idxs])/100 * (d[idxs] - 150))
  
  idxs <- which(d >= 300 & d < 600)
  ov[idxs] <- ((u1[idxs]-l1[idxs])/100 * 50) + ((u2[idxs]-l2[idxs])/100 * 100) + ((u3[idxs]-l3[idxs])/100 * 150)  + ((u4[idxs]-l4[idxs])/100 * (d[idxs] - 300))
  
  idxs <- which(d >= 600)
  ov[idxs] <- ((u1[idxs]-l1[idxs])/100 * 50) + ((u2[idxs]-l2[idxs])/100 * 100) + ((u3[idxs]-l3[idxs])/100 * 150)  + ((u4[idxs]-l4[idxs])/100 * 300)  + ((u5[idxs]-l5[idxs])/100 * (pmin(d[idxs]-600, 400)))
  
  idxs <- which(ov > 150)
  if(length(idxs)>0){
    ov[idxs] <- 150
  }
  writeValues(outR100, ov, chk$starts[i], chk$nlines[i])
  
}

writeStop(outR100)
readStop(rl1);readStop(rl2);readStop(rl3);readStop(rl4);readStop(rl5)
readStop(ru1);readStop(ru2);readStop(ru3);readStop(ru4);readStop(ru5)
readStop(rd);


dir.create(paste('/scratch1/', ident, '/AWC_CalcsV3'))

ar1 <- ru1 - rl1
writeRaster(ar1, paste0('/scratch1/', ident, '/AWC_CalcsV3/AWC_000_005_EV_N_P_AU_TRN_N_20210614.tif'))
ar2 <- ru2 - rl2
writeRaster(ar2, paste0('/scratch1/', ident, '/AWC_CalcsV3/AWC_005_015_EV_N_P_AU_TRN_N_20210614.tif'))
ar3 <- ru3 - rl3
writeRaster(ar3, paste0('/scratch1/', ident, '/AWC_CalcsV3/AWC_015_030_EV_N_P_AU_TRN_N_20210614.tif'))
ar4 <- ru4 - rl4
writeRaster(ar4, paste0('/scratch1/', ident, '/AWC_CalcsV3/AWC_030_060_EV_N_P_AU_TRN_N_20210614.tif'))
ar5 <- ru5 - rl5
writeRaster(ar5, paste0('/scratch1/', ident, '/AWC_CalcsV3/AWC_060_100_EV_N_P_AU_TRN_N_20210614.tif'))
ar6 <- ru6 - rl6
writeRaster(ar6, paste0('/scratch1/', ident, '/AWC_CalcsV3/AWC_100_200_EV_N_P_AU_TRN_N_20210614.tif'))

boxplot(ar1)
summary

####### generate AWC surfaces




# if(d >= 0 & d < 0.05){
#   rout <- (u1-l1) * (d*1000)
# }else if(d >= 0.05 & d < 0.15){
#   rout <- ((u1-l1) * (0.05*1000)) + ((u2-l2) * ((d - 0.05) * 1000))
# }else if(d >= 0.15 & d < 0.30){
#   rout <- ((u1-l1) * (0.05*1000)) + ((u2-l2) * ((0.10) * 1000)) + ((u3-l3) * ((d - 0.15) * 1000))
# }else if(d >= 0.30 & d < 0.60){
#   rout <- ((u1-l1) * (0.05*1000)) + ((u2-l2) * ((0.10) * 1000)) + ((u3-l3) * ((0.15) * 1000))  + ((u4-l4) * ((d - 0.30) * 1000))
# }else if(d >= 0.60 & d < 1.0){
#   rout <- ((u1-l1) * (0.05*1000)) + ((u2-l2) * ((0.10) * 1000)) + ((u3-l3) * ((0.15) * 1000))  + ((u4-l4) * ((0.30) * 1000))  + ((u5-l5) * ((d - 0.60) * 1000))
# }else if(d >= 1.0 ){
#   rout <- ((u1-l1) * (0.05*1000)) + ((u2-l2) * ((0.10) * 1000)) + ((u3-l3) * ((0.15) * 1000))  + ((u4-l4) * ((0.30) * 1000))  + ((u5-l5) * ((0.40) * 1000)) + ((u6-l6) * ((d - 1.0) * 1000))
# }

