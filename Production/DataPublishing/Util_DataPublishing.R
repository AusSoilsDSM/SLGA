
library(reticulate)
library(xml2)
library(htmltidy)
library(stringr)
library(terra)


source('/datasets/work/af-digiscapesm/work/Ross/SLGA/PrivateInfo.R')



gdalPath <- '/apps/gdal/3.2.2/bin'
tmpDir <- paste0('/scratch1/', ident, '/tmp')

geo <- import('osgeo')

des <- c('Lower Confidence Limit (5%)', 'Estimated Value', 'Upper Confidence Limit (95%)')
code <- c('05', 'EV', '95')

##### Soil depth Specific
# des <- c('Lower Confidence Limit (10%)', 'Estimated Value', 'Upper Confidence Limit (90%)')
# code <- c('10', 'EV', '90')
prods <- data.frame(code, des)

des <- c('0-5cm','5-15cm','15-30cm','30-60cm','60-100cm','100-200cm')
code <- c('005', '015', '030', '060', '100', '200')

##### Soil depth Specific
# des <- c('0-200cm')
# code <- c('200')

depths <- data.frame(code, des)

# isValidCOG('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/AWC/AWC_000_005_EV_N_P_AU_TRN_N_20210614.tif', quiet=F)

isValidCOG <- function(fname, quiet=T){
  py <- '/datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Production/NationalMaps/DataPublishing/validateCOG.py'
  source_python(py)
  r <- validateCOG(fname, quiet)
  if(r==1){
    return(F)
  }else{
    return(T)
  }
}


publishRaster  <- function(inRasterPath=NULL, outDir=NULL, rasterTemplate=NULL, doResample=F, isIntegerRaster=F, adjustVal=1, setMin=NULL, fillHoles=F, applyMask=T, sigFigures=-1, metadataTable=NULL, RATTable=NULL, MakeCOG=F){
  
  tmpDir <- paste0('/scratch1/', ident, '/tmp')
  tmpRpath <- paste0(tmpDir, '/',basename(inRasterPath))
  dst_filename = paste0(outDir, '/', basename(inRasterPath))
  
  templateR <- terra::rast(rasterTemplate)
  
  print(paste0('Making copy of ', basename(inRasterPath), ' to ', tmpRpath))
  file.copy(from=inRasterPath, to=tmpRpath, overwrite = T)
  
  if(doResample)
  {
    print(paste('Resampling ',tmpRpath))
    terraOptions(prog=1)
    tr <- terra::rast(inRasterPath)
    trs <- terra::resample(tr, templateR, method = 'near')
    print(paste0('Writing resampled raster to - ', tmpRpath))
    terra::writeRaster(trs, filename=tmpRpath, overwrite=T)
  }
  
  if(fillHoles){
    terraOptions(prog=1)
    rrs <- terra::rast(tmpRpath)
    print('Filling holes .....')
    r2 <- terra::focal(rrs, w=matrix(1,3,3),  fun=mean, na.rm=T, NAonly=T)
    terra::writeRaster(r2, filename=tmpRpath, overwrite=T)
  }else{
    print('Not filling holes')
  }
  
  if(applyMask)
  {
    print('Applying mask ......')
    terraOptions(memmax=10)
    tr <- terra::rast(tmpRpath)
    mR <- terra::mask(tr, templateR)
    terra::writeRaster(mR, filename=tmpRpath, overwrite=T)
  }else{
    print('Not applying mask')
  }
  
    if(adjustVal != 1){
      print(paste0('Applying value adjustment of ', adjustVal))
      mR <- terra::rast(tmpRpath)
      mRA <- mR * adjustVal  
      terra::writeRaster(mRA, filename=tmpRpath, overwrite=T)
    }else{
      print('Not adjusting values')
    }
 
  #, gdal=c("COMPRESS=NONE", "TILED=NO")
  
  ###### Set minimum value
  if(!setMin ==- 1){
    print(paste0('Setting minimum value to : ', setMin))
    r <- rast(tmpRpath)
    rmin <- terra::clamp(r, lower=0) 
    writeRaster(rmin, tmpRpath, overwrite=TRUE)
  }else{
    print(paste0('Not setting minimum value'))
  }
  
  ###### Implement significant figures
  if(sigFigures == -1){
    print(paste0('Not enforcing significant figures'))
   
  }else{
    r <- rast(tmpRpath)
    print(paste0('Enforcing ', sigFigures, ' Decimal places in output raster'))
    rr <- round(r,digits=sigFigures)
    writeRaster(rr, tmpRpath, overwrite=T)
  }
  
 
  
#  tmpRpath <- paste0('/scratch1/', ident, '/tmp/CFD_000_005_EV_N_P_AU_TRN_N_20221006_Dominant_Class.tif')
  dtype='FLT4S'
  if(isIntegerRaster){
     print('Making integer raster')
    dtype='INT2U'
    # rr<- r*1
    # raster::writeRaster(rr, tmpRpath, gdal=c("COMPRESS=NONE", "TILED=NO", "COPY_SRC_OVERVIEWS=NO"  ),  datatype='INT2U', overwrite=T)
  }
  # else{
  #   print('Making floating point raster')
  #   r <- raster(tmpRpath)
  #   raster::writeRaster(r, tmpRpath, gdal=c("COMPRESS=NONE", "TILED=NO", "COPY_SRC_OVERVIEWS=NO"), datatype='FLT4S', overwrite=T)
  # }
  
  r <- terra::rast(tmpRpath)
  tfl <- paste0(tempfile(tmpdir = tmpDir), '.tif')
  terra::writeRaster(r, tfl, gdal=c("COMPRESS=NONE", "TILED=NO", "COPY_SRC_OVERVIEWS=NO"), datatype=dtype, overwrite=T)
  file.remove(tmpRpath)
  file.rename(from=tfl, to=tmpRpath)
  
  
  ds = geo$gdal$Open(tmpRpath, geo$gdal$GA_Update)
  band<- ds$GetRasterBand(as.integer(1))
  
  if(!is.null(metadataTable)){
    #insertMetadata(metadataTable)
    print(paste0('Adding Metadata....'))
    for (b in 1:nrow(metadataTable)) {
      itemName <- metadataTable[b,1]
      itemVal <- metadataTable[b,2]
      # if(itemName=='DATASET_TITLE'){
      #   bn <- basename(inRasterPath)
      #   bits <- str_split(bn, '_')
      #   p <- bits[[1]][4]
      #   d <- bits[[1]][3]
      #   prodv <- prods[prods$code==p, 2]
      #   depthv <- depths[depths$code==d, 2]
      #   itemVal<- str_replace(itemVal, 'XXXX', depthv)
      #   itemVal<- str_replace(itemVal, 'YYYY', prodv)
      # }
      ds$SetMetadataItem(itemName,itemVal )
    }
    xml <- geo$gdal$Info(ds)
    xml_view(xml)
  }
  
  
  if(!is.null(RATTable)){
   #makeRatTable(RATTable)
    print(paste0('Adding RAT....'))
    
    rat = geo$gdal$RasterAttributeTable()
    rat$CreateColumn(as.character("Value"),  geo$gdalconst$GFT_Integer, geo$gdalconst$GFU_MinMax )
    rat$CreateColumn(as.character("Count"),  geo$gdalconst$GFT_Integer, geo$gdalconst$GFU_PixelCount )
    rat$CreateColumn(as.character("R"),  geo$gdalconst$GFT_Integer, geo$gdalconst$GFU_Red )
    rat$CreateColumn(as.character("G"),  geo$gdalconst$GFT_Integer, geo$gdalconst$GFU_Blue )
    rat$CreateColumn(as.character("B"),  geo$gdalconst$GFT_Integer, geo$gdalconst$GFU_Red )
    rat$CreateColumn(as.character("A"),  geo$gdalconst$GFT_Integer, geo$gdalconst$GFU_Red )
    rat$CreateColumn(as.character("Class_Name"),  geo$gdalconst$GFT_String, geo$gdalconst$GFU_Name )
    
    
    for (i in 1:nrow(RATTable)) {
      intV <- as.integer(i-1)
      rat$SetValueAsInt(intV, as.integer(0), as.integer(RATTable$Value[i]))
      rat$SetValueAsInt(intV, as.integer(1), as.integer(1))
      rat$SetValueAsInt(intV, as.integer(2), as.integer(RATTable$Red[i]))
      rat$SetValueAsInt(intV, as.integer(3), as.integer(RATTable$Green[i]))
      rat$SetValueAsInt(intV, as.integer(4), as.integer(RATTable$Blue[i]))
      rat$SetValueAsInt(intV, as.integer(5), as.integer(255))
      rat$SetValueAsString(intV, as.integer(6), as.character(RATTable$Class_Name[i]))
    }
    
    band$SetDefaultRAT(rat)
    dr <- band$GetDefaultRAT()
    dr$DumpReadable()
    
  }
  
  band$FlushCache()
  ds$FlushCache()
  band=NULL
  ds=NULL
  
  
  if(MakeCOG){

   op <- generateCOG(inRasterPath=tmpRpath, outPath=dst_filename)
  }else{
    print('Copy file to final location.....')
    file.copy(from=tmpRpath, to=dst_filename, overwrite = T)
  }
  
  system(paste0(gdalPath, '/gdalinfo -mdd all ', dst_filename))
  
  return(dst_filename)
  
}

generateCOG <- function(inRasterPath, outPath){
  print('Generating external overviews')
  system(paste0(gdalPath, '/gdaladdo --config GDAL_CACHEMAX 5000 -ro ',inRasterPath))
  print('Generating COG')
  cmd <- paste0(gdalPath, '/gdal_translate ', inRasterPath, ' ', outPath ,' --config GDAL_CACHEMAX 5000 -stats -co TILED=YES -co COMPRESS=DEFLATE -co BIGTIFF=YES -co COPY_SRC_OVERVIEWS=YES' ) 
  system(cmd)
}

doResample <- function(inRaster, resampleTemplate){
  print('Resampling ......')
  trs <- terra::resample(inRaster, resampleTemplate, method = 'near')
  return(trs)
}

doMask <- function(inRaster, maskTemplate){
  print('Applying mask ......')
  mR <- terra::mask(inRaster, maskTemplate)
  return(mR)
}

insertMetadata <- function(metadataTable){
      
    #return(ds)
}


makeRatTable <- function(RATTable, band){
      
      print(paste0('Adding RAT....'))
      
        rat = geo$gdal$RasterAttributeTable()
        rat$CreateColumn(as.character("Value"),  geo$gdalconst$GFT_Integer, geo$gdalconst$GFU_MinMax )
        rat$CreateColumn(as.character("Count"),  geo$gdalconst$GFT_Integer, geo$gdalconst$GFU_PixelCount )
        rat$CreateColumn(as.character("R"),  geo$gdalconst$GFT_Integer, geo$gdalconst$GFU_Red )
        rat$CreateColumn(as.character("G"),  geo$gdalconst$GFT_Integer, geo$gdalconst$GFU_Blue )
        rat$CreateColumn(as.character("B"),  geo$gdalconst$GFT_Integer, geo$gdalconst$GFU_Red )
        rat$CreateColumn(as.character("A"),  geo$gdalconst$GFT_Integer, geo$gdalconst$GFU_Red )
        rat$CreateColumn(as.character("Class_Name"),  geo$gdalconst$GFT_String, geo$gdalconst$GFU_Name )
        
        
        for (i in 1:nrow(RATTable)) {
          intV <- as.integer(i-1)
          rat$SetValueAsInt(intV, as.integer(0), as.integer(RATTable$Value[i]))
          rat$SetValueAsInt(intV, as.integer(1), as.integer(1))
          rat$SetValueAsInt(intV, as.integer(2), as.integer(RATTable$Red[i]))
          rat$SetValueAsInt(intV, as.integer(3), as.integer(RATTable$Green[i]))
          rat$SetValueAsInt(intV, as.integer(4), as.integer(RATTable$Blue[i]))
          rat$SetValueAsInt(intV, as.integer(5), as.integer(255))
          rat$SetValueAsString(intV, as.integer(6), as.character(RATTable$Class_Name[i]))
        }
        
        band$SetDefaultRAT(rat)
        dr <- band$GetDefaultRAT()
        dr$DumpReadable()
        
    }
    
