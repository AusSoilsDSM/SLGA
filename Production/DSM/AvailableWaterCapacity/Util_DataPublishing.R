#library(Rcpp)
library(reticulate)
library(xml2)
library(htmltidy)
library(stringr)
# library(terra)
# library(raster)

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


publishRaster  <- function(inRasterPath=NULL, outDir=NULL, adjustVal=1, resampleTemplate=NULL, maskTemplate=NULL, sigFigures=-1, metadataTable=NULL, RATTable=NULL, MakeCOG=F){
  
  tmpDir <- paste0('/scratch2/', ident, '/tmp')
  
  # froot <- str_replace( basename(inRasterPath), '.tif', '')
  # fls <- list.files(tmpRpath, pattern = froot, full.names = T)
  # 
  # if(length(fls)>0){
  #   for (h in 1:length(fls)) {
  #     
  #     r <- unlink(fls[h])
  #     if(r==1){
  #       stop('Problem deleting files.....')
  #     }
  #   }
  # }
  # 
  
  
  filt <- paste0(att, '_', depth, '_', prod)
  
  fls <- list.files(inDir, recursive = F, pattern = paste0(filt, '.tif'), full.names = T )
  print(paste0("Number of tiles = ", length(fls)))
  csvPath <- paste0(dirname(inDir), '/', filt,'.csv' )
  write.table(fls, csvPath, row.names = F,  col.names = F, quote = F)
  print('Generating VRTs')
  system(paste0(gdalPath, '/gdalbuildvrt -input_file_list ',csvPath, ' ', paste0(dirname(inDir), '/', filt,'.vrt' )))
  
  inRasterPath <- paste0(dirname(inDir), '/', filt,'.vrt' )
  tmpRpath <- paste0(tmpDir, '/',str_replace(basename(inRasterPath), '.vrt', '.tif'))
  dst_filename = paste0(outDir, '/', basename(tmpRpath))
  
  
  # templateR <- rast('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif')
  # print('Resampling ......')
  # tr <- terra::rast(paste0(dirname(inDir), '/', filt, '.vrt'))
  # trs <- terra::resample(tr, templateR, method = 'near')
  # #trsc <- terra::clamp(trs, upper=50)
  # 
  
  tr=NULL
  
  if(!is.null(resampleTemplate))
  {
    print(paste('Resampling ',inRasterPath))
    terraOptions(prog=1)
    tr <- terra::rast(inRasterPath)
    templateR <- terra::rast(resampleTemplate)
    #tr <- doResample(inRaster=tr, resampleTemplate=templateR)
    trs <- terra::resample(tr, templateR, method = 'near')
    print(paste0('Writing resampled raster to - ', tmpRpath))
    terra::writeRaster(trs, filename=tmpRpath, overwrite=T)
  }
  
  # if(fillHoles){
  #   raster::rasterOptions(progress = 'text') 
  #   templateR <- raster::raster('/datasets/work/af-tern-mal-deb/work/datasets/national/covariates/mosaics/90m/Relief_dems_3s_mosaic1.tif')
  #   rrs <- raster::raster(tmpRpath)
  #   print('Filling .....')
  #   r2 <- raster::focal(rrs, w=matrix(1,3,3),  fun=mean, na.rm=T, NAonly=T)
  #   raster::writeRaster(r2, filename=tmpRpath, overwrite=T)
  # }
  
  if(!is.null(maskTemplate))
  {
    print('Applying mask ......')
    terraOptions(memmax=10)
    tr <- terra::rast(tmpRpath)
    templateMR <- terra::rast(maskTemplate)
    mR <- terra::mask(tr, templateMR)
  }
  
  
  if(is.null(mR)){
    print(paste0('Making copy of ', basename(inRasterPath), ' to ', tmpRpath))
    file.copy(from=inRasterPath, to=tmpRpath, overwrite = T)
  }else{
    
    if(adjustVal != 1){
      print('Adjusting values.....')
      print(paste0('Applying value adjustment of ', adjustVal))
      mRA <- mR * adjustVal   ####  adjust value to percent
    }else{
      mRA <- mR
    }
    print(paste0('Writing raster ', tmpRpath))
    writeRaster(mRA, tmpRpath, overwrite=TRUE, gdal=c("COMPRESS=NONE", "TILED=NO"))
  }
  
  #####3 Implement significant figures
  if(sigFigures == -1){
    print(paste0('Not enforcing significant figures'))
   
  }else{
    r <- rast(tmpRpath)
    print(paste0('Enforcing ', sigFigures, ' Decimal places in output raster'))
    if(sigFigures==0){
        rr <- round(r,digits=0)
        rr
        writeRaster(rr,  tmpRpath, gdal=c("COMPRESS=NONE", "TILED=NO"), datatype='int1U', overwrite=T)
    }else{
      rr <- round(r,digits=sigFigures)
      rr
      writeRaster(rr, tmpRpath, gdal=c("COMPRESS=NONE", "TILED=NO"), datatype='FLT4S', overwrite=T)
    }
  }
  

  ds = geo$gdal$Open(tmpRpath, geo$gdal$GA_Update)
  band<- ds$GetRasterBand(as.integer(1))
  
  if(!is.null(metadataTable)){
    #insertMetadata(metadataTable)
    print(paste0('Adding Metadata....'))
    for (b in 1:nrow(metadataTable)) {
      itemName <- metadataTable[b,1]
      itemVal <- metadataTable[b,2]
      if(itemName=='DATASET_TITLE'){
        bn <- basename(inRasterPath)
        bits <- str_split(bn, '_')
        p <- bits[[1]][4]
        d <- bits[[1]][3]
        prodv <- prods[prods$code==p, 2]
        depthv <- depths[depths$code==d, 2]
        itemVal<- str_replace(itemVal, 'XXXX', depthv)
        itemVal<- str_replace(itemVal, 'YYYY', prodv)
      }
     # ds$SetMetadataItem(itemName,itemVal )
    }
    
    print("Finished metadata")
   # xml <- geo$gdal$Info(ds)
  #  xml_view(xml)
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
  print('Generating COG')
  #outfilename <- str_replace(dst_filename, '.tif', '_cog.tif')
  system(paste0(gdalPath, '/gdaladdo --config GDAL_CACHEMAX 5000 -ro ',inRasterPath))
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
    
