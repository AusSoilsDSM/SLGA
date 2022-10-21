library(Rcpp)
library(reticulate)
library(xml2)
library(htmltidy)
library(stringr)



geo <- import('osgeo')


insertMetadata <- function(inRasterPath=NULL, outDir=NULL, metadataTable=NULL, RATTable=NULL, MakeCOG=T){

  #geo$gdal$AllRegister()
  
  froot <- str_replace( basename(inRasterPath), '.tif', '')
  fls <- list.files(outDir, pattern = froot, full.names = T)
  
  if(length(fls)>0){
    for (h in 1:length(fls)) {
      
      r <- unlink(fls[h])
      if(r==1){
        stop('Problem deleting files.....')
      }
    }
  }
 
  
  dst_filename = paste0(outDir, '/', basename(inRasterPath))
  
  print(paste0('Making copy of ', inRasterPath))
  file.copy(from=inRasterPath, to=dst_filename, overwrite = T)
  
 # src_ds = geo$gdal$Open(inRasterPath)
 # dst_ds = geo$gdal$Driver$CreateCopy(dst_filename, src_ds)
    ds = geo$gdal$Open(dst_filename, geo$gdal$GA_Update)
    
    band<- ds$GetRasterBand(as.integer(1))
    
    
    if(!is.null(metadataTable)){
      print(paste0('Adding Metadata....'))
        for (b in 1:nrow(metadataTable)) {
      #for (b in 1:1) {
          ds$SetMetadataItem(metadataTable[b,1], metadataTable[b,2])
          print(metadataTable[b,1])
        }
    }
    
    
    xml <- geo$gdal$Info(ds)
    xml_view(xml)
    
    
    
    if(!is.null(RATTable)){
      
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
    
    system(paste0('C:/LocalProgs/QGIS3.2/bin/gdalinfo -mdd all ', dst_filename))
    
    if(MakeCOG){
      
      print(paste0('Making COG.....'))
      system(paste0('C:/LocalProgs/QGIS3.2/bin/gdal_translate ', dst_filename ,' ', str_replace(dst_filename, '.tif', '_COG.tif'),' -ot byte -of COG -co COMPRESS=LZW'))
      dst_filename<- str_replace(dst_filename, '.tif', '_COG.tif')
      
    }

    return(dst_filename)
}


# system(paste0('C:/LocalProgs/QGIS3.2/bin/gdal_translate ', ,'C:/Temp/meta/smips_totalbucket_mm_20190105.tif C:/Temp/meta/smips_totalbucket_mm_20190105_cog.tif -of COG -co COMPRESS=LZW'))
# 
# source_python("C:/Temp/meta/Validate_COG.py")
# py_run_file("C:/Temp/meta/Validate_COG.py  filename=smips_totalbucket_mm_20190105_cog.tif")
# 
# validate_cloud_optimized_geotiff("smips_totalbucket_mm_20190105_cog.tif")
