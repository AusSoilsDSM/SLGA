### Creation of RGB Images
library(raster)

root<- "Z:/projects/ternlandscapes_2019/soilColour/spatialPredictions/"

files<- list.files(path = root, pattern = ".tif", full.names = T)
files

s1<- stack(raster(files[8]),raster(files[7]),raster(files[6]))
s1

plotRGB(s1)
writeRaster(x = s1,filename = paste0(root, "surface_RGB_updated.tif"),format = "GTiff",datatype = "INT1U", overwrite = TRUE)


files

s1<- stack(raster(files[5]),raster(files[4]),raster(files[3]))
s1

plotRGB(s1)
writeRaster(x = s1,filename = paste0(root, "subsoil_RGB_updated.tif"),format = "GTiff",datatype = "INT1U", overwrite = TRUE)


## Bare earth data
be.files<- "Z:/datasets/national/covariates/mosaics/"

be.files1<- list.files(path = be.files,pattern = "Veg_",full.names = T)

s1<- stack(raster(be.files1[35]),raster(be.files1[34]),raster(be.files1[33]))
s1

plotRGB(s1)
writeRaster(x = s1,filename = paste0(root, "bareEarth_RGB.tif"),format = "GTiff",datatype = "INT1U", overwrite = TRUE)
Z

