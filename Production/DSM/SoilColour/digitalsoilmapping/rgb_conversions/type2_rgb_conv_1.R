## SOIL COLOR 
# COVERT ESTIMATED LAB PREDCIITONS TO NEAREST RGB.
# Model type 2: 2,5,12

## libraries
library(parallel);library(sp);library(rgdal);library(doParallel);library(raster)

source.tiles<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/spatialPredictions/tiles/"
root.slurm<- "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/rcode/slurm/spatialprediction/rgb/type2/"

ref.colors<- read.csv(file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/data/soilColorRef_updated.csv")


fols<- as.numeric(list.files(source.tiles, full.names = FALSE))
length(fols)
fols<- fols[order(fols)]
fols

models<- c(2,5,12)
srt<- 1
fin<- 250

###
# begin parallel cluster and register it with foreach
cpus<- 8
cl<- makeCluster(spec=cpus)
# register with foreach
registerDoParallel(cl)


# Apply model to each tile
oper1<- foreach(i=srt:fin, .packages = c("raster", "sp", "rgdal")) %dopar% {
  
  #select the folder
  sfol<- fols[i]
  nm1<- paste0(source.tiles,sfol)
  
  # L models
  # top
  l.top<- raster(x = paste0(nm1,"/model_",models[1],"/surface_model_",models[1],"_L.tif"))
  a.top<- raster(x = paste0(nm1,"/model_",models[2],"/surface_model_",models[2],"_A.tif"))
  b.top<- raster(x = paste0(nm1,"/model_",models[3],"/surface_model_",models[3],"_B.tif"))
  
  # bottom
  l.bot<- raster(x = paste0(nm1,"/model_",models[1],"/subsoil_model_",models[1],"_L.tif"))
  a.bot<- raster(x = paste0(nm1,"/model_",models[2],"/subsoil_model_",models[2],"_A.tif"))
  b.bot<- raster(x = paste0(nm1,"/model_",models[3],"/subsoil_model_",models[3],"_B.tif"))
  
  # stack
  s1<- stack(l.top,a.top,b.top,l.bot,a.bot,b.bot)
  
  # convert to table
  tempD <- data.frame(cellNos = seq(1:ncell(s1)))
  vals <- as.data.frame(getValues(s1))
  tempD <- cbind(tempD, vals)
  comca<- which(complete.cases(tempD))
  tempD$type2_surface_R<- NA
  tempD$type2_surface_G<- NA
  tempD$type2_surface_B<- NA
  tempD$type2_subsoil_R<- NA
  tempD$type2_subsoil_G<- NA
  tempD$type2_subsoil_B<- NA
  tempD$type2_surface_cieL<- NA
  tempD$type2_surface_cieA<- NA
  tempD$type2_surface_cieB<- NA
  tempD$type2_subsoil_cieL<- NA
  tempD$type2_subsoil_cieA<- NA
  tempD$type2_subsoil_cieB<- NA
  tempE <- tempD[comca, ]
  cellNos <- c(tempD$cellNos)
  gXY <- data.frame(xyFromCell(s1, cellNos, spatial = FALSE))
  tempD <- cbind(gXY, tempD)
  str(tempD)
  names(tempE)
  names(tempD)
  
  # Go through each row
  for (j in 1:nrow(tempE)){
    
    # disitance calcs
    d1<-sqrt((tempE$surface_model_2_L[j] -  ref.colors$L.)^2 + (tempE$surface_model_5_A[j] -  ref.colors$a.)^2 + (tempE$surface_model_12_B[j] -  ref.colors$b.)^2)
    d1.sel<- which(d1 == min(d1))
    d2<-sqrt((tempE$subsoil_model_2_L[j] -  ref.colors$L.)^2 + (tempE$subsoil_model_5_A[j] -  ref.colors$a.)^2 + (tempE$subsoil_model_12_B[j] -  ref.colors$b.)^2)
    d2.sel<- which(d2 == min(d2))
    
    
    tempE[j,c(14,15,16,8,9,10)]<- ref.colors[d1.sel,c(10,11,12,13,14,15)]
    tempE[j,c(17,18,19,11,12,13)]<- ref.colors[d2.sel,c(10,11,12,13,14,15)]}
  
  # Make rasters
  tempD[comca,3:ncol(tempD)]<- tempE
  
  #RGB rasters
  for (k in 10:15){
    r1<- rasterFromXYZ(tempD[,c(1,2,k)])
    crs(r1)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    nmz<- paste0(nm1,"/",names(r1),".tif")
    writeRaster(x = r1,filename = nmz,format = "GTiff",datatype = "INT1U", overwrite = TRUE)}
  
  #LAB rasters
  for (k in 16:21){
    r1<- rasterFromXYZ(tempD[,c(1,2,k)])
    crs(r1)<- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    nmz<- paste0(nm1,"/",names(r1),".tif")
    writeRaster(x = r1,filename = nmz,format = "GTiff",datatype = "FLT4S", overwrite = TRUE)}
  

  # SLURM CHECK
  itOuts<- c(i,as.character(Sys.time()))
  nmx<- paste0(root.slurm,"/slurmckeck_", i,"_",sfol, ".txt")
  write.table(itOuts, 
              file = nmx,
              row.names = F, col.names = F, sep=",")}
    
  
  
  
  


