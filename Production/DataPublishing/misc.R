library(terra)
terraOptions(prog=1)

od <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/CLY/CLY_000_005_05_N_P_AU_TRN_N_20210902.tif')
nd <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SOC/SOC_000_005_05_N_P_AU_NAT_N_20220727.tif')

compareGeom(nd, od)



nd <- rast('/datasets/work/af-digiscapesm/work/Ross/TERN/SOCV2/SLGAReady/SOC_000_005_EV_N_P_AU_NAT_N_20120404.tif')
od <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/V1/SOC/SOC_000_005_EV_N_P_AU_NAT_C_20140801.tif')
compareGeom(nd, od)


inRaster <- rast('/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/spatialPredictions/subsoil_RGB.tif')
resampleTemplate <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SOC/SOC_000_005_EV_N_P_AU_NAT_C_20140801.tif')
compareGeom(inRasterPath, resampleTemplate)


#tr <- doResample(inRaster=tr, resampleTemplate=templateR)
trs <- terra::resample(inRaster, resampleTemplate, method = 'near')
print(paste0('Writing resampled raster to - ', tmpRpath))
terra::writeRaster(trs, filename=tmpRpath, overwrite=T)

fname <- '/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SOC/SOC_000_005_EV_N_P_AU_NAT_C_20140801.tif'
fname <- '/datasets/work/af-digiscapesm/work/Ross/TERN/SOCV2/TOC_0_5_pred.tif'


fls <- list.files('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA', pattern = '.tif$', recursive = T, full.names = F)

for (i in 1:length(fls)) {
  #cat(i, " ")
  res <- isValidCOG(fls[i], quiet=T)
  if(!res){
    print(paste0('Problem with - ', fls[i]))
  }
}







