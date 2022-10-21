library(raster)
library(terra)


rasterOptions(tmpdir = paste0('/scratch2/', ident, '/Rtmp'), progress = 'text')
terra::terraOptions(tempdir = paste0('/scratch2/', ident, '/Rtmp'), progress=1)

r2 <- terra::rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SOC/SOC_000_005_05_N_P_AU_TRN_N_20220727.tif')
r3 <- r2 * 10

terra::tempdir()
tmpFiles(orphan=T, old=T)
tmpDir()

tmpFiles()

filename(r3)

terra::sources(r3)
