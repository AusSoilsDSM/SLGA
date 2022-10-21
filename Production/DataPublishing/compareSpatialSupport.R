library(terra)

slga <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/V1/AWC/AWC_000_005_05_N_P_AU_NAT_C_20140801.tif')

v2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/CLY/CLY_000_005_95_N_P_AU_TRN_N_20210902.tif')
v2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SLT/SLT_000_005_95_N_P_AU_TRN_N_20210902.tif')
v2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/SND/SND_000_005_95_N_P_AU_TRN_N_20210902.tif')

v2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/PHW/PHW_000_005_95_N_P_AU_TRN_N_20222005.tif')

v2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DUL/DUL_000_005_95_N_P_AU_TRN_N_20210614.tif')
v2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/L15/L15_000_005_95_N_P_AU_TRN_N_20210614.tif')

v2 <- rast('/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/DES/DES_000_200_90_N_P_AU_TRN_C_20190901.tif')

compareGeom(slga, v2)

