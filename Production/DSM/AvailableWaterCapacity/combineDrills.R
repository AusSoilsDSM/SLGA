
rootDir = '/datasets/work/af-digiscapesm/work/Ross/TERN/AWC/MeasuredTextures/V2/Parsimonious'
outDir = paste0(rootDir)

fls  <- list.files(paste0(rootDir, '/Drills'),  pattern = '.rds', full.names = T)

dfs <- lapply(fls, readRDS)
dfAll <- do.call(rbind , dfs)
head(dfAll)
nrow(dfAll)
saveRDS(dfAll, paste0(rootDir,'/allDrills', '.rds'))

colnames(dfAll)
