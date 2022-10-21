flss <- list.files('/datasets/work/af-digiscapesm/work/Ross/TERN/ASCOnly/BootModels', pattern = '.rds', full.names = T)

for (i in 1:length(flss)) {
  print(i)
  model <- readRDS(flss[i])
  d <- getRasterDecodes(model)
  print(d)
  
}

getRasterDecodes <- function(model = model){
  ids <- model$forest$class.values
  cats <-  as.character(model$forest$levels[ids])
  decodes <- data.frame( RID = ids, Category = cats)
  decodesOrd <- decodes[order(decodes$RID),]
  return (decodesOrd)
}