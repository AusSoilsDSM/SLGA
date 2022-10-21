
set.seed(5344)
k1370_random <- distinctColorPalette(k = 1370)

leaflet() %>%
  addTiles(group="OSM (default)") %>%
  addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>%  
  addRasterImage(PdgnAus, 
                 layerId = "Pedogenons",
                 colors=k1370_random, project=FALSE,
                 maxBytes = 300000000, group = "Pedogenons") #%>%
  leafem::addMouseCoordinates() %>%
  leafem::addImageQuery(PdgnAus, type="mousemove", layerId = "Pedogenons")%>%
  addLayersControl(
    baseGroups = c("OSM (default)","World Imagery"),
    overlayGroups = c("Pedogenons"),
    options = layersControlOptions(collapsed = FALSE))#%>%
  # addOpacityControls(collapsed = FALSE,
  #                    group = "Pedogenons",
  #                    position = "bottomright", 
  #                    title = "Opacity Control")

k1370.df <- k1370_excl.out$branch.centroids.ord

set.seed(233)
centroids.umap = umap(centroids)
plot((centroids.umap$layout[,2]),(centroids.umap$layout[,1]),
     col=k1370_random,
     xlab= "Umap Centroids 1", ylab="Umap Centroids 2",
     pch=19)

### Let's use a bivariate colour palette better
library(biscale)
#install.packages("biscale", dependencies = TRUE)
centroids.umap.dim <- as.data.frame(centroids.umap$layout)
# equal breaks
biscale.cols <- bi_class(.data = centroids.umap.dim, keep_factors = TRUE, style = "quantile", dim = 4, x = V1, y=V2)

### Attach biclass to colours and umap dimensions
k1370.df$biclass <- biscale.cols$bi_class
centroids.umap.dim$biclass <- biscale.cols$bi_class

#BlueOr
#PinkGrn
bi_pal(pal="DkBlue2", dim = 4, preview = TRUE, flip_axes = FALSE, rotate_pal = FALSE)

### Map umap dimensions
ggplot() +  geom_point(aes(x = V1, y =V2, colour= biclass,), size=2,
                       data = centroids.umap.dim, show.legend = FALSE) +
  bi_scale_color(pal = "PinkGrn", dim = 4) + theme_bw()

legend <- bi_legend(pal = "PinkGrn",
                    dim = 4,
                    xlab = "Umap1",
                    ylab = "Umap2",
                    size = 20);legend
a <- bi_pal("BlueOr", dim = 4, preview = FALSE, flip_axes = FALSE, rotate_pal = FALSE)
col.code <- data.frame(biclass = names(a), BlueOr = a)
k1370.df <- left_join(x = k1370.df, y=col.code)
head(k1370.df)
table(k1370.df$biclass)

### Map with biclass colours
leaflet() %>%
  addTiles(group="OSM (default)") %>%
  #addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>%  
  addRasterImage(PdgnAus, 
                 layerId = "Pedogenons",
                 colors=k1370.df$BlueOr, project=FALSE,
                 maxBytes = 300000000, group = "Pedogenons") #%>%
  # addLayersControl(
  #   baseGroups = c("OSM (default)","World Imagery"),
  #   overlayGroups = c("Pedogenons"),
  #   options = layersControlOptions(collapsed = FALSE))
  #leafem::addMouseCoordinates() %>%
  #leafem::addImageQuery(PdgnAus, type="mousemove", layerId = "Pedogenons")%>%
 

