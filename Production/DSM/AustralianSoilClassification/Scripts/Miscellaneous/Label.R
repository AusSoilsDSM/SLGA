##########################################################################
###       Packages
##########################################################################

library(plyr)


##########################################################################
###       Initialise
##########################################################################

clr = read.table("Z:/Harry/QGIS Legend/ASC_SUBORDER.clr", header = FALSE, sep = "")

root.directory = '//osm-27-cdc.it.csiro.au/OSM_CBR_AF_DIGISCAPESM_work/Harry'
order.original = readRDS(paste0(root.directory, '/OriginalFactors.rds'))
suborder.original = readRDS(paste0(root.directory, '/OriginalSubOrders.rds'))

order.name = c('Vertosol',
               'Sodosol',
               'Dermosol',
               'Chromosol',
               'Ferrosol',
               'Kurosol',
               'Tenosol',
               'Kandosol',
               'Hydrosol',
               'Podosol',
               'Rudosol',
               'Calcarasol',
               'Organosol',
               'Anthroposol')

suborder.name = c("Red", "Brown", "Leptic", "Bleached-Orthic", 
                  "Chernic", "Brown-Orthic", "Semiaquic", "Aeric", 
                  "Yellow", "Oxyaquic", "Brown-Orthic", "Arenic", "Clastic",
                  "Grey", "Black", "Shelly", "Redoxic", "Grey-Orthic", "Stratic",
                  "Spolic", "Aquic", "Chernic-Leptic", "Lutic",
                  "Supratidal", "Extratidal", "Hemic",
                  "Sapric", "Fibric", " Bleached-Leptic", "Red-Orthic",
                  "Salic", "Hypersalic", "Calcic", 
                  "Hypocalcic", "Hortic", "Dredgic", "Black-Orthic", "Cumulic", 
                  "Urbic", "Carbic", "Hypercalcic", "Scalpic", "Lithocalcic", 
                  "Calcenic", "Supracalcic", "Hypergypsic", "Sesqui-Nodular", "Garbic")

##########################################################################
###       Construct table for QGIS label
##########################################################################

new = data.frame(matrix(nrow = length(suborder.name)*length(order.name), ncol = 6))
n=1
for (i in 1:length(order.original)){
  for (j in 1:length(suborder.original)){
    new[n,1] = i*100 + j
    new[n, c(2,3,4,5)] = c(100,100,100,100)
    new[n,6] = paste(suborder.name[j], order.name[i], sep = " ")
    n = n + 1
    cat(subord, "\n")
  }
}



write.csv(new,"Z:/Harry/QGIS Legend/ASC_SUBORDER.csv")
