## Variogram Model fitting
library(matrixStats);library(automap);library(gstat);library(sp);library(rgdal)
## Model residuals

source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/residual_modelling/resids_clay_d1.R")
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/residual_modelling/resids_clay_d2.R")
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/residual_modelling/resids_clay_d3.R")
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/residual_modelling/resids_clay_d4.R")
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/residual_modelling/resids_clay_d5.R")
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/residual_modelling/resids_clay_d6.R")
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/residual_modelling/resids_sand_d1.R")
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/residual_modelling/resids_sand_d2.R")
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/residual_modelling/resids_sand_d3.R")
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/residual_modelling/resids_sand_d4.R")
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/residual_modelling/resids_sand_d5.R")
source("/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/rcode/digitalsoilmapping/residual_modelling/resids_sand_d6.R")


