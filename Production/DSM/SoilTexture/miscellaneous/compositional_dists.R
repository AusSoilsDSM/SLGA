install.packages("Compositional")
install.packages("soiltexture")
library(Compositional);library(soiltexture)


## texture class centroids from Minasny and Mcbratney 2001
dat<- read.csv(file = "Z:/projects/ternlandscapes_2019/soiltexture/data/textureclasscentroids.csv")
dat


# select a texture class: Loam
sel<- dat[dat$name_short == "LS",]
sel
# sample from distribution
x <- rdiri(n = 25, c(sel[,"Clay"], sel[,"Silt"], sel[,"Sand"]) )
x

diri.est(x)
colMeans(x)
sel

names(x)
x<- as.data.frame(x)
names(x)<- c("CLAY", "SILT", "SAND")
x<- x*100

#TT.plot( class.sys = "AU2.TT" )
TT.plot(
  class.sys = "AU2.TT",
  tri.data = x,
  main = "Soil texture data"
) #

