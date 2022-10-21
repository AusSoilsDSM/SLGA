### Soil Depth
### Prepare data for modelling
# set 2m as the cut off for deep vs. not deep soils
# Assign GA rock obs as a binary variable (rock vs. rock)

## 
### modified 19/07/19
cnt=1 ## sort of a job identifier




#data
# observations
obs_dat<- readRDS("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/sd_siteDat_covariates_20192207.rds")
obs_dat<- as.data.frame(obs_dat)

## obs to keep
keeps<- read.csv("/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/selected_obs20192803.csv")
keeps<- c(as.matrix(keeps))


## remove spurious data
obs_dat2<- obs_dat[obs_dat$myFID %in% keeps,]
obs_dat2$depth_bin<- NA

# binary depth
obs_dat2[which(obs_dat2$FromDepth >= 2), "depth_bin"]<- 1
obs_dat2[which(obs_dat2$FromDepth < 2), "depth_bin"]<- 0
obs_dat2$depth_bin<- as.factor(obs_dat2$depth_bin)
summary(obs_dat2$depth_bin)

# GA rock outcrops vs everything else
len_gaRock<- nrow(obs_dat2[obs_dat2$type == "ga_rocks",])
len_gaRock
obs_dat2$GA_rocks<- NA
obs_dat2$GA_rocks[which(obs_dat2$type == "ga_rocks")]<- 1
obs_dat2$GA_rocks[which(obs_dat2$type != "ga_rocks")]<- 0
obs_dat2$GA_rocks<- as.factor(obs_dat2$GA_rocks)
summary(obs_dat2$GA_rocks)


str(obs_dat2)





## save new object
saveRDS(obs_dat2, "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/sd_siteDat_covariates_winnowed_20192207.rds")

## save object where soils are < 2m
sel.dat<- obs_dat2[which(obs_dat2$depth_bin == 0), ]
saveRDS(sel.dat, "/OSM/CBR/AF_TERN_MAL_DEB/WORK/projects/soilDepth_2019/soilDepth/data/sd_siteDat_covariates_lessthan2_20192207.rds")

# simulate data for the zero depth observations
#len_boreRock<- nrow(obs_dat[obs_dat$type == "bore_rock",])
len_gaRock<- nrow(sel.dat[sel.dat$type == "ga_rocks",])




## do some exploration
### data simulation
sel.dat$simDat<- NA
  
# simulate data for the zero depth observations
#len_boreRock<- nrow(obs_dat[obs_dat$type == "bore_rock",])
len_gaRock<- nrow(sel.dat[sel.dat$type == "ga_rocks",])
  
#obs_dat[which(obs_dat$type == "bore_rock"), "simDat"]<- runif(len_boreRock, min = 0, max = 0.15)
sel.dat[which(sel.dat$type == "ga_rocks"), "simDat"]<- runif(len_gaRock, min = 0, max = 0.10)
  
# simulate data for censored observations
len_censoredDat<- nrow(sel.dat[sel.dat$type == "ternsoil_censored",])
brC<- c(rbeta(len_censoredDat, shape1 = 2, shape2 =5.5))

censored_dat<- sel.dat[sel.dat$type == "ternsoil_censored", "FromDepth"]
sim_censoreddat<- censored_dat + (brC * censored_dat)
sel.dat[which(sel.dat$type == "ternsoil_censored"), "simDat"]<- sim_censoreddat
  
# the other sites
sel.dat[which(sel.dat$type == "bore_soil"), "simDat"]<- sel.dat[which(sel.dat$type == "bore_soil"), "FromDepth"]
sel.dat[which(sel.dat$type == "ternSoil_actual"), "simDat"]<- sel.dat[which(sel.dat$type == "ternSoil_actual"), "FromDepth"]
  
  
td<- sel.dat$simDat
td_log<- sqrt((sel.dat$simDat+0))

hist(td)
hist(td_log)
td_log[!is.finite(td_log)]

hist(log(10 - sqrt(sqrt(sel.dat$simDat))))
td2<-sqrt(sel.dat$simDat+0.001)
ad.test(td2)

par(mfrow = c(1, 2))
hist(td)
qqnorm(td, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(td, col = "red", lwd = 2)

par(mfrow = c(1, 2))
hist(td_log)
qqnorm(td_log, plot.it = TRUE, pch = 4, cex = 0.7)
qqline(td2, col = "red", lwd = 2)

hist(td2^2)



  