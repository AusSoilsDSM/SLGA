### TERN LANDSCAPES 
# AWC 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.8.22
# modified: 29.8.22

# CODE PURPOSE
# Calculate AWC from subtracting LL15 from DUL
# test code in order to take account of prediction variance 
# apply to small area and for single depth

library(terra)
terraOptions(progress=1)

source('c:/PrivateInfo/PrivateInfo.R')

data.root<- "/datasets/work/lw-soildatarepo/work/http/Products/TERN/SLGA/"

att = 'AWC'
depths <- c("000_005", "005_015", "015_030", "030_060", "060_100", "100_200")

for (i in 3:length(depths)) {
  
    d <- depths[i]
    d
    print(paste0('Processing ', d))
    
    # dul rasters
    dul.ll <- rast( paste0(data.root, "DUL/DUL_", d, "_05_N_P_AU_TRN_N_20210614.tif"))
    dul.ev <- rast( paste0(data.root, "DUL/DUL_", d, "_EV_N_P_AU_TRN_N_20210614.tif"))
    dul.ul <- rast( paste0(data.root, "DUL/DUL_", d, "_95_N_P_AU_TRN_N_20210614.tif"))
    # ll raster
    l15.ll <- rast( paste0(data.root, "L15/L15_", d, "_05_N_P_AU_TRN_N_20210614.tif"))
    l15.ev <- rast( paste0(data.root, "L15/L15_", d, "_EV_N_P_AU_TRN_N_20210614.tif"))
    l15.ul <- rast( paste0(data.root, "L15/L15_", d, "_95_N_P_AU_TRN_N_20210614.tif"))
    
    #### calculate AWC... DUL - LL15
    awc.raster<- dul.ev - l15.ev
    awc.raster
    #plot(awc.raster)
    
    ### calculate uncertainty of AWC
    # variance of DUL
    dul.var<- ((dul.ul-dul.ll)/(2*1.64))^2
    dul.var
    #plot(dul.var)
    
    # variance of LL15
    ll.var<- ((l15.ul -l15.ll)/(2*qnorm(0.95)))^2
    ll.var
    #plot(ll.var)
    
    # combined variance [assuming no correlation]
    comb.var<- dul.var + ll.var
    comb.var
    
    
    ### derive awc prediciton intervals
    # standard deviation
    comb.sd <- sqrt(comb.var)
    # standard error
    comb.se <- comb.sd * qnorm(0.95)
    # upper prediction limit
    awc.upl<- awc.raster + comb.se
    awc.upl
    plot(awc.upl)
    # lower prediction limit
    awc.lpl<- awc.raster - comb.se
    awc.lpl
    # change all <0 to 0
    awc.lpl[awc.lpl < 0] <- 0
    plot(awc.lpl)
    
    outD <- paste0('/scratch1/', ident, '/AWCPreds')
    if(!dir.exists(outD)){dir.create(outD)}
    
    terra::writeRaster(awc.lpl, paste0(outD, "/AWC_", d, "_05_N_P_AU_TRN_N_20210614.tif"))
    terra::writeRaster(awc.raster, paste0(outD, "/AWC_", d, "_EV_N_P_AU_TRN_N_20210614.tif"))
    terra::writeRaster(awc.upl, paste0(outD, "/AWC_", d, "_95_N_P_AU_TRN_N_20210614.tif"))

}



