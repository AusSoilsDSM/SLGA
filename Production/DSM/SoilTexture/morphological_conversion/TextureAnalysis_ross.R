#################################
###  Author : Ross Searle         
###  Date : Tue Nov 27 11:11:00 2018                      
###  Project : TERN    
###  Purpose : to analyse the texture class data to generate numeric values we can use in the PSA spatial mmodelling
#################################

library(DBI)
library(odbc)
library(stringr)
library(dplyr)
library(ggplot2)
library(RSQLite)
library(stringr)

source('c:/PrivateInfo/PrivateInfo.R')

gitRoot <- paste0('C:/Users/', ident, '/Dropbox/RossRCode/Git')
dataRoot <- 'C:/Projects/TernLandscapes/Site Data'

source(paste0(gitRoot, '/AusSoilsDSM/SLGA/Production/NSSC/NSSCHelpers.R'))



dbPath <- paste0(dataRoot, "/NSSC_2.0.0.sqlite")
Litecon <- dbConnect(RSQLite::SQLite(), dbPath)

### get the vaild texture codes from the database
sql <- "select * from codes where code_domain = 'C_H_TEXTURE'"
codes <- doQuery(Litecon, sql)
write.csv(codes, 'c:/temp/textureCodes.csv')


# query the data from the database
sql <- "SELECT dbo_LAB_METHODS.LABM_SHORT_NAME, dbo_SITES.agency_code, dbo_SITES.proj_code, dbo_SITES.s_id, dbo_OBSERVATIONS.o_longitude_GDA94, dbo_OBSERVATIONS.o_latitude_GDA94, dbo_SAMPLES.samp_upper_depth, dbo_SAMPLES.samp_lower_depth, dbo_OBSERVATIONS.o_ppf, dbo_OBSERVATIONS.o_gsg, dbo_OBSERVATIONS.o_asc_ord, dbo_OBSERVATIONS.o_asc_subord, dbo_HORIZONS.h_texture, dbo_LAB_RESULTS.labm_code, dbo_LAB_RESULTS.labr_value, dbo_HORIZONS.h_texture_qual
FROM (dbo_SITES INNER JOIN (((dbo_OBSERVATIONS INNER JOIN dbo_HORIZONS ON (dbo_OBSERVATIONS.agency_code = dbo_HORIZONS.agency_code) AND (dbo_OBSERVATIONS.proj_code = dbo_HORIZONS.proj_code) AND (dbo_OBSERVATIONS.s_id = dbo_HORIZONS.s_id) AND (dbo_OBSERVATIONS.o_id = dbo_HORIZONS.o_id)) INNER JOIN dbo_SAMPLES ON (dbo_HORIZONS.agency_code = dbo_SAMPLES.agency_code) AND (dbo_HORIZONS.proj_code = dbo_SAMPLES.proj_code) AND (dbo_HORIZONS.s_id = dbo_SAMPLES.s_id) AND (dbo_HORIZONS.o_id = dbo_SAMPLES.o_id) AND (dbo_HORIZONS.h_no = dbo_SAMPLES.h_no)) INNER JOIN dbo_LAB_RESULTS ON (dbo_SAMPLES.agency_code = dbo_LAB_RESULTS.agency_code) AND (dbo_SAMPLES.proj_code = dbo_LAB_RESULTS.proj_code) AND (dbo_SAMPLES.s_id = dbo_LAB_RESULTS.s_id) AND (dbo_SAMPLES.o_id = dbo_LAB_RESULTS.o_id) AND (dbo_SAMPLES.h_no = dbo_LAB_RESULTS.h_no) AND (dbo_SAMPLES.samp_no = dbo_LAB_RESULTS.samp_no)) ON (dbo_SITES.agency_code = dbo_OBSERVATIONS.agency_code) AND (dbo_SITES.proj_code = dbo_OBSERVATIONS.proj_code) AND (dbo_SITES.s_id = dbo_OBSERVATIONS.s_id)) INNER JOIN dbo_LAB_METHODS ON dbo_LAB_RESULTS.labm_code = dbo_LAB_METHODS.LABM_CODE
WHERE (((dbo_LAB_METHODS.LABM_SHORT_NAME)='Clay'));"

res <- doQuery(Litecon, sql)
head(res)
nrow(res)

# Calculate some stats

grps <- aggregate(res[, 'labr_value'], list(res$h_texture), mean)
grpsSD <- aggregate(res[, 'labr_value'], list(res$h_texture), sd)
grpscnt <- aggregate(res[, 'labr_value'], list(res$h_texture), length)
grpsmin <- aggregate(res[, 'labr_value'], list(res$h_texture), min)
grpsmax <- aggregate(res[, 'labr_value'], list(res$h_texture), max)

textStats <- data.frame(class=grps$Group.1, mean=grps$x, sd=grpsSD$x, cnt=grpscnt$x, min=grpsmin$x, max=grpsmax$x, stringsAsFactors = F)


g2 <- textStats[order(textStats$mean),]
g3 <- g2[g2$cnt > 50,]
g3

res2 <- res[res$h_texture %in% g3$class, ]
res2$h_texture = reorder(res2$h_texture, res2$labr_value, mean)
boxplot(labr_value~h_texture, data = res2, col=rainbow(nrow(g3)))


####  Residuals distribution for a couple of texture classes
mc <- res2[res2$h_texture== 'MC',]
mcMean <- mean(mc$labr_value)
mcRes <- mc$labr_value - mcMean
hist(mcRes)

mc <- res2[res2$h_texture== 'SL',]
mcMean <- mean(mc$labr_value)
mcRes <- mc$labr_value - mcMean
hist(mcRes)


# check to see if there is diference in the Krasnozems etc compared to the rest of the soils

sql <- "SELECT dbo_LAB_METHODS.LABM_SHORT_NAME, dbo_SITES.agency_code, dbo_SITES.proj_code, dbo_SITES.s_id, dbo_OBSERVATIONS.o_longitude_GDA94, dbo_OBSERVATIONS.o_latitude_GDA94, dbo_SAMPLES.samp_upper_depth, dbo_SAMPLES.samp_lower_depth, dbo_OBSERVATIONS.o_ppf, dbo_OBSERVATIONS.o_gsg, dbo_OBSERVATIONS.o_asc_ord, dbo_OBSERVATIONS.o_asc_subord, dbo_HORIZONS.h_texture, dbo_LAB_RESULTS.labm_code, dbo_LAB_RESULTS.labr_value, dbo_HORIZONS.h_texture_qual
FROM (dbo_SITES INNER JOIN (((dbo_OBSERVATIONS INNER JOIN dbo_HORIZONS ON (dbo_OBSERVATIONS.agency_code = dbo_HORIZONS.agency_code) AND (dbo_OBSERVATIONS.proj_code = dbo_HORIZONS.proj_code) AND (dbo_OBSERVATIONS.s_id = dbo_HORIZONS.s_id) AND (dbo_OBSERVATIONS.o_id = dbo_HORIZONS.o_id)) INNER JOIN dbo_SAMPLES ON (dbo_HORIZONS.agency_code = dbo_SAMPLES.agency_code) AND (dbo_HORIZONS.proj_code = dbo_SAMPLES.proj_code) AND (dbo_HORIZONS.s_id = dbo_SAMPLES.s_id) AND (dbo_HORIZONS.o_id = dbo_SAMPLES.o_id) AND (dbo_HORIZONS.h_no = dbo_SAMPLES.h_no)) INNER JOIN dbo_LAB_RESULTS ON (dbo_SAMPLES.agency_code = dbo_LAB_RESULTS.agency_code) AND (dbo_SAMPLES.proj_code = dbo_LAB_RESULTS.proj_code) AND (dbo_SAMPLES.s_id = dbo_LAB_RESULTS.s_id) AND (dbo_SAMPLES.o_id = dbo_LAB_RESULTS.o_id) AND (dbo_SAMPLES.h_no = dbo_LAB_RESULTS.h_no) AND (dbo_SAMPLES.samp_no = dbo_LAB_RESULTS.samp_no)) ON (dbo_SITES.agency_code = dbo_OBSERVATIONS.agency_code) AND (dbo_SITES.proj_code = dbo_OBSERVATIONS.proj_code) AND (dbo_SITES.s_id = dbo_OBSERVATIONS.s_id)) INNER JOIN dbo_LAB_METHODS ON dbo_LAB_RESULTS.labm_code = dbo_LAB_METHODS.LABM_CODE
WHERE (((dbo_LAB_METHODS.LABM_SHORT_NAME)='Clay') AND ((dbo_OBSERVATIONS.o_gsg)='K')) OR (((dbo_LAB_METHODS.LABM_SHORT_NAME)='Clay') AND ((dbo_OBSERVATIONS.o_gsg)='E')) OR (((dbo_LAB_METHODS.LABM_SHORT_NAME)='Clay') AND ((dbo_OBSERVATIONS.o_asc_ord)='FE')) OR (((dbo_LAB_METHODS.LABM_SHORT_NAME)='Clay') AND ((dbo_OBSERVATIONS.o_ppf)='gn3.11')) OR (((dbo_LAB_METHODS.LABM_SHORT_NAME)='Clay') AND ((dbo_OBSERVATIONS.o_ppf)='gn3.12')) OR (((dbo_LAB_METHODS.LABM_SHORT_NAME)='Clay') AND ((dbo_OBSERVATIONS.o_ppf)='gn3.14')) OR (((dbo_LAB_METHODS.LABM_SHORT_NAME)='Clay') AND ((dbo_OBSERVATIONS.o_ppf)='gn3.10')) OR (((dbo_LAB_METHODS.LABM_SHORT_NAME)='Clay') AND ((dbo_OBSERVATIONS.o_ppf)='gn3.17'));"

resK <- doQuery(Litecon, sql)
head(resK)
nrow(resK)



krazIDs <- which(  (str_to_upper( res2$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( res2$o_asc_ord ) == 'FE'  )
                 | (str_to_upper( res2$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( res2$o_gsg) == 'K'  )
                 | (str_to_upper( res2$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( res2$o_gsg) == 'E'   )
                 | (str_to_upper( res2$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( res2$o_ppf) == 'GN3.11'  )
                 | (str_to_upper( res2$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( res2$o_ppf) == 'GN3.12'  )
                 | (str_to_upper( res2$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( res2$o_ppf) == 'GN3.14'  )
                 | (str_to_upper( res2$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( res2$o_ppf) == 'GN3.10'  )
                 | (str_to_upper( res2$LABM_SHORT_NAME) == 'CLAY' & str_to_upper( res2$o_ppf) == 'GN3.17'  )
              )
length(krazIDs)

kraz <- res2[krazIDs,]
others <- res2[-krazIDs,]

kgrps <- aggregate(kraz[, 'labr_value'], list(kraz$h_texture), mean)
kgrpscnt <- aggregate(kraz[, 'labr_value'], list(kraz$h_texture), length)
kdf <- data.frame(kgrps,kgrpscnt$x)

ogrps <- aggregate(others[, 'labr_value'], list(others$h_texture), mean)
ogrpscnt <- aggregate(others[, 'labr_value'], list(others$h_texture), length)
odf <- data.frame(ogrps, ogrpscnt)

cdf <- merge(kdf, odf, by='Group.1')
cdf <- cdf[,-5]
colnames(cdf) <- c('Texture', 'Kraz', 'krazCnt', 'AllOther', 'otherCnt')
cdf <- cdf[order(cdf$AllOther), ]


plot(cdf$Kraz, t='l', xaxt="n", col= 'red', xlab = 'Texture Group', ylab = "Percent Clay", main = "Clay Percentages - Texture Grps Vs PSA")
lines(cdf$AllOther, t='l')
axis(1, at=seq(1:length(cdf$Texture)),labels=cdf$Texture, col.axis="black", las=2)
legend("bottomright", title = "Soil Types", c('Ferrosols', 'Other Soils'),lwd=1, col = c('red', 'black'))



cdf$Dif <- cdf$Kraz - cdf$AllOther

mcalc <- cdf[cdf$krazCnt > 50, ]
kadjust<- mean(mcalc$Dif)
kadjust
mcalc <- cdf[cdf$krazCnt > 100, ]
kadjust<- mean(mcalc$Dif)
kadjust




lcs <- others[others$h_texture == 'MC',]
mlcs <- mean(others$labr_value)
