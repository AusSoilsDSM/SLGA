### TERN LANDSCAPES 
# Soil pH 
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 26.2.20
# modified: 26.02.20

# CODE PURPOSE
# Check for co-located pH across different methods and seek to develop tranfers between the methods
# possible scenarios:
# convert field estimates to 4A1
# convert field estimates to 4B1/4B2
# convert 4NR to 4A1
# convert 4NR to 4B1/4B2
# 
# 
# CURRENT WORK IS PREDOMINANTLY FOR 4A1 - FIELD RELATIONSHIP

library(sp)

# get mode of a dataset
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# root directory
root.lab<- "Z:/projects/ternlandscapes_2019/soil_pH/data/curated_lab/"
root.field<- "Z:/projects/ternlandscapes_2019/soil_pH/data/fieldObs/curated_field/"
root.out<- "Z:/projects/ternlandscapes_2019/soil_pH/data/field_2_4a1_dists/"

# FILES
# FIELD DATA
field.dat<- readRDS(paste0(root.field, "field_ph_data_splined_dsm_ARD.rds"))

# LAB DATA
# 4A1
lab.4a1<- readRDS(paste0(root.lab, "4A1/lab_ph_4A1_data_splined_dsm_ARD.rds"))

# 4B1
lab.4b1<- readRDS(paste0(root.lab, "4B1/lab_ph_4B1_data_splined_dsm_ARD.rds"))

# 4B2
lab.4b2<- readRDS(paste0(root.lab, "4B2/lab_ph_4B2_data_splined_dsm_ARD.rds"))

# 4NR
lab.4NR<- readRDS(paste0(root.lab, "4_NR/lab_ph_4NR_data_splined_dsm_ARD.rds"))


# Check 1. Field and 4a
field.dat.ch1<- field.dat[0,]
lab.4a1.ch1<- lab.4a1[0,]

cnt<- 1
for (i in 1:nrow(lab.4a1)){
  dists<- sp::spDistsN1(pts = as.matrix(field.dat[,4:5]),pt = as.matrix(lab.4a1[i,4:5]),longlat = T)
  min.dist.loc<- which(dists == min(dists))
  min.dist<- min(dists)
  if (min.dist <= 0.002){
    field.dat.ch1[cnt,]<- field.dat[min.dist.loc,]
    lab.4a1.ch1[cnt,]<- lab.4a1[i,]
    cnt<- cnt+1
    print("MATCH!!")}
  print(i)
}

field.dat.ch1 == -9999

plot(lab.4a1.ch1$`0-5 cm`, vecR)
plot(lab.4a1.ch1$Longitude, lab.4a1.ch1$Latitude)

names(field.dat.ch1)
field.dat.ch1[which(field.dat.ch1$`5-15 cm` == -9999),10]<- NA
field.dat.ch1[which(field.dat.ch1$`15-30 cm` == -9999),11]<- NA
field.dat.ch1[which(field.dat.ch1$`30-60 cm` == -9999),12]<- NA
field.dat.ch1[which(field.dat.ch1$`60-100 cm` == -9999),13]<- NA
field.dat.ch1[which(field.dat.ch1$`100-200 cm` == -9999),14]<- NA

# rounding to nearest pH unit
field.dat.ch1$`0-5 cm`<- round(field.dat.ch1$`0-5 cm`/0.5)*0.5 
field.dat.ch1$`5-15 cm`<- round(field.dat.ch1$`5-15 cm`/0.5)*0.5 
field.dat.ch1$`15-30 cm`<- round(field.dat.ch1$`15-30 cm`/0.5)*0.5 
field.dat.ch1$`30-60 cm`<- round(field.dat.ch1$`30-60 cm`/0.5)*0.5 
field.dat.ch1$`60-100 cm`<- round(field.dat.ch1$`60-100 cm`/0.5)*0.5 
field.dat.ch1$`100-200 cm`<- round(field.dat.ch1$`100-200 cm`/0.5)*0.5 


uniqs_field<- c(3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 6.5,
                7.0, 7.5, 8.0, 8.5, 9.0, 9.5, 10.0)
names()
for (i in 9:14){
  field_1D<- field.dat.ch1[,i]
  lab_1D<- lab.4a1.ch1[,i]
  J1D<- cbind(field_1D,lab_1D)
  assign(paste0("d",i), J1D)}
all.dat<- rbind(d9,d10,d11,d12,d13,d14)
all.dat<- all.dat[complete.cases(all.dat),]
  
uniqs1<- unique(all.dat[,1])
uniqs1<- uniqs1[order(uniqs1)]
uniqs1
uniqs1<- uniqs1[4:23]
uniqs1

nos1<- c()
for (i in 1:length(uniqs1)){
  nos1[i]<- length(which(all.dat[,1] == uniqs1[i]))
}
out1<- cbind(uniqs1,nos1)
out1
all.dat[all.dat[,1] == 2,1]<- 3.5
all.dat[all.dat[,1] == 2.5,1]<- 3.5
all.dat[all.dat[,1] == 3,1]<- 3.5
all.dat[all.dat[,1] == 11,1]<- 10.5
all.dat[all.dat[,1] == 11.5,1]<- 10.5

uniqs1<- unique(all.dat[,1])
uniqs1
uniqs1<- uniqs1[order(uniqs1)]
uniqs1<- uniqs1[4:18]


nos1<- c()
for (i in 1:length(uniqs1)){
  nos1[i]<- length(which(all.dat[,1] == uniqs1[i]))
}
out1<- cbind(uniqs1,nos1)
out1

# distributions
dist.names<- c("dists_35_field_2_4a1","dists_40_field_2_4a1",
               "dists_45_field_2_4a1", "dists_50_field_2_4a1",
               "dists_55_field_2_4a1","dists_60_field_2_4a1",
               "dists_65_field_2_4a1","dists_70_field_2_4a1", 
               "dists_75_field_2_4a1","dists_80_field_2_4a1",
               "dists_85_field_2_4a1","dists_90_field_2_4a1",
               "dists_95_field_2_4a1", "dists_100_field_2_4a1",
               "dists_105_field_2_4a1")
length(dist.names)
uniqs1
for (i in 1:length(dist.names)){
dist1<- all.dat[all.dat[,1] == uniqs1[i],2]
chc<- length(which(dist1<0))
if(chc != 0){dist1<- dist1[-which(dist1<0)]}
hist(dist1)
getmode(dist1)
tparams.1<- as.matrix(quantile(dist1,probs = c(0.1,0.5,0.9)))
tparams.1
dist1<- dist1[-which(dist1<tparams.1[1])]
dist1<- dist1[-which(dist1>tparams.1[3])]
assign(dist.names[i],dist1)

# save output
saveRDS(object = dist1, file = paste0(root.out,dist.names[i],".rds"))}

rms<- c(which(all.dat[,1]<0),which(all.dat[,2]<0))
rms<- unique(rms)
all.dat<- all.dat[-rms,]
plot(all.dat[,2],all.dat[,1])

x<- all.dat[,2]
y<- all.dat[,1]

minVal <- min(x, y)
maxVal <- max(x, y)
maxRange <- c(minVal, maxVal)
buffer <- (maxRange[2] - maxRange[1]) / (xbins / 2)
bindata = data.frame(x=x,y=y,factor=as.factor(1))

h <- hexbin(bindata, xbins = xbins, IDs = TRUE, xbnds = maxRange, ybnds = maxRange)
h

counts <- hexTapply (h, bindata$factor, table)
counts <- t (simplify2array (counts))
counts <- melt (counts)
colnames (counts)  <- c ("factor", "ID", "counts")
counts$factor =as.factor(counts$factor)

hexdf <- data.frame (hcell2xy (h),  ID = h@cell)
hexdf <- merge (counts, hexdf)

my_breaks <- c(10, 50, 100, 500, 1000, 10000)
clrs <- rev(brewer.pal(length(my_breaks) + 3, "Blues"))
clrs <- clrs[3:length(clrs)]
hexdf$countColor <- cut(hexdf$counts,
                        breaks = c(0, my_breaks, Inf),
                        labels = rev(clrs))

### START OF NEW CODE ###

# create new bin variable
all_breaks <- c(0, my_breaks)
breaks_n <- 1:length(all_breaks)
get_break_n <- function(n) {
  break_idx <- max(which((all_breaks - n) < 0))
  breaks_n[break_idx]
}
hexdf$bin <- sapply(hexdf$counts, get_break_n)

# create legend labels
all_break_labs <- as.character(all_breaks[1:(length(all_breaks)-1)])

# create final plot
ggplot(hexdf, aes(x=x, y=y)) +
  geom_hex(stat="identity", aes(fill=bin)) +
  scale_fill_gradientn(colors=rev(clrs[-1]),
                       guide="legend",
                       labels=all_break_labs,
                       name="Count") +
  geom_abline(intercept = 0, color = "black", size = 1) +
  labs(x = "lab measured pH (4A1)", y = "field measured pH") +
  coord_fixed(xlim = c(3.5, 10.5),
              ylim = c(3.5, 10.5)) +
  theme(aspect.ratio=1,text = element_text(size=20)) 


