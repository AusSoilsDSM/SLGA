## hex bin plotting of soil thickness estimates
### XY-plotting
# Model 2 (ranger regression)
root<- "Z:/projects/ternlandscapes_2019/soiltexture/models/"
library(hexbin);library(reshape2);library(RColorBrewer);library(ggplot2)


# external validation files
### CLAY
## D1
vari<- "clay"
depth<- "d1"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

x<- c()
y<- c()




for (i in 7:ncol(dat)){
  x<- c(x,dat$target)
  y<- c(y,dat[,i])}

set.seed(1)
xbins <- 25


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

my_breaks <- c(500, 1000, 5000, 10000, 20000, 50000)
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
  labs(x = "observed ilr comp clay", y = "predicted ilr comp clay") +
  coord_fixed(xlim = c(-2, 5),
              ylim = c(-2, 5)) +
  theme(aspect.ratio=1,text = element_text(size=20)) 


# external validation files
### CLAY
## D2
vari<- "clay"
depth<- "d2"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# internal validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

x<- c()
y<- c()




for (i in 7:ncol(dat)){
  x<- c(x,dat$target)
  y<- c(y,dat[,i])}

set.seed(1)
xbins <- 25


minVal <- min(x, y)
maxVal <- max(x, y)
maxRange <- c(minVal, maxVal)
buffer <- (maxRange[2] - maxRange[1]) / (xbins / 2)
bindata = data.frame(x=x,y=y,factor=as.factor(1))

h <- hexbin(bindata, xbins = xbins, IDs = TRUE, xbnds = maxRange, ybnds = maxRange)
h
plot(h)

counts <- hexTapply (h, bindata$factor, table)
counts <- t (simplify2array (counts))
counts <- melt (counts)
colnames (counts)  <- c ("factor", "ID", "counts")
counts$factor =as.factor(counts$factor)

hexdf <- data.frame (hcell2xy (h),  ID = h@cell)
hexdf <- merge (counts, hexdf)

my_breaks <- c(500, 1000, 5000, 10000, 20000, 50000)
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
  labs(x = "observed irl comp clay", y = "predicted irl comp clay") +
  coord_fixed(xlim = c(-2, 5),
              ylim = c(-2, 5)) +
  theme(aspect.ratio=1,text = element_text(size=20)) 


# external validation files
### CLAY
## D3
vari<- "clay"
depth<- "d3"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

x<- c()
y<- c()




for (i in 7:ncol(dat)){
  x<- c(x,dat$target)
  y<- c(y,dat[,i])}

set.seed(1)
xbins <- 25


minVal <- min(x, y)
maxVal <- max(x, y)
maxRange <- c(minVal, maxVal)
buffer <- (maxRange[2] - maxRange[1]) / (xbins / 2)
bindata = data.frame(x=x,y=y,factor=as.factor(1))

h <- hexbin(bindata, xbins = xbins, IDs = TRUE, xbnds = maxRange, ybnds = maxRange)
h
plot(h)

counts <- hexTapply (h, bindata$factor, table)
counts <- t (simplify2array (counts))
counts <- melt (counts)
colnames (counts)  <- c ("factor", "ID", "counts")
counts$factor =as.factor(counts$factor)

hexdf <- data.frame (hcell2xy (h),  ID = h@cell)
hexdf <- merge (counts, hexdf)

my_breaks <- c(500, 1000, 5000, 10000, 20000, 50000)
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
  labs(x = "observed irl comp clay", y = "predicted irl comp clay") +
  coord_fixed(xlim = c(-2, 5),
              ylim = c(-2, 5)) +
  theme(aspect.ratio=1,text = element_text(size=20)) 


# external validation files
### CLAY
## D4
vari<- "clay"
depth<- "d4"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

x<- c()
y<- c()




for (i in 7:ncol(dat)){
  x<- c(x,dat$target)
  y<- c(y,dat[,i])}

set.seed(1)
xbins <- 25


minVal <- min(x, y)
maxVal <- max(x, y)
maxRange <- c(minVal, maxVal)
buffer <- (maxRange[2] - maxRange[1]) / (xbins / 2)
bindata = data.frame(x=x,y=y,factor=as.factor(1))

h <- hexbin(bindata, xbins = xbins, IDs = TRUE, xbnds = maxRange, ybnds = maxRange)
h
plot(h)

counts <- hexTapply (h, bindata$factor, table)
counts <- t (simplify2array (counts))
counts <- melt (counts)
colnames (counts)  <- c ("factor", "ID", "counts")
counts$factor =as.factor(counts$factor)

hexdf <- data.frame (hcell2xy (h),  ID = h@cell)
hexdf <- merge (counts, hexdf)

my_breaks <- c(1000, 5000, 10000, 20000, 50000, 90000)
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
  labs(x = "observed irl comp clay", y = "predicted irl comp clay") +
  coord_fixed(xlim = c(-2, 5),
              ylim = c(-2, 5)) +
  theme(aspect.ratio=1,text = element_text(size=20)) 



# external validation files
### CLAY
## D5
vari<- "clay"
depth<- "d5"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

x<- c()
y<- c()




for (i in 7:ncol(dat)){
  x<- c(x,dat$target)
  y<- c(y,dat[,i])}

set.seed(1)
xbins <- 25


minVal <- min(x, y)
maxVal <- max(x, y)
maxRange <- c(minVal, maxVal)
buffer <- (maxRange[2] - maxRange[1]) / (xbins / 2)
bindata = data.frame(x=x,y=y,factor=as.factor(1))

h <- hexbin(bindata, xbins = xbins, IDs = TRUE, xbnds = maxRange, ybnds = maxRange)
h
plot(h)

counts <- hexTapply (h, bindata$factor, table)
counts <- t (simplify2array (counts))
counts <- melt (counts)
colnames (counts)  <- c ("factor", "ID", "counts")
counts$factor =as.factor(counts$factor)

hexdf <- data.frame (hcell2xy (h),  ID = h@cell)
hexdf <- merge (counts, hexdf)

my_breaks <- c(1000, 5000, 10000, 20000, 50000, 90000)
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
  labs(x = "observed irl comp clay", y = "predicted irl comp clay") +
  coord_fixed(xlim = c(-2, 5),
              ylim = c(-2, 5)) +
  theme(aspect.ratio=1,text = element_text(size=20)) 


# external validation files
### CLAY
## D6
vari<- "clay"
depth<- "d6"
new.root<- paste0(root,vari,"/",depth,"/")
new.root

# external validation files
files<- list.files(path = new.root,pattern = "ranger_EXT_preds_clay",full.names = T)
files
dat<- read.table(files[1],header = T,sep = ",")
names(dat)
dat<- dat[,1:6]

for (i in 1:length(files)){
  datz<- read.table(files[i],header = T,sep = ",")
  dat<- cbind(dat, datz$prediction)
}


# structure of data
str(dat)

x<- c()
y<- c()




for (i in 7:ncol(dat)){
  x<- c(x,dat$target)
  y<- c(y,dat[,i])}

set.seed(1)
xbins <- 25


minVal <- min(x, y)
maxVal <- max(x, y)
maxRange <- c(minVal, maxVal)
buffer <- (maxRange[2] - maxRange[1]) / (xbins / 2)
bindata = data.frame(x=x,y=y,factor=as.factor(1))

h <- hexbin(bindata, xbins = xbins, IDs = TRUE, xbnds = maxRange, ybnds = maxRange)
h
plot(h)

counts <- hexTapply (h, bindata$factor, table)
counts <- t (simplify2array (counts))
counts <- melt (counts)
colnames (counts)  <- c ("factor", "ID", "counts")
counts$factor =as.factor(counts$factor)

hexdf <- data.frame (hcell2xy (h),  ID = h@cell)
hexdf <- merge (counts, hexdf)

my_breaks <- c(500, 1000, 5000, 10000, 20000, 50000)
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
  labs(x = "observed irl comp clay", y = "predicted irl comp clay") +
  coord_fixed(xlim = c(-2, 5),
              ylim = c(-2, 5)) +
  theme(aspect.ratio=1,text = element_text(size=20)) 




