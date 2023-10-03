### GRDC soil-water NOW
# plotting probe estimates against model predictions
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 23.3.22
# modified: 23.3.22

# CODE PURPOSE
# Have a look at plots of probe data v model estimates
# here trialing just putting it all together for the entire year
# no covariates
# 

### function
cccF<- function(observed, predicted){
  # Concordance
  mx <- mean(observed)
  my <- mean(predicted)
  s2x <- var(observed)
  s2y <- var(predicted)
  sxy <- mean((observed-mx) * (predicted-my))
  ccc <- 2 * sxy / (s2x + s2y + (mx - my)^2)
  return(ccc)}



models.gen<- "/datasets/work/af-tern-mal-deb/work/projects/GRDC_soilwaternow/model_averaging/"
data.in<- paste0(models.gen,"analysis/bars/")
plot.out<- paste0(models.gen,"analysis/bars/plots/rolling_coeffs/")


data.files<- list.files(path = data.in,pattern = ".csv",full.names = T, recursive = F)
data.files
data.files.short<- list.files(path = data.in,pattern = ".csv",full.names = F, recursive = F)
data.files.short<- substr(data.files.short,start = 1, stop = nchar(data.files.short)-4)
data.files.short.short<- as.numeric(substr(x = data.files.short,start = 15,stop = 17))

# cycle through the files
cols<- c("red", "orange", "blue", "green", "black", "purple", "yellow")

gra.coeffs<- matrix(NA, nrow = 364, ncol = 5)
probe.cors<- matrix(NA, nrow = length(data.files), ncol = 6)
probe.ccc<- matrix(NA, nrow = length(data.files), ncol = 6)

# set up a template
template.dat<- read.csv(file = data.files[1])
template.dat$ID<-data.files.short.short[1] 
template.dat$ma<- NA
template.dat$EW<- NA
template.dat<- template.dat[0,]


strt<- 1
fini<- 25
# go through each day
for (j in 25:364){
  # go through each file and get the needed data
  temp.dat<- template.dat[0,]
  result.dat<- template.dat[0,]
  for (i in 1:length(data.files)){
  read.in<- read.csv(file = data.files[i])
  read.in$ID<- data.files.short.short[i]
  read.in<- read.in[strt:fini,]
  temp.dat<- rbind(temp.dat,read.in)
  result.dat[i,1:7]<- read.in[nrow(read.in), ]
  }
  
  # GRA model average
  MA.mod<- lm(bars_probe ~ smips + anu + usyd + awra,data = temp.dat)
  MA.mod
  summary(MA.mod)
  MA.out<-as.data.frame(predict(MA.mod,result.dat, int = "conf"))
  result.dat$ma<- MA.out$fit
  gra.coeffs[j,]<- t(as.matrix(coefficients(MA.mod)))

  # EW model average
  EW.mod<- (0.25*result.dat$smips) + (0.25 * result.dat$anu) + (0.25 * result.dat$usyd) + (0.25 * result.dat$awra)
  result.dat$EW<- EW.mod
  
  # append data
  template.dat<- rbind(template.dat,result.dat)
  
  # update counter
  strt<- strt + 1
  fini<- fini + 1
  print(j)}
  
  
# go through each probe
  # split up back into single probes
  dat.split<- split(template.dat,template.dat$ID)
  checks<- dat.split[[1]]
  
  for (i in 1:length(dat.split)){
    read.in<- dat.split[[i]]
    read.in$date<- as.Date(read.in$date)
    # re-arrange
    read.in<- read.in[,c(1,7,2:6,8:9)]
    
    
    # save GRA model  correlations
    probe.cors[i,]<- t(as.matrix(cor(read.in[,c(3:9)])[1,2:7]))
    
    #concordance
    cc.out<- c()
    temp.mat<- read.in[,c(3:9)]
    cnt<- 1
    for (xx in 2:ncol(temp.mat)){
      cc.out[cnt]<- cccF(observed = temp.mat$bars_probe,predicted = temp.mat[,xx])
      cnt<- cnt + 1}
    probe.ccc[i,]<-cc.out
    
    # generate the plot
    plabel<- paste0("2021_BARS_probe_",read.in$ID[1])
    fname<- paste0(plot.out,plabel,".tiff")
    tiff(file=fname,width=25,height=12,units="cm",res=300,pointsize=8)
    ylimits= c(0,200) 
    plot(read.in$date,read.in$bars_probe,
         ylim= ylimits, 
         main = plabel,
         type= "n",
         axes=F,
         ylab="mm", 
         xlab= "date",
         col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
    axis(side=2,at=seq(from = 0,to = 200,by = 50),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
    axis(1, read.in$date, format(read.in$date, "%b %d"), cex.axis = .7)
    
    for (m in 3:ncol(read.in)){
      lines(read.in$date,read.in[,m],col = cols[m-2])}
    legend("topleft", inset=.02, title="",
           c("BARS_probe","SMIPS","ANU", "USYD", "AWRA", "GRA_MA", "EW_MA" ), fill=cols, horiz=T, cex=0.5)
    dev.off()
  }
  
  # outputs
  gra.coeffs<- as.data.frame(gra.coeffs)
  names(gra.coeffs)<- c("Intercept", "smips", "anu", "usyd", "awra")  
  
  probe.cors<- as.data.frame(probe.cors)
  names(probe.cors)<- c("SMIPS","ANU", "USYD", "AWRA", "GRA_MA", "EW_MA" )
  probe.cors<- cbind(data.files.short,probe.cors)
  
  probe.ccc<- as.data.frame(probe.ccc)
  names(probe.ccc)<- c("SMIPS","ANU", "USYD", "AWRA", "GRA_MA", "EW_MA" )
  probe.ccc<- cbind(data.files.short,probe.ccc)
  
  # save outputs
  write.csv(x = probe.cors, file = paste0(plot.out,"bars_probe_model_correlations.csv"),row.names = F)
  write.csv(x = gra.coeffs, file = paste0(plot.out,"bars_probe_GRA_model_coeffs.csv"),row.names = F)
  write.csv(x = probe.ccc, file = paste0(plot.out,"bars_probe_model_concordance.csv"),row.names = F)
  
  
