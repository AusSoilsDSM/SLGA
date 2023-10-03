### GRDC soil-water NOW
# plotting probe estimates against model predictions
# Author: Brendan Malone
# Email: brendan.malone@csiro.au
# created: 29.3.22
# modified: 5.4.22

# CODE PURPOSE
# Have a look at plots of probe data v model estimates
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
data.in<- paste0(models.gen,"analysis/EP/")
plot.out<- paste0(models.gen,"analysis/EP/plots/")


data.files<- list.files(path = data.in,pattern = ".csv",full.names = T, recursive = F)
data.files
data.files.short<- list.files(path = data.in,pattern = ".csv",full.names = F, recursive = F)
data.files.short<- substr(data.files.short,start = 1, stop = nchar(data.files.short)-4)
data.files.short


# cycle through the files
cols<- c("red", "orange", "blue", "green", "black", "purple", "yellow")

gra.coeffs<- matrix(NA, nrow = length(data.files), ncol = 5)
probe.cors<- matrix(NA, nrow = length(data.files), ncol = 6)
probe.ccc<- matrix(NA, nrow = length(data.files), ncol = 6)



for (i in 1:length(data.files)){
  read.in<- read.csv(file = data.files[i])
  read.in$date<- as.Date(read.in$date)
  
  # GRA model average
  MA.mod<- lm(EP_probe ~ smips + anu + usyd + awra,data = read.in)
  gra.coeffs[i,]<- t(as.matrix(coefficients(MA.mod))) # save model coefficients
  MA.out<-as.data.frame(predict(MA.mod, int = "conf"))
  read.in$ma<- MA.out$fit
  
  
  
  # EW model average
  EW.mod<- (0.25*read.in$smips) + (0.25 *read.in$anu) + (0.25 * read.in$usyd) + (0.25 * read.in$awra)
  read.in$EW<- EW.mod
  
  # save GRA model coefficents and correlations
  probe.cors[i,]<- t(as.matrix(cor(read.in[,2:8])[1,2:7]))
  
  #concordance
  cc.out<- c()
  temp.mat<- read.in[,c(2:8)]
  cnt<- 1
  for (xx in 2:ncol(temp.mat)){
    cc.out[cnt]<- cccF(observed = temp.mat$EP_probe,predicted = temp.mat[,xx])
    cnt<- cnt + 1}
  probe.ccc[i,]<-cc.out
  
  
  
  # generate the plot
  plabel<- paste0("2021_",data.files.short[i])
  fname<- paste0(plot.out,plabel,".tiff")
  tiff(file=fname,width=25,height=12,units="cm",res=300,pointsize=8)
  ylimits= c(0,200) 
  plot(read.in$date,read.in$EP_probe,
       ylim= ylimits, 
       main = plabel,
       type= "n",
       axes=F,
       ylab="mm", 
       xlab= "date",
       col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
  axis(side=2,at=seq(from = 0,to = 200,by = 50),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
  axis(1, read.in$date, format(read.in$date, "%b %d"), cex.axis = .7)
  
  for (m in 2:ncol(read.in)){
      lines(read.in$date,read.in[,m],col = cols[m-1])}
  legend("topleft", inset=.02, title="",
         c("EP_probe","SMIPS","ANU", "USYD", "AWRA", "GRA_MA", "EW_MA" ), fill=cols, horiz=T, cex=0.5)
  dev.off()
}

# outputs
gra.coeffs<- as.data.frame(gra.coeffs)
names(gra.coeffs)<- c("Intercept", "smips", "anu", "usyd", "awra")  
gra.coeffs<- cbind(data.files.short,gra.coeffs)

probe.cors<- as.data.frame(probe.cors)
names(probe.cors)<- c("SMIPS","ANU", "USYD", "AWRA", "GRA_MA", "EW_MA" )
probe.cors<- cbind(data.files.short,probe.cors)

probe.ccc<- as.data.frame(probe.ccc)
names(probe.ccc)<- c("SMIPS","ANU", "USYD", "AWRA", "GRA_MA", "EW_MA" )
probe.ccc<- cbind(data.files.short,probe.ccc)

# save outputs
write.csv(x = probe.cors, file = paste0(plot.out,"EP_probe_model_correlations.csv"),row.names = F)
write.csv(x = gra.coeffs, file = paste0(plot.out,"EP_probe_GRA_model_coeffs.csv"),row.names = F)
write.csv(x = probe.ccc, file = paste0(plot.out,"EP_probe_model_concordance.csv"),row.names = F)
