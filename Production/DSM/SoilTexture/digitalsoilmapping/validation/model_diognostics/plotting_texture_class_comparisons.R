### plotting texture class diognostics

## Version 2 outputs
v2.out<- readRDS("S:/projects/ternlandscapes_2019/soiltexture/models/diognostics/compiled_texture_class_summaries_extdata.rds")


## Version 1 outputs
v1.out<- readRDS("S:/projects/ternlandscapes_2019/soiltexture/models/diognostics/compiled_texture_class_summaries_extdata_SLGA_v1_check.rds")


### output folder
root.out<- "S:/projects/ternlandscapes_2019/soiltexture/outs/dsm_externalvalidation/"

# depth 1
xlimits= c(1, 10)
ylimits= c(0, 100) 
tiff(file=paste0(root.out,"texture_class_compare_d1.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(seq(from = 1,to = 10,length.out = 10), v1.out[1,],xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="Percentage of cases", xlab= "maximum class discrepancy",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 1,to = 10,length.out = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (seq(from = 1,to = 10,length.out = 10),(v2.out[1,]*100),pch=1, col="black", cex=0.75)
points (seq(from = 1,to = 10,length.out = 10),(v1.out[1,]),pch=1, col="red", cex=0.75)

dev.off()


# depth 2
xlimits= c(1, 10)
ylimits= c(0, 100) 
tiff(file=paste0(root.out,"texture_class_compare_d2.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(seq(from = 1,to = 10,length.out = 10), v1.out[2,],xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="Percentage of cases", xlab= "maximum class discrepancy",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 1,to = 10,length.out = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (seq(from = 1,to = 10,length.out = 10),(v2.out[2,]*100),pch=1, col="black", cex=0.75)
points (seq(from = 1,to = 10,length.out = 10),(v1.out[2,]),pch=1, col="red", cex=0.75)

dev.off()


# depth 3
xlimits= c(1, 10)
ylimits= c(0, 100) 
tiff(file=paste0(root.out,"texture_class_compare_d3.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(seq(from = 1,to = 10,length.out = 10), v1.out[3,],xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="Percentage of cases", xlab= "maximum class discrepancy",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 1,to = 10,length.out = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (seq(from = 1,to = 10,length.out = 10),(v2.out[3,]*100),pch=1, col="black", cex=0.75)
points (seq(from = 1,to = 10,length.out = 10),(v1.out[3,]),pch=1, col="red", cex=0.75)

dev.off()


# depth 4
xlimits= c(1, 10)
ylimits= c(0, 100) 
tiff(file=paste0(root.out,"texture_class_compare_d4.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(seq(from = 1,to = 10,length.out = 10), v1.out[4,],xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="Percentage of cases", xlab= "maximum class discrepancy",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 1,to = 10,length.out = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (seq(from = 1,to = 10,length.out = 10),(v2.out[4,]*100),pch=1, col="black", cex=0.75)
points (seq(from = 1,to = 10,length.out = 10),(v1.out[4,]),pch=1, col="red", cex=0.75)

dev.off()



# depth 5
xlimits= c(1, 10)
ylimits= c(0, 100) 
tiff(file=paste0(root.out,"texture_class_compare_d5.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(seq(from = 1,to = 10,length.out = 10), v1.out[5,],xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="Percentage of cases", xlab= "maximum class discrepancy",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 1,to = 10,length.out = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (seq(from = 1,to = 10,length.out = 10),(v2.out[5,]*100),pch=1, col="black", cex=0.75)
points (seq(from = 1,to = 10,length.out = 10),(v1.out[5,]),pch=1, col="red", cex=0.75)

dev.off()



# depth 6
xlimits= c(1, 10)
ylimits= c(0, 100) 
tiff(file=paste0(root.out,"texture_class_compare_d6.tiff"),width=12,height=12,units="cm",res=300,pointsize=8)
plot(seq(from = 1,to = 10,length.out = 10), v1.out[6,],xlim= xlimits, ylim= ylimits, type= "n",axes=F,ylab="Percentage of cases", xlab= "maximum class discrepancy",col="black", font.lab=2,cex.lab=1.5,font=2, font.axis=2, family="sans")
axis(side=2,at=seq(from = 0,to = 100,by = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
axis(side=1,at=seq(from = 1,to = 10,length.out = 10),font=2, font.axis=2, family="sans",lty=1, lwd=1,cex.axis=1.2, col="black")
points (seq(from = 1,to = 10,length.out = 10),(v2.out[6,]*100),pch=1, col="black", cex=0.75)
points (seq(from = 1,to = 10,length.out = 10),(v1.out[6,]),pch=1, col="red", cex=0.75)

dev.off()
