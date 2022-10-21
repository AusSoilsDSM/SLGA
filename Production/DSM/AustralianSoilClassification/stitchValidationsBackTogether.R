
########  stitch all ASC validation data set together
all <- read.csv('C:/Projects/TernLandscapes/ASC/InputData/ASCdrill.csv')
v <- read.csv('C:/Projects/TernLandscapes/ASC/InputData/validationData.csv')
outdf <- data.frame()

c=1

for (j in 1:nrow(v)) {
  
    r <- v[j,]

    for (i in c:nrow(all)) {
      
        ra <- all[i, c(14, 18:103)]
        ra2 <- ra[, -7]
        
       if(all(r==ra2)){
         outdf <- rbind(outdf, all[i,])
         print(j)
         c=i
         break;
       }
    }

}

write.csv(outdf, 'C:/Projects/TernLandscapes/ASC/InputData/validationDataWithExtras.csv', row.names = F)


########  stitch ASC only validation data set together

all <- read.csv('C:/Projects/TernLandscapes/ASC/InputData/ASCdrill.csv')
idxs <- which(is.na(all[,9]))
all2 <- all[-idxs,]
nrow(all2)

head(all)
v <- read.csv('C:/Projects/TernLandscapes/ASC/InputData/ASCOnlyvalidationData.csv')
colnames(v)


outdf <- data.frame()

c=1

for (j in 1:nrow(v)) {
  
  r <- v[j,]
  
  for (i in c:nrow(all2)) {
    
    ra <- all2[i, c(9, 18:103)]
    #ra2 <- ra[, -7]
    ra2 <- ra
    
    if(all(r==ra2)){
      outdf <- rbind(outdf, all[i,])
      print(j)
      c=i
      break;
    }
  }
  
}

write.csv(outdf, 'C:/Projects/TernLandscapes/ASC/InputData/ASCOnlyvalidationDataWithExtras.csv', row.names = F)
