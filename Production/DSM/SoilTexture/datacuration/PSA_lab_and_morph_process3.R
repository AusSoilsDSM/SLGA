## Workflow for process combining lab PSA data and morph PSA data
## TERN Landscapes work
## PROCESS 3: 
# For each profile of the morph data want to check whether there is corresponding lab data.
# Want to flag the repeats and give preference to the lab data over the morph data
library(sp)

# lab data
lab.data<- readRDS(file = "Z:/projects/ternlandscapes_2019/soiltexture/data/process2/combined_lab_PSA_data.rds")

# morph data
morph.data<- readRDS(file = "Z:/projects/ternlandscapes_2019/soiltexture/data/process2/combined_morph_PSA_data.rds")
morph.data$checks<- NA


# tops
tlabs<- lab.data[lab.data$UpperDepth == 0,]
tmorphs<- morph.data[morph.data$UpperDepth == 0,]
names(tmorphs)
nas<- which(is.na(tmorphs$DataStore))
tmorphs<- tmorphs[-nas,]
nas<- which(is.na(tlabs$DataStore))
tlabs<- tlabs[-nas,]



for (i in 1:nrow(tmorphs)){
  
  # subset data
  subs<- morph.data[which(morph.data$Observation_ID == tmorphs[i,"Observation_ID"]),]
  
  # distance
  dists<- spDistsN1(pts = as.matrix(tlabs[,7:8]), pt = as.matrix(tmorphs[i,7:8]))
  
  obs<- length(which(dists == 0))
  
  if (obs == 0){ # no matches
    subs$checks <- 1
    morph.data[which(morph.data$Observation_ID == tmorphs[i,"Observation_ID"]),]<- subs}
  
  if (obs > 0){ # a match
    subs$checks <- -9999
    morph.data[which(morph.data$Observation_ID == tmorphs[i,"Observation_ID"]),]<- subs
    print(paste0(tmorphs[i,"Observation_ID"]," MORPH needs removal"))
    }
  print (i)}

saveRDS(morph.data, "Z:/projects/ternlandscapes_2019/soiltexture/data/process3/combined_morph_PSA_data_labPSA_checks.rds")

## remove the -9999 check data as these have corresponding lab data
new.morph.data<- morph.data[-which(morph.data$checks == -9999),]

saveRDS(new.morph.data, "Z:/projects/ternlandscapes_2019/soiltexture/data/process3/combined_morph_PSA_data_labPSA_checks_removed.rds")



