### Gets soil color data in the format used in federator into a usable form.
## make seperate columns for H V and C
## Needs to be flexible to work with the variable data


HVC_splitter<- function(uniq.dat){
  YR.split<- strsplit(uniq.dat$orig, split="YR")
  Y.split<- strsplit(uniq.dat$orig, split="Y")
  R.split<- strsplit(uniq.dat$orig, split="R")
  GY.split<- strsplit(uniq.dat$orig, split="GY")
  BG.split<- strsplit(uniq.dat$orig, split="BG")
  G.split<- strsplit(uniq.dat$orig, split="G")
  B.split<- strsplit(uniq.dat$orig, split="B")
  
  
  # Go through each list
  # YR split # 1
  lis<- YR.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) >= 1){next} else {
        subs<- lis[[i]]
        uniq.dat$hue[i]<- as.numeric(subs[1])
        vcd<- strsplit(subs[2],"")
        uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
        uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
        uniq.dat$angle[i]<- "YR"
      }}}
  
  # YR split # 2
  lis<- YR.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) == 0){next} else {
        subs<- lis[[i]]
        uniq.dat$hue[i]<- as.numeric(subs[1])
        vcd<- strsplit(subs[2],"/")
        uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
        uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
        uniq.dat$angle[i]<- "YR"
      }}}
  
  # Go through each list
  # Y split # 1
  lis<- Y.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) >= 1){next} else {
        subs<- lis[[i]]
        uniq.dat$hue[i]<- as.numeric(subs[1])
        vcd<- strsplit(subs[2],"")
        uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
        uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
        uniq.dat$angle[i]<- "Y"
      }}}
  
  # Y split # 2
  lis<- Y.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) == 0){next} else {
        subs<- lis[[i]]
        uniq.dat$hue[i]<- as.numeric(subs[1])
        vcd<- strsplit(subs[2],"/")
        if(is.na(as.numeric(vcd[[1]][1]))==TRUE){next}
        if(length(vcd[[1]]) != 2){next} else {
          uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
          uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
          uniq.dat$angle[i]<- "Y"}
      }}}
  
  
  # Go through each list
  # R split # 1
  lis<- R.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) >= 1){next} else {
        subs<- lis[[i]]
        uniq.dat$hue[i]<- as.numeric(subs[1])
        vcd<- strsplit(subs[2],"")
        uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
        uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
        uniq.dat$angle[i]<- "R"
      }}}
  
  # R split # 2
  lis<- R.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) == 0){next} else {
        subs<- lis[[i]]
        if(is.na(as.numeric(subs[1])) == TRUE){next} else {
          uniq.dat$hue[i]<- as.numeric(subs[1])
          vcd<- strsplit(subs[2],"/")
          if(is.na(as.numeric(vcd[[1]][1]))==TRUE){next}
          if(length(vcd[[1]]) != 2){next} else {
            uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
            uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
            uniq.dat$angle[i]<- "R"}}
      }}}
  
  
  # Go through each list
  # GY split # 1
  lis<- GY.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) >= 1){next} else {
        subs<- lis[[i]]
        uniq.dat$hue[i]<- as.numeric(subs[1])
        vcd<- strsplit(subs[2],"")
        uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
        uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
        uniq.dat$angle[i]<- "GY"
      }}}
  
  # GY split # 2
  lis<- GY.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) == 0){next} else {
        subs<- lis[[i]]
        uniq.dat$hue[i]<- as.numeric(subs[1])
        vcd<- strsplit(subs[2],"/")
        uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
        uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
        uniq.dat$angle[i]<- "GY"
      }}}
  
  
  # Go through each list
  # BG split # 1
  lis<- BG.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) >= 1){next} else {
        subs<- lis[[i]]
        uniq.dat$hue[i]<- as.numeric(subs[1])
        vcd<- strsplit(subs[2],"")
        uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
        uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
        uniq.dat$angle[i]<- "BG"
      }}}
  
  # BG split # 2
  lis<- BG.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) == 0){next} else {
        subs<- lis[[i]]
        uniq.dat$hue[i]<- as.numeric(subs[1])
        vcd<- strsplit(subs[2],"/")
        uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
        uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
        uniq.dat$angle[i]<- "BG"
      }}}
  
  
  # Go through each list
  # B split # 1
  lis<- B.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) >= 1){next} else {
        subs<- lis[[i]]
        uniq.dat$hue[i]<- as.numeric(subs[1])
        vcd<- strsplit(subs[2],"")
        uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
        uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
        uniq.dat$angle[i]<- "B"
      }}}
  
  # B split # 2
  lis<- B.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) == 0){next} else {
        subs<- lis[[i]]
        uniq.dat$hue[i]<- as.numeric(subs[1])
        vcd<- strsplit(subs[2],"/")
        if(is.na(as.numeric(vcd[[1]][1]))==TRUE){next}
        if(length(vcd[[1]]) != 2){next} else {
          uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
          uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
          uniq.dat$angle[i]<- "B"}
      }}}
  
  
  # Go through each list
  # G split # 1
  lis<- G.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) >= 1){next} else {
        subs<- lis[[i]]
        uniq.dat$hue[i]<- as.numeric(subs[1])
        vcd<- strsplit(subs[2],"")
        uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
        uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
        uniq.dat$angle[i]<- "G"
      }}}
  
  # G split # 2
  lis<- G.split
  for (i in 1:nrow(uniq.dat)){
    subs<- as.numeric(lis[[i]])
    if(length(subs) == 1){next} else {
      if(length(subs[is.na(subs)]) == 0){next} else {
        subs<- lis[[i]]
        if(is.na(as.numeric(subs[1])) == TRUE){next} else {
          uniq.dat$hue[i]<- as.numeric(subs[1])
          vcd<- strsplit(subs[2],"/")
          if(is.na(as.numeric(vcd[[1]][1]))==TRUE){next}
          if(length(vcd[[1]]) != 2){next} else {
            uniq.dat$value[i]<- as.numeric(vcd[[1]][1])
            uniq.dat$chroma[i]<- as.numeric(vcd[[1]][2])
            uniq.dat$angle[i]<- "G"}}
      }}}
  return(uniq.dat)}
