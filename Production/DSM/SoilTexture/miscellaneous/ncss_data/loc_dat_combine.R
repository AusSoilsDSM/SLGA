## National site collation soil texture 
## matching lab measures with location and horizon information

# location data
loc.dat<- read.csv("S:/projects/ternlandscapes_2019/soiltexture/data/NSSC1/nssc_observations_horizons.csv")
head(loc.dat)

# lab measurment data
lab.dat<- read.csv("Z:/projects/ternlandscapes_2019/soiltexture/data/NSSC1/nssc_lab_texture.csv")
head(lab.dat)
lab.dat$concat<- paste0(lab.dat$agency_code, "_", lab.dat$proj_code, "_", lab.dat$s_id, "_", lab.dat$o_id)
los<- length(unique(lab.dat$concat))
samps<- unique(lab.dat$concat)

# get lab methods onto table
lab.meth<- c(as.character(unique(lab.dat$labm_code)))
temp.mat<- matrix(NA, ncol = length(lab.meth), nrow = nrow(loc.dat))
temp.mat<- as.data.frame(temp.mat)
names(temp.mat)<- lab.meth
head(temp.mat)
loc.dat<- cbind(loc.dat, temp.mat)
loc.dat$dataP<- NA

for (i in 527:los){
  #lab data
  subs<- lab.dat[lab.dat$concat == samps[i], ]
  subs
  
  #location data
  locr<- nrow(loc.dat[loc.dat$concat.of.dbo_OBSERVATIONS == samps[i], ])
  if(locr != 0){
    locs<- loc.dat[loc.dat$concat.of.dbo_OBSERVATIONS == samps[i], ]
    locs[,1:15]} else {
      next}
  
  # go through each lab data row and assign lab value to the correct lab method column
  for (j in 1:nrow(subs)){
    r1<- subs[j,]
    locs[r1$h_no,which(colnames(locs)==r1$labm_code)]<- r1$labr_value
    locs[r1$h_no, "dataP"]<- 1}
  
  # place back into data frame
  loc.dat[loc.dat$concat.of.dbo_OBSERVATIONS == samps[i], ] <- locs
  
  print(i)}


text.dat<- loc.dat[which(loc.dat$dataP == 1),]

saveRDS(text.dat,file = "Z:/projects/ternlandscapes_2019/soiltexture/data/NSSC1/compiled_texture_locs.rds")
write.csv(text.dat,file = "Z:/projects/ternlandscapes_2019/soiltexture/data/NSSC1/compiled_texture_locs.csv", row.names = F)
