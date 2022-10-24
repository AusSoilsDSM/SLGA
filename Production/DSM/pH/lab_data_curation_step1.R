## Data Mongering: TERN soil pH
## Lab data
## Step 1: Unique locations and selection of specific lab methods



root<- "Z:/projects/ternlandscapes_2019/soil_pH/data/"
root.out<- "Z:/projects/ternlandscapes_2019/soil_pH/data/curated_step1/"

#DATA
lab.methods<- read.csv(file = paste0(root, "available_lab_methods.csv"))



files<- list.files(path = root, pattern = "LAB", full.names = T)
files
files.short<- list.files(path = root, pattern = "LAB", full.names = F)
files.short


out.mat<- matrix(NA, nrow= length(files), ncol = nrow(lab.methods)+1)
# find out how many observations for each method are in each dataset
for (i in 1: length(files)){
  sel.dat<- readRDS(files[i])
  
  # how many unqique methods
  out.mat[i,1]<- length(unique(sel.dat$ObservedProperty)) #1
  
  for (j in 1:nrow(lab.methods)){
    selmeth<- lab.methods$Property[j]
    if (length(which(sel.dat$ObservedProperty == selmeth)) == 0){
      out.mat[i,j+1]<- 0} else {
        out.mat[i,j+1]<- length(which(sel.dat$ObservedProperty == selmeth))}
    
  }}

out.mat<- as.data.frame(out.mat)
names(out.mat)[2:25]<- as.character(lab.methods$Property)
out.mat<- cbind(files.short, out.mat)
write.csv(out.mat, file = paste0(root, "lab_method_summary_data.csv"),row.names=F)
out.mat

# selected lab methods
sel.methods<- lab.methods$Property[c(9,11:17,19:20)]
sel.methods

for (i in 1: length(files)){
  sel.dat<- readRDS(files[i])
  
  # determine the unique site location
  sel.dat$uniqLOC<- paste0(sel.dat$Longitude, "_", sel.dat$Latitude)
  sel.dat$loc_fid<- NA
  uniq.locs<- length(unique(sel.dat$uniqLOC))
  uniq.locs
  locs<- unique(sel.dat$uniqLOC)
  
  # create a new data.frame to put outputs
  new.sel.dat<- sel.dat[0,]
  
  # cycle through each location
  for (j in 1:uniq.locs){
    sel.locs<- locs[j]
    grouped.loc.dat<- sel.dat[which(sel.dat$uniqLOC == sel.locs),]
    # only get the data that we want the methods for
    grouped.loc.dat<- grouped.loc.dat[grouped.loc.dat$ObservedProperty %in% as.character(sel.methods),]
    if(nrow(grouped.loc.dat) == 0){
      next} else {
        # split accroding to method
        sp.grouped.loc.dat<- split(grouped.loc.dat, grouped.loc.dat$ObservedProperty)
        
        # for each method at each site..basically make sure the the layers are in the right sequence and then append to new main table
        for (k in 1:length(sp.grouped.loc.dat)){
          subs.dat<- sp.grouped.loc.dat[[k]]
          subs.dat$loc_fid <- j
          subs.dat<- subs.dat[order(subs.dat$UpperDepth),]
          # put into big output table
          new.sel.dat <- rbind(new.sel.dat,subs.dat)}
    print(uniq.locs - j)
      }}
  # Outside loop now: save new table to file
  if (nrow(new.sel.dat) == 0){
    next} else {
      # save the output
      nm1<- substr(as.character(out.mat[i,1]),start = 1,stop = nchar(as.character(out.mat[i,1]))-4)
      nm2<- paste0(root.out,nm1, "_curated_LAB_step1.rds")
      saveRDS(new.sel.dat, file = nm2)}
  }
