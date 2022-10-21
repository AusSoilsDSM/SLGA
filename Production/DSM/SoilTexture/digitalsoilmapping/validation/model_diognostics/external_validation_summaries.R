# summarise the summaries
# tern landscapes soil texture


root<- "S:/projects/ternlandscapes_2019/soiltexture/models/"


# CLAY
vart<- "clay"
depth<- "d1"

dat<- read.csv(paste0(root, vart, "/",depth, "/", "ranger_EXT_diogs_",vart,"_",depth,"_model_summary.csv"))
d1.clay.out<- colMeans(dat[,4:8])

vart<- "clay"
depth<- "d2"

dat<- read.csv(paste0(root, vart, "/",depth, "/", "ranger_EXT_diogs_",vart,"_",depth,"_model_summary.csv"))
d2.clay.out<- colMeans(dat[,4:8])

vart<- "clay"
depth<- "d3"

dat<- read.csv(paste0(root, vart, "/",depth, "/", "ranger_EXT_diogs_",vart,"_",depth,"_model_summary.csv"))
d3.clay.out<- colMeans(dat[,4:8])

vart<- "clay"
depth<- "d4"

dat<- read.csv(paste0(root, vart, "/",depth, "/", "ranger_EXT_diogs_",vart,"_",depth,"_model_summary.csv"))
d4.clay.out<- colMeans(dat[,4:8])

vart<- "clay"
depth<- "d5"

dat<- read.csv(paste0(root, vart, "/",depth, "/", "ranger_EXT_diogs_",vart,"_",depth,"_model_summary.csv"))
d5.clay.out<- colMeans(dat[,4:8])

vart<- "clay"
depth<- "d6"

dat<- read.csv(paste0(root, vart, "/",depth, "/", "ranger_EXT_diogs_",vart,"_",depth,"_model_summary.csv"))
d6.clay.out<- colMeans(dat[,4:8])

clay.out<- rbind(d1.clay.out,d2.clay.out,d3.clay.out,d4.clay.out,d5.clay.out,d6.clay.out)
levs<- 1:6
clay.out<- as.data.frame(cbind(levs, clay.out))
names(clay.out)[1]<- "depth"
write.csv(clay.out, file = "S:/projects/ternlandscapes_2019/soiltexture/outs/dsm_externalvalidation/clay_tern_v2_val_summary.csv", row.names = F)



# SAND
vart<- "sand"
depth<- "d1"

dat<- read.csv(paste0(root, vart, "/",depth, "/", "ranger_EXT_diogs_",vart,"_",depth,"_model_summary.csv"))
d1.sand.out<- colMeans(dat[,4:8])

vart<- "sand"
depth<- "d2"

dat<- read.csv(paste0(root, vart, "/",depth, "/", "ranger_EXT_diogs_",vart,"_",depth,"_model_summary.csv"))
d2.sand.out<- colMeans(dat[,4:8])

vart<- "sand"
depth<- "d3"

dat<- read.csv(paste0(root, vart, "/",depth, "/", "ranger_EXT_diogs_",vart,"_",depth,"_model_summary.csv"))
d3.sand.out<- colMeans(dat[,4:8])

vart<- "sand"
depth<- "d4"

dat<- read.csv(paste0(root, vart, "/",depth, "/", "ranger_EXT_diogs_",vart,"_",depth,"_model_summary.csv"))
d4.sand.out<- colMeans(dat[,4:8])

vart<- "sand"
depth<- "d5"

dat<- read.csv(paste0(root, vart, "/",depth, "/", "ranger_EXT_diogs_",vart,"_",depth,"_model_summary.csv"))
d5.sand.out<- colMeans(dat[,4:8])

vart<- "sand"
depth<- "d6"

dat<- read.csv(paste0(root, vart, "/",depth, "/", "ranger_EXT_diogs_",vart,"_",depth,"_model_summary.csv"))
d6.sand.out<- colMeans(dat[,4:8])

sand.out<- rbind(d1.sand.out,d2.sand.out,d3.sand.out,d4.sand.out,d5.sand.out,d6.sand.out)
levs<- 1:6
sand.out<- as.data.frame(cbind(levs, sand.out))
names(sand.out)[1]<- "depth"
write.csv(sand.out, file = "S:/projects/ternlandscapes_2019/soiltexture/outs/dsm_externalvalidation/sand_tern_v2_val_summary.csv", row.names = F)
