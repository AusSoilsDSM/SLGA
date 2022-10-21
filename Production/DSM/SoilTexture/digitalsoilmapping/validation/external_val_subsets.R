## Training and calibration datasets
# clay
# d1
d1.clay.10per.sample<- sample(nrow(test4), 0.10 * nrow(test4), replace = F)
d1_clay_external_val<- test4[d1.clay.10per.sample,]
saveRDS(d1.clay.10per.sample, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d1.clay.10per.sampled1.clay.10per.sample.rds")
saveRDS(d1_clay_external_val, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d1_clay_external_val.rds")

#d2
d2.clay.10per.sample<- sample(nrow(test4), 0.10 * nrow(test4), replace = F)
d2_clay_external_val<- test4[d2.clay.10per.sample,]
saveRDS(d2.clay.10per.sample, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d2.clay.10per.sampled1.clay.10per.sample.rds")
saveRDS(d2_clay_external_val, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d2_clay_external_val.rds")

#d3
d3.clay.10per.sample<- sample(nrow(test4), 0.10 * nrow(test4), replace = F)
d3_clay_external_val<- test4[d3.clay.10per.sample,]
saveRDS(d3.clay.10per.sample, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d3.clay.10per.sampled1.clay.10per.sample.rds")
saveRDS(d3_clay_external_val, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d3_clay_external_val.rds")


#d4
d4.clay.10per.sample<- sample(nrow(test4), 0.10 * nrow(test4), replace = F)
d4_clay_external_val<- test4[d4.clay.10per.sample,]
hist(d4_clay_external_val$target)
saveRDS(d4.clay.10per.sample, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d4.clay.10per.sampled1.clay.10per.sample.rds")
saveRDS(d4_clay_external_val, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d4_clay_external_val.rds")


#d5
d5.clay.10per.sample<- sample(nrow(test4), 0.10 * nrow(test4), replace = F)
d5_clay_external_val<- test4[d5.clay.10per.sample,]
hist(d5_clay_external_val$target)
saveRDS(d5.clay.10per.sample, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d5.clay.10per.sampled1.clay.10per.sample.rds")
saveRDS(d5_clay_external_val, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d5_clay_external_val.rds")

#d6
d6.clay.10per.sample<- sample(nrow(test4), 0.10 * nrow(test4), replace = F)
d6_clay_external_val<- test4[d6.clay.10per.sample,]
saveRDS(d6.clay.10per.sample, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d6.clay.10per.sampled1.clay.10per.sample.rds")
saveRDS(d6_clay_external_val, file = "/datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soiltexture/models/d6_clay_external_val.rds")



