## Workflow for process combining lab PSA data and morph PSA data
## TERN Landscapes work
## PROCESS 4: 
# For each profile of the morph data want to simulate plausible realities of the texture fractions

# morph data
morph.data<- readRDS(file = "Z:/projects/ternlandscapes_2019/soiltexture/data/process3/combined_morph_PSA_data_labPSA_checks.rds")
write.csv(morph.data, file = "Z:/projects/ternlandscapes_2019/soiltexture/data/process3/combined_morph_PSA_data_labPSA_checks.csv" )


dat1<- readRDS("Z:/projects/ternlandscapes_2019/soiltexture/data/process4/morph_sims/combined_morph_data_2020-01_sim_1.rds")
dat2<- readRDS("Z:/projects/ternlandscapes_2019/soiltexture/data/process4/morph_sims/combined_morph_data_2020-01_sim_2.rds")
dat9<- readRDS("Z:/projects/ternlandscapes_2019/soiltexture/data/process4/morph_sims/combined_morph_data_2020-01_sim_9.rds")


# lab data
lab.data<- readRDS("Z:/projects/ternlandscapes_2019/soiltexture/data/process2/combined_lab_PSA_data.rds")
