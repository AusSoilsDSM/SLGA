#!/bin/sh
module load R/4.1.3
/apps/R/4.1.3/lib64/R/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Production/NationalMaps/DataPublishing/publishSLGA.R $SLURM_ARRAY_TASK_ID BioDiv /datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/Soil_Microbial_Biodiversity/SLGAReady FALSE 1 F 2 T -1 FALSE FALSE