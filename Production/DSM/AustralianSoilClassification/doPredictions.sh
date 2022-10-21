#!/bin/sh
module load R/3.6.1
/apps/R/3.6.1/lib64/R/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Production/NationalMaps/AustralianSoilClassificationRoss/doPredictions.R $SLURM_ARRAY_TASK_ID ASCOnly RFmodel_ASCOnly.rds 20 F