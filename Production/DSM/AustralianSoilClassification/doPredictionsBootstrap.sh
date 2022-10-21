#!/bin/sh
module load R/3.6.1
/apps/R/3.6.1/lib64/R/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Production/NationalMaps/AustralianSoilClassificationRoss/doPredictionsBootstrap.R $SLURM_ARRAY_TASK_ID ASCOnly 20 F