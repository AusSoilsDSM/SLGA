#!/bin/sh
module load R/3.6.1
/apps/R/3.6.1/lib64/R/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/AWC/APSOIL_RF/doPredictions.R $SLURM_ARRAY_TASK_ID DUL 5 20 F