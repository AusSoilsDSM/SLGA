#!/bin/sh
module load R/4.1.3
/apps/R/4.1.3/lib64/R/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/AWC/MeasuredTextures/V2/makeMosaics.R $SLURM_ARRAY_TASK_ID DUL Parsimonious