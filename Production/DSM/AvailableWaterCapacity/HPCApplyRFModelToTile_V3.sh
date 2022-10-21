#!/bin/sh
module load R/4.1.3
/apps/R/4.1.3/lib64/R/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/AWC/MeasuredTextures/V2/HPCApplyRFModelToTile_V3.R $SLURM_ARRAY_TASK_ID AWC AWCv3 200 Parsimonious 4