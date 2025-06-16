#!/bin/sh
module load R/4.1.3
/apps/R/4.1.3/lib64/R/bin/Rscript /datasets/work/af-digiscapesm/work/Ross/SLGA/SLGA/Development/Ross/Scripts/AWC/MeasuredTextures/V2/publishSLGA.R $SLURM_ARRAY_TASK_ID L15 /datasets/work/af-digiscapesm/work/Ross/TERN/AWC/MeasuredTextures/SLGAReady/L15 1 F 0 T
