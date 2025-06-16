#!/bin/sh
module load R/3.6.3
/apps/R/3.6.3/lib64/R/bin/Rscript /datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soilColour/rcode/digitalsoilmapping/spatialPrediction/nir_spatial_all.R $SLURM_ARRAY_TASK_ID 
