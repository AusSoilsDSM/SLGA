#!/bin/bash
#SBATCH --job-name="lds_d1"
#SBATCH --time=90:00:00
#SBATCH --mem=64GB
#SBATCH --mincpus=1
#SBATCH --nodes=1
#SBATCH -o /datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/model_fitting/geoms/slurm/labdat_sims80_mf_out_d1.txt
#SBATCH -e /datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/model_fitting/geoms/slurm/labdat_sims80_mf_error_d1.txt

module load R/4.0.2
module load gdal
module load proj
Rscript /datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/model_fitting/geoms/d1/rangerModelling_modelfit_ph_sims80_labdat_d1.R $SLURM_ARRAY_TASK_ID
