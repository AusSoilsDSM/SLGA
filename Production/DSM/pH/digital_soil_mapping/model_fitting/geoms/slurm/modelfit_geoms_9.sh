#!/bin/bash
#SBATCH --job-name="ads_d3"
#SBATCH --time=90:00:00
#SBATCH --mem=64GB
#SBATCH --mincpus=1
#SBATCH --nodes=1
#SBATCH -o /datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/model_fitting/geoms/slurm/alldat_sims80_mf_out_d3.txt
#SBATCH -e /datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/model_fitting/geoms/slurm/alldat_sims80_mf_error_d3.txt

module load R/4.0.2
module load gdal
module load proj
Rscript /datasets/work/af-tern-mal-deb/work/projects/ternlandscapes_2019/soil_pH/rcode/digital_soil_mapping/model_fitting/geoms/d3/rangerModelling_modelfit_ph_sims80_alldat_d3.R $SLURM_ARRAY_TASK_ID
