#!/bin/bash

JOBNAME = ${JOBNAME}

#SBATCH --job-name=${JOBNAME}
#SBATCH --output=${JOBNAME}.%j.out
#SBATCH --error=${JOBNAME}.%j.err
##format could be dd-hh:mm:ss, hh:mm:ss, mm:ss, or mm
#SBATCH --time=2:00:00
# We are submitting to the dev partition, there are several on sherlock: normal, gpu, bigmem (jobs requiring >64Gigs RAM)
#SBATCH -p normal
#SBATCH --mem=32G
#SBATCH --mail-type=BEGIN,END,FAIL # notifications for job done & fail
#SBATCH --mail-user=pjlevi@stanford.edu

cd ~/dr_stoch_uc/julia_ver/code

module load julia/0.6.4
module load gurobi
## julia ercot_stoch.jl 48h1groups n3_m1.0_0.2pp ercot_testing 24 dr_availability_daytime_2016 0 0 0
julia ercot_stoch.jl
