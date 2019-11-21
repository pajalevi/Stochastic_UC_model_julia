#!/bin/bash

JOBNAME = 6intmultitest
#SBATCH --job-name=${JOBMNAME}
#SBATCH --output=${JOBMNAME}.%j.out
#SBATCH --error=${JOBMNAME}.%j.err
##format could be dd-hh:mm:ss, hh:mm:ss, mm:ss, or mm
#SBATCH --time=24:00:00
# We are submitting to the dev partition, there are several on sherlock: normal, gpu, bigmem (jobs requiring >64Gigs RAM)
#SBATCH -p normal
#SBATCH --mem=120G
#SBATCH --mail-type=END,FAIL # notifications for job done & fail
#SBATCH --mail-user=pjlevi@stanford.edu

cd ~/dr_stoch_uc/julia_ver/code

DATE=`date '+%Y-%m-%d_%H-%M-%S'`

module load julia/0.6.4
module load gurobi
#["<date>" ,"<inputs_file_name>", "<multi-runTF>", "<period_name>" ]
julia ercot_stoch.jl ${DATE} inputs_ercot.csv true periods_48h1groups.csv
