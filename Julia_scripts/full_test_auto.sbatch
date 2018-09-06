#!/bin/bash

#SBATCH --job-name=test_full
#SBATCH --output=test_full.%j.out
#SBATCH --error=test_full.%j.err
##format could be dd-hh:mm:ss, hh:mm:ss, mm:ss, or mm
#SBATCH --time=2:00:00
# We are submitting to the dev partition, there are several on sherlock: normal, gpu, bigmem (jobs requiring >64Gigs RAM)
#SBATCH -p normal
#SBATCH --mem=4G
#SBATCH --mail-type=BEGIN,END,FAIL # notifications for job done & fail
#SBATCH --mail-user=pjlevi@stanford.edu

cd ~/dr_stoch_uc/julia_ver/code

module load julia/0.6.4
module load gurobi
julia full_universe_stoch.jl 144h2groups n3_m1.0_0.2pp base_testing_small
