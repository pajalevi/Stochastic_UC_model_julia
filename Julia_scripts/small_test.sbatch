#!/bin/bash

#SBATCH --job-name=test_small
#SBATCH --output=test_small.%j.out
#SBATCH --error=test_small.%j.err
##format could be dd-hh:mm:ss, hh:mm:ss, mm:ss, or mm
#SBATCH --time=1:00:00
# We are submitting to the dev partition, there are several on sherlock: normal, gpu, bigmem (jobs requiring >64Gigs RAM)
#SBATCH -p dev
#SBATCH --mem=6G
#SBATCH --mail-type=END,FAIL,BEGIN # notifications for job done & fail
#SBATCH --mail-user=pjlevi@stanford.edu

cd ~/dr_stoch_uc/julia_ver/code

module load gurobi
module load julia/0.6.4
julia full_universe_stoch.jl 144h2groups n3_m1.0_0.2pp base_testing_small
