#!/bin/bash

#SBATCH --job-name=test_small
#SBATCH --output=test_small.%j.out
#SBATCH --error=test_small.%j.err
##format could be dd-hh:mm:ss, hh:mm:ss, mm:ss, or mm
#SBATCH --time=6:00:00
# We are submitting to the dev partition, there are several on sherlock: normal, gpu, bigmem (jobs requiring >64Gigs RAM)
#SBATCH -p normal
#SBATCH --mem=4G
#SBATCH --mail-type=END,FAIL # notifications for job done & fail
#SBATCH --mail-user=pjlevi@stanford.edu

cd ~/dr_stoch_uc/julia_ver/code

module load julia
julia small_universe_stoch.jl
