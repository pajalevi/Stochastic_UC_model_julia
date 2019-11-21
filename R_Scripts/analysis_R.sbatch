#!/bin/bash



#SBATCH --job-name=R_analysis
#SBATCH --output=r_analysis.%j.out
#SBATCH --error=r_analysis.%j.err
##format could be dd-hh:mm:ss, hh:mm:ss, mm:ss, or mm
#SBATCH --time=2:00:00
# We are submitting to the dev partition, there are several on sherlock: normal, gpu, bigmem (jobs requiring >64Gigs RAM)
#SBATCH -p normal
#SBATCH --mem=50G
#SBATCH --mail-type=BEGIN,END,FAIL # notifications for job done & fail
#SBATCH --mail-user=pjlevi@stanford.edu

cd ~/dr_stoch_uc/julia_ver/code/R_Scripts/


module load R


srun R CMD BATCH --no-save --quiet --slave visualize_DR_use.R

