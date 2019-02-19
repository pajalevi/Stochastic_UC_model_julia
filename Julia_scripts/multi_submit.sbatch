#!/bin/bash
#SBATCH --job-name=test4_multirun
#SBATCH --output=test4_multirun_%A_%a.out
#SBATCH --error=test4_multirun_%A_%a.err
#SBATCH --time=10:00:00
#SBATCH -p normal
#SBATCH --mem=64G
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=pjlevi@stanford.edu

## CHANGE ME ######
#SBATCH --array=0-56

## CHANGE ME ######
cd ~/dr_stoch_uc/julia_ver/inputs/7d_12o_periods/
declare -ga PERIODS
for file in *.csv
do
        PERIODS=(${PERIODS[@]} "$file")
done
echo ${#PERIODS[@]}
echo "${PERIODS[@]}"

export PERIODS

cd ~/dr_stoch_uc/julia_ver/code

module load julia/0.6.4
module load gurobi
PERIODNAME=${PERIODS[$SLURM_ARRAY_TASK_ID]}
echo $PERIODNAME
DATE=`date '+%Y-%m-%d'`

## CHANGE ME ######
julia ercot_stoch.jl $DATE inputs_ercot.csv test4 true $PERIODNAME
