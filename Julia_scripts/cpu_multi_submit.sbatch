#!/bin/bash
#SBATCH --job-name=advNot1_keyDays_noRampLim
#SBATCH --output=advNot1_keyDays_noRampLim_%A_%a.out
#SBATCH --error=advNot1_keyDays_noRampLim_%A_%a.err
#SBATCH --time=10:00:00
#SBATCH -p normal
#SBATCH --mem=30G
#SBATCH --cpus-per-task=4
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=pjlevi@stanford.edu

## CHANGE ME ######
#SBATCH --array=0-21

## CHANGE ME ######
cd ~/dr_stoch_uc/julia_ver/inputs/5d_keyDays/
declare -ga PERIODS
for file in periods_*.csv
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
julia ercot_stoch.jl $DATE inputs_ercot.csv advNot1_keyDays_noRampLim true $PERIODNAME
