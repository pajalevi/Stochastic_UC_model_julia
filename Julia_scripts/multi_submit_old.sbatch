#!/bin/bash
#SBATCH --job-name=7d_6int_multirun
#SBATCH --output=7d_6int_multirun_%A_%a.out
#SBATCH --error=7d_6int_multirun_%A_%a.err
#SBATCH --time=10:00:00
#SBATCH -p normal
#SBATCH --mem=64G
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=pjlevi@stanford.edu
#SBATCH --array=0-9

cd ~/dr_stoch_uc/julia_ver/inputs/7d_12o_periods_10subsel/
declare -ga PERIODS
for file in *.csv
do
        PERIODS=(${PERIODS[@]} "$file")
done
echo ${#PERIODS[@]}
echo "${PERIODS[@]}"

export PERIODS

# then make zero based array index for number of files
# NUMPERIODS=${#PERIODS[@]}
# ZBNUMPERIODS=$(($NUMPERIODS - 1))
#
# # submit as
# if [ $NUMPERIODS -ge 0 ]
# sbatch --array=0-$ZBNUMPERIODS multi_submit.sbatch
# fi


cd ~/dr_stoch_uc/julia_ver/code
module load julia/0.6.4
module load gurobi
PERIODNAME=${PERIODS[$SLURM_ARRAY_TASK_ID]}
echo $PERIODNAME
DATE=`date '+%Y-%m-%d_%H-%M-%S'`
julia ercot_stoch.jl $DATE inputs_ercot.csv true $PERIODNAME
env | egrep SLURM

## prior to submitting this, need to run the following in cmd line:
## export PERIODS=("$(ls -d periods*.csv)")
## inside the folder that has all the periods
##  OR

## declare -ga PERIODS
## for file in *.csv
## do
##         PERIODS=(${PERIODS[@]} "$file")
## done
## echo ${#PERIODS[@]}
## echo "${PERIODS[@]}"
#
## then make zero based array index for number of files
## NUMPERIODS=${#PERIODS[@]}
## ZBNUMPERIODS=$(($NUMPERIODS - 1))

## submit as
## if [ $NUMPERIODS -ge 0 ]
## sbatch --array=0-$ZBNUMPERIODS multi_submit.sbatch
## fi
