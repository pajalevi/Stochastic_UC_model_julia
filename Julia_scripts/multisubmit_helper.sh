#!/bin/bash

## NOT USED ANYMORE
 declare -ga PERIODS
 for file in *.csv
 do
         PERIODS=(${PERIODS[@]} "$file")
 done
 echo ${#PERIODS[@]}
 echo "${PERIODS[@]}"

 export PERIODS

# then make zero based array index for number of files
 NUMPERIODS=${#PERIODS[@]}
 ZBNUMPERIODS=$(($NUMPERIODS - 1))

# submit as
 if [ $NUMPERIODS -ge 0 ]
 sbatch --array=0-$ZBNUMPERIODS multi_submit.sbatch
 fi
