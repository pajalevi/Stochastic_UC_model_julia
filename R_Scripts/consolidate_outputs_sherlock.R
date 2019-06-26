# combine dual variable information
# for downloading -- script for submission on sherlock
# using LoadTimeseriesData
# can be used instead of the heavier-duty 'visualize_DR_use.R' or 'combine_run_results.R'
# june 2019

library(tidyverse)
library(data.table)

SHRLK = TRUE

## FILE STRUCTURE ##
if(SHRLK){
  baseFol = "/home/users/pjlevi/dr_stoch_uc/julia_ver/"
  outputFolBase  = "outputs/"
  inputFol = "inputs/"
} else{
  baseFol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
  outputFolBase = "Data/julia_output/forIAEE_1Pmin/"
  inputFol = "Data/julia_input/"
}

## source files ##
source(paste0(baseFol,"/code/R_Scripts/mergeTimeseriesData.R")) # contains loadTimeseriesData
source(paste0(baseFol,"/code/R_Scripts/consolidatedAnalysisFns.R"))

runID = "base_noDRfullyear" #"hour1" #"avail2"
runDate = "2019-05-02" 
overlaplength = 24

outputID = paste0(runID,"_",runDate)
output_fol = paste0(baseFol,outputFolBase,outputID,"/")

# consolidate and save info
demanddual = loadTimeseriesData(output_fol,"supplydemand_shadow", overlaplength,1)
write.csv(demanddual, paste0(output_fol,"demanddual.csv"))
rm(demanddual) #memory cleanup

mingendual = loadTimeseriesData(output_fol,"mingen_shadow", overlaplength,1) ## DOESNT WORK!!
write.csv(mingendual, paste0(output_fol,"mingendual.csv"))