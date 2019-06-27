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
overlaplength = 6

outputID = paste0(runID,"_",runDate)
output_fol = paste0(baseFol,outputFolBase,outputID,"/")

# consolidate and save info

print("Loading fast production data")
fastprod = loadTimeseriesData(output_fol, "fast_production", overlaplength,2,instance_in_fol,params$nrandp,dist_ID = params$stochID, probabilities = F)
print("Loading slow production data")
slowprod = loadTimeseriesData(output_fol, "slow_production", overlaplength,2,instance_in_fol,params$nrandp,dist_ID = params$stochID, probabilities = F)
slowprod$speed = "slow"
fastprod$speed = "fast"
prod = rbind(slowprod, fastprod)
names(prod)[names(prod) == 'value'] <- 'MWout'
rm(fastprod, slowprod)
# write_csv(prod, paste0(output_fol,"prod.csv")) # cannot turn off scientific notation in write_csv
prod$prob = 1/25
write.csv(prod, paste0(output_fol,"prod.csv"))
rm(prod)

print("Loading demand dual")
demanddual = loadTimeseriesData(output_fol,"supplydemand_shadow", overlaplength,2)
write.csv(demanddual, paste0(output_fol,"demanddual.csv"))
rm(demanddual) #memory cleanup

print("Loading mingen dual data")
mingendual = loadTimeseriesData(output_fol,"mingen_shadow", overlaplength,2) ## DOESNT WORK!!
write.csv(mingendual, paste0(output_fol,"mingendual.csv"))