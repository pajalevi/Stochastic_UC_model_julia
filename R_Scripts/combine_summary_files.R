# combine_summary_files.csv

# load model data
library(tidyverse)
library(viridis)
library(data.table)

SHRLK = TRUE


### PARAMS ####----##
runIDs7 = c(
  "sensNScen1","sensNScen2",
  "sensOvLap1","sensOvLap2",
  "sensOvLap3","sensOvLap4","sensPeriodLen1","sensPeriodLen2","sensPeriodLen3",
  "sensPeriodLen4","sensBinary")
runDates7 = rep("2019-07-04",11)

runIDs = runIDs7#c(runIDs1,runIDs2)
runDates = runDates7#c(runDates1, runDates2)


# iterate through all summary files and combine them ####
combineSummaryFiles = function(runIDs, runDates, SHRLK){
  if(SHRLK){
    base_fol = "/home/users/pjlevi/dr_stoch_uc/julia_ver/"
    output_fol_base  = "outputs/"
    input_fol = "inputs/"
    inputs_file = paste0(base_fol,"/code/inputs_ercot.csv")
  } else{
    base_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
    output_fol_base = "Data/julia_output/forIAEE_1Pmin/"
    input_fol = "Data/julia_input/"
    inputs_file = paste0(base_fol,"/Julia_UC_Github/Julia_scripts/inputs_ercot.csv")
  }
  allinputs = read_csv(inputs_file)
  
  
  for(i in 1:length(runIDs)){
    # load summary file
    summaryfile = read_csv(paste0(base_fol,output_fol_base,runIDs[i],"_",runDates[i],
                                  "/summary_stats",runIDs[i],".csv"))
    
    # clean summary file
    
    # combine with inputs
    params = allinputs[,c("input_name",runIDs[i])]

    # name cols of alloutputs if needed
    if(i==1){
      # create outputs matrix
      alloutputs = as_tibble(matrix(nrow = length(runIDs), ncol = 1+ nrow(params) + nrow(summaryfile)))
      
      names(alloutputs) = c("runID",params$input_name,summaryfile$output_type)
    }
    
    # combine with previous data
    alloutputs[i,] = c(runIDs[i],params[[2]],summaryfile[[2]])
      
  }
  
  # save
  write_csv(alloutputs,paste0(base_fol,output_fol_base,"combined_summary.csv"))
}