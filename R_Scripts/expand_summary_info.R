# analysis_functions.R

# expand_summary_info()
# takes in CSVs of summary files
# combines them and calculates additional columns

library(tidyverse)

expand_summary_info = function(input_files,base_run_name,output_name,folder){
  # load and combine files
  for(i in 1:length(input_files)){
    if(i==1){
      results = read_csv(paste0(folder,input_files[i]))
    } else {
      results = bind_rows(results,read_csv(paste0(folder,input_files[i])))
    }
  }
  
  # calculate additional columns
  
}

expand_summary_info(input_files = c("combined_summary_availability_runs.csv","combined_summary_INFORMS_noDR.csv","combined_summary_INFORMS_rand2.csv"),
                    base_run_name = "noDR_keyDays2",
                    output_name = "INFORMS_combine_summary.csv",
                    folder = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/")


# plot hours DR committed
# plot cost savings
# plot cost savings per MWh hour shed
# plot cost savings per MWh hour shed vs MWh hour shed