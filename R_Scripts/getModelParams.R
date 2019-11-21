# return model params

# MAKE A FN TO DO THIS. fn inputs: date, model run. fn outputs: tibble of model params
#params = getModelParams(run_dates = runDates,run_names = runIDs)
library(tidyverse)
library(lubridate)

#data_loc = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/model_param_lookup.csv"

getModelParams = function(run_dates,run_names,randTF,data_loc = "/home/users/pjlevi/dr_stoch_uc/julia_ver/outputs/model_param_lookup.csv"){
  paramLookup = read_csv(data_loc)
  paramLookup$`first date` = mdy(paramLookup$`first date`)
  paramLookup$`last date` = mdy(paramLookup$`last date` )
  # make empty tibble for returned values
  paramReturn = as.data.frame(matrix(nrow = length(run_dates),ncol = 6,
                                     dimnames=list(c(),names(paramLookup[(ncol(paramLookup)-5):ncol(paramLookup)]))))
  
  # parse run_dates to DATE type -- must deal with -vX at the end of some
  run_dates = ymd(substr(run_dates,1,10)) # assume in format like "2019-01-01"
  
  for(i in 1:length(run_dates)){
    # ID which row of paramLookup is correct
    if(!is.na(randTF[i]) & randTF[i]){
      # subsel for RAND part of paramLookup
      randSel = which(paramLookup$randTF)
    } else {
      #subsel for other part of paramLookup
      randSel = which(!paramLookup$randTF)
    }
    paramRow = randSel[which(paramLookup$`first date`[randSel] <= run_dates[i] & paramLookup$`last date`[randSel] >= run_dates[i])] 
    ## EXCEPT some of randTF is NA where it should be FALSE
    ## TODO: This doesn't work yet. currently returns nothing. Also need to back out of the subsetting.
    
    # TODO: ID if run is RAND! to do this... can use contents of alloutputs (are any of inputs related to rand run?), or runID
    # use alloutputs DRrand column (which is pretty much NA or TRUE)
    
    if(length(paramRow) == 0){
      print(paramLookup)
      print("is run_date[i] a date?")
      print(as.character(is.Date(run_dates[i])))
      stop(paste0("No date matches for run_date ",i,", which is ",run_dates[i]))
    } else if(length(paramRow)>1){ 
      warning(paste0("Multiple date matches for run_date ",i,", which is ",run_dates[i],". Using first one"))
      paramRow = paramRow[1]
    }
    # put that row into paramReturn
    paramReturn[i,] = paramLookup[paramRow,(ncol(paramLookup)-5):ncol(paramLookup)]
  }
  return(paramReturn)
}