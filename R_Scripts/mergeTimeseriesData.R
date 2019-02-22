# mergeTimeseriesData.R
# reads in timeseries data from multi-period runs
# and concatenates it into one file
library(tidyverse)
base_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
output_fol_base = "Data/julia_output/"
input_fol = "Data/julia_input/"

inputfolID = "test_multi" #"7d_12o_periods_10subsel"
outputID = "test3_2019-02-20"

instance_in_fol = paste0(base_fol,input_fol,inputfolID,"/")
output_fol = paste0(base_fol,output_fol_base,outputID,"/")

loadproduction <- function(instance_in_fol, output_fol) {
  # instance_in_fol is the periods folder (in inputs)
  # output_fol is the output data folder
  all_periods = list.files(path = instance_in_fol)
  for(i in 1:length(all_periods)){
    xx = all_periods[i]
    periodinfo = strsplit(xx,"_")[[1]]
    firstperiod = as.numeric(periodinfo[3])
    lastperiod = as.numeric(strsplit(periodinfo[4],"\\.")[[1]][1])
    periodID = paste0(periodinfo[2],"_",periodinfo[3],"_",periodinfo[4])
    
    slow_prod = read_csv(file = paste0(output_fol,"slow_production_p",periodinfo[2],"_",periodinfo[3],"-",periodinfo[4]))
    slow_prod$speed = "slow"
    fast_prod = read_csv(file = paste0(output_fol,"fast_production_p",periodinfo[2],"_",periodinfo[3],"-",periodinfo[4]))
    fast_prod$speed = "fast"
    prod = rbind(slow_prod, fast_prod)
    
    prod$t = prod$t + firstperiod -1
    
    #TRIM BOTH SIDES UNLESS THIS IS FIRST PERIOD. THEN TRIM END
    if(as.numeric(periodinfo[2]) ==1){
      
    } else{
      
    }
    # HOW TO FIND OVERLAP LENGTH FOR JUST ONE PERIOD?
    # 1) have it as an input
    # 2) read it from inputs file
    
    # overlaplength = lastperiod_prev - firstperiod +1
    # ## DEAL WITH OVERLAP
    # ## TRIM ONLY THE JUST-LOADED OBJECT
    # # find # of periods of overlap
    # # remove half of those from former, half from latter
    # # if odd number, remove more from latter
    # if(overlaplength %% 2 == 0){
    #   # remove stuff from end of previous period
    #   # lastsavedperiod = firstperiod + (overlaplength/2) -1
    #   # rmprevsel=(prod_all$t <= lastperiod_prev & prod_all$t > lastsavedperiod)
    #   # prod_all = prod_all[!rmprevsel,]
    #   
    #   # remove stuff from beginning of new period
    #   first
    # }
    
    
    if(i==1){ prod_all = prod 
    } else { 
      prod_all = rbind(prod_all, prod)
    }
    
    # PERIODS MIGHT NOT BE SEQUENTIAL
    firstperiod_prev = firstperiod
    lastperiod_prev = lastperiod
  }
  return(prod_all)
}

  loadstuff <- function(instance_in_fol, output_fol) {
    # instance_in_fol is the periods folder (in inputs)
    # output_fol is the output data folder
    all_periods = list.files(path = instance_in_fol)
    
    #CREATE DUMMY HOLDERS?
    
    for(i in 1:length(all_periods)){
      xx = all_periods[i]
      periodinfo = strsplit(xx,"_")[[1]]
      periodID = paste0(periodinfo[2],"_",periodinfo[3],"_",periodinfo[4])
      # makedemandgraphs(default_in_fol, instance_in_fol, output_fol, periodID,stochID,outputID)
    
    
    
      slow_commit = read_csv(file = paste0(output_fol,"slow_commitment_p",periodinfo[1],"_",periodinfo[2],"-",periodinfo[3]))
      
      timeperiods = read_csv(file = paste0(instance_in_fol,"periods_",periodID), col_names = "hour")
      
      # slow_commit = read_csv(file = paste0(output_fol,"slow_commitment.csv"))
      slow_commit = read_csv(file = paste0(output_fol,"slow_commitment_p",periodinfo[1],"_",periodinfo[2],"-",periodinfo[3]))
      slow_commit$t = timeperiods$hour
      
      slow_prod = read_csv(file = paste0(output_fol,"slow_production_p",periodinfo[1],"_",periodinfo[2],"-",periodinfo[3]))
      slow_prod$speed = "slow"
      fast_prod = read_csv(file = paste0(output_fol,"fast_production_p",periodinfo[1],"_",periodinfo[2],"-",periodinfo[3]))
      fast_prod$speed = "fast"
      prod = rbind(slow_prod, fast_prod)
      
      # load dr dispatch
      
      #MERGE FILES
      # removing a portion of beginning and end...
    }
    #SAVE FILES
    
    #MAKE GRAPHS
    
    #DETERMINE KEY VALUES
    # max first-stage committed capacity
    # max up and down ramp rates of rest of system (flexibility value)
    # co2 emissions
    
  }


  # mergeOtherData()
# reads in non-timeseries data from multi-period runs
# and concatenates it into one file

# cost, cost breakdown
# TODO: save this by hour, or calculate from startup, production data


# loadRunInputs()
# given one input file for a multi-period run
# loads up the generator data and distribution data
