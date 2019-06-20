# use plotDRUse()
# March 2019

# load model data
library(tidyverse)
library(viridis)
library(data.table)

SHRLK = TRUE

## FILE STRUCTURE ##
if(SHRLK){
  base_fol = "/home/users/pjlevi/dr_stoch_uc/julia_ver/"
  output_fol_base  = "outputs/"
  input_fol = "inputs/"
} else{
  base_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
  output_fol_base = "Data/julia_output/forIAEE_1Pmin/"
  input_fol = "Data/julia_input/"
}

### PARAMS ####----##
inputfolID = "5d_keyDays"
runIDs1 = c("advNot1_keyDays","advNot2_keyDays","advNot3_keyDays","avail1_keyDays","avail2_keyDays","hour1_keyDays","hour2_keyDays")#,"start1_keyDays","start2_keyDays")
runDates1 = c(rep("2019-03-15",2),rep("2019-03-16",2),rep("2019-03-17",2),"2019-03-16")#,"2019-03-17","2019-03-16") #this might differ across runs! need to consolidate
overlaplength = 12
nperiods = 22
runIDs2 = c("start1_keyDays","start2_keyDays","baseOldGen_keyDays" ,"energy1_keyDays","energy2_keyDays")
runDates2=c("2019-03-23",     "2019-03-22",     rep("2019-03-23",3))
runIDs3 = c("start3_keyDays","basenoDR_keyDays", "sensDR1_keyDays")
runDates3=c("2019-03-28","2019-03-27","2019-03-28")
runIDs4 = c("c2advNot1_keyDays","c2advNot2_keyDays","c2advNot3_keyDays")
runDates4 = rep("2019-03-29",3)

runIDs5 = c("base_noDRfullyear")
runDates5 = c("2019-05-02")

runIDs = runIDs5#c(runIDs1,runIDs2)
runDates = runDates5#c(runDates1, runDates2)

## run options
summary_combine = T # needed to create prod.csv
plotDR = F
genbreakdown_only = F
rampdata_df = F
##----##----##----##

instance_in_fol = paste0(base_fol,input_fol,inputfolID,"/")
default_in_fol = paste0(base_fol,input_fol,"ercot_default/")

if(!SHRLK){
  source(paste0(base_fol,"Julia_UC_Github/R_Scripts/mergeTimeseriesData.R")) # contains loadTimeseriesData
  source(paste0(base_fol,"Julia_UC_Github/R_Scripts/consolidatedAnalysisFns.R"))
  source(paste0(base_fol,"Julia_UC_Github/R_Scripts/combine_run_results.R"))
} else{
  source(paste0(base_fol,"code/R_Scripts/mergeTimeseriesData.R")) # contains loadTimeseriesData
  source(paste0(base_fol,"code/R_Scripts/consolidatedAnalysisFns.R"))
  source(paste0(base_fol,"code/R_Scripts/combine_run_results.R"))
}

# combine run results
if(summary_combine){
  options(readr.num_columns = 0) # turn off read_csv messages
  for(r in 1:length(runIDs)){
    print(runIDs[r])
    combineRunResults(runIDs[r],runDates[r],graphs=F)
    print("Done, resting")
    Sys.sleep(60) #let the computer cool down for 2 minutes
  }
}

# plot DR use
if(plotDR){
  for(r in 1:length(runIDs)){
    outputID = paste0(runIDs[r],"_",runDates[r])
    output_fol = paste0(base_fol,output_fol_base,outputID,"/")
    output_fol = paste0(base_fol,output_fol_base,outputID,"/")
    allcomt = loadTimeseriesData(output_fol,"u_commitment",overlaplength,2, probabilities=F,instance_in_fol,params$nrandp,dist_ID = params$stochID)
    drcomt = filter(allcomt,str_detect(GEN_IND,"DR-"))
    rm(allcomt)
    # iterate over function
    # plot dr commitment and demand
  
    plotDRUse(runIDs[r],runDates[r], drcommit = drcomt, inputfolID,outputID)
  
  } #end for loop
}

# only generate genbreakdown plot - done with in combineRunResults too
if(genbreakdown_only){
  for(r in 1:length(runIDs)){
    print(runIDs[r])
    outputID = paste0(runIDs[r],"_",runDates[r])
    output_fol = paste0(base_fol,output_fol_base,outputID,"/")
    # load gendat
    if(!SHRLK){
     inputs_file = paste0(base_fol,"/Julia_UC_Github/Julia_scripts/inputs_ercot.csv")
    }else{
      inputs_file = paste0(base_fol,"/code/inputs_ercot.csv")
    }
    allinputs = read_csv(inputs_file)
    params = allinputs[,c("input_name",runID)]
    params = spread(params, key = input_name, value = runID)
    gendat = read_csv(paste0(default_in_fol,params$genFile))
    # load prod
    print("Loading prod.csv")
    prod = read_csv(file = paste0(output_fol,"prod.csv"))
    prod$prob = 1/25
    # merge
    prod2 = prod %>%
      merge(gendat[,c("Capacity","PMin","plantUnique","VCost","Fuel")], by.x = "GEN_IND", by.y = "plantUnique") %>%
      filter(MWout > 0)
  
    fuelBreakdown(prod2,paste0(base_fol,output_fol_base,"plots/"),runIDs[r])
  }
}

# create dataframe of ramping data
if(rampdata_df){
  for(r in 1:length(runIDs)){
    print(runIDs[r])
    outputID = paste0(runIDs[r],"_",runDates[r])
    output_fol = paste0(base_fol,output_fol_base,outputID,"/")
    # load gendat
    if(!SHRLK){
      inputs_file = paste0(base_fol,"/Julia_UC_Github/Julia_scripts/inputs_ercot.csv")
    }else{
      inputs_file = paste0(base_fol,"/code/inputs_ercot.csv")
    }
    allinputs = read_csv(inputs_file)
    params = allinputs[,c("input_name",runID)]
    params = spread(params, key = input_name, value = runID)
    gendat = read_csv(paste0(default_in_fol,params$genFile))
    # load prod
    print("Loading prod.csv")
    prod = read_csv(file = paste0(output_fol,"prod.csv"))
    prod$prob = 1/25
    # merge
    prod2 = prod %>%
      merge(gendat[,c("Capacity","PMin","plantUnique","VCost","Fuel")], by.x = "GEN_IND", by.y = "plantUnique") %>%
      filter(MWout > 0)
  
    
    if(r == 1){
      xx = rampInfo(prod2,runIDs[r])
    } else {
      xx = bind_rows(xx,rampInfo(prod2,runIDs[r]))
    }
  }
  xx=arrange(xx,REgen,speed,runName)
  write_csv(xx,path = paste0(base_fol,output_fol_base,"Ramp_data3.csv"))
}
