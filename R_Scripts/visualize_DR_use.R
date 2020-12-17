# visualize_DR_use.R
# this function wraps around combine_run_results.R and adds 
# the ability to iterate over multiple runs, and generate selected plots
# use plotDRUse()
# March 2019

# load model data
library(plyr)
library(tidyverse)
library(viridis)
library(data.table)
options(warn=1) # print warnings as they occur

SHRLK = TRUE

## FILE STRUCTURE ##
if(SHRLK){
  base_fol = "/home/users/pjlevi/dr_stoch_uc/julia_ver/"
  output_fol_base  = "/home/groups/weyant/plevi_outputs/"#slowgas/"
  # output_fol_base = "/home/users/pjlevi/dr_stoch_uc/julia_ver/outputs/"
  input_fol = "inputs/"
} else{
  base_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
  output_fol_base = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/forIAEE_1Pmin/"
  input_fol = "Data/julia_input/"
}

# get all run parameters
if(!SHRLK){
  inputs_file = paste0(base_fol,"/Julia_UC_Github/Julia_scripts/inputs_ercot.csv")
}else{
  inputs_file = paste0(base_fol,"/code/inputs_ercot.csv")
}
allinputs = read_csv(inputs_file)

### PARAMS ####----##
runIDs1 = c("advNot1_keyDays","advNot2_keyDays","advNot3_keyDays","avail1_keyDays","avail2_keyDays","hour1_keyDays","hour2_keyDays")#,"start1_keyDays","start2_keyDays")
runDates1 = c(rep("2019-03-15",2),rep("2019-03-16",2),rep("2019-03-17",2),"2019-03-16")#,"2019-03-17","2019-03-16") #this might differ across runs! need to consolidate
# nperiods = 22
runIDs2 = c("start1_keyDays","start2_keyDays","baseOldGen_keyDays" ,"energy1_keyDays","energy2_keyDays")
runDates2=c("2019-03-23",     "2019-03-22",     rep("2019-03-23",3))
runIDs3 = c("start3_keyDays","basenoDR_keyDays", "sensDR1_keyDays")
runDates3=c("2019-03-28","2019-03-27","2019-03-28")
runIDs4 = c("c2advNot1_keyDays","c2advNot2_keyDays","c2advNot3_keyDays")
runDates4 = rep("2019-03-29",3)
runIDs5 = c("base_noDRfullyear")
runDates5 = c("2019-05-02")
runIDs6 = c("advNot1_keyDays_noRampLim","advNot2_keyDays_noRampLim","advNot3_keyDays_noRampLim")
runDates6 = rep("2019-06-28",3)
runIDs7 = c(
  "sensNScen1","sensNScen2",
  "sensOvLap1","sensOvLap2",
            "sensOvLap3","sensOvLap4","sensPeriodLen1","sensPeriodLen2","sensPeriodLen3",
            "sensPeriodLen4","sensBinary")
runDates7 = rep("2019-07-04",11)

runIDs8 = c("oldGen_newSettings1","oldGen_newSettings2","newGen_newSettings1","newGen_newSettings2")
runDates8 = rep("2019-08-30",4)

runIDs9 = c("oldGen_newSett_bin_DR1",	"oldGen_newSett_bin_DR3",	
            "newGen_newSett_bin_DR1",	"newGen_newSett_bin_DR3",	
            "newGen_startuplim_nonbinary",	"newGen_startuplim_binary")
runDates9 = rep("2019-09-04",6)
runIDs10 = c("base_noDRfullyear2")
runDates10 = c("2019-09-12")

runIDs11 = c("rand_test","newGen_newSett_ARMAtest1",
             "newGen_newSett_ARMAtest2","newGen_newSett_ARMAtest3")
runDates11 = c("2019-09-23",rep("2019-09-24",3))

#INFORMS main results
runIDs12 = c("advNot1_keyDays2","advNot2_keyDays2","advNot3_keyDays2", #9-30
             "advNot1_c2_keyDays2","advNot2_c2_keyDays2","advNot3_c2_keyDays2", #9-30,9-30,10-01
             "avail1_keyDays2","avail2_keyDays2","avail3_keyDays2", "avail4_keyDays2", #10-02
             "hour1_keyDays2","hour2_keyDays2",
             "hour3_keyDays2", #10-02
             "energy1_keyDays2","energy2_keyDays2","energy3_keyDays2", #10-01
             "start1_keyDays2","start2_keyDays2","start3_keyDays2")#, #10-01
             # "rand_base") #10-01
runDates12 = c(rep("2019-09-30",3), #advnot
               rep("2019-09-30",2),"2019-10-01", #advnot_c2
               rep("2019-10-02",4), #avail
               rep("2019-10-02",2), #hour
               "2019-10-04", #hour
               rep("2019-10-01",3), #energy
               rep("2019-10-01",3))#, #start
               # "2019-10-01") #rand_base

#INFORMS extra results that looked weird so I didn't use them... except for hour3?
runIDs13 = c("hour3_keyDays2", "medRE_keyDays2",
             "rand_u40pp","rand_u60pp","rand_u80pp")
runDates13 = c("2019-10-02","2019-10-04",
               rep("2019-10-08",3))

# noDR comparison for INFORMS results
runIDs14 = c("noDR_keyDays2")
runDates14 = "2019-10-08"

# not sure
runIDs15 = c("rand_u20pp","rand_u40pp","rand_u60pp","rand_u80pp")
runDates15 = rep("2019-10-14",4)

# Cost sensitivity runs for INFORMS that looked weird so I didnt use them
runIDs16 = c("avail1_c2_keyDays2","avail2_c2_keyDays2","avail3_c2_keyDays2","avail4_c2_keyDays2", #10-16
             "hour1_c2_keyDays2","hour2_c2_keyDays2","hour3_c2_keyDays2", #10-16
             "energy1_c2_keyDays2","energy2_c2_keyDays2","energy3_c2_keyDays2", #10-15
             "start1_c2_keyDays2","start2_c2_keyDays2","start3_c2_keyDays2") #10-15
runDates16 = c(rep("2019-10-16",7),
               rep("2019-10-15",6))

runIDs17 = c("advNot1_c2_keyDays2","advNot2_c2_keyDays2","advNot3_c2_keyDays2")
runDates17 = rep("2019-10-17",3)

runIDs18 = rep("advNot1_keyDays2",4)
runDates18 = c("2019-10-21-v1","2019-10-21-v2","2019-10-21-v3","2019-10-21-v4")
runDates18b = c("2019-10-23-v5","2019-10-23-v6","2019-10-23-v7","2019-10-23-v8")

runIDs19 = c("noDR_keyDays2","noDR_keyDays2","avail1_keyDays2","avail2_keyDays2","avail3_keyDays2","avail4_keyDays2", #10-16
             "hour1_keyDays2","hour2_keyDays2","hour3_keyDays2", #10-16
             "energy1_keyDays2","energy2_keyDays2","energy3_keyDays2", #10-15
             "start1_keyDays2","start2_keyDays2","start3_keyDays2",
             rep("rand_u20pp",3)) #10-15
runDates19 = c("2019-11-04",rep("2019-10-30",14),
               paste0("2019-10-30-v",c(1,2,3)))

# testing effect of removing newer inputs from input files
runIDs20 = c("noDR_keyDays2","advNot1_keyDays2",
             "energy1_keyDays2","energy2_keyDays2","energy3_keyDays2")
runDates20 = rep("2019-11-07",5)

runIDs21 = c("advNot2_keyDays2","advNot3_keyDays2")
runDates21 = rep("2019-11-15",2)

yy = list.files(path = "/home/groups/weyant/plevi_outputs/", pattern = glob2rx("*_o25*keyDays2*"))
last_loc = as.vector(regexpr("\\_[^\\_]*$", yy))
runIDs22 = substr(yy, 1, last_loc - 1)
runDates22 = substr(yy, last_loc+1,100)

yy = list.files(path = "/home/groups/weyant/plevi_outputs/", pattern = "*_o25_c2_keyDays2")
runIDs23 = substr(yy, 1, nchar(yy)-11)
runDates23 = substr(yy, nchar(yy)-9,100)


runIDs24 = "advNot1_o25_c2_keyDays2"
runDates24 = "2019-99-99" # THIS IS A TEST WITH A SMALL PROD

yy = list.files(path = "/home/groups/weyant/plevi_outputs/", pattern = glob2rx("advNot*_o25_c2_keyDays2*"))
runIDs25 = substr(yy, 1, nchar(yy)-11)
runDates25 = substr(yy, nchar(yy)-9,100)

yy = list.files(path = "/home/groups/weyant/plevi_outputs/", pattern = glob2rx("rand_o25_u*"))
last_loc = as.vector(regexpr("\\_[^\\_]*$", yy))
runIDs26 = substr(yy, 1, last_loc - 1)
runDates26 = substr(yy, last_loc+1,100)

#HYDRO AS SLOW GENERATOR RUNS
xx = list.files(path = "/home/groups/weyant/plevi_outputs/slow_hydro/", pattern = glob2rx("*_o25_*keyDays2*"))
runIDs27 = substr(xx, 1, nchar(xx)-11)
runDates27 = substr(xx, nchar(xx)-9,100)

xx = c("avail3_o25_c2_keyDays2_2020-03-25","avail3_o25_keyDays2_2020-03-25",
       "avail4_o25_c2_keyDays2_2020-03-25","avail4_o25_keyDays2_2020-03-25")
runIDs28 = substr(xx, 1, nchar(xx)-11)
runDates28 = substr(xx, nchar(xx)-9,100)

# avail slow hydro
xx = list.files(path = "/home/groups/weyant/plevi_outputs/slow_hydro/", pattern = glob2rx("avail*_o25_*keyDays2*04-01"))
runIDs29 = substr(xx, 1, nchar(xx)-11)
runDates29 = substr(xx, nchar(xx)-9,100)

# cheap DR runs
xx = c("advNot1_o25_c3_keyDays2_2020-04-01","advNot1_o25_c4_keyDays2_2020-04-01")
runIDs30 = substr(xx, 1, nchar(xx)-11)
runDates30 = substr(xx, nchar(xx)-9,100)

xx = c("dur1_o25_keyDays2_2020-07-20","dur2_o25_keyDays2_2020-07-20","dur3_o25_keyDays2_2020-07-20")
runIDs31 = substr(xx, 1, nchar(xx)-11)
runDates31 = substr(xx, nchar(xx)-9,100)

xx = c("noDR_o25_keyDays2_2020-07-20")
runIDs32 = substr(xx, 1, nchar(xx)-11)
runDates32 = substr(xx, nchar(xx)-9,100)

xx = list.files(path = "/home/groups/weyant/plevi_outputs/", pattern = glob2rx("dur*"))
runIDs33 = substr(xx, 1, nchar(xx)-11)
runDates33 = substr(xx, nchar(xx)-9,100)

xx = list.files(path = "/home/groups/weyant/plevi_outputs/slowgas/", pattern = glob2rx("*_o25_*keyDays2*"))
runIDs34 = substr(xx, 1, nchar(xx)-11)
runDates34 = substr(xx, nchar(xx)-9,100)

xx = c("dur4_o25_keyDays2_2020-07-20")
runIDs35 = substr(xx, 1, nchar(xx)-11)
runDates35 = substr(xx, nchar(xx)-9,100)

xx = list.files(path = "/home/groups/weyant/plevi_outputs/", pattern = glob2rx("rand*_o2_*keyDays2*"))
runIDs36 = substr(xx, 1, nchar(xx)-11)
runDates36 = substr(xx, nchar(xx)-9,100)

xx = list.files(path = "/home/groups/weyant/plevi_outputs/", pattern = glob2rx("rand*_o1_*keyDays2*"))
runIDs37 = substr(xx, 1, nchar(xx)-11)
runDates37 = substr(xx, nchar(xx)-9,100)

xx = c("rand_o2_100mean_5050_keyDays2_2020-10-05")
runIDs38 = substr(xx, 1, nchar(xx)-11)
runDates38 = substr(xx, nchar(xx)-9,100)

runIDs = c(runIDs38)#, runIDs12)#c(runIDs15,runIDs14,runIDs12)#c(runIDs1,runIDs2)
runDates = c(runDates38)#,runDates12)#c(runDates15,runDates14,runDates12)#c(runDates1, runDates2)
inputfolID = "5d_6o_keyDays2" # for plotDR - need to fix to read in dynamically.
# overlaplength = 12 # should be read in from inputs_ercot

## run options
summary_combine = T # needed to create prod.csv
plotDR = F
genbreakdown_only = F # this is done in summary_combine
rampdata_df = F
loadOverrideOption = F# should prod be re-calculated?
##----##----##----##

instance_in_fol = paste0(base_fol,input_fol,inputfolID,"/") # e.g. "/home/users/pjlevi/dr_stoch_uc/julia_ver/inputs/5d_6o_keyDays2/"
default_in_fol = paste0(base_fol,input_fol,"ercot_default/")  # e.g. "/home/users/pjlevi/dr_stoch_uc/julia_ver/inputs/ercot_default/"

if(!SHRLK){
  source(paste0(base_fol,"Julia_UC_Github/R_Scripts/mergeTimeseriesData.R")) # contains loadTimeseriesData
  source(paste0(base_fol,"Julia_UC_Github/R_Scripts/consolidatedAnalysisFns.R")) # contains plotting functions
  source(paste0(base_fol,"Julia_UC_Github/R_Scripts/combine_run_results.R")) # contains combineRunResults()
} else{
  source(paste0(base_fol,"code/R_Scripts/mergeTimeseriesData.R")) # contains loadTimeseriesData()
  source(paste0(base_fol,"code/R_Scripts/consolidatedAnalysisFns.R")) # contains plotting functions
  source(paste0(base_fol,"code/R_Scripts/combine_run_results.R")) # contains combineRunResults()
}

# combine run results
if(summary_combine){
  print("starting combineRunResults")
  options(readr.num_columns = 0) # turn off read_csv messages
  for(r in 1:length(runIDs)){
    print(runIDs[r])
    # if(file.exists(paste(base_fol,outputFolBase,runIDs[r],"_",runDates[r]))){
      combineRunResults(runIDs[r],runDates[r],graphs=F,load_override = loadOverrideOption,
                        base_fol = base_fol, output_fol_base = output_fol_base)
    # } else if(file.exists(paste(scratch_output_fol,runIDs[r],"_",runDates[r]))) {
      # combineRunResults(runIDs[r],runDates[r],graphs=F,load_override = loadOverrideOption,
                        # input_fol = scratch_)
    # } else {error("file not found")}
    
    warnings()
    print("Done, resting")
    Sys.sleep(15) #let the computer cool down
  }
}

# plot DR use
if(plotDR){
  print("starting plotDRUse")
  for(r in 1:length(runIDs)){
    params = allinputs[,c("input_name",runIDs[r])]
    params = spread(params, key = input_name, value = runIDs[r])
    overlaplength = as.numeric(params$overlapLength)
    
    outputID = paste0(runIDs[r],"_",runDates[r])
    output_fol = paste0(output_fol_base,outputID,"/")
    output_fol = paste0(output_fol_base,outputID,"/")
    allcomt = loadTimeseriesData(output_fol,"u_commitment",overlaplength,2, probabilities=F,instance_in_fol,params$nrandp,dist_ID = params$stochID,endtrim=6)
    drcomt = filter(allcomt,str_detect(GEN_IND,"DR-"))
    rm(allcomt)
    # iterate over function
    # plot dr commitment and demand
  
    plotDRUse(runIDs[r],runDates[r], drcommit = drcomt, 
              inputfolID=inputfolID,outputfolID = outputID,
              period = "p2_1020_1140")
  
  } #end for loop
}

# only generate genbreakdown plot - done with in combineRunResults too
if(genbreakdown_only){
  print("starting genbreakdown")
  for(r in 1:length(runIDs)){
    print(runIDs[r])
    outputID = paste0(runIDs[r],"_",runDates[r])
    output_fol = paste0(output_fol_base,outputID,"/")
    # load gendat
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
  
    fuelBreakdown(prod2,paste0(output_fol_base,"plots/"),runIDs[r])
  }
}

# create dataframe of ramping data
if(rampdata_df){
  print("starting rampdata")
  for(r in 1:length(runIDs)){
    print(runIDs[r])
    outputID = paste0(runIDs[r],"_",runDates[r])
    output_fol = paste0(output_fol_base,outputID,"/")

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
  write_csv(xx,path = paste0(output_fol_base,"Ramp_data3.csv"))
}
