# combine_summary_files.csv

# load model data
library(tidyverse)
library(data.table)
source("./getModelParams.R")
# library(stringr)
library(plyr) #for rbind.fill

SHRLK = TRUE


### PARAMS ####----##
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
# "2019-10-01") #rand_base
runIDs13 = c("hour3_keyDays2", "medRE_keyDays2",
             "rand_u40pp","rand_u60pp","rand_u80pp")
runDates13 = c("2019-10-02","2019-10-04",
               rep("2019-10-08",3))

runIDs14 = c("noDR_keyDays2")
runDates14 = "2019-10-08"

runIDs15 = c("rand_u20pp","rand_u40pp","rand_u60pp","rand_u80pp")
runDates15 = rep("2019-10-14",4)

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

xx = list.files(path = "/home/groups/weyant/plevi_outputs/", pattern = glob2rx("*_o25*keyDays2*"))
last_loc = as.vector(regexpr("\\_[^\\_]*$", xx))
runIDs22 = substr(xx, 1, last_loc - 1)
runDates22 = substr(xx, last_loc+1,100)

# runIDs24 = "advNot1_o25_c2_keyDays2"
# runDates24 = "2019-99-99" # THIS IS A TEST WITH A SMALL PROD
yy = list.files(path = "/home/groups/weyant/plevi_outputs/", pattern = glob2rx("rand_o25_u*"))
last_loc = as.vector(regexpr("\\_[^\\_]*$", yy))
runIDs26 = substr(yy, 1, last_loc - 1)
runDates26 = substr(yy, last_loc+1,100)

runIDs = c(runIDs22, runIDs26)#, runIDs12)#c(runIDs15,runIDs14,runIDs12)#c(runIDs1,runIDs2)
runDates = c(runDates22, runDates26)#,runDates12)#c(runDates15,runDates14,runDates12)#c(runDates1, runDates2)


# iterate through all summary files and combine them ####
combineSummaryFiles = function(runIDs, runDates, SHRLK = TRUE, SCRATCH = "/scratch/users/pjlevi/julia_outputs/INFORMS/"){
  library(plyr) #for rbind.fill
  if(SHRLK){
    base_fol = "/home/users/pjlevi/dr_stoch_uc/julia_ver/"
    output_fol_base  = "/home/groups/weyant/plevi_outputs/"
    input_fol = "inputs/"
    inputs_file = paste0(base_fol,"/code/inputs_ercot.csv")
  } else{
    base_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
    output_fol_base = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/forIAEE_1Pmin/"
    input_fol = "Data/julia_input/"
    inputs_file = paste0(base_fol,"/Julia_UC_Github/Julia_scripts/inputs_ercot.csv")
  }
  
  # allinputs = read_csv(inputs_file)
  
  # load summary_stats created by visualize_DR_use/combineRunResults for each run
  # combine this with inputs from the model-generated copy of inputs
  # put all runs in the same dataframe
  for(i in 1:length(runIDs)){
    print(runIDs[i])
    # load summary file
    summaryFilePath = paste0(runIDs[i],"_",runDates[i],"/summary_stats",runIDs[i],".csv")
    if(file.exists(paste0(output_fol_base,summaryFilePath))){
      summaryfile = read_csv(paste0(output_fol_base,summaryFilePath))
      HOME = TRUE
    } else if(file.exists(paste0(SCRATCH,summaryFilePath))){ #does summary file and output folder live in SCRATCH?
      summaryfile = read_csv(paste0(SCRATCH,summaryFilePath))
      HOME = FALSE
    } else if(file.exists(paste0(SCRATCH,runIDs[i],"_",runDates[i]))){
      stop(paste0("File ",runIDs[i],"_",runDates[i]," exists in SCRATCH, but there is no summary file"))
    } else {
      stop(paste0("File ",runIDs[i],"_",runDates[i]," either lives in HOME but doesn't have a summary file, or doesn't exist"))
    }
    
    # clean summary file
    # params = spread(inputs, key = input_name, value = names(inputs)[2])
    summary2 = spread(summaryfile,key = output_type, value = output_value) # should make a 1-row dataframe
    
    # combine with inputs
    # params = getModelParam(run_date = runDates[i],run_name = runIDs[i], output_fol_base)  # should return params in format: ?
    # this returns a dataframe that has one row.

    # combine params, summary file, append to existing outputs
    if(i==1){
      # create outputs matrix
      # alloutputs = cbind(runIDs[i],params,summary2)
      # names(alloutputs) = c("runID",names(params),names(summary2))
      alloutputs = cbind(runIDs[i],summary2)
      names(alloutputs) = c("runID",names(summary2))
    } else {
      # bind a new row
      # newoutputs = cbind(runIDs[i],params,summary2) 
      # names(newoutputs) = c("runID",names(params),names(summary2))
      newoutputs = cbind(runIDs[i],summary2) 
      names(newoutputs) = c("runID",names(summary2))
      alloutputs = rbind.fill(alloutputs,newoutputs)#bind_rows(alloutputs,newoutputs)
       
    }
    
  } #end iteration over runs
  
  ## calculate new outputs if noDR run present##
    # make TYPE column for easier plotting. grep the first number or _ in runID
    alloutputs$type = substr(alloutputs$runID,1,regexpr("[0-9_]",alloutputs$runID)-1)
    # expected MWh shed
    alloutputs$`Expected MWh Shed by DR` = as.numeric(alloutputs$`expected DR prod costs`)/as.numeric(alloutputs$`dr_varcost`)
    # expected hours DR is on
    alloutputs$`Expected hours DR is committed` = as.numeric(alloutputs$`Hours DR is on`)/as.numeric(alloutputs$nrandp)
  
  ## For outputs that require reference to noDR
    # repair NA `expected Total costs` columns
    repairsel = which(is.na(alloutputs$`expected Total costs`))
    alloutputs$`expected Total costs` = rowSums(cbind(as.numeric(alloutputs$`all costs slow gens`), 
                                            as.numeric(alloutputs$`expected fast all costs`), 
                                            as.numeric(alloutputs$`expected DR all costs`)), na.rm = T)
    
    ## repair any that have o25 but nrandp is 5
    # fixsel = which(alloutputs$nrandp == 5 & str_detect(alloutputs$runID,pattern="_o25_"))
    # alloutputs$`expected Total costs`[fixsel] = alloutputs$`expected Total costs`[fixsel]/5
    
    
    # identify noDR row
    noDR_row = which(str_detect(alloutputs$runID,"noDR")) # if there are multiple noDR rows, use first one
    if(length(noDR_row) > 0){ 
      noDR_row = noDR_row[1]
      # total cost savings rel to noDR, absolute and as pct
      alloutputs$`expected Total costs` = as.numeric(alloutputs$`expected Total costs`)
      alloutputs$`Expected cost reduction from DR` = alloutputs$`expected Total costs`[noDR_row] - alloutputs$`expected Total costs`
      alloutputs$`Expected cost reduction from DR, frac` = 
        (alloutputs$`expected Total costs`[noDR_row] - alloutputs$`expected Total costs`)/alloutputs$`expected Total costs`[noDR_row]
      
      # cost reduction per MWh shed
        # mwh shed is DR prod cost / dr_varcost
      alloutputs$`Expected cost reduction per MWh shed` = alloutputs$`Expected cost reduction from DR` / alloutputs$`Expected MWh Shed by DR`
      
      # CO2 reduction rel to noDR
      alloutputs$`Total CO2 emissions` = as.numeric(alloutputs$`expected slow CO2 emissions`) + 
        as.numeric(alloutputs$`expected fast CO2 emissions`) + as.numeric(alloutputs$`expected DR CO2 emissions`)
      # alloutputs$`Total CO2 emissions`[fixsel] = alloutputs$`Total CO2 emissions`[fixsel]/5
      alloutputs$`Expected CO2 reduction from DR` = alloutputs$`Total CO2 emissions`[noDR_row] - alloutputs$`Total CO2 emissions`
    }
    
  # save
  write_csv(alloutputs,paste0(output_fol_base,"combined_summary.csv")) # regardless of where output files are, this will be saved in HOME directories
}