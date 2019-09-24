# make_key_days.R
# improvement on write_key_days.R
# now that combineRunResults is a function
# that I run on sherlock
# Sept 2019

# *** Need to save -- prod, cmtcap

library(tidyverse)
library(lubridate)

baseFol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
outputFolBase = "Data/julia_output/"
inputFol = "Data/julia_input/"

# run_fol = "forIAEE_1Pmin/base_noDRfullyear2_2019-09-12/"
# gens = read_csv(paste0(base_fol,input_fol,"ercot_default/complete_generator_correctedGas_3-12-19_alldata.csv"))


# Selection basis for key days:
# max up and down ramp for slow and all generators (4)
# max marginal price (1)
# spread in marginal price (1) 
# max slow committed capacity (1)
# max demand hours (non-stochastic demand) (1)

# steps for myself here --
#X 0 - consider if the types of keydays I picked are the best ones
#X 1 - code up in a replicable way what I did in write_key_days
#X 2 - run combineRunResults for new baserun, save objects noted above. 
# >>        also save for old baserun
#X 3 - download prod.csv etc for old baserun and new baserun, debug script
# 4 - compare the keydays selected for old and new -- why?

pickKeyDays = function(gendat,output_fol,top_n = 20,summer_demand_days = 5,winter_demand_days = 3,base_fol = baseFol, output_fol_base = outputFolBase){
  # inputs: prod and other outputs from a full-year run
  # outputs: days of year that should be included in smaller runs
  # loads prod, cmtcap, and demand
  
  #------------------------------------------#
  #### Find max days for various criteria ####
  
  # Load prod data
  filename = paste0(base_fol,output_fol_base,output_fol,"prod.csv")
  prod = read_csv(filename)
  prod = prod %>%
    merge(gendat[,c("Capacity","PMin","plantUnique","VCost","Fuel","PLC2ERTA")], by.x = "GEN_IND", by.y = "plantUnique") 
  
  # ID max up and down ramp hours for fast and all generators (4)
  prodramp = prod %>%
    group_by(scenario,speed,t) %>%
    summarise(MWtot = sum(MWout)) %>%
    group_by(scenario,speed) %>%
    mutate(ramp = MWtot - lag(MWtot, default=0))
  prodslowsel = which(prodramp$speed == "slow")
  prodfastsel = which(prodramp$speed == "fast")
  
  # all generators
  # downward ramp
  topInd = which(rank(prodramp$ramp) <= top_n & prodramp$ramp < -1e-6)
  topT = prodramp$t[topInd]
  n = length(unique(floor(topT/24)))
  print(paste("there are",n,"unique days that have the top",top_n,"downward ramps"))
  all_t = unique(topT)
  # upward ramp
  topInd = which(rank(prodramp$ramp) >= nrow(prodramp)-top_n)
  topT = prodramp$t[topInd]
  n = length(unique(floor(topT/24)))
  print(paste("there are",n,"unique days that have the top",top_n,"upward ramps"))
  all_t = c(all_t, unique(topT))
  
  # fast generators
  # downward ramp
  topInd = which(rank(prodramp$ramp[prodfastsel]) <= top_n & prodramp$ramp[prodfastsel] < -1e-6)
  topT = prodramp$t[prodfastsel[topInd]]
  n = length(unique(floor(topT/24)))
  print(paste("there are",n,"unique days that have the top",top_n,"fast downward ramps"))
  all_t = c(all_t, unique(topT))
  # upward ramp
  topInd = which(rank(prodramp$ramp[prodslowsel]) >= nrow(prodramp[prodslowsel,])-top_n)
  topT = prodramp$t[prodslowsel][topInd]
  n = length(unique(floor(topT/24)))
  print(paste("there are",n,"unique days that have the top",top_n,"fast upward ramps"))
  all_t = c(all_t, unique(topT))
  
  # price data
  # NB: assumes that gendat is included in prod
  prod_margprice = prod %>%
    filter(MWout > 2) %>%
    group_by(t,scenario) %>%
    summarise(margprice = max(VCost),
              marggen = GEN_IND[which.max(VCost)],
              marggencap = Capacity[which.max(VCost)],
              marggenspd = speed[which.max(VCost)])
  
  # ID max marginal price hours (1)
  topInd = which(rank(prod_margprice$margprice) >= nrow(prod_margprice)-top_n)
  topT = prod_margprice$t[topInd]
  n = length(unique(floor(topT/24)))
  print(paste("there are",n,"unique days that have the top",top_n,"marginal generator price hours"))
  all_t = c(all_t, unique(topT))
  
  # ID max spread in marginal price hours (1) 
  prod_pricediff = prod_margprice %>%
    group_by(t) %>%
    summarise(gendiff = max(margprice) - min(margprice))
  topInd = which(rank(prod_pricediff$gendiff) >= nrow(prod_pricediff)-top_n)
  topT = prod_pricediff$t[topInd]
  n = length(unique(floor(topT/24)))
  print(paste("there are",n,"unique days that have the top",top_n,"marginal generator price spread hours"))
  all_t = c(all_t, unique(topT))
  
    
  # Load commitment data
  cmtcap = read_csv(file = paste0(base_fol,output_fol_base,output_fol,"slow_committed_capactity.csv"))
  # ID max slow committed capacity (1)
  topInd = which(rank(cmtcap$allcmtcap) >= nrow(cmtcap)-top_n)
  topT = cmtcap$t[topInd]
  n = length(unique(floor(topT/24)))
  print(paste("there are",n,"unique days that have the top",top_n,"slow committed capacity hours"))
  all_t = c(all_t, topT)

  # Load demand data
  # ID max demand hours (non-stochastic demand) (1)
  demand = read_csv(paste0(input_fol,"ercot_default/ercot_demand_2016.csv"))
  demand$date = mdy_hm(demand$time_CST)
  demand$day = date(demand$date)
  demand$month = month(demand$date)
  demand_daymax = demand %>%
    group_by(day) %>%
    summarise(maxdemand = max(demand))
  demand_daymax$month = month(demand_daymax$day)
  summer_sel = which(demand_daymax$month >= 5 & demand_daymax$month < 9)
  winter_sel = which(demand_daymax$month < 4 | demand_daymax$month >= 9)
  #summer max
  topInd = which(rank(demand_daymax$maxdemand[summer_sel]) >= nrow(demand_daymax[summer_sel,])-summer_demand_days+1)
  topT = unique(yday(date(demand_daymax$day[summer_sel][topInd])))
  n = length(unique(topT))
  print(paste("there are",n,"unique days that have the top",summer_demand_days,"summer demand days"))
  all_t = c(all_t, topT*24-12) #convert day of year to the hour of the year of that day's noon.(ignoring DST)
  
  #winter max
  topInd = which(rank(demand_daymax$maxdemand[winter_sel]) >= nrow(demand_daymax[winter_sel,])-winter_demand_days+1)
  topT = unique(yday(date(demand_daymax$day[winter_sel][topInd])))
  n = length(unique(topT))
  print(paste("there are",n,"unique days that have the top",winter_demand_days,"non-summer demand days"))
  all_t = c(all_t, topT*24-12) #convert day of year to the hour of the year of that day's noon.(ignoring DST)
  
  
  #--------------------------------------------#
  #### Find unique days across all max days ####
  
  all_t = unique(all_t)
  print(paste("there are",length(all_t),"unique hours identified"))
  keydays = unique(floor(all_t/24))
  print(paste("there are",length(keydays),"unique days identified"))
  
  return(all_t)
}

writeKeyDays = function(all_t,runLength,overlapLength,run_type = "keyDays2",buffer = 12){
  # inputs: key days, length of run, overlap of run, buffer around key hours
  # outputs: na
  # Action: writes period_X_XXXX_XXXX.csv files to appropriate folder. 
  #       and makes that folder if necessary
  
  inputfolID = paste0(runlength,"d_",overlap_length,"o_",run_type)
  instance_in_fol = paste0(base_fol,input_fol,inputfolID,"/")
  
  keydays = unique(floor(all_t/24))
  
  # create holders
  firsthr = rep(0,length(keydays))
  lasthr = rep(0,length(keydays))
  lastperiodhr = rep(0,length(keydays))
  
  # iterate to ID potential periods and must-run times
  for(i in 1:length(keydays)){
    firsthr[i] = keydays[i]-buffer # start run 12h before key day
    lasthr[i] = keydays[i]+24 + buffer # end run requirement 12h after key day
    
    lastperiodhr[i] = keydays[i] + runlength*24 - buffer # runs are 5 d long
  }
  # make dir
  if(!file.exists(instance_in_fol)){
    dir.create(instance_in_fol)
  }
  
  # write unique periods
  lastwrittenperiod = 0
  periodnum = 1
  for(i in 1:length(keydays)){
    if(i==1){ 
      #write a new period
      periods = c(firsthr[i]:lastperiodhr[i])
      write.table(periods,col.names=F,sep=",",row.names=F, file = paste0(instance_in_fol,"periods_",periodnum,"_",firsthr[i],"_",lastperiodhr[i],".csv"))
      lastwrittenperiod = lastperiodhr[i]
      periodnum = periodnum +1
    } else if(lasthr[i] > lastwrittenperiod){
      #write a new period
      # is new period consecutive with previous period?
      if(firsthr[i] - lastwrittenperiod < 12 ){ # consecutive
        print(paste("period",periodnum,"is consecutive. lasttwrittenperiod is",lastwrittenperiod,"and firsthr is",firsthr[i]))
        last = min(lastperiodhr[i],lastwrittenperiod-overlap_length + runlength*24)
        first = lastwrittenperiod-overlap_length
        periods = c(first:last)
        write.table(periods,col.names=F,sep=",",row.names=F, file = paste0(instance_in_fol,"periods_",periodnum,"_",first,"_",last,".csv"))
        lastwrittenperiod = last
      } else { # not consecutive
        periods = c(firsthr[i]:lastperiodhr[i])
        write.table(periods,col.names=F,sep=",",row.names=F, file = paste0(instance_in_fol,"periods_",periodnum,"_",firsthr[i],"_",lastperiodhr[i],".csv"))
        lastwrittenperiod = lastperiodhr[i]
      }
      periodnum = periodnum +1
    } 
  }
}

# gen_arma() function
source(paste0(base_fol,"Julia_UC_Github/R_Scripts/gen_arma.R"))

writeARMAInputs = function(period_dir, n_omegas, p=26,q=0, xarma=NULL){
  # inputs: location of period files, number of scenarios, p and q for XARMA creation
  # outputs: na
  # action: writes vdem and prob for a set of period files in a folder
  
  if(is.null(xarma)){ # create xarma simulation
    # load and format demand data
    data_directory = "/Users/patricia/Documents/Google Drive/stanford/SEED/"
    all_dat = read_csv(paste0(data_directory,"EBA_BA_level.csv"))
    ercot = select(all_dat, X1,contains("ERCO-ALL")) #ERCOT
    date = select(all_dat, contains("X1")) 
    date$year = year(date$X1)
    date$month = month(date$X1)
    rm(all_dat)
    ercot$error = ercot$`EBA.ERCO-ALL.DF.H` - ercot$`EBA.ERCO-ALL.D.H`
    dercot = cbind(ercot, date)
    # calculate error as fraction of demand
    dercot$errorfrac = (dercot$`EBA.ERCO-ALL.DF.H` - dercot$`EBA.ERCO-ALL.D.H`)/dercot$`EBA.ERCO-ALL.D.H`
    errsel16 = !is.na(dercot[,"errorfrac"]) & dercot$year==2016
    errf = dercot[errsel16,"errorfrac"]
    
    # create model
    xarma = arima(errf, order = c(p,0,q))
  }
  
  # load period files
  period_dir = instance_in_fol
  all_files = list.files(path=period_dir, pattern = "periods")
  
  for(i in 1:length(all_files)){
    print(all_files[i])
    gen_arma(period_dir,all_files[i],xarma,p,q,errf,n_omegas)
  }
  
}