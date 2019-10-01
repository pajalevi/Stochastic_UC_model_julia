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
#       - print dates corresponding to each selection using as.Date() which is 0 based so hour/24 is an appropriate input
#       - possibly find max day for each category instead of max hours
# 4 - compare the keydays selected for old and new -- why?

# key_days = pickKeyDays(Gendat,output_fol = Output_fol, prod = prod)
pickKeyDays = function(gendat,output_fol,top_n = 3,
                       summer_demand_days = 5,winter_demand_days = 3, price_days = 10,
                       base_fol = baseFol, output_fol_base = outputFolBase, buffer = 12,prod=NULL){
  # inputs: prod and other outputs from a full-year run
  # outputs: days of year that should be included in smaller runs
  # loads prod, cmtcap, and demand
  
  #------------------------------------------#
  #### Find max days for various criteria ####
  
  # Load prod data
  filename = paste0(base_fol,output_fol_base,output_fol,"prod.csv")
  if(is.null(prod)){
    prod = read_csv(filename)
  }
  prod = prod[prod$t>buffer,]
  if(!("Capacity" %in% colnames(prod))){
    prod = prod %>%
      merge(gendat[,c("Capacity","PMin","plantUnique","VCost","Fuel","PLC2ERTA")], by.x = "GEN_IND", by.y = "plantUnique") 
  }
  
  # ID max up and down ramp hours for fast and all generators (4)
  # group by day
  prod$date = as.Date(floor(prod$t/24),origin="2016-01-01")
  # separate RE from 'fast' generators
  REsel = str_detect(prod$GEN_IND,"WIND") | str_detect(prod$GEN_IND,"SOLAR")
  prod$speed[REsel] = "non-dispatchable"
  prodramp_hour = prod %>%
    group_by(scenario,speed,t) %>%
    summarise(MWtot = sum(MWout)) %>%
    group_by(scenario,speed) %>%
    mutate(ramp = MWtot - lag(MWtot, default=0))
  first_t = min(prodramp_hour$t)
  prodramp_hour$ramp[prodramp_hour$t == first_t] = 0 #make sure first timestep doesnt have a huge ramp

  prodramp_hour$date = as.Date(floor(prodramp_hour$t/24),origin="2016-01-01")
  
  prodramp = prodramp_hour %>%
    group_by(scenario,date,speed) %>%
    summarise(maxramp = max(ramp),
              minramp = min(ramp))
  
  prodslowsel = which(prodramp$speed == "slow") # hydro is considered slow, which isn't... quite right.
  prodfastsel = which(prodramp$speed == "fast") # This includes RE, so fast ramps could just be a reflection of RE ramps...
  
  # slow generators
  # downward ramp
  topInd = which(rank(prodramp$minramp[prodslowsel]) <= top_n & prodramp$minramp[prodslowsel] < -1e-6)
  topT = prodramp$date[prodslowsel][topInd]
  # n = length(unique(floor(topT/24)))
  n = length(unique(topT))
  print(paste("there are",n,"unique days that have the top",top_n,"slow downward ramps"))
  print(sort(topT))
  all_t = unique(topT)
  # upward ramp
  topInd = which(rank(prodramp$maxramp[prodslowsel]) > nrow(prodramp[prodslowsel,])-top_n)
  topT = prodramp$date[prodslowsel][topInd]
  # n = length(unique(floor(topT/24)))
  n = length(unique(topT))
  print(paste("there are",n,"unique days that have the top",top_n,"slow upward ramps"))
  print(sort(topT))
  all_t = c(all_t, unique(topT))
  
  # fast generators
  # downward ramp
  topInd = which(rank(prodramp$minramp[prodfastsel]) <= top_n & prodramp$minramp[prodfastsel] < -1e-6)
  topT = prodramp$date[prodfastsel[topInd]]
  # n = length(unique(floor(topT/24)))
   n=length(unique(topT))
  print(paste("there are",n,"unique days that have the top",top_n,"fast downward ramps"))
  print(sort(topT))
  all_t = c(all_t, unique(topT))
  # upward ramp
  topInd = which(rank(prodramp$maxramp[prodfastsel]) > nrow(prodramp[prodfastsel,])-top_n)
  topT = prodramp$date[prodfastsel][topInd]
  # n = length(unique(floor(topT/24)))
   n=length(unique(topT))
  print(paste("there are",n,"unique days that have the top",top_n,"fast upward ramps"))
  print(sort(topT))
  all_t = c(all_t, unique(topT))
  
  # price data
  # NB: assumes that gendat is included in prod
  prod_margprice = prod %>%
    filter(MWout > 2) %>%
    group_by(date,scenario) %>%
    summarise(margprice = max(VCost),
              marggen = GEN_IND[which.max(VCost)],
              marggencap = Capacity[which.max(VCost)],
              marggenspd = speed[which.max(VCost)])
  
  # ID max marginal price hours (1)
  topInd = which(rank(prod_margprice$margprice) > nrow(prod_margprice)-price_days)
  topT = prod_margprice$date[topInd]
  # n = length(unique(floor(topT/24)))
   n=length(unique(topT))
  print(paste("there are",n,"unique days that have the top",price_days,"marginal generator price hours"))
  print(sort(topT))
  prod_margprice[topInd,]
  all_t = c(all_t, unique(topT))
  
  # ID max spread in marginal price hours (1) 
  prod_pricediff = prod %>%
    filter(MWout > 2) %>%
    group_by(t,scenario,date) %>%
    summarise(margprice = max(VCost),
              marggen = GEN_IND[which.max(VCost)],
              marggencap = Capacity[which.max(VCost)],
              marggenspd = speed[which.max(VCost)]) %>%
    group_by(date) %>%
    summarise(maxspread = max(margprice)-min(margprice))
  
  # prod_pricediff = prod_margprice %>%
  #   group_by(t) %>%
  #   summarise(maxspread = max(margprice) - min(margprice))
  topInd = which(rank(prod_pricediff$maxspread) > nrow(prod_pricediff)-top_n)
  topT = prod_pricediff$date[topInd]
  # n = length(unique(floor(topT/24)))
   n=length(unique(topT))
  print(paste("there are",n,"unique days that have the top",top_n,"marginal generator price spread days"))
  print(sort(topT))
  prod_pricediff[topInd,]
  all_t = c(all_t, unique(topT))
  
  # Load commitment data
  cmtcap = read_csv(file = paste0(base_fol,output_fol_base,output_fol,"slow_committed_capactity.csv"))
  cmtcap$date = as.Date(floor(cmtcap$t/24),origin="2016-01-01")
  cmtcap = cmtcap %>%
    group_by(date) %>%
    summarise(maxcmtcap = max(allcmtcap))
  # ID max slow committed capacity (1)
  topInd = which(rank(cmtcap$maxcmtcap) > nrow(cmtcap)-top_n)
  topT = cmtcap$date[topInd]
  # n = length(unique(floor(topT/24)))
  n=length(unique(topT))
  print(paste("there are",n,"unique days that have the top",top_n,"slow committed capacity days"))
  print(sort(topT))
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
  summer_sel = which(demand_daymax$month >= 5 & demand_daymax$month < 10)
  winter_sel = which(demand_daymax$month < 4 | demand_daymax$month >= 10)
  #summer max
  topInd = which(rank(demand_daymax$maxdemand[summer_sel]) >= nrow(demand_daymax[summer_sel,])-summer_demand_days+1)
  topT = demand_daymax$day[summer_sel][topInd]
  n = length(unique(topT))
  print(paste("there are",n,"unique days that have the top",summer_demand_days,"summer demand days"))
  print(sort(topT))
  demand_daymax[summer_sel[topInd],]
  all_t = c(all_t, topT) 
  
  #winter max
  topInd = which(rank(demand_daymax$maxdemand[winter_sel]) >= nrow(demand_daymax[winter_sel,])-winter_demand_days+1)
  topT = demand_daymax$day[winter_sel][topInd]
  n = length(unique(topT))
  print(paste("there are",n,"unique days that have the top",winter_demand_days,"non-summer demand days"))
  print(sort(topT))
  demand_daymax[winter_sel[topInd],]
  all_t = c(all_t, topT) 
  
  
  #--------------------------------------------#
  #### Find unique days across all max days ####
  
  all_t = unique(all_t)
  print(paste("there are",length(all_t),"unique days identified"))
  # keydays = unique(floor(all_t/24))
  # print(paste("there are",length(keydays),"unique days identified"))
  
  return(all_t)
}

# periods = writeKeyDays(keydays = key_days,runLength = 5,overlap_length = 6,input_fol = inputFol, base_fol = baseFol)
writeKeyDays = function(keydays,runLength,overlap_length,run_type = "keyDays2",buffer = 12,input_fol, base_fol){
  # inputs: key days, length of run, overlap of run, buffer around key day
  # outputs: na
  # Action: writes period_X_XXXX_XXXX.csv files to appropriate folder. 
  #       and makes that folder if necessary
  
  inputfolID = paste0(runLength,"d_",overlap_length,"o_",run_type)
  instance_in_fol = paste0(base_fol,input_fol,inputfolID,"/")
  
  all_t = sort((yday(keydays) * 24))
  
  # create holders
  firsthr = rep(0,length(all_t))
  lasthr = rep(0,length(all_t))
  lastperiodhr = rep(0,length(all_t))
  
  # iterate to ID potential periods and must-run times
  for(i in 1:length(all_t)){
    firsthr[i] = all_t[i]-buffer # start run 12h before key day
    lasthr[i] = all_t[i]+24 + buffer # end run requirement 12h after key day
    
    lastperiodhr[i] = all_t[i] + runLength*24 - buffer # runs are 5 d long
  }
  # make dir
  if(!file.exists(instance_in_fol)){
    dir.create(instance_in_fol)
  }
  
  # write unique periods
  lastwrittenperiod = 0
  periodnum = 1
  all_hours = NULL
  for(i in 1:length(all_t)){
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
        last = min(lastperiodhr[i],lastwrittenperiod-overlap_length + runLength*24)
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
    all_hours = c(all_hours,periods)
  }
  return(all_hours)
}

# plot demand with included hours
# plot_periods(periods,demand,baseFol,inputFol,"5d_6o_keyDays2")
plot_periods = function(all_hours,demand,base_fol,input_fol,folder_name){
  if(unique(diff(demand$date)) != 1){
    error("demand is not sequentially ordered")
  }
  demand$t = 1:(nrow(demand))
  demand$included = 0
  index = demand$t %in% all_hours
  demand$included[index] = 1
  ggplot(demand,aes(x=date,y=demand,color = included)) + geom_line() +
    guides(color = "none") +
    labs(title = paste("2016 Demand and",folder_name,"modeled periods (light blue)"), y = "Demand (MW)", x = "Date") +
    ggsave(filename = paste0(base_fol,input_fol,folder_name,"/demand.png"),width = 12,height=8)
}

# gen_arma() function
source(paste0(base_fol,"Julia_UC_Github/R_Scripts/gen_arma.R"))

# writeARMAInputs(period_dir = paste0(baseFol,inputFol,"5d_6d_keyDays2/"),n_omegas=10)
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