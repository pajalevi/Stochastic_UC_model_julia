# write_key_days.R
# March 2019
# Patricia Levi
# takes in key days (found in combine_run_results, but should be copied here)
# and creates period input information that covers all the key days
# with some buffer.

# Also has capability to generate period files covering a set of key days
# with a variable period length and period overlap
# and to generate the prob and vdem files needed for demand uncertainty
# (see bottom) -- added July 2019
library(tidyverse)
library(lubridate)

base_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
output_fol_base = "Data/julia_output/"
input_fol = "Data/julia_input/"
inputfolID = "5d_6o_keyDays2"
instance_in_fol = paste0(base_fol,input_fol,inputfolID,"/")

#### stuff to run alongside combine_run_results.R ####
# find top X hours for min ramp rate across scenarios ####
X = 70
# prodramp could be with or without DR. Should be without DR if done after running the full script

# TODO: prodramp has a $speed column - need to find max within each speed, not across speeds!

prodslowsel = which(prodramp$speed == "slow")
top10ind = which(rank(prodramp$ramp[prodslowsel]) <= X & prodramp$ramp[prodslowsel] < -1e-6)
top10t = prodramp$t[prodslowsel[top10ind]]
top10t
unique(floor(top10t/24))
length(unique(floor(top10t/24)))
prodramp$ramp[prodslowsel[top10ind]]
write.csv(top10t,file=paste0(output_fol,"top",X,"_minramp_hours.csv"))
write.csv(unique(floor(top10t/24)),paste0(output_fol,"top",X,"_slow_minramp_days.csv"))

X=40
prodfastsel = which(prodramp$speed == "fast")
top10ind = which(rank(prodramp$ramp[prodfastsel]) <= X & prodramp$ramp[prodfastsel] < -1e-6)
top10t = prodramp$t[prodfastsel[top10ind]]
top10t
unique(floor(top10t/24))
length(unique(floor(top10t/24)))
prodramp$ramp[prodfastsel[top10ind]]
# write.csv(top10t,file=paste0(output_fol,"top",X,"_minramp_hours.csv"))
write.csv(unique(floor(top10t/24)),paste0(output_fol,"top",X,"_fast_minramp_days.csv"))

# find top 10 hours for max ramp rate across scenarios ####
# excluding start of sim
X = 60
# prodramp could be with or without DR.Should be without DR if done after running the full script
# TODO: prodramp has a $speed column - need to find max within each speed, not across speeds!
# TODO: find max ramp for whole system AND for fast generators
# slow
nostart = prodramp$t[prodslowsel]>6
top10ind = which(rank(prodramp$ramp[prodslowsel[nostart]]) >= nrow(prodramp[prodslowsel[nostart],])-X)
top10t = prodramp$t[prodslowsel[nostart]][top10ind]
prodramp$ramp[prodslowsel[nostart]][top10ind]
top10t
unique(floor(top10t/24))
length(unique(floor(top10t/24)))

write.csv(top10t,paste0(output_fol,"top",X,"_slow_maxramp_hours.csv"))
write.csv(unique(floor(top10t/24)),paste0(output_fol,"top",X,"_slow_maxramp_days.csv"))

# write.csv(unique(floor(top10t/24)),paste0(output_fol,"top",X,"_slowcmt_days.csv"))
# fast
X=20
nostart = prodramp$t[prodfastsel]>6
top10ind = which(rank(prodramp$ramp[prodfastsel[nostart]]) >= nrow(prodramp[prodfastsel[nostart],])-X)
top10t = prodramp$t[prodfastsel[nostart]][top10ind]
prodramp$ramp[prodfastsel[nostart]][top10ind]
top10t
unique(floor(top10t/24))
length(unique(floor(top10t/24)))

write.csv(top10t,paste0(output_fol,"top",X,"_fast_maxramp_hours.csv"))
write.csv(unique(floor(top10t/24)),paste0(output_fol,"top",X,"_fast_maxramp_days.csv"))

# find top 10 hours for marginal cost across scenarios ####
X =70
top10ind = which(rank(prod_margprice$margprice) >= nrow(prod_margprice)-X)
top10t = prod_margprice$t[top10ind]
unique(floor(top10t/24))
length(unique(floor(top10t/24)))
prod_margprice$margprice[top10ind]
top10t

write.csv(top10t,paste0(output_fol,"top",X,"_maxmarginalprice_hours.csv"))
write.csv(unique(floor(top10t/24)),paste0(output_fol,"top",X,"_maxmarginalprice_days.csv"))

# find top X hours for max first stage capacity ####
X = 60
top10ind = which(rank(cmtcap$allcmtcap) >= nrow(cmtcap)-X)
top10t = cmtcap$t[top10ind]
cmtcap$allcmtcap[top10ind]
top10t
unique(floor(top10t/24))
length(unique(floor(top10t/24)))

write.csv(top10t,paste0(output_fol,"top",X,"_slowcmt_hours.csv"))
write.csv(unique(floor(top10t/24)),paste0(output_fol,"top",X,"_slowcmt_days.csv"))

# find top x hours for max demand ####
X = 40
demand = read_csv(paste0(input_fol,"ercot_default/ercot_demand_2016.csv"))
top10ind = which(rank(demand$demand) >= nrow(demand)-X)
demand$date = mdy_hm(demand$time_CST)
unique(date(demand$date[top10ind]))
length(unique(date(demand$date[top10ind])))
top10t = unique(yday(date(demand$date[top10ind])))

write.csv(top10t,paste0(base_fol,"Data/unformatted data/","top",X,"_demand_hours.csv"))


#### After combining results from above ####
# keydays = read.csv(paste0(instance_in_fol,"keydays2.csv"),header=F)[[1]]
keydays = top10t
keydays = keydays * 24

# settings
buffer = 12 # number of hours
runlength = 7 #number of days
overlap_length = 6
inputfolID = paste0(runlength,"d_",overlap_length,"o_testDays")
instance_in_fol = paste0(base_fol,input_fol,inputfolID,"/")

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

## write vdem and prob for newly created files ####
# Taken from arma_scenario_gen.R
# settings:
n_omegas = 25
p=26
q=0

notloaded = F
if(notloaded){
  # gen_arma() function
  source(paste0(base_fol,"Julia_UC_Github/R_Scripts/gen_arma.R"))

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
