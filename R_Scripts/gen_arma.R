# gen_arma.R
# creates the _vdem_ and _prob_ files to go with
# a set of pre-existing period files
# (e.g. 5d_6d_testDays/periods_X_xxxx_xxxx.csv)
# taken & improved upon from arma_scenario_gen.R

gen_arma = function(period_dir,period_file,xarma=NULL,p,q,err_data,nsim){
  # period_dir: location/pathp of periods_X_xxxx_xxxx.csv files
  # period_file: name of period file to generate _vdem_ and _prob_ files for
  # xarma: an existing ARMA model
  # err_data: original data
  # if xamra not given, provide --
    # p: order of AR model
    # q: order of MA model
  # nsim: how many simulations to create
  
  # identify length of period
  periodinfo = strsplit(period_file,"_|\\.|-|p")[[1]]
  lenInfo = length(periodinfo)
  firstperiod = as.numeric(periodinfo[lenInfo - 2])
  lastperiod = as.numeric(periodinfo[lenInfo - 1])
  periodnum = as.numeric(periodinfo[lenInfo - 3])
  
  nhrs = lastperiod - firstperiod + 1
  
  # create model
  if(is.null(xarma)){
    print("creating model")
    xarma = arima(err_data, order = c(p,0,q))
    print("done creating model")
  } else {
    print("using provided model")
    p = xarma$arma[1]
    q = xarma$arma[2]
  }
  # create empty holder for simulations
  vdem = matrix(NA, nrow = nhrs, ncol = nsim)
  
  # generate simulations
  for(i in 1:nsim){
    if(q>0 & p>0){
      macoef <- xarma$coef[(p+1):(p+q)]
      xarmasim = arima.sim(model = list(ar = xarma$coef[1:p], ma = macoef),sd = sd(err_data), n=nhrs)
    } else if(q==0){ 
      xarmasim = arima.sim(model = list(ar = xarma$coef[1:p]),sd = sd(err_data), n=nhrs)
    } else if(p==0){
      xarmasim = arima.sim(model = list(ma = xarma$coef[1:q]),sd = sd(err_data), n=nhrs)
    }
    
    correction = sd(xarmasim)/sd(err_data)
    xarmasim = xarmasim/correction 
    
    # save simulation in holder
    vdem[,i]=xarmasim+1 # add one so that it can be multiplied by demand
  }
  # save vdem
  write.csv(vdem, paste0(period_dir,"demandScenarios_vdem_ARMA",p,".",q,"_o",nsim,"_p",periodnum,"_",firstperiod,"_",lastperiod,".csv"),
            row.names = F)
  
  # save prob
  prob = matrix(1/nsim,nrow=nsim, ncol=1)
  write.csv(prob, paste0(period_dir,"demandScenarios_prob_ARMA",p,".",q,"_o",nsim,"_p",periodnum,"_",firstperiod,"_",lastperiod,".csv"),
            row.names=F)
  
}

# load period files
period_dir = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/5d_6o_keyDays2/"
# period_dir = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/5d_6o_periods/"
all_files = list.files(path=period_dir, pattern = "periods")
# 

####do this if youre strating from scratch
library(tidyverse)
library(lubridate)
data_directory = "/Users/patricia/Documents/Google Drive/stanford/SEED/"
all_dat = read_csv(paste0(data_directory,"EBA_BA_level.csv"))
ercot = select(all_dat, X1,contains("ERCO-ALL")) #ERCOT

# select date column 
# this loads as a 'date-time' object aka POSIXct
# manipulable with functions from package 'lubridate'
date = select(all_dat, contains("X1")) 
date$year = year(date$X1)
date$month = month(date$X1)
date$hour = hour(date$X1)
date$yday = yday(date$X1)
date$wday = wday(date$X1, label=TRUE)

rm(all_dat)
ercot$error = ercot$`EBA.ERCO-ALL.DF.H` - ercot$`EBA.ERCO-ALL.D.H`

dercot = cbind(ercot, date[,2:6])
# make col for season
dercot$season = "1_winter"
dercot$season[dercot$month >= 3 & dercot$month <= 5] = "2_spring"
dercot$season[dercot$month >= 6 & dercot$month <= 8] = "3_summer"
dercot$season[dercot$month >= 9 & dercot$month <= 11] = "4_fall"

dercot$errorfrac = (dercot$`EBA.ERCO-ALL.DF.H` - dercot$`EBA.ERCO-ALL.D.H`)/dercot$`EBA.ERCO-ALL.D.H`

errsel16 = !is.na(dercot[,"errorfrac"]) & dercot$year==2016
errf = dercot[errsel16,"errorfrac"]

# create model
p=26
q=0
xarma = arima(errf, order = c(p,0,q))
for(i in 1:length(all_files)){
  print(all_files[i])
  gen_arma(period_dir,period_file = all_files[i],xarma,err_data = errf,nsim=20)
}

# visualize scenarios:
xx = read_csv(paste0(period_dir,"demandScenarios_vdem_ARMA26.0_o25_p9_5142_5244.csv"))
xx$t = 1:nrow(xx)
xx = gather(xx,key = "scenario",value = "error",V1:V25)
ggplot(xx,aes(x=t,y=error,color=scenario)) + geom_line() + facet_wrap(~scenario)
# means
mean(xx$error)
sd(xx$error)

colSums(xxin)/nrow(xxin)



