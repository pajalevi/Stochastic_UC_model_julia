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
    # nsim: how many simulations to use for making xarma
  
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

