# make_acf_scenarios.R
# generate error scenarios 

# NB: gen_arma.R has the more up-to-date version of gen_arma


errsel16 = !is.na(dercot[,"errorfrac"]) & dercot$year==2016
errf = dercot[errsel16,"errorfrac"]
acf(errf, lag.max = 200)
pacf(errf)

acf(diff(errf), lag.max = 50)
pacf(diff(errf))

# try an AR model
xar2 = ar(errf,order.max = 30)
arcoefs = xar2$ar
xar2$order
xarsim = arima.sim(model = list(ar=arcoefs, order=c(29,0,0)), n=100, sd = sd(errf))

xarsim = arima.sim(model = list(ar=arcoefs, order=c(29,0,0)), n=100, sd = sd(errf),
                   start.innov = errf[8412:8712])

plot(c(errf[8412:8712], xarsim), type = "l")

# try an ARMA model
p=26 #AR
q=0 #MA
xarma = arima(errf, order = c(p,0,q))

# xarma.ar = coef(xarma)

if(q>0){
  macoef <- xarma$coef[(p+1):(p+q)]
  xarmasim = arima.sim(model = list(ar = xarma$coef[1:p], ma = macoef),sd = sd(errf), n=1000)
} else{ 
  xarmasim = arima.sim(model = list(ar = xarma$coef[1:p]),sd = sd(errf), n=1000)
}
acf(errf, lag.max=40)
acf(xarmasim, lag.max=40)
pacf(errf)
pacf(xarmasim)

summary(errf)
summary(xarmasim)
summary(errf) - summary(xarmasim)
correction = sd(xarmasim)/sd(errf)
summary(errf) - summary(xarmasim/correction)
correction = diff(range(xarmasim))/diff(range(errf))
summary(errf) - summary(xarmasim/correction)
correction = diff(quantile(xarmasim,c(0.25,0.75)))/diff(quantile(errf,c(0.25,0.75)))
summary(errf) - summary(xarmasim/correction)
# summary(xarmasim/(correction))

plot(c(errf[4000:4400], xarmasim/correction), type = "l") 
abline(v=400, col="red")


# TODO: Use sd-based correction
# TODO: make a function that will generate p=25 realizations
#       given the period file, and save it appropriately.
#       Also save the probability file, which weights all realizations equally = 1/p = 1/25 = 0.04
#       possibly use preivous time period as the start.innov input

gen_arma = function(period_file,period_fol,p,q,err_data,nsim, xarma=NULL){
  # identify length of period
  periodinfo = strsplit(period_fol,"_|\\.|-|p")[[1]]
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
  } else {print("using provided model")}
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
    xarmasim = 1 + xarmasim/correction
    
    # save simulation in holder
    vdem[,i]=xarmasim
  }
  # save vdem
  write.csv(vdem, paste0(period_file,"demandScenarios_vdem_ARMA",p,".",q,"_p",periodnum,"_",firstperiod,"_",lastperiod,".csv"),
            row.names = F)
  
  # save prob
  prob = matrix(1/nsim,nrow=nsim, ncol=1)
  write.csv(prob, paste0(period_file,"demandScenarios_prob_ARMA",p,".",q,"_p",periodnum,"_",firstperiod,"_",lastperiod,".csv"),
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
  gen_arma(period_file = period_dir,period_fol = all_files[i],p=26,q=0,errf,25,xarma)
}

# call gen_arma for each


# If I wanted to base probability on the prob of start.innov:
# adapt arima.sim to return more information
# my.arima.sim = function (model, n, rand.gen = rnorm, innov = rand.gen(n, ...), 
#           n.start = NA, start.innov = rand.gen(n.start, ...), ...) 
# {
#   if (!is.list(model)) 
#     stop("'model' must be list")
#   if (n <= 0L) 
#     stop("'n' must be strictly positive")
#   p <- length(model$ar)
#   if (p) {
#     minroots <- min(Mod(polyroot(c(1, -model$ar))))
#     if (minroots <= 1) 
#       stop("'ar' part of model is not stationary")
#   }
#   q <- length(model$ma)
#   if (is.na(n.start)) 
#     n.start <- p + q + ifelse(p > 0, ceiling(6/log(minroots)), 
#                               0)
#   if (n.start < p + q) 
#     stop("burn-in 'n.start' must be as long as 'ar + ma'")
#   d <- 0
#   if (!is.null(ord <- model$order)) {
#     if (length(ord) != 3L) 
#       stop("'model$order' must be of length 3")
#     if (p != ord[1L]) 
#       stop("inconsistent specification of 'ar' order")
#     if (q != ord[3L]) 
#       stop("inconsistent specification of 'ma' order")
#     d <- ord[2L]
#     if (d != round(d) || d < 0) 
#       stop("number of differences must be a positive integer")
#   }
#   if (!missing(start.innov) && length(start.innov) < n.start) 
#     stop(sprintf(ngettext(n.start, "'start.innov' is too short: need %d point", 
#                           "'start.innov' is too short: need %d points"), n.start), 
#          domain = NA)
#   x <- ts(c(start.innov[seq_len(n.start)], innov[1L:n]), start = 1 - 
#             n.start)
#   if (length(model$ma)) {
#     x <- filter(x, c(1, model$ma), sides = 1L)
#     x[seq_along(model$ma)] <- 0
#   }
#   if (length(model$ar)) 
#     x <- filter(x, model$ar, method = "recursive")
#   if (n.start > 0) 
#     x <- x[-(seq_len(n.start))]
#   if (d > 0) 
#     x <- diffinv(x, differences = d)
#   as.ts(x)
#   #TODO: edit to return n.start, start.innov, and ~ probability of start.innov
#   
#   # to calculate approx probability of start.innov:
#   # (1) for each point, calculate probability of selecting a point within 
#   #     a range d (say +/- 0.01) around that point
#   # (2) multiply all those probabilities together
#   
#   # NB that for my uses, I probabably want to discard the first X results
#   # as part of a burn-in period.
#   
#   
#   # RETURN A LIST OF ALL THESE THINGS
# }
