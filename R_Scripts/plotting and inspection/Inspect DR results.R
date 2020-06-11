# Inspect advNot3_c2 output
# Feb 2020


library(tidyverse)


baseFol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
outputFolBase = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/"
inputFol = "Data/julia_input/"

drdat = read_csv(file = paste0(outputFolBase,"advNot3_o25_c2_keyDays2_2019-12-12_dralldata.csv")) #from selected lines in consolidatedAnalysisFns.R - plotDRUse()
# drdat = read_csv(file = paste0(outputFolBase,"advNot3_o25_keyDays2_2019-12-12_dralldata.csv"))


# stats  on  dispatch
# filter  for commit >0

summary(drdat$value.prod[drdat$value.commit > 0])
prodtimes =  arrange(filter(drdat,value.commit>0),t)#, scenario == "o1"), t)
prodtimes$nperiod = as.factor(prodtimes$nperiod)
lengtht = nrow(prodtimes)
jumps = which(diff(prodtimes$t)>1)
# prodtimes$periodcolor = as.factor(as.numeric(prodtimes$nperiod) %% 6)
# prodtimes$periodshape = as.factor(floor(as.numeric(prodtimes$nperiod)/4))
prodtimes$periodshape = as.factor(as.numeric(prodtimes$nperiod) %% 6)


p = ggplot(prodtimes) + theme_minimal()

ggplot(prodtimes, aes(x = 1:nrow(prodtimes), y = sort(value.prod))) + geom_point() +
  theme_minimal()
prodtimeslt200 = prodtimes[prodtimes$value.prod<200,]
ggplot(prodtimeslt200, aes(x = 1:nrow(prodtimeslt200), y = sort(value.prod))) + geom_point() +
  theme_minimal()

p +
  geom_histogram(aes(x = value.prod),breaks = seq(0,1000,by=5)) +
  labs(x = "dispatch level (MW)", title = 
         "Distribution of dispatch level for advNot3_c2 \n(zeroes removed)") +
         # "Distribution of dispatch level for advNot3 \n(zeroes removed)") +
  ggsave(paste0(outputFolBase,"advNot3_o25_c2_keyDays2_2019-12-12_dr_dispatch_hist.png"),
  # ggsave(paste0(outputFolBase,"advNot3_o25_keyDays2_2019-12-12_dr_dispatch_hist.png"), 
         width = 5, height = 5)
# why is this so often dispatched at a low value? maybe compare to a generator with a similar marginal value?

p +
  geom_point(aes(y = value.prod, x =1:lengtht, color = nperiod,  shape = periodshape)) + 
  geom_vline(xintercept = jumps+0.5, color = "red", linetype="dotted")
# use scale_shape_manual(values=1:nlevels(df$sn)) +


### given $t, assign a unique event ID quickly, assuming t are sequential and only one of each
prodtimes$diff  = c(0,diff(prodtimes$t)) #assume t sorted
prodtimes$end = -1 #if  any -1 remain,  error. also logic is broken.
prodtimes$end[prodtimes$diff == 1] = 0
prodtimes$end[prodtimes$diff != 1] = 1
# prodtimes$end3  = !(c(0,diff(prodtimes$t)) == 1) #this is just the oneliner of  the previous three lines to see if  I could... but it is harder to read
prodtimes$eventnum  = cumsum(prodtimes$end)
# prodtimes$eventnum3  = cumsum(prodtimes$end3)


#################################
# Sherlock code to prep dralldata
# modified from plotDRUse()
# and visualize_DR_use.R
#################################
library(tidyverse)

## inputs ##

SHRLK  = TRUE
if(SHRLK){
  baseFol = "/home/users/pjlevi/dr_stoch_uc/julia_ver/"
  outputFolBase = "/home/groups/weyant/plevi_outputs/"
  inputFol = "inputs/"
  scriptsFol = "code/"
  source(paste0(baseFol,"code/R_Scripts/mergeTimeseriesData.R")) # contains loadTimeseriesData()
}
  
runID = "advNot3_o25_keyDays2"
runDate  = "2019-12-12"
inputfolID = "5d_6o_keyDays2"
outputfolID = paste0(runID,"_",runDate)
scenarios  = 1:10

## function ##

# load params
instance_in_fol = paste0(baseFol,inputFol,inputfolID,"/")
if(!dir.exists(instance_in_fol)){stop("julia input file doesnt exist ", instance_in_fol)}
output_fol = paste0(outputFolBase,outputfolID,"/")

if(!SHRLK){
  inputs_file = paste0(baseFol,"/Julia_UC_Github/Julia_scripts/inputs_ercot.csv")
}else{
  inputs_file = paste0(baseFol,"/code/inputs_ercot.csv")
}
allinputs = read_csv(inputs_file)
params = allinputs[,c("input_name",runID)]
params = spread(params, key = input_name, value = runID)

endtrim = as.numeric(params$overlapLength)/2

allcomt = loadTimeseriesData(output_fol,"u_commitment",as.numeric(params$overlapLength),2, probabilities=F,instance_in_fol,
                             params$nrandp,dist_ID = params$stochID,endtrim)
drcomt = filter(allcomt,str_detect(GEN_IND,"DR-"))
rm(allcomt)

drprod = loadTimeseriesData(output_fol,"DR_production",as.numeric(params$overlapLength),2, probabilities=F,instance_in_fol,
                            params$nrandp,dist_ID = params$stochID, endtrim)

dralldata = merge(drcomt, drprod, by=c("t","scenario","nperiod"), suffixes = c(".commit",".prod"))
dralldata$scenarionum = substr(dralldata$scenario,2,4)

write_csv(dralldata, paste0(output_fol,runID,"_",runDate,"_dralldata.csv"))

