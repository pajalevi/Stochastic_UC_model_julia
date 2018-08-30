# write_input_day_subselection.R
# defines the function makeoutputs()
# let user specify which days of year should be outputted
# creates the following input files for julia:
# periods_<ndays>.txt
# first_periods_<ndays>.txt
# notfirst_periods_<ndays>.txt
# solar_input_<ndays>.txt
# wind_input_<ndays>.txt
# demand_2015_<ndays>.txt

# day input is given as day of year
library(lubridate)
library(tidyverse)
gams_folder = "/Users/patricia/Documents/Google Drive/stanford/second year paper/Tutorial II/Data/gams_input/"
output_folder = "/Users/patricia/Documents/Google Drive/stanford/second year paper/Tutorial II/Data/gams_input/20days/"

##### randomly select 12 weeks out of the year ####
# one in each month
##output should be a set of firstdays
firstdays = c()
days = c()
ndays = 5
for(i in 1:12){
  daysInMonth = days_in_month(mdy(paste(i,"1 2015")))
  # first day must be at least 7 days from end of month
  monthday = sample(1:(daysInMonth-(ndays-1)), 1)
  yearday = yday(mdy(paste(i,monthday,"2015", sep="/")))
  
  firstdays = c(firstdays, yearday)
  days = c(days, yearday:(yearday+(ndays-1)))
}
firstdays
days

makeoutputs(days = days, firstdays = firstdays, gams_folder,output_folder, id = "h")


#### choose two 10 day intervals around the january and the summer peak
output_folder = "/Users/patricia/Documents/Google Drive/stanford/second year paper/Tutorial II/Data/gams_input/20days/"
peaks = which(r>(8760-4))
# [1]  439  440  441 4960
peaks = c(439, 4960) #these are hours. Need to identify day
peaks_day = round(peaks/24,0)
first = peaks_day - 5
alldays = c(first[1]:(first[1]+10),first[2]:(first[2]+10))

makeoutputs(firstdays = first, days = alldays, gams_folder,output_folder, id = "h")


##### specify desired days #####
days = c(6:10)
#id days that begin a new time segment
firstdays = c(6)
#special identifier
id = ""

### to make inputs for runtime tests: consecutive##
# firstdays = c(1)

for(i in 3:8){
  makeoutputs(days = c((10-i+1):10), firstdays = (10-i+1))
}

### make inputs for runtime tests: independent ###
firstday = day = seq(10,80,10)

for(i in 3:8){
  makeoutputs(days = day[1:i], firstdays = firstday[1:i])
}

################################
makeoutputs = function(days, firstdays, id, gams_folder, output_folder){
  
  if(sum(duplicated(days))>0) {stop("days are duplicated")}
  if(max(days)>365 | min(days <1)){stop("invalid day")}
  
  nhours = length(days) *24
  
  if(missing(id)){
    id = paste0("h",length(firstdays),"groups")
  }
  #### write periods ####
  hours = c()
  for(i in 1:length(days)){
    dayhours = (24*days[i]):(24*days[i]+23)
    hours = c(hours, dayhours)
  }
  # hours
  # periods = paste0("h",hours)
  # periods
  periods = hours
  write.table(matrix(periods, ncol=1), file= paste0(output_folder,"periods_",nhours,id,".csv"), row.names = FALSE, quote = FALSE,col.names = FALSE, sep=",")
  
  # first_periods = paste0("h",24*firstdays)
  first_periods = 24*firstdays
  write.table(matrix(first_periods, ncol=1), file= paste0(output_folder,"first_periods_",nhours,id,".csv"), row.names = FALSE, quote = FALSE,col.names = FALSE, sep=",")
  
  #remove first_periods from periods list to make notfirst_periods
  firstsel = hours %in% (24*firstdays)
  write.table(matrix(periods[!firstsel], nrow=1), file= paste0(output_folder,"notfirst_periods_",nhours,id,".csv"), row.names = FALSE, quote = FALSE,col.names = FALSE, sep=",")
  
  ###### solar input ####
  # load full dataset
  solar = read.table(file = paste0(gams_folder,"solar_input_8760.txt"), sep=",", row.names = 1, header = T)
  # ugh this converts colnames
  colnames(solar) = c("SOLAR-1","SOLAR-2","SOLAR-3","SOLAR-4","SOLAR-5","SOLAR-6","SOLAR-7","SOLAR-8")
  # solar = read_csv(file = paste0(gams_folder,"solar_input_8760.txt"), sep=",", row.names = 1, header = T)
  # ugh this doesnt do row names
  
  solar_input = solar[hours,]
  # head(solar_subsel)
  
  solar_assign = matrix(nrow=nrow(solar_input)*ncol(solar_input),ncol=1)
  counter = 1
  for(r in 1:nrow(solar_input)){
    for(c in 1:ncol(solar_input)){
      solar_assign[counter] = paste("pf('",row.names(solar_input)[r],"','",colnames(solar_input)[c], "') = ", round(solar_input[r,c],4)," ;",sep="")
      counter = counter+1
    }
  }
  
  write.table(file = paste0(output_folder,"solar_input_",nhours,id,".txt"),solar_assign, quote = FALSE,row.names = FALSE, col.names = FALSE)
  
  ######## wind input ######
  # load full dataset
  wind = read.table(file = paste0(gams_folder,"wind_input_8760.txt"), sep=",", row.names = 1, header = T)
  colnames(wind) = c("WIND-1","WIND-2","WIND-3")
  
  wind_input = wind[hours,]
  wind_assign = matrix(nrow=nrow(wind_input)*ncol(wind_input),ncol=1)
  counter = 1
  for(r in 1:nrow(wind_input)){
    for(c in 1:ncol(wind_input)){
      wind_assign[counter] = paste("pf('",row.names(wind_input)[r],"','",colnames(wind_input)[c], "') = ", round(wind_input[r,c],4)," ;",sep="")
      counter = counter+1
    }
  }
  write.table(file = paste0(output_folder,"wind_input_",nhours,id,".txt"),wind_assign, quote = FALSE,row.names = FALSE, col.names = FALSE)
  
  ##### demand response availability ####
  # dr = read.table(file = paste0(gams_folder,"dr_MOO_availability.csv"), sep=",", row.names = 1, header=T)
  dr = read.table(file = paste0(gams_folder,"dr_availability_PJM_onpeak.csv"), sep=",", row.names = 1, header=T)
  colnames(dr) = c("DR-1")
  
  dr_input = as_tibble(dr)[hours,] #matrices and dataframes return a vector if only one column >.<
  dr_assign = matrix(nrow=nrow(dr_input)*ncol(dr_input),ncol=1)
  counter = 1
  for(r in 1:nrow(dr_input)){
    for(c in 1:ncol(dr_input)){
      dr_assign[counter] = paste("pf('h",hours[r],"','",colnames(dr_input)[c], "') = ", round(dr_input[r,c],4)," ;",sep="")
      counter = counter+1
    }
  }
  write.table(file = paste0(output_folder,"dr_input_",nhours,id,".txt"),dr_assign, quote = FALSE,row.names = FALSE, col.names = FALSE)
  
  ##### demand data ######
  # load demand data
  demand = read.csv(file = paste0(gams_folder,"demand_2015.csv"), header = F)
  
  new_demand = demand[hours,]
  
  write.table(new_demand, quote = FALSE,file = paste0(output_folder,"demand_2015_",nhours,id,".csv"),col.names = FALSE, sep = "," ,row.names = F)
} #endfunction




demand = read.csv(file = paste0(gams_folder,"demand_2015.csv"), header = F)
demand$hour = as.numeric(substring(demand$V1,2,10))

ggplot(demand,aes(y=V2, x = hour)) + geom_line() + labs(y="demand (MWh)", title = "whole year demand")
