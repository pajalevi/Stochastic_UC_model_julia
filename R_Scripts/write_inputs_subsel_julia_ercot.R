# write_inputs_subsel_julia_ercot.R
# defines the function makeoutputs()
# let user specify which days of year should be outputted
# creates the following input files for julia:
# periods_<ndays>.txt
# first_periods_<ndays>.txt
# notfirst_periods_<ndays>.txt
# solar_input_<ndays>.txt
# wind_input_<ndays>.txt
# ercot_demand_2016_<ndays>.txt

# Feb 2019 adapted from write_input_day_subsel_julia.R
# to have the filepaths needed for ERCOT

# Working timezone is CST; ignore DST


# day input is given as day of year
library(lubridate)
library(tidyverse)
unformatted_input_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/unformatted data/"
wind_solar_input_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_ercot_input/"
output_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/"
demand_file_name = "ercot_demand_data.csv"
demand_output_name = "ercot_demand_2016_"

solar_input_name = "solar_availability_factors_2016.csv"
wind_input_name = "wind_availability_factors_2016.csv"
dr_input_name = "dr_availability_daytime_2016.csv"


#### choose two 10 day intervals around the january and the summer peak
output_folder = "/Users/patricia/Documents/Google Drive/stanford/second year paper/Tutorial II/Data/gams_input/4days/"
peaks = which(rank(demand$V2)>(8760-4))
# [1]  439  440  441 4960
peaks = c(440, 4960) #these are hours. Need to identify day
peaks_day = round(peaks/24,0)
first = peaks_day - 5
alldays = c(first[1]:(first[1]+10),first[2]:(first[2]+10))

makeoutputs(firstdays = first, days = alldays, input_folder = unformatted_input_fol,output_folder = output_fol)

### choose two 2 day intervals around two peaks
first = peaks_day - 1
alldays = c(first[1]:(first[1]+2),first[2]:(first[2]+2))

makeoutputs(firstdays = first, days = alldays, input_folder = unformatted_input_fol,output_folder = output_fol)


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

### test for ercot ###
# index 9786 is max
# run 2 days
dem = read_csv(paste0(unformatted_input_fol, demand_file_name))
daysel = yday(dem$time_UTC[9786]):(yday(dem$time_UTC[9786])+1) #yday is 1-indexed
firstday = daysel[1]

makeoutputs(days = daysel, firstdays = firstday, id = "ercot_2day_test",
            input_folder = unformatted_input_fol,
            output_folder = output_fol,
            dr_name = dr_input_name,
            solar_name = solar_input_name,
            wind_name = wind_input_name,
            re_input_fol = wind_solar_input_fol)


 ################################
# MAKEOUTPUTS FUNCTION BEGIN
################################
makeoutputs = function(days, firstdays, id, input_folder, output_folder, dr_name, solar_name, wind_name, re_input_fol){
  # days: vector, day of year
  # first days: days that begin a period
  # ID: identify this time period
  
  if(sum(duplicated(days))>0) {stop("days are duplicated")}
  if(max(days)>365 | min(days <1)){stop("invalid day")}
  
  nhours = length(days) *24
  
  # create a unique identifier for this set of files
  if(missing(id)){
    id = paste0(nhours,"h",length(firstdays),"groups")
  }
  
  if(!file.exists(paste(output_folder,id, sep=""))){
    dir.create(paste(output_folder,id, sep=""))
  }
  
  #### write periods ####
  hours = c()
  for(i in 1:length(days)){
    dayhours = (24*(days[i]-1)+1):(24*days[i])
    hours = c(hours, dayhours)
  }

  periods = hours
  write.table(matrix(periods, ncol=1), file= paste0(output_folder,id,"/periods_",id,".csv"), row.names = FALSE, quote = FALSE,col.names = FALSE, sep=",")
  
  # first_periods = paste0("h",24*firstdays)
  first_periods = 24*firstdays
  write.table(matrix(first_periods, ncol=1), file= paste0(output_folder,id,"/first_periods_",id,".csv"), row.names = FALSE, quote = FALSE,col.names = FALSE, sep=",")
  
  #remove first_periods from periods list to make notfirst_periods
  firstsel = hours %in% (24*firstdays)
  write.table(matrix(periods[!firstsel], ncol=1), file= paste0(output_folder,id,"/notfirst_periods_",id,".csv"), row.names = FALSE, quote = FALSE,col.names = FALSE, sep=",")
  
  ###### solar input ####
  # load full dataset
  solar = read_csv(file = paste0(re_input_fol,solar_name))
  # remove time_UTC col, select hours, write
  # solar = select(solar, -time_UTC)
  solar_input = solar[hours,]
  write.table(file = paste0(output_folder,id,"/solar_input_",id,".csv"),solar_input, quote = FALSE,row.names = FALSE, col.names = TRUE, sep=",")
  
  ######## wind input ######
  # load full dataset
  wind = read_csv(file = paste0(re_input_fol,wind_name))

  wind_input = wind[hours,]
  write.table(file = paste0(output_folder,id,"/wind_input_",id,".csv"),wind_input, quote = FALSE,row.names = FALSE, col.names = TRUE, sep = ",")
  
  ##### demand response availability ####
  # dr = read.table(file = paste0(input_folder,"dr_MOO_availability.csv"), sep=",", row.names = 1, header=T)
  dr = read.table(file = paste0(re_input_fol,dr_name), sep=",", row.names = 1, header=T)
  colnames(dr) = c("DR-1")
  
  dr_input = as_tibble(dr)[hours,] #matrices and dataframes return a vector if only one column >.<
  write.table(file = paste0(output_folder,id,"/dr_input_",id,".txt"),dr_input, quote = FALSE,row.names = FALSE, col.names = TRUE, sep=",")
  
  ##### demand data ######
  # load demand data
  demand = read_csv(file = paste0(input_folder,demand_file_name))
  demand$time_CST = demand$time_UTC - hours(6)
  demand16 = demand[year(demand$time_CST) == 2016,]
  new_demand = demand16[hours,]
  # new_demand$hour = substr(new_demand$hour,2,100)
  write_demand = select(new_demand, time_CST, demand)
  
  write.table(write_demand, quote = FALSE,file = paste0(output_folder,id,"/",demand_output_name,id,".csv"),col.names = TRUE, sep = "," ,row.names = F)
} 
################################
# MAKEOUTPUTS FUNCTION END
################################