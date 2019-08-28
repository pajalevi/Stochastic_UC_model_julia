# update_start_cost_ramp.R
# update start costs and ramp rates
# load current genset, as well as ramp and start data
# change info in genset with new ramp and start
# save with given name
# Patricia Levi Aug 2019

library(tidyverse)
library(readxl)


update_start_ramp_cost = function(initial_genset,ramp_file,start_file,output_name,gen_folder,data_folder){
  genset = read_csv(paste0(gen_folder,initial_genset))
  start_costs = read_xlsx(path = paste0(data_folder,start_file),sheet="for_model",range = "A1:D16")
  ramp_rates = read_xlsx(path = paste0(data_folder,ramp_file),sheet="for_model",range = "A1:D16")
  
  # match on basis of technology/fuel and size (only relevant for coal)

  genset$smcoal = F
  genset$smcoal[genset$Capacity <= 300 & genset$Fuel == "COAL"] = T

  new_data = merge(start_costs,ramp_rates, by = c("technology","max_capacity","min_capacity"), all = T)
  new_data$smcoal = F
  new_data$smcoal[new_data$technology == "COAL" & new_data$max_capacity <= 300] = T
  
  new_genset = merge(genset,new_data,by.x = c("Fuel","smcoal"),by.y = c("technology","smcoal"),all.x=T)
  new_genset$StartCost = new_genset$startup_cost * new_genset$Capacity
  new_genset$ramprate = new_genset$ramp_rates/100
  new_genset2 = select(new_genset, -ramp_rates,-startup_cost,-smcoal)
  
  write_csv(new_genset2, path=paste0(gen_folder,output_name))
}

initial_genset = "complete_generator_correctedGas_3-12-19.csv"
gen_folder = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/ercot_default/"
output_name = "complete_generator_correctedGas_Start_Ramp_8-28-19.csv"

ramp_file = "ramp_rates.xlsx"
start_file = "startup_costs.xlsx"
data_folder = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/unformatted data/"
