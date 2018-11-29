# write_julia_generator_data.R
# takes generator data as output by merge_gen_dat_ramp.R
# and formats it for use in the julia model
# Patricia Levi Nov 2018

# most code adapted from 'write_gams_input' which was in turn adapted from 'plat_level_data_readin_DR.R'
# which is adapted from the Virginia project done for John's class
library(tidyverse)
library(gdata)


inputfolder = "/Users/patricia/Documents/Google Drive/stanford/second year paper/Tutorial II/Data/unformatted data/"
inputfile = "gen_data_merged_ramp.csv"
outputfolder = "/Users/patricia/Documents/Google Drive/stanford/second year paper/Tutorial II/Data/julia_input/default/"
#inputfile = "complete_generator_data_2030_fuel_realImports.csv"
# x = gen_merge_final # from merge_gen_dat.R
x = read.csv(file=paste(inputfolder,inputfile,sep=""))#file.choose()) # complete_generator_data.csv
# gams_folder = "/Users/patricia/Documents/Google Drive/stanford/second year paper/Tutorial II/Data/julia_input/"

x = as_tibble(x)
# x = as_tibble(gen_merge_final) #can use this if the output from merge_gen_dat_ramp are in active memory
names(x)
# set header names on generator data (called x)
dimnames(x)[[2]]=c("Fuel","Plant.Name","Code","Capacity",
                   "VCost","FCost","CapCost", "StartCost", 
                   "Technology", "PMin", "EIACapacity","ramprate")

# Convert units ####
# capacity: MW - all good

# var cost: $/GWh - convert from $/MWh. *1000
x$VCost = x$VCost*1000

# Fixed cost: $/GW - convert from $/MW
x$FCost = x$FCost*1000

# cap cost: $/GW - convert from $/MW
x$CapCost = x$CapCost*1000

### remove storage! #
remove = (x[,"Fuel"]=="HYDRO_PUMPED_STORAGE")
xx = x[!remove,]

### name DR properly 
# change Fuel to be string, not factor
xx$Fuel = as.character(xx$Fuel)
xx$Fuel[str_detect(xx$Fuel,"DEMAND RESPONSE")] = "DR"
# switch it back
xx$Fuel = as.factor(xx$Fuel)

##### create a unique identifier for each plant ####
# because some plant types are subsets of other plant type names, must use FACTORS, not strings... ugh
counter = 1
plantUnique = vector(length = dim(xx)[[1]])
for (i in unique(xx$Fuel)){
  # whichplants = which(str_detect(xx$Fuel,i))
  whichplants = which(xx$Fuel==i)
  for(j in 1:length(whichplants)){
    plantUnique[whichplants[j]]=paste(as.character(i),"-",j,sep="")
    counter = counter+1
  }
}

### write file with plant_names
write.csv(cbind(xx,plantUnique),file = paste0(outputfolder,"gen_merged_withIDs_ramp.csv"))
