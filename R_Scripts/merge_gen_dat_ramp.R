# merge_gen_dat.R
# this script merges EIA Form 890 data with the dataset
# assembeled for the virginia analysis (taken from eGrid data)
# And adds P_min values from EIA data, and categorizes
# generators on the basis of EIA data for determining what
# startup cost to use.
# Patricia Levi, April 2018

#### setup ####
library(tidyverse)
library(stringr)

data_folder = "/Users/patricia/Documents/Google Drive/stanford/second year paper/Tutorial II/Data/unformatted data/"
# eia_folder = "/Users/patricia/Documents/Google Drive/stanford/second year paper/Tutorial II/Data/unformatted data/eia8602016/"

#### load data ####
# * load EIA data
#       * load VA generator data, includes import generators
eia_va = read_csv(file = paste0(data_folder, "complete_generators_existing_generators_EIA_data.csv"))
#       * load proposed plants
eia_proposed = read_csv(file = paste0(data_folder,"complete_generator_proposed_generators_EIA_data.csv"))
# * load generator_data
gen_dat = read_csv(file = paste0(data_folder, "complete_generator_data_updated_DR_toy.csv"))
# load ramp rate data
ramp_rate = read_csv(file = paste0(data_folder, "ramp_rates_pct_low.csv"))

#### aggregate EIA data to plant level ####
# * sum up EIA data over pmin, nameplate
# * retain characteristics:
#   * “Technology”
#   * “Operating Year”
#   * “Supercritical Technology?”
#   * “Ultrasupercritical Technology?”
#   * “Energy Source 1"
# * These will be used to determine the startup cost

# spec(eia_va)

eia_va_agg = eia_va %>%
  group_by(`Plant Name`,
           `Technology`,`Energy Source 1`,
           `Supercritical Technology?`,`Ultrasupercritical Technology?`) %>%
  summarise(EIAPMin_MW = sum(`Minimum Load (MW)`),
            EIAnameplate_MW = sum(`Nameplate Capacity (MW)`),
            minOperatingYear = min(`Operating Year`, na.rm=T),
            maxOperatingYear = max(`Operating Year`, na.rm=T)
            # technology1 = unique(`Technology`)[1],
            # technology2 = unique(`Technology`)[2],
            # energySource1 = unique('Energy Source 1')[1],
            # energySource2 = unique('Energy Source 1')[2]
            ) #%>%
  # spread(value = 'Technology')
# ideally i'd like to save technology by the associated size.... how do I spread technology and capacity simultaneously?

# View(eia_va_agg)

# For now: DO NOT allow multiple rows for different generator technologies at same plant
# this will keep the number of generators down and thus keep the solve time down.
#   * for each plant, find largest generator row.
#   * Save that row, but update the capacity and p_min values with the sum of all rows.
plants = unique(eia_va_agg$`Plant Name`)

# create new structure to hold updated data
collapsed_eia_va = as.tibble(matrix(nrow=length(plants), ncol = ncol(eia_va_agg),dimnames = list(c(),names(eia_va_agg))))

for(p in 1:length(plants)){
  # select generators in this plant
  psel = which(str_detect(eia_va_agg$`Plant Name`, fixed(plants[p])))
  
  if(length(psel) == 0){stop(paste("zero plants match", plants[p], "which is index",p))}
  
  if(length(unique(eia_va_agg$`Plant Name`[psel]))>1){warning("plant name ", plants[p], " is matching multiple plants \n ",
                                                              print(paste(unique(eia_va_agg$`Plant Name`[psel]))))}
  
  # IF more than one row, find largest capacity, ELSE just add that row
  if(length(psel) >1){
    # find row with greatest installed capacity
    print(paste("collapsing",plants[p]))
    maxsel = which.max(eia_va_agg$EIAnameplate_MW[psel])
    max_index = psel[maxsel]
    if(length(max_index) > 1){stop(paste("at index",p,"max_index is >1. This is plant", plants[p]))}
    
    collapsed_eia_va$`Plant Name`[p]                    = eia_va_agg$`Plant Name`[max_index]
    collapsed_eia_va$`Energy Source 1`[p]               = eia_va_agg$`Energy Source 1`[max_index]
    collapsed_eia_va$Technology[p]                      = eia_va_agg$Technology[max_index]
    collapsed_eia_va$`Supercritical Technology?`[p]     = eia_va_agg$`Supercritical Technology?`[max_index]
    collapsed_eia_va$`Ultrasupercritical Technology?`[p]= eia_va_agg$`Ultrasupercritical Technology?`[max_index]
    collapsed_eia_va$minOperatingYear[p]                = eia_va_agg$minOperatingYear[max_index]
    collapsed_eia_va$maxOperatingYear[p]                = eia_va_agg$maxOperatingYear[max_index]
    
    nameplate = sum(eia_va_agg$EIAnameplate_MW[psel], na.rm=T)
    if(length(nameplate)>1){stop(paste("nameplate sum length>1 at plant", plants[p]))}
    collapsed_eia_va$EIAnameplate_MW[p] = nameplate
    collapsed_eia_va$EIAPMin_MW[p] = sum(eia_va_agg$EIAPMin_MW[psel], na.rm=T)
    
  } else { #there's only one row to begin with
    collapsed_eia_va[p,] = eia_va_agg[psel,]
    print(paste("inserting", plants[p]))
  }
}

# check for duplicated listings
sum(duplicated(collapsed_eia_va$`Plant Name`))

#### merge datasets ####
# * merge with generator_data on the basis of plant name
# * check for full merge, clean up stragglers

gen_merge = merge(x = gen_dat, y = collapsed_eia_va, by.y = "Plant Name", by.x = "Virginia_GAMS/BIOMASS_plants.txt",
                  # all = T) # useful to check how the merge is working
                  all.x=T) #what I ultimately want: add info to known generators, not adding new ones
gen_merge = rename(gen_merge, `Plant Name` = `Virginia_GAMS/BIOMASS_plants.txt`)
# View(gen_merge)
sum(duplicated(gen_merge$`Plant Name`))
# View(gen_merge[,c(1,2,4,17,21:24)])

#### fill in missing pmin ####
pmin_default = tibble(plant_type = unique(gen_merge$`Plant primary coal/oil/gas/ other fossil fuel category`), cap_to_pmin_slope = NA, intercept = NA)
#deselect import, DR categories
not_import = !str_detect(pmin_default$plant_type, "IMPORT") 
pmin_default = pmin_default[not_import,]

for(i in 1:nrow(pmin_default)){ #ITERATE OVER PLANT TYPES
  # print(pmin_default$plant_type[i])
  gensel = str_detect(string = gen_merge$`Plant primary coal/oil/gas/ other fossil fuel category`, pattern = pmin_default$plant_type[i])
  # better to make a regression of pmin on nameplate, or just find the average ratio?
  # https://stats.stackexchange.com/questions/269358/why-use-linear-regression-instead-of-average-y-per-x
  ratio = mean(gen_merge$EIAPMin_MW[gensel] / gen_merge$`Nameplate Capacity (MW)`[gensel], na.rm=T)
  # print(paste(pmin_default$plant_type[i], "has ratio", ratio))
  if(!is.na(ratio) & !is.infinite(ratio) & sum(gensel)>1){
    fit = lm(gen_merge$EIAPMin_MW[gensel] ~ gen_merge$`Nameplate Capacity (MW)`[gensel])
    # print(paste(pmin_default$plant_type[i], "has intercept", fit$coefficients[1], "and slope", fit$coefficients[2]))
    
    pmin_default$cap_to_pmin_slope[i] = fit$coefficients[2]
    pmin_default$intercept[i] = fit$coefficients[1]
    
  } else if (!is.na(ratio) & !is.infinite(ratio) & sum(gensel)==1){
    pmin_default$intercept[i] = 0
    pmin_default$cap_to_pmin_slope[i] = ratio
  } else {
    pmin_default$intercept[i] = 0
    pmin_default$cap_to_pmin_slope[i] = 0
  }
}
pmin_default

# identify missing pmin, and fill in
gen_merge_new = gen_merge
missing_pmin = which(is.na(gen_merge_new$EIAPMin_MW))
for(i in missing_pmin){
  type = gen_merge_new$`Plant primary coal/oil/gas/ other fossil fuel category`[i]
  ind = which(str_detect(pmin_default$plant_type,pattern = type)) #DOESNT WORK FOR IMPORTS
  #if(length(ind) != 1){stop("length of ind is ", length(ind))}
  
  if(length(ind) == 1){
    gen_merge_new$EIAPMin_MW[i] = gen_merge_new$`Nameplate Capacity (MW)`[i]* pmin_default$cap_to_pmin_slope[ind] + pmin_default$intercept[ind]
  }
}

View(gen_merge_new[missing_pmin,c(1,2,4,17,21:24)])
# fill in ones that arent working by hand
i=38
gen_merge_new$EIAPMin_MW[i] = gen_merge_new$`Nameplate Capacity (MW)`[i]* pmin_default$cap_to_pmin_slope[8] + pmin_default$intercept[8]
i=45
gen_merge_new$EIAPMin_MW[i] = 0
i=57
gen_merge_new$EIAPMin_MW[i] = gen_merge_new$`Nameplate Capacity (MW)`[i]* pmin_default$cap_to_pmin_slope[8] + pmin_default$intercept[8]


#### determine startup cost category ####
# * file generators into categories for the purposes of determining startup cost
# I will use warm startup cost data for now: taken from NREL report on cycling costs
# another resource: https://www.stiftung-mercator.de/media/downloads/3_Publikationen/nature_energy_Start-up_costs__of_thermal_power_plants.pdf
# categories:
# * coal < 300 MW
coal_small = 157
# * coal >= 300 MW
coal_large = 65
# * coal supercritical
coal_super = 64
# * gas CT
gas_ct = 126
# * gas CC
gas_cc = 55
# * gas steam (I will put landfill gas, biomass, municipal solid waste here)
gas_steam = 58
# * nuclear: will assume $300/MW, as this is just above upper bound for most expensive plant.
nuclear = 300
# * other: zero start cost (renewables)
# other = 0
# * oil. WHAT DO I DO FOR OIL? Its a low cost startup i think. lets call it the same as gas_cc?
oil = 55

# clean up 'supercritical technology' column
gen_merge_new$`Supercritical Technology?`[is.na(gen_merge_new$`Supercritical Technology?`)] = "N"

for(i in 1:nrow(gen_merge_new)){
  # if coal
  if(str_detect(gen_merge_new$`Plant primary coal/oil/gas/ other fossil fuel category`[i], "COAL")){
    if(str_detect(gen_merge_new$`Supercritical Technology?`[i], "Y" )){
      gen_merge_new$`Startup cost`[i] = coal_super * gen_merge_new$`Nameplate Capacity (MW)`[i]
    } else {
      if(gen_merge_new$`Nameplate Capacity (MW)`[i] > 300){
        gen_merge_new$`Startup cost`[i] = coal_large * gen_merge_new$`Nameplate Capacity (MW)`[i]
      } else {
        gen_merge_new$`Startup cost`[i] = coal_small * gen_merge_new$`Nameplate Capacity (MW)`[i]
      }
        
    }
  # if gas
  } else if(str_detect(gen_merge_new$`Plant primary coal/oil/gas/ other fossil fuel category`[i], "GAS")){
    if(str_detect(gen_merge_new$`Plant primary coal/oil/gas/ other fossil fuel category`[i], "CC")) {
      gen_merge_new$`Startup cost`[i] = gas_cc * gen_merge_new$`Nameplate Capacity (MW)`[i]
    } else if(str_detect(gen_merge_new$`Plant primary coal/oil/gas/ other fossil fuel category`[i], "CT")){
      gen_merge_new$`Startup cost`[i] = gas_ct * gen_merge_new$`Nameplate Capacity (MW)`[i]
    } else{
      gen_merge_new$`Startup cost`[i] = gas_steam * gen_merge_new$`Nameplate Capacity (MW)`[i]
    }
  # if nuclear
  } else if(str_detect(gen_merge_new$`Plant primary coal/oil/gas/ other fossil fuel category`[i], "NUCLEAR")){
    gen_merge_new$`Startup cost`[i] = nuclear * gen_merge_new$`Nameplate Capacity (MW)`[i]
  # landfill gas, biomass, municipal solid waste
  } else if(sum(str_detect(gen_merge_new$`Plant primary coal/oil/gas/ other fossil fuel category`[i], 
                       c("LANDFILL", "MUNICIPAL","BIOMASS"))) == 1) {
    gen_merge_new$`Startup cost`[i] = gas_steam * gen_merge_new$`Nameplate Capacity (MW)`[i]
  } else if(str_detect(gen_merge_new$`Plant primary coal/oil/gas/ other fossil fuel category`[i], "OIL")) {
    gen_merge_new$`Startup cost`[i] = oil * gen_merge_new$`Nameplate Capacity (MW)`[i]
  } else {
    gen_merge_new$`Startup cost`[i] = 0
  }
}
# View(gen_merge_new[,c(1,2,4,8,17,21:24)])

## Merge in ramp rate max data ##
gen_merge_ramp = merge(x = gen_merge_new, y = ramp_rate, by.x = "Plant primary coal/oil/gas/ other fossil fuel category",
                      by.y = "PLANTTYPE", all.x = T, all.y = F)
gen_merge_ramp$ramprate[is.na(gen_merge_ramp$ramprate)] = 1
# View(gen_merge_ramp[,c(1:8,17,21:22,25)])

# select only the relevant columns
gen_merge_final = gen_merge_ramp[,c(1:8,17,21:22,25)]

### write new csv ####
write_csv(gen_merge_final, path = paste0(data_folder,"gen_data_merged_ramp.csv"))
