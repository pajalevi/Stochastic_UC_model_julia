# setup_egrid_generators.R
# input: raw eGrid data, from the plants sheet
#        EIA form 860 data
# output: generators in ERCOT with following info:
#   - marginal cost **
#   - capacity
#   - startup cost **
#   - fuel type
#   - emissions of key pollutants
#   - unique ID
# ** requires assumptions from me
# Patricia Levi, Jan 2019

library(tidyverse)
library(readxl)

# year desired
currentyear = 2016
# havent actually gone through the whole script to ensure it uses this.

#data folder
datain = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/unformatted data/"
dataout = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/"

# load data
# load ramp rate data
ramp_rate = read_excel(path = paste0(datain, "ramp_rates.xlsx"),sheet="for_model_low") #UPDATE FOR ERCOT

# Load variable O&M costs - use mean values
varOM = read_excel(path = paste0(datain,"Variable_OM_Costs_NREL.xlsx"), sheet = "NREL",range = "A2:G19")
# I want the last column

# load startup cost data
startup_cost = read_excel(paste0(datain,"startup_costs.xlsx"),range = "A1:D10")

# eGrid
egridplntraw = read_excel(path = paste0(datain, "egrid2016_data.xlsx"),sheet = "PLNT16",skip=1)
egridgenraw = read_excel(path = paste0(datain, "egrid2016_data.xlsx"),sheet = "GEN16",skip=1)
# skip ensures that colnames are abbreviations - see spreadsheet for longhand names

# EIA
# data desired:
# - data needed to match to eGrid 
# - plant technology (in generator dataset)
# - operating year
# - planned retirement year
# - minimum load
# need to use plant dataset as well because this has balancing authority
eiagen = read_excel(path = paste0(datain,"eia form 8602016/3_1_Generator_Y2016.xlsx"), sheet = "Operable", skip=1)
eiaplant = read_excel(path = paste0(datain,"eia form 8602016/2___Plant_Y2016.xlsx"), sheet = "Plant", skip=1)

#--------------------#
#### EIA Form 860 ####
#--------------------#

# sub-select eiagen for plants in texas
# eiagenTX = eiagen %>%
#   filter(State == "TX")

# summarise eiagen by plant code 
eiagen_byplant = eiagen %>% #Using the unfiltered eiagen database just in case - theres 1 extra generator match if I do that
  group_by(`Plant Code`,Technology) %>%
  summarize(PlantName = unique(`Plant Name`),
            nNames = length(unique(`Plant Name`)),
            # Technology1 = unique(`Technology`)[1],
            # Technology2 = unique(`Technology`)[2],
            # Technology3 = unique(`Technology`)[3],
            MinLoadMW = sum(`Minimum Load (MW)`),
            Capacity = sum(`Nameplate Capacity (MW)`),
            OperatingYr1 = unique(`Operating Year`)[1],
            OperatingYr2 = unique(`Operating Year`)[2],
            RetirementYr1 = unique(`Planned Retirement Year`)[2],
            RetirementYr2 = unique(`Planned Retirement Year`)[2],
            supercritical = max(unique(`Supercritical Technology?`)[1])) #max will return Y above NA or N

# sub-select eiaplant for plants in ERCOT (by Balancing Authority Name and Balancing Authority Code?)
eiaplantERCOT = eiaplant %>%
  filter(`Balancing Authority Name` == "Electric Reliability Council of Texas, Inc." | `Balancing Authority Code` == "ERCO" |
           `NERC Region` == "TRE")

# merge eiagen and eiaplant by plantcode
eiadata = merge(eiagen_byplant, eiaplantERCOT, by = "Plant Code", all.y=T)

# save egriddata for later reference


#--------------------#
#### eGrid        ####
#--------------------#
# isolate ERCOT plants in eGrid data
ercotplnt = egridplntraw %>%
  filter((SRNAME == "ERCOT All" & BANAME== "No balancing authority") | BANAME == "Electric Reliability Council of Texas, Inc.") 
# Use "eGRID subregion name" in lieu of "Blanncing Authority Name" 
# because some wind projects in TX are listed as "no balancing authority" for BANAME
# but this also catches some generators that are in other balancing authorities, like MISO and SPP

# sanity check - total generation
sum(ercotplnt$NAMEPCAP)

# isolate only useful columns
cols = c("BANAME", # balancing authority name
         "SRNAME", # eGRID subregion acronym
         "PNAME", # plant name
         "PLPRMFL", # primary fuel
         "PLFUELCT", # Plant primary coal/oil/gas/ other fossil fuel category
         "PLHTRT", # Plant nominal heat rate Btu/kWh
         "NAMEPCAP", #Plant nameplate capacity (MW)
         "CAPFAC", # Plant capacity factor
         "CHPFLAG", # Combined heat and power (CHP) plant adjustment flag
         "PSFLAG", # Plant pumped storage flag
         "PLNOXRTA", # Plant annual nox emission rate lb/MWh
         "PLSO2RTA", # Plant annual so2 emission rate lb/MWh
         "PLC2ERTA", # Plant annual CO2 equivalent total output emission rate (lb/MWh)
         "PLGENAHY", # Plant annual hydro net generation (MWh)
         "PLTRPR", # Plant total renewables generation percent (resource mix)
         "LAT",
         "LON",
         "ORISPL" #Plant code
         )

egridplntdat = ercotplnt %>%
  select(one_of(cols))

# merge in data from generator tab by plant code 'ORISPL'
  # first compress generator data by plant code
egridgen_byplant = egridgenraw %>% #Using the unfiltered eiagen database just in case - theres 1 extra generator match if I do that
  group_by(ORISPL) %>%
  summarize(PlantName = unique(PNAME)[1],
            nNames = length(unique(PNAME)),
            OperatingYr1 = unique(GENYRONL)[1],
            OperatingYr2 = unique(GENYRONL)[2],
            RetirementYr1 = unique(GENYRRET)[1],
            RetirementYr2 = unique(GENYRRET)[2])

# merge plant and generator data by plant ID
egriddata = merge(egridplntdat,egridgen_byplant,by="ORISPL",all.x=T)

# save egriddata for later reference
write_csv(egriddata,path = paste0(datain,"egrid2016_data_ercotsubset.csv"))


#-----------------------------#
#### Merge eGrid, eiadata  ####
#-----------------------------#

# merge eGrid, eiadata

# merge by name
# alldat = merge(egriddata, eiadata, by.x = "PNAME", by.y = "Plant Name", all=T)
# # View(alldat %>% arrange(PNAME))
# # View(sort(unique(alldat$PNAME)))

# # merge by lat/lon rounded to nearest 0.0001
# egriddata = egriddata%>%
#   mutate(LAT_round = round(LAT, digits = 4),
#          LON_round = round(LON, digits = 4))
# eiadata = eiadata%>%
#   mutate(LAT_round = round(Latitude, digits = 4),
#          LON_round = round(Longitude, digits = 4))
# alldat = merge(egriddata, eiadata, by = c("LAT_round","LON_round"))

# merge by plant code
alldat = merge(egriddata, eiadata, by.x = "ORISPL", by.y = "Plant Code", all=T, 
               suffixes = c(".eGrid",".eia"))

# check/inspect generators without a match
# View(alldat[(is.na(alldat$NAMEPCAP) | is.na(alldat$Capacity)),
#             c("PNAME","PlantName.eia","PlantName.eGrid","PLFUELCT","Technology","NAMEPCAP","Capacity",
#               "OperatingYr1.eia","OperatingYr1.eGrid","RetirementYr1.eia","RetirementYr1.eGrid")])

#--------------------------------------------------------------#
#### Remove generators that aren't yet operating or retired ####
#--------------------------------------------------------------#
## Not Operating:
# Ie those that DO have an operating year in the future and
# DO NOT have an operating year in the past
maxyr = currentyear
noexistsel = ((alldat$OperatingYr1.eGrid > maxyr | is.na(alldat$OperatingYr1.eGrid)) & 
              (alldat$OperatingYr1.eia > maxyr | is.na(alldat$OperatingYr1.eia > maxyr)) &
              (alldat$OperatingYr2.eGrid > maxyr | is.na(alldat$OperatingYr2.eGrid)) &
              (alldat$OperatingYr2.eia > maxyr | is.na(alldat$OperatingYr2.eia > maxyr)))
#inspect
# View(alldat[which(noexistsel),c("PNAME","PlantName.eia","PlantName.eGrid","PLFUELCT","Technology","NAMEPCAP","Capacity",
              # "OperatingYr1.eia","OperatingYr1.eGrid","OperatingYr2.eia","OperatingYr2.eGrid")])

alldat_tmp = alldat[which(!noexistsel),]

## Retired:
retiredsel = ((alldat_tmp$RetirementYr1.eGrid <= maxyr | is.na(alldat_tmp$RetirementYr1.eGrid)) & 
              (alldat_tmp$RetirementYr1.eia <= maxyr | is.na(alldat_tmp$RetirementYr1.eia > maxyr)) &
              (alldat_tmp$RetirementYr2.eGrid <= maxyr | is.na(alldat_tmp$RetirementYr2.eGrid)) &
              (alldat_tmp$RetirementYr2.eia <= maxyr | is.na(alldat_tmp$RetirementYr2.eia > maxyr)))
#inspect
# View(alldat[which(retiredsel),c("PNAME","PlantName.eia","PlantName.eGrid","PLFUELCT","Technology","NAMEPCAP","Capacity",
#   "RetirementYr1.eia","RetirementYr1.eGrid","RetirementYr2.eia","RetirementYr2.eGrid")])

alldat = alldat_tmp[which(retiredsel),]


#----------------------------------------------------#
# Consolidate technology and capacity information ####
#----------------------------------------------------#

# for all plants that don't have a Technology (eia generator)
# assign them one based on PLFUELCT (egrid)
alldat$technologyAll = alldat$Technology

# TODO: separate out supercritical coal

# make an index from PLFUELCT to Technology
# with help from https://www.epa.gov/sites/production/files/2018-02/documents/egrid2016_technicalsupportdocument_0.pdf
# OTHF: waste heat/unknown/unknown purchased fuel/"process gas"
#     one generator (152) relies on this defn for technology
# OFSL: Other fossil: blast furnace gas,other gas, tire-derived fuel
#     No generators rely on this defn for technology
# NB: Assigning all Gas to CT (not CC). In the future, could look at egrid GEN16 (generator) data
# which has Prime mover type (ie generator type) which differentiates between CC and CT.
# however, the egrid technical support document says that this data comes from EIA-860, so it might not actually help
PLFUELCT_Technology = array(dimnames = list(c(),as.vector(unique(alldat$PLFUELCT))),
                 dim = c(1,length(unique(alldat$PLFUELCT))),
                 data = c(NA,"Conventional Steam Coal",
                          "Conventional Hydroelectric","Natural Gas Fired Combustion Turbine", #All GAS is assumed to be CT (not CC)
                          "Petroleum Liquids","Nuclear","Other Gases",NA,"Landfill Gas","Onshore Wind Turbine",
                          "Solar Photovoltaic"))

techsel = which(is.na(alldat$Technology))
for(i in techsel){
  if(!is.na(alldat$PLFUELCT[i])){
    alldat$technologyAll[i] = PLFUELCT_Technology[,alldat$PLFUELCT[i]]
  } else {
    alldat$technologyAll[i] = "All Other"
  }
}

#### create mapping from technologyAll to shortened reference name
fuelref = array(dimnames = list(c(),sort(unique(alldat$technologyAll))),
                dim=c(1,length(sort(unique(alldat$technologyAll)))),
                data = c("OTHER","BATTERY",
                         "HYDRO","COAL","LANDFILL_GAS",
                         "GAS_CC","GAS_CT",
                         "GAS_ICE","GAS_ST",
                         "NUCLEAR","WIND",
                         "GAS","BIOMASS", #Not sure about mapping of "Other Gases" to "Other" or "natgas"
                         "OIL","OIL", # Not sure about mapping "Petroleum Coke" to "Petroleum"
                         "SOLAR","BIOMASS"
                ))
alldat$Fuel = NA
for(i in 1:nrow(alldat)){
  alldat$Fuel = fuelref[,alldat$technologyAll]
}

#inspect
# View(alldat[,c("Technology","PLFUELCT","technologyAll","Fuel")])


# for all plants that dont have a capacity from eia
# assign them the capacity from eigrid (NAMEPCAP)
alldat$capacityAll = alldat$Capacity

capsel = which(is.na(alldat$Capacity))
for(i in capsel){
  alldat$capacityAll[i] = alldat$NAMEPCAP[i]
}

# check that technologyAll worked
# uniquetechall = unique(alldat$technologyAll)
# for(i in uniquetechall){
#   techsel = which(str_detect(alldat$technologyAll, i))
#   uniquePLFUELCT = unique(alldat$PLFUELCT[techsel])
#   print(paste("techAll type",i," has unique types "))
#   print(uniquePLFUELCT)
# }

#### EXPLORE HEAT RATE BY TECHNOLOGY ###
# ULTIMATELY - VARIATION NOT MEANINGFUL
# NOT A USEFUL WAY TO DIFFERENTIATE TECHNOLOGIES

# see how much heat rate varies by technology

# all_gas = alldat%>%
#   filter(PLFUELCT == "GAS")
# ggplot(all_gas, aes(x=Technology, y=PLHTRT)) + 
#   geom_violin(draw_quantiles = c(.25,.50,.75))

# theres a fair amount of overlap, and the middle 50% overlaps for CT vs CC, but I could make some
# sort of arbitrary distinction for efficiency
# I wonder if it varies by year -- but thats not helpful because year info comes from the 
# same place as technology info

# all_gas$decade = floor((all_gas$OperatingYr1 - 1900)/10)
# ggplot(all_gas, aes(x=Technology, y=PLHTRT)) + geom_boxplot() + facet_wrap(~decade)

# BUT -- egrid Generator tab has "Generator year on-line". So, for plants that have a listing in the
# generator file, I could assign a year. But this is a lot of work for a refinement of what is 
# still just a rule-of-thumb. At least EIA has a plant code that an be used for matching.

# What's a quick way to assign VarOM costs while leaving all this for later?
# decide on a heat-rate cutoff and assign to CC or CT based on that
# OR -- just give them an average of the two. Theyre pretty close anyways

# what's the best cutoff? The one that minimizes error in existing dataset 
# (we are assuming the unknown and the known dataset have same distribution 
# which is not a great assumption but its what we've got)
# CTsel = all_gas$Technology == "Natural Gas Fired Combustion Turbine"
# CCSel = all_gas$Technology == "Natural Gas Fired Combined Cycle"
# CC_only_htrt = all_gas$PLHTRT[CCSel]
# start = quantile(CC_only_htrt, probs = 0.75,na.rm=T)
# # 8033
# cutoff = start #41
# sum(all_gas$PLHTRT[CTsel] > cutoff,na.rm=T) + sum(all_gas$PLHTRT[CCSel] < cutoff, na.rm=T)
# cutoff = 9000 #45
# cutoff = 8000 #41
# for(cutoff in seq(4000,6000, by = 100)){
#   errorval =sum(all_gas$PLHTRT[CTsel] > cutoff,na.rm=T) + sum(all_gas$PLHTRT[CCSel] < cutoff, na.rm=T)
#   print(paste("at cutoff=",cutoff," error is ", errorval))
# }
# # 5500 is best cutoff
# cutoff = 5500

# identify gas plants without technology and assign them to CC or CT
# do this in a separate variable

#------------------------#
##### Match O&M costs ####
#------------------------#

# create varOM dataframe with Technology as column
varOMref = array(dimnames = list(c(),as.vector(varOM$Technology)),
                 dim = c(1,nrow(varOM)),
                 data = varOM$`MEAN-2016value`)

# create a dictionary that maps from technology in alldat to varOM
techtypesOM = array(dimnames = list(c(),sort(unique(alldat$Technology))),
                  dim=c(1,length(sort(unique(alldat$Technology)))),
                  data = c("Other","Other",
                           "Hydroelectric","Coal",
                           "Biomass","Combined Cycle",
                           "Combustion Turbine","Natural Gas Internal Combustion Engine",
                           "Natural Gas Steam Turbine","Nuclear",
                           "Wind","Other", #Not sure about mapping of "Other Gases" to "Other"
                           "Biomass","Petroleum", # Not sure about mapping "Petroleum Coke" to "Petroleum"
                           "Petroleum","Solar PV",
                           "Biomass"
                           ))

# For entries without a techology listed, map the primary fuel to varOM
# fueltypes = array(dimnames = list(c(),sort(unique(alldat$PLFUELCT))),
#                   dim=c(1,length(sort(unique(alldat$PLFUELCT)))),
#                   data = c("Biomass","Coal","Other Gas","Hydro","Nuclear","Other",
#                            "Petroleum","Other","Solar PV","Wind"))

# create column for varOM cost
alldat$varOM = NA
for(i in 1:nrow(alldat)){
  # if Tech is not NA, use that to index into varOMref
  if(!is.na(alldat$technologyAll[i])){
    alldat$varOM[i] = varOMref[,techtypesOM[,alldat$technologyAll[i]]]
#  } else if(!is.na(alldat$PLFUELCT[i])){  # else, use fueltype to index into varOMref
#    alldat$varOM[i] = varOMref[,fueltypesOM[,alldat$PLFUELCT[i]]]
  } else {
    alldat$varOM[i] = 0
    print(paste0("Generator ",alldat$PNAME[i]," at i=",i," has no Technology or Fuel"))
  }
}


#---------------------#
# Match Fuel costs ####
#---------------------#

# load fuel prices $/MBtu (except for nuclear)
fuelcosts = read_excel(path = paste0(datain, "Fuel_costs_rough.xlsx"),skip=1, range = "A2:G8")

# create dataframe with Technology as column
fuelcost_ref = array(dimnames = list(c(),as.vector(fuelcosts$Fuel)),
                 dim = c(1,nrow(fuelcosts)),
                 data = fuelcosts$Mean)

# create a dictionary that maps from primary fuel in alldat to fuel
# use Technology since this differentiates between different generators within a plant
techtypesF = array(dimnames = list(c(),sort(unique(alldat$Technology))),
                  dim=c(1,length(sort(unique(alldat$Technology)))),
                  data = c("Other","Other",
                           "Renewable","Coal",
                           "Renewable","NatGas",
                           "NatGas","NatGas",
                           "NatGas","Nuclear",
                           "Renewable","NatGas", #Not sure about mapping of "Other Gases" to "Other" or "natgas"
                           "Renewable","Distillate fuel oil", # Not sure about mapping "Petroleum Coke" to "Petroleum"
                           "Distillate fuel oil","Renewable",
                           "Renewable"
                           ))

# For entries without a techology listed, map the primary fuel to varOM
# fueltypes = array(dimnames = list(c(),sort(unique(alldat$PLFUELCT))),
#                   dim=c(1,length(sort(unique(alldat$PLFUELCT)))),
#                   data = c("Renewable","Coal","NatGas","Renewable","Nuclear","Other",
#                            "Distillate fuel oil","Other","Renewable","Renewable"))


# create col for fuel cost
alldat$fuel_dollars_mmbtu = NA
for(i in 1:nrow(alldat)){
  # if Tech is not NA, use that to index into fuelcost_ref
  if(!is.na(alldat$technologyAll[i])){
    alldat$fuel_dollars_mmbtu[i] = fuelcost_ref[,techtypesF[,alldat$technologyAll[i]]]
  # } else if(!is.na(alldat$PLFUELCT[i])){  # else, use fueltype to index into fuelcost_ref
  #   alldat$fuel_dollars_mmbtu[i] = fuelcost_ref[,fueltypes[,alldat$PLFUELCT[i]]]
  } else {
    alldat$fuel_dollars_mmbtu[i] = 0
    print(paste0("Generator ",alldat$PNAME[i]," at i=",i," has no Technology or Fuel"))
  }
}

#-----------------------#
# fill in heat rates ####
#-----------------------#

# many generators (203 of 383!) have no heat rate :(

# find average by technology
htrtavg = alldat %>%
  group_by(technologyAll) %>%
  summarise(heatrate = mean(PLHTRT,na.rm=T))
# Renewables, batteries and nuclear have no heat rate given
# flip so that "Technology" is column name
htrtavg_ref = array(dimnames = list(c(),as.vector(htrtavg$technologyAll)),
                 dim = c(1,nrow(htrtavg)),
                 data = htrtavg$heatrate)


# calculate fuel cost per MWh with heat rate (btu/kWh)
convert_mmbtu_btu = 1/1e6 #convert MBtu -> btu, 
convert_kWh_MWh = 1e3 #convert kWh -> MWh

# special case for Nuclear
nuclear_sel = which(alldat$PLFUELCT == "NUCLEAR")
fuel_nuc = which(fuelcosts$Fuel =="Nuclear")
#alldat$fuel_dollars_MWh[nuclear_sel] = fuelcosts$Nuclearcost[fuel_nuc]

# assign heat rates and calculate fuel costs per MWh
# handling special cases for renewables and nuclear (no heat rate given)
errorseq = NA
for(i in 1:nrow(alldat)){
  # if heat rate given, use that
  if(!is.na(alldat$PLHTRT[i])){
    alldat$fuel_dollars_MWh[i] = (alldat$fuel_dollars_mmbtu[i] *convert_mmbtu_btu) * (alldat$PLHTRT[i] * convert_kWh_MWh)
    # heat rate is btu/kWh
    # fuel cost is $/Mbtu
    # ($/kWh) = ($/btu)*(btu/kWh)
    
  }else{ # eiher get Technology, or Technology from PLFUELCT, then get heat rate
    # if(!is.na(alldat$Technology[i])){
    #   thistech = alldat$Technology[i]
    # }else if(!is.na(alldat$PLFUELCT[i])){
    #   thistech = PLFUELCT_Technology[1,alldat$PLFUELCT[i]]
    # }else{
    #   thistech = NA
    # }
  

    # Get heat rate from average values
    # Deal with categories that have no heat rate
    thistech = alldat$technologyAll[i]
    if(!is.na(thistech)){
      if(!is.na(htrtavg_ref[1,thistech])){
        alldat$fuel_dollars_MWh[i] = (alldat$fuel_dollars_mmbtu[i] *convert_mmbtu_btu) * 
          (htrtavg_ref[1,thistech] * convert_kWh_MWh)
      } else if(!is.na(thistech)){
      # if htrtavg is NA
        #if Nuclear
        if(thistech == "Nuclear"){
          alldat$fuel_dollars_MWh[i] = fuelcosts$Nuclearcost[fuel_nuc]
        }else if(is.na(htrtavg_ref[1,thistech])){
         # maps to renewables, batteries
          # NB: ASSUMES THAT BIOMASS IS FREE
          alldat$fuel_dollars_MWh[i] = 0
        }
      }
    } else { #thistech is NA
      alldat$fuel_dollars_MWh = NA
      print(paste0("Generator ",alldat$PNAME[i]," at i=",i," has no Technology assigned"))
      errorseq=c(errorseq,i)
    }
  }
}

#-----------------------#
# Sum Fuel, O&M cost ####
#-----------------------#
alldat$VCost = alldat$fuel_dollars_MWh + alldat$varOM

#--------------------------------#
# remove some types of plants ####
#--------------------------------#

# remove plants with no fuel/MWh cost
length(which(is.na(alldat$fuel_dollars_MWh)))
# turns out this is a null set

# remove 'all other' plants
allothersel = (alldat$technologyAll == "All Other")
alldat_sub = alldat[!allothersel,]

# remove 'battery' plants
battsel = which(alldat_sub$technologyAll != "Batteries")
alldat_sub = alldat_sub[battsel,]

# remove plants that only exist in EIA dataset

#---------------------------#
# fill in missing PMin   ####
#---------------------------#
# code adapted from merge_gen_dat_ramp.R
# create a linear approximation by type
# and print results of approx

# what plants are missing pmin?
missing_pmin = is.na(alldat_sub$MinLoadMW)
types_missing = unique(alldat_sub$technologyAll[missing_pmin])

for(i in 1:length(types_missing)){
  gensel = str_detect(string = alldat_sub$technologyAll, pattern = types_missing[i])
  
  # there is more than one other plant of this type, we can do a linear regression 
  if(sum(gensel & !missing_pmin) > 1){
    # make a linear approximation
    print(paste("Generator type", types_missing[i],"has",sum(gensel & !missing_pmin),"plants to approximate with"))
    
    fit = lm(alldat_sub$MinLoadMW[gensel & !missing_pmin] ~ alldat_sub$capacityAll[gensel & !missing_pmin])
    intercept = fit$coefficients[1]
    slope = fit$coefficients[2]
    
    alldat_sub$MinLoadMW[gensel & missing_pmin] = max(intercept + slope*alldat_sub$capacityAll[gensel & missing_pmin],
                                                      0)
    
  } else if(sum(gensel & !missing_pmin) == 1){
    # there is only one other plan of this type, use a ratio
    ratio = mean(alldat_sub$MinLoadMW[gensel & !missing_pmin] / alldat_sub$capacityAll[gensel & !missing_pmin], na.rm=T)
    print(paste("Generator type", types_missing[i],"only has one other plant to base pmin on"))
    
    # fill in for missing plants
    alldat_sub$MinLoadMW[gensel & missing_pmin] = alldat_sub$capacityAll[gensel & missing_pmin] * ratio
  } else {
    # there are no other plants of this type, you're screwed
    print(paste("Generator type", types_missing[i],"does not have other plants with a PMin assign"))
    alldat_sub$MinLoadMW[gensel & missing_pmin] = 0
  }
    
}

# inspect
# ggplot(alldat_sub, aes(x=capacityAll, y=MinLoadMW)) + geom_point() + facet_wrap(~technologyAll)

#------------------------------------#
# fill in missing CO2 emissions   ####
#------------------------------------#
alldat_sub$PLC2ERTA[alldat_sub$Fuel == "BIOMASS"] = 0
alldat_sub$PLC2ERTA[alldat_sub$Fuel == "LANDFILL_GAS"] = 0
alldat_sub$PLC2ERTA[alldat_sub$Fuel == "SOLAR"] = 0
alldat_sub$PLC2ERTA[alldat_sub$Fuel == "HYDRO"] = 0
alldat_sub$PLC2ERTA[alldat_sub$Fuel == "WIND"] = 0

missing_co2=is.na(alldat_sub$PLC2ERTA)
types_missing = unique(alldat_sub$Fuel[missing_co2])
for(i in 1:length(types_missing)){
    gensel = str_detect(string = alldat_sub$Fuel, pattern = types_missing[i])
  # if there is more than one other plant of this type with the data, we can take the average
    if(sum(gensel & !missing_co2)){
      avg_co2 = mean(alldat_sub$PLC2ERTA[gensel & !missing_co2])
      alldat_sub$PLC2ERTA[gensel & missing_co2] = avg_co2
    } else {
      print(paste("generator type",types_missing[i],"has no examples with CO2 emissions"))
    }

}


#-----------------------#
# Create unique ID   ####
#-----------------------#

##### create a unique identifier for each plant #
# because some plant types are subsets of other plant type names, must use FACTORS, not strings... ugh
counter = 1
plantUnique = vector(length = dim(alldat_sub)[[1]])
for (i in unique(alldat_sub$Fuel)){
  # whichplants = which(str_detect(alldat_sub$Fuel,i))
  whichplants = which(alldat_sub$Fuel==i)
  for(j in 1:length(whichplants)){
    plantUnique[whichplants[j]]=paste(as.character(i),"-",j,sep="")
    counter = counter+1
  }
}
# View(cbind(alldat_sub$technologyAll, alldat_sub$Fuel, plantUnique))
alldat_sub$plantUnique = plantUnique
# View(cbind(alldat_sub$technologyAll, alldat_sub$Fuel, alldat_sub$plantUnique))

#-----------------------#
# add ramp rates     ####
#-----------------------#
# merge in ramp rate data on the basis of $Fuel
alldat_sub = merge(x = alldat_sub, y = ramp_rate, all.x=T, by.x = "Fuel",by.y = "PLANTTYPE")

#check
# View(alldat_sub[is.na(alldat_sub$ramprate),])
sum(is.na(alldat_sub$ramprate))

#-----------------------#
# add startup cost   ####
#-----------------------#
alldat_sub$StartCost = 0

for(i in 1:nrow(startup_cost)){
  gensel = str_detect(alldat_sub$Fuel, pattern = startup_cost$technlogy[i])
  sizesel = (alldat_sub$capacityAll >= startup_cost$min_capacity[i] &
               alldat_sub$capacityAll < startup_cost$max_capacity[i])
  
  alldat_sub$StartCost[gensel & sizesel] = startup_cost$start_cost[i] * alldat_sub$capacityAll[gensel & sizesel]
}

#inspect
# ggplot(alldat_sub, aes(x= capacityAll, y=startCost)) + geom_point() + facet_wrap(~Fuel)
# there are no coal plants in the dataset <300 MW

#-----------------------#
# Convert units      ####
#-----------------------#
# taken from write_julia_generator_data.R
# we want $/GW or $/GWh (convert from $/MW : *1000)
# so far everything is in $/MW
# WEIRD: why does merge_gen_dat_ramp.R / write_julia_generator_data.R 
#         leave capacity in MW, but var cost and start cot in GW?

# var cost

# start cost

# capacity?

# rename some columns for model compatability
names(alldat_sub)[names(alldat_sub) == 'MinLoadMW'] <- 'PMin'
names(alldat_sub)[names(alldat_sub) == 'Capacity'] <- 'EIACapacity'
names(alldat_sub)[names(alldat_sub) == 'capacityAll'] <- 'Capacity'

#-----------------------#
# Add a DR unit      ####
#-----------------------#

dr_row = as_tibble(matrix(nrow = 1, ncol = ncol(alldat_sub),
                          dimnames = list(c(),names(alldat_sub))))

dr_row$Fuel = "DR"
dr_row$PNAME = "test DR"
dr_row$Capacity = 1000
dr_row$PMin = 0
dr_row$StartCost = 0
dr_row$VCost = 30
dr_row$ramprate = 1
dr_row$plantUnique = "DR-1"
dr_row$PLC2ERTA = 0

alldat_sub = rbind(alldat_sub, dr_row)

#----------------------------#
# Insepect                ####
#----------------------------#
desiredcols = c("BANAME","PNAME","PlantName.eGrid","PlantName.eia",
                "PLPRMFL","PLFUELCT","Technology","technologyAll","Fuel","plantUnique",
                "supercritical",
                "PLHTRT",
                "NAMEPCAP","EIACapacity","Capacity",
                "varOM","fuel_dollars_mmbtu",
                "fuel_dollars_MWh","VCost",
                "PMin", "StartCost","ramprate",
                "OperatingYr1.eGrid","OperatingYr2.eGrid","RetirementYr1.eGrid","RetirementYr2.eGrid",
                "CAPFAC", # Plant capacity factor
                "CHPFLAG", # Combined heat and power (CHP) plant adjustment flag
                "PSFLAG", # Plant pumped storage flag
                "PLNOXRTA", # Plant annual nox emission rate lb/MWh
                "PLSO2RTA", # Plant annual so2 emission rate lb/MWh
                "PLC2ERTA", # Plant annual CO2 equivalent total output emission rate (lb/MWh)
                "PLGENAHY", # Plant annual hydro net generation (MWh)
                "PLTRPR", # Plant total renewables generation percent (resource mix)
                "LAT","LON"
)
# View(alldat_sub[,desiredcols])


# compared to the plot in http://blogs.edf.org/energyexchange/2012/07/19/the-texas-electric-market-isnt-being-manipulated-its-just-built-that-way-and-thats-not-a-good-thing/
# the distribution of variable costs that I found seems reasonable

#-----------------------#
# Save relevant cols ####
#-----------------------#
savedat = alldat_sub[,desiredcols]
write_csv(savedat,path=paste0(dataout,"unformatted data/complete_generator_listing_ERCOT_012019.csv"))

write_csv(alldat_sub,path = paste0(dataout,"unformatted data/all_cols_complete_generator_listing_ERCOT_012019.csv"))
