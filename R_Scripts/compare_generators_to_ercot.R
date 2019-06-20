# compare_generators_to_ERCOT.R

library(readxl)
library(tidyverse)

gendatfol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/ercot_default/"
gendatfile = "complete_generator_EIAonly_alldata.csv"
ercotfile = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/primary documents/CapacityDemandandReserveReport_May2016.xlsx"
datafile = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/"

mygens = read_csv(paste0(gendatfol,gendatfile))
# remove row with "NA" name
mygens = mygens[!is.na(mygens$Plant.Name),]

# only load & compare thermal capacity
truegens = read_xlsx(ercotfile,sheet = "SummerCapacities",
                     # skip=1,
                     range = "B2:S345")
# remove first row of truegens because it has more colname stuff
truegens = truegens[2:nrow(truegens),]
truegens = truegens[truegens$FUEL != "STORAGE",]

# read "switchable operational resources"
# B385:S401
switchgens = read_xlsx(ercotfile,
                      sheet = "SummerCapacities",range = "B385:S401",
                      col_names = names(truegens))
# read "Planned thermal resources with exectuted...
# B593:S609
plantherm = read_xlsx(ercotfile,
                     sheet = "SummerCapacities",range = "B593:S609",
                     col_names = names(truegens)) 

truegens = rbind(truegens, switchgens,plantherm)

# make unique ID for truegen
truegens$ID = c(1:nrow(truegens))

# need to create a mapping from generator types in ercot data to my data
# COAL --> COAL
# NUCLEAR --> NUCLEAR
# GAS --> GAS, GAS_CC, GAS_CT, GAS_ICE, GAS_ST
# BIOMASS --> BIOMASS, LANDFILL_GAS
fuelmap = list(c("NUCLEAR"),c("COAL"),
               c("GAS", "GAS_CC", "GAS_CT", "GAS_ICE", "GAS_ST"),
               c("BIOMASS","LANDFILL_GAS"))
f = c("NUCLEAR","COAL","GAS","BIOMASS"
)

for(i in 1:length(f)){
  thisercot = truegens[truegens$FUEL==f[i],]
  thismygens = mygens[mygens$Fuel %in% fuelmap[[i]],]
  thismygens$ercotID = list(NA)
  thismygens$ercotName = NA
  thismygens$ercotCap = NA
  thismygens$ercotNMatch = NA
  thismygens$ercotIDstring = NA
  # try to do a string match of names
  # how?
  # match on the first word of name
  
  # which dataset should I iterate over?
  # I do want to save both
  
  # iterate over my dataset
  for(j in 1:nrow(thismygens)){
    # look at length of all words in name. Take first with length >1
    nwords = sum(!is.na(word(thismygens$Plant.Name[j],1:10)))
    whichgenname = min(which(nchar(word(thismygens$Plant.Name[j],1:10)) > 1))
    
    mygenname = word(thismygens$Plant.Name[j],whichgenname)
    # unitmatch = which(toupper(mygenname[j]) %in% thisercot$`UNIT NAME`)
    unitmatch = grep(toupper(mygenname),toupper(thisercot$`UNIT NAME`))
    
    # if more than one match, expand name to first two words
    if(length(unitmatch) >1 & nwords >1){
      mygenname2 = word(thismygens$Plant.Name[j],1,2)
      unitmatch2 = grep(toupper(mygenname2),toupper(thisercot$`UNIT NAME`))
      
      # if that worked (between 1-20 matches), overwrite
      if(length(unitmatch2) != 0 & length(unitmatch2) < 20){
        unitmatch = unitmatch2
      } else if(length(unitmatch) >=20){
        # if both approaches have too many/no matches, assign no matches
        unitmatch = integer(0)
      }
    }
    
    # save IDs that match
    thismygens$ercotID[j] = list(thisercot$ID[unitmatch])
    thismygens$ercotIDstring[j] = toString(thisercot$ID[unitmatch])
    
    # save name and capacity
    if(length(unitmatch) ==1 ){
      thismygens$ercotName[j] = thisercot$`UNIT NAME`[unitmatch]
      thismygens$ercotCap[j] = thisercot$`2017`[unitmatch]
      thismygens$ercotNMatch[j] =1
    } else { #>1
      thismygens$ercotName[j] = thisercot$`UNIT NAME`[unitmatch[1]]
      thismygens$ercotCap[j] = sum(thisercot$`2017`[unitmatch],na.rm=T)
      thismygens$ercotNMatch[j] = length(unitmatch)
    }
  }
  View(thismygens[,c("Plant.Name","ercotName","Capacity","ercotCap","Fuel","ercotNMatch","ercotID")])
  
  # identify generators in thisercot that were assigned to multiple generators
  dupes1 = duplicated(unlist(thismygens$ercotID))
  dupes2 = duplicated(unlist(thismygens$ercotID),fromLast = T)
  dupesTot = dupes1|dupes2
  thismygens$duplicatedMatch = relist(dupesTot,thismygens$ercotID)
  
  # print amt duplicated capacity
  dupeind = which(dupes1)
  print(paste("There is", sum(thisercot$`2017`[thisercot$ID %in% dupeind]), "MW duplicated",f[i],"capacity"))
  
  # Identify which generators in thisercot did not find a match
  # by inspecting thismygens$ercotID
  missinggens = which(!(unique(thisercot$ID) %in% unique(unlist(thismygens$ercotID))))
  # make a dataframe with data fom thisercot[missing,] that has same colnames as thismygens
  tem = thisercot[missinggens,]
  missingercot = data.frame(ercotName = tem$`UNIT NAME`, ercotCap = tem$`2017`, ercotID = tem$ID)
  print(paste("There is",sum(tem$`2017`),"MW of unmatched ERCOT", f[i], "capacity."))
  library(plyr)
  thismygens = rbind.fill(thismygens, missingercot)
  library(dplyr)
  
  # write to CSV for manual inspection
  # where I will manually determine (1) keep my generator (TF), or (2) add ercot generator (TF)
  savemygens = select(thismygens, Plant.Name, Fuel, ercotName, Capacity, ercotCap, Fuel, ercotNMatch, ercotIDstring, 
                      OperatingYr1.eia, OperatingYr2.eia, RetirementYr1.eia, RetirementYr2.eia, 
                      OperatingYr1.eGrid, OperatingYr2.eGrid, RetirementYr1.eGrid, RetirementYr2.eGrid)
  write_csv(savemygens, paste0(datafile, "comparegens_opsandretire_",f[i],".csv"))
}


### STEP TWO ###
# Load GAS comparison and extract discarded rows
# in order to recalculate ercot generators which dont have a match
library(readxl)
comparegas = read_xlsx(paste0(datafile,"comparegens_GAS.xlsx"))
discardedID = comparegas$ID[which(comparegas$discard==1)]

thismygens$ercotCap[discardedID] = NA
thismygens$ercotName[discardedID] = NA
thismygens$ercotNMatch[discardedID] = NA
thismygens$ercotID[discardedID] = NA
# Identify which generators in thisercot did not find a match
# by inspecting thismygens$ercotID
missinggens = which(!(unique(thisercot$ID) %in% unique(unlist(thismygens$ercotID))))
# make a dataframe with data fom thisercot[missing,] that has same colnames as thismygens
tem = thisercot[missinggens,]
print(paste("There is",sum(tem$`2017`),"MW of unmatched ERCOT", f[i], "capacity."))
missingercot = data.frame(ercotName = tem$`UNIT NAME`, ercotCap = tem$`2017`, ercotID = tem$ID)
write_csv(missingercot,paste0(datafile, "secondround_missinggens_",f[i],".csv"))

### STEP THREE ###
# Load processeed CSV
# match to mygens on the baseis of name/fuel or ID
# update based on "discard" and "useErcotCap"
# save
comparegas = read_xlsx(paste0(datafile,"comparegens_GAS.xlsx"), range = "A1:N199")
mygens = read_csv(paste0(gendatfol,gendatfile))

mygens$discard = NA
mygens$useErcotCap = NA
mygens$ercotCap = NA

for(g in 1:nrow(comparegas)){
  # print(paste("g=",g))
  mygenmatch = which((mygens$Plant.Name %in% comparegas$Plant.Name[g]) & 
                       mygens$Fuel %in% comparegas$Fuel[g])
  # print(paste("Matching plants:"))
  # print(comparegas$Plant.Name[g])
  # print(mygens$Plant.Name[mygenmatch])
  # print(paste("fuel types:"))
  # print(comparegas$Fuel[g])
  # print(mygens$Fuel[mygenmatch])
  
  if(length(mygenmatch) == 0){
    print("no match for plant")
    print(paste(comparegas$Plant.Name[g],"AKA", comparegas$ercotName[g]))
  }
  
  mygens$discard[mygenmatch] = comparegas$discard[g]
  mygens$useErcotCap[mygenmatch] = comparegas$useErcotCap[g]
  mygens$ercotCap[mygenmatch] = comparegas$ercotCap[g]
}

# remove discarded plants
# rmsel = (mygens$discard==1)
keepsel = mygens$discard!=1 | is.na(mygens$discard)
mynewgens = mygens[which(keepsel),]

# update capacity
updatesel = which(mynewgens$useErcotCap ==1)
mynewgens$Capacity[updatesel] = mynewgens$ercotCap[updatesel]

# check
View(mynewgens[str_detect(mynewgens$Fuel,"GAS"),
               c("Plant.Name","Capacity","Fuel")])

# write genset
write_csv(mynewgens, paste0(datafile,"julia_input/ercot_default/complete_generator_correctedGas_3-12-19_alldata.csv"))
mynewsavegens = select(mynewgens,
                       Fuel, PMin, Capacity, varOM, VCost, plantUnique, ramprate, StartCost, PLC2ERTA)
write_csv(mynewsavegens, paste0(datafile,"julia_input/ercot_default/complete_generator_correctedGas_3-12-19.csv"))

# For each fuel type:
# (1) try to do a rough string match
# (2) save a csv with generators from both files
# including names, capacities, and an indicator of 
# which dataset the generator appears in
# then I can manually sort the generators


