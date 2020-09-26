# make_SI_figs.R
# make some basic figures for the 
# supplementary info
# Aug 2020
# Patricia Levi


library(tidyverse)
library(lubridate)

######## Demand Uncertainty ##########
inputs_dir = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/"
arma_loc = paste0(inputs_dir, "5d_6o_keyDays2/")
demand = read.csv(paste0(inputs_dir, "ercot_default/ercot_demand_2016.csv"))
demand$p = 1:nrow(demand)

armalist = list.files(arma_loc, pattern = glob2rx("demandScenarios_vdem_ARMA*o25*"))

# trim=3
# for(i in 1:length(armalist)){
i=5
  firstp = as.numeric(strsplit(armalist[i], "_")[[1]][6])
  lastp = as.numeric(strsplit(armalist[i], "_|\\.")[[1]][8])
  thisarma = read.csv(paste0(arma_loc, armalist[i]))
  thisarma$p = firstp:lastp
  thisarma = arrange(thisarma,p)
  # thisarma= thisarma[(trim+1):(nrow(thisarma)-trim),]
  
  # if(i == 1){
  #   allarma = thisarma
  # } else {
  #   allarma = rbind(allarma, thisarma)
  # }
# }

# allarma2 = arrange(allarma, p)
# sum(duplicated(allarma2$p))
# 
# randemand = merge(randemand, allarma,by="p", all.x=T)
# sum(duplicated(allarma2$p))
  

dem = demand$demand[firstp:lastp]
mat = dem * thisarma[,1:25]
mat$t = firstp:lastp
mat$date = as.POSIXct(mdy("01-01-2016") + hours(mat$t))
matlong = pivot_longer(mat, cols=V1:V25, names_to = "scenario",values_to = "demand")
matlong$scenarionum = as.numeric(substr(matlong$scenario,2,100))

ggplot(matlong, aes(x = date, y = demand)) + geom_line() +
  facet_wrap(~scenario)
ggplot(filter(matlong,scenarionum <6), aes(x = date, y = demand, color = scenario)) + geom_line() +
  theme_minimal() + theme(legend.position = "bottom") + 
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(x = "Date (2016)",
       y = "Demand (GW)") +
  ggsave(filename = "demand_examples.png",path = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020",
         width = 7*1.2, height = 3*1.2)

matgroup = matlong %>%
  mutate(demandGW = demand/1000) %>%
  group_by(date) %>%
  summarise(meandemand = mean(demandGW),
            Q1 = quantile(demandGW,probs = 0.25),
            Q3 = quantile(demandGW,probs = 0.75),
            mind = min(demandGW),
            maxd = max(demandGW)) 

ggplot(matgroup, aes(x = date)) + 
  geom_line(aes(y = meandemand)) + 
  geom_line(aes(y = Q1), linetype = "dashed") +
  geom_line(aes(y = Q3), linetype = "dashed") +
  geom_line(aes(y = mind), linetype = "dotted") +
  geom_line(aes(y = maxd), linetype = "dotted") +
  theme_minimal()

ggplot(matgroup, aes(x = date)) + 
  geom_line(aes(y = meandemand, color = "black")) + 
  geom_ribbon(aes(ymin=mind, ymax=maxd, fill = "blue"), alpha=0.2) +
  geom_ribbon(aes(ymin=Q1, ymax=Q3, fill = "black"), alpha=0.4) +
  scale_fill_manual(name = '', values = c("blue"="blue","black"="black"),labels = c("Interquartile range","Min to max")) + 
  scale_color_manual(name = '', values = c("black" = "black"), labels = c("Mean")) +
  guides(fill = guide_legend(override.aes= list(alpha = 0.2))) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  labs(x = "Date (2016)",
       y = "Demand (GW)") +
  ggsave(filename = "demand_uncertainty.png",path = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020",
         width = 7*1.2, height = 3*1.2)


############ Generator supply curve #######
xx = read.csv(file = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/ercot_default/complete_generator_correctedGas_Start_Ramp_8-28-19.csv")
ordered = order(xx[,"VCost"]) # gives the indices of VCost in ascending value
sorted = xx[,"Capacity"][ordered]
cumsorted = cumsum(sorted)

plotdf <- data.frame(x=cumsorted, 
                     cost=xx[,"VCost"][ordered],
                     totcost = (xx[,"VCost"][ordered] + xx[,"StartCost"][ordered]/120), #spread startup costs over 264 hours for full_test
                     Technology=xx[,"Fuel"][ordered],
                     cap = xx[,"Capacity"][ordered],
                     GENID = xx[,"plantUnique"][ordered])
plotdf$loc <- cumsum(plotdf$cap) - plotdf$cap/2
plotdf$cost[which(plotdf$cost==0)] = -5

plotdf$Technology = factor(plotdf$Technology,levels = c("BIOMASS","COAL", "DR", "GAS_CC", "GAS_CT", "GAS_ICE", "GAS_ST", "HYDRO", "LANDFILL_GAS", "NUCLEAR", "OIL", "SOLAR", "WIND"))
cbbPalette <- c( "#999999",#biomass
                 "black",#coal
                 "purple",  #DR
                 "#56B4E9", #gas CC
                 "#E69F00", # gas CT
                 "#a6cee3", #gas ICE
                 "#0072B2", #gas ST
                 "#000099", #hydro
                 "#b2df8a",  #landfill gas
                 "#009E73",#nuclear
                 "#CC0000", #oil
                 "yellow",#solar
                 "#CC79A7",#wind
                 "#F0E442",
                 "green",
                 "red")


ggplot(plotdf, aes(x=loc,y=cost,width=cap,fill=Technology)) + 
  #  geom_point(aes(color=Technology)) +
  #  geom_bar(binwidth = 0,aes(fill=Technology), stat="identity", position="identity") +
  geom_bar(stat="identity") +
  scale_fill_manual(values=cbbPalette) +
  labs(x="Cumulative Capacity (MW)",y="Variable Cost ($/MWh)") + 
  ggtitle(paste("ERCOT's Electriciy Supply Curve with $35/MWh DR")) + 
  theme(legend.position = "bottom")

## without DR
xx = read.csv(file = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/ercot_default/complete_generator_correctedGas_Start_Ramp_8-28-19.csv")
xx = filter(xx, Fuel != "DR", Capacity >0)
ordered = order(xx[,"VCost"]) # gives the indices of VCost in ascending value
sorted = xx[,"Capacity"][ordered]
cumsorted = cumsum(sorted)

plotdf <- data.frame(x=cumsorted, 
                     cost=xx[,"VCost"][ordered],
                     totcost = (xx[,"VCost"][ordered] + xx[,"StartCost"][ordered]/120), #spread startup costs over 264 hours for full_test
                     Technology=xx[,"Fuel"][ordered],
                     cap = xx[,"Capacity"][ordered],
                     GENID = xx[,"plantUnique"][ordered])
plotdf$loc <- cumsum(plotdf$cap) - plotdf$cap/2
plotdf$cost[which(plotdf$cost==0)] = -2

plotdf$Technology = factor(plotdf$Technology,levels = c("BIOMASS","COAL", "GAS_CC", "GAS_CT", "GAS_ICE", "GAS_ST", "HYDRO", "LANDFILL_GAS", "NUCLEAR", "OIL", "SOLAR", "WIND"))

cbbPalette <- c( "#999999",#biomass
                 "black",#coal
                 # "purple",  #DR
                 "#56B4E9", #gas CC
                 "#E69F00", # gas CT
                 "#a6cee3", #gas ICE
                 "#0072B2", #gas ST
                 "#000099", #hydro
                 "#b2df8a",  #landfill gas
                 "#009E73",#nuclear
                 "#CC0000", #oil
                 "yellow",#solar
                 "#CC79A7",#wind
                 "#F0E442",
                 "green",
                 "red")
ggplot(plotdf, aes(x=loc,y=cost,width=cap,fill=Technology)) + 
  #  geom_point(aes(color=Technology)) +
  #  geom_bar(binwidth = 0,aes(fill=Technology), stat="identity", position="identity") +
  geom_bar(stat="identity") +
  labs(x="Cumulative Capacity (MW)",y="Variable Cost ($/MWh)") + 
  ggtitle(paste("Modeled Electriciy Supply Curve")) + theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = "", values=cbbPalette, 
                    labels = c("Biomass","Coal","Gas Combined Cycle","Gas Combusion Turbine","Gas Internal Combusion Engine","Gas Steam Turbine","Hydro","Landfill Gas","Nuclear","Oil","Solar","Wind")) +
  ggsave(filename = "supply_curve.png",path = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020",
         width = 8, height = 5)

