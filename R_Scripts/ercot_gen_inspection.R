# ercot_gen_inspection.R
# inspect generator data - ERCOT
# create supply curve from generator data
# March 2019

library(tidyverse)
dataout = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/"
# gendat = read.csv(file = paste0(dataout,"unformatted data/all_cols_complete_generator_listing_ERCOT_012019.csv"))
gendat = read.csv(file = paste0(dataout,"julia_input/ercot_default/complete_generator_correctedGas_3-12-19_alldata.csv"))
plotsfol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/plots/ERCOT_data/"
# for supply curve with demand
useddemand = read_csv(file = file.choose())

## working towards ideal ##
# slow_gens = c("HYDRO",
             # "COAL","NUCLEAR","LANDFILL_GAS",
             # "BIOMASS","GAS","GAS_CC")
# fast_gens = c("GAS_CT","GAS_ST", "GAS_ICE","OIL", "SOLAR", "WIND")
# HYDRO?

## current model ##
slow_gens = c("HYDRO",
              "COAL","NUCLEAR","LANDFILL_GAS",
              "BIOMASS","GAS_ST","GAS","GAS_CC")
fast_gens = c("GAS_CT","GAS_ICE","OIL", "SOLAR", "WIND")


# taken from "plat_level_data_readin_DR.R"
# plot cumulative supply ####
# REMOVE DR
DRsel = gendat$Fuel == "DR"
gendat1 = gendat
gendat = gendat1[!DRsel,]

ordered = order(gendat[,"VCost"]) # gives the indices of VCost in ascending value
sorted = gendat[,"Capacity"][ordered]
cumsorted = cumsum(sorted)
plotdf <- data.frame(x=cumsorted, 
                     cost=gendat[,"VCost"][ordered]/1e3,
                     totcost = (gendat[,"VCost"][ordered] + gendat[,"StartCost"][ordered]/264)/1e3, #spread startup costs over 264 hours for full_test
                     Technology=gendat[,"Fuel"][ordered],
                     cap = gendat[,"Capacity"][ordered],
                     GENID = gendat[,"plantUnique"][ordered])
plotdf$loc <- cumsum(plotdf$cap) - plotdf$cap/2
plotdf$cost[which(plotdf$cost==0)] = -0.001
# ggplot(plotdf, aes(x,y)) + geom_point(aes(color=Technology)) +labs(x="Cumulative Capacity (MW)",y="Variable Cost ($/MWh)") + ggtitle("Virginia's Electriciy Supply Curve") + theme(legend.position = c(.25,.65))

### add a column to plot df with FAST or SLOW
fastind = plotdf$Technology %in% fast_gens
plotdf$speed = "SLOW"
plotdf$speed[fastind] = "FAST"


# View(arrange(cbind(plotdf,fastind,plotdf$cost - plotdf$totcost),cost))

ggplot(plotdf[!DRsel,], aes(x=loc,y=cost,width=cap,fill=speed)) + 
  #  geom_point(aes(color=Technology)) +
  #  geom_bar(binwidth = 0,aes(fill=Technology), stat="identity", position="identity") +
  geom_bar(stat="identity") +
  # scale_fill_manual(values=cbbPalette) +
  scale_fill_manual(values=c("#5ab4ac","#d8b365"), name = "Generator Speed", labels = c("Fast","Slow")) +
  labs(x="Cumulative Capacity (MW)",y="Variable Cost ($/MWh)") + 
  ggtitle(paste("Texas' Electriciy Supply Curve: 2016")) + 
  theme(legend.position = "bottom") #+ 
  #  scale_y_continuous(limits=c(-10,60), minor_breaks = seq(0, 60, 5),breaks=seq(0,60,10)) +
  #  scale_y_continuous(limits=c(-10,300))#, minor_breaks = seq(0, 60, 5),breaks=seq(0,60,10)) +
  # scale_x_continuous(limits=c(-10, 37500)) 
  ggsave(file = paste0(plotsfol,"supplycurve_ercot_speed_IAEEmodel.png"), width = 9, height = 4.55)


  # testing just a line on top of generator supply curve
ggplot(plotdf[!DRsel,], aes(x=loc,y=cost,width=cap)) + 
  #  geom_point(aes(color=Technology)) +
  #  geom_bar(binwidth = 0,aes(fill=Technology), stat="identity", position="identity") +
  geom_bar(stat="identity") +
  geom_line() +
  labs(x="Cumulative Capacity (MW)",y="Variable Cost ($/MWh)") + 
  ggtitle(paste("Texas' Electriciy Supply Curve: 2016")) + 
  theme(legend.position = "bottom") #+ 

  
  
  cbbPalette <- c( "purple", #biomass
                   "black", #coal
                   # "#CC79A7", #DR
                   "#56B4E9", #gas
                   "#a6cee3", # gas CC
                   "#0072B2", # gas ct
                   "#999999", #gas_ice
                   "#000099", #gas st
                   "#b15928",  #hydro
                   "#b2df8a",#landfill gas
                   "#CC0000", #nuclear
                   "#E69F00",#oil
                   "#F0E442" , #solar
                   "#009E73")#, #wind

ggplot(plotdf, aes(x=loc,y=cost,width=cap,fill=Technology)) + 
  #  geom_point(aes(color=Technology)) +
  #  geom_bar(binwidth = 0,aes(fill=Technology), stat="identity", position="identity") +
  geom_bar(stat="identity") +
  # scale_fill_manual(values=cbbPalette, name = "Generator Type", labels = c("Biomass","Coal","Unknown Gas","Gas Combined Cycle",
  #                                                                          "Gas Combustion Turbine","Gas Internal Combustion Engine",
  #                                                                          "Gas Steam Turbine","Hydro","Landfill Gas","Nuclear",
  #                                                                          "Oil-fired","Solar","Wind")) +
  scale_fill_brewer(type = "qual", palette = "Paired")+
  labs(x="Cumulative Capacity (MW)",y="Variable Cost ($/MWh)") + 
  ggtitle(paste("Texas' Electriciy Supply Curve: 2016")) + 
  theme(legend.position = "bottom") #+ 
  #  scale_y_continuous(limits=c(-10,60), minor_breaks = seq(0, 60, 5),breaks=seq(0,60,10)) +
  #  scale_y_continuous(limits=c(-10,300))#, minor_breaks = seq(0, 60, 5),breaks=seq(0,60,10)) +
  # scale_x_continuous(limits=c(-10, 37500)) 
  ggsave(file = paste0(plotsfol,"supplycurve_ercot_technology_IAEEmodel.png"), width = 9, height = 4.5)

# sum(plotdf$cap < 250 & plotdf$speed =="FAST")
# 
# View(plotdf[plotdf$cap <250 & plotdf$speed == "SLOW",])
# 
plot1 <- ggplot(plotdf, aes(x=loc,y=cost,width=cap,fill=speed)) + 
  #  geom_point(aes(color=Technology)) +
  #  geom_bar(binwidth = 0,aes(fill=Technology), stat="identity", position="identity") +
  geom_bar(stat="identity") +
  scale_fill_manual(values=cbbPalette) +
  labs(x="Cumulative Capacity (MW)",y="Variable Cost ($/MWh)") + 
  ggtitle(paste("Virginia's Electriciy Supply Curve: 2030")) + 
  theme(legend.position = "bottom") + 
  #  scale_y_continuous(limits=c(-10,60), minor_breaks = seq(0, 60, 5),breaks=seq(0,60,10)) +
  #  scale_y_continuous(limits=c(-10,300))#, minor_breaks = seq(0, 60, 5),breaks=seq(0,60,10)) +
  scale_x_continuous(limits=c(-10, 120000), breaks=seq(0,120000,10000)) 

library(cowplot)

plot2 <-  ggplot(useddemand, aes(y = 1:nrow(useddemand), x = demand )) + geom_point(shape=20,size=0.3) + coord_cartesian(xlim = c(-10,120000)) +
  labs(x = "demand MW", y = "timestep")
plot_grid(plot1, plot2, ncol =1, align = "h", axis = "b") 
  ggsave(file = paste0(plotsfol,"validate_supplycurve_ercot_speed.png"), width = 7, height = 6)



 ### amount of capacity by type ##
gensum = gendat %>%
  group_by(Fuel) %>%
  summarise(totcap = sum(Capacity))

### inspect amount of gas capacity that might not exist ####
egridgas = str_detect(gendat$Fuel, "GAS") & is.na(gendat$PlantName.eia)
eiagas = str_detect(gendat$Fuel, "GAS") & is.na(gendat$PlantName.eGrid)
dupegas = str_detect(gendat$Fuel, "GAS") & duplicated(gendat$PlantName.eGrid)
dupegas2 = str_detect(gendat$Fuel, "GAS") & duplicated(gendat$PNAME)
dupegas2r = str_detect(gendat$Fuel, "GAS") & duplicated(gendat$PNAME, fromLast = T)
gassel = str_detect(gendat$Fuel, "GAS")

sum(egridgas)
sum(gendat$Capacity[egridgas])
sum(gendat$Capacity[eiagas])
sum(gendat$Capacity[dupegas])
sum(gendat$Capacity[dupegas2])
sum(gendat$Capacity[dupegas2r])
sum(gassel)
sum(gassel & !duplicated(gendat$PNAME))
sum(gendat$Capacity[dupegas | egridgas])
sum(gendat$Capacity[dupegas | eiagas])

dupegasall = str_detect(gendat$Fuel, "GAS") & (duplicated(gendat$PlantName.eGrid) | duplicated(gendat$PlantName.eGrid, fromLast = T))

View(arrange(gendat[which(dupegasall),c("PNAME","PlantName.eGrid","PlantName.eia","Fuel","Capacity")],PNAME))
gendat2 = gendat[!(dupegas | egridgas),]


gendat2 = gendat[!egridgas,]
gendatog = gendat
gendat = gendat2

gendat = gendatog

# look for other types of capacity that are duplicates
dupegen =  duplicated(gendat$PlantName.eGrid)
sum(gendat$Capacity[dupegen])
dupegen =  duplicated(gendat$PNAME)
sum(gendat$Capacity[dupegen])
dupegen =  duplicated(gendat$PlantName.eia)
sum(gendat$Capacity[dupegen])
# only meaningful duplicates are small oil starters at larger plants - shouldnt remove


# look for capacity that has already retired
yr = 2016
retiredsel = gendat$OperatingYr1.eGrid <= yr | gendat$OperatingYr2.eGrid <= yr | 
              gendat$OperatingYr1.eia <= yr | gendat$OperatingYr2.eia <= yr 
sum(gendat$Capacity[retiredsel], na.rm=T)
View(gendat[which(retiredsel),c("RetirementYr1.eGrid","RetirementYr2.eGrid","RetirementYr1.eia","RetirementYr2.eia")])

retiredsel = gendat$OperatingYr1.eGrid <= yr | 
  gendat$OperatingYr1.eia <= yr 
sum(gendat$Capacity[retiredsel], na.rm=T)

futuresel = gendat$OperatingYr1.eGrid > yr | gendat$OperatingYr2.eGrid > yr|
          gendat$OperatingYr1.eia > yr | gendat$OperatingYr2.eia > yr
sum(gendat$Capacity[futuresel], na.rm=T)
View(gendat[which(futuresel),c("OperatingYr1.eGrid","OperatingYr2.eGrid","OperatingYr1.eia","OperatingYr2.eia","EIACapacity")])

futuresel = gendat$OperatingYr1.eGrid > yr |
  gendat$OperatingYr1.eia > yr 
sum(gendat$Capacity[futuresel], na.rm=T)

# plot cumulative demand ####
ordered = order(useddemand$demand) # gives the indices of demand in ascending value
# used to determine location of bars
sorted = gendat[,"demand"][ordered]
cumsorted = cumsum(sorted) 
# create dataframe
plotdf_demand <- data.frame(loc=nrow(useddemand):1, 
                     demand=useddemand$demand[ordered]/1e3)#,
                     # totcost = (gendat[,"VCost"][ordered] + gendat[,"StartCost"][ordered]/264)/1e3, #spread startup costs over 264 hours for full_test
                     # Technology=gendat[,"Fuel"][ordered],
                     # cap = gendat[,"Capacity"][ordered],
                     # GENID = gendat[,"plantUnique"][ordered])
# plotdf$loc <- cumsum(plotdf$cap) - plotdf$cap/2
# plotdf$cost[which(plotdf$cost==0)] = -0.001
# ggplot(plotdf, aes(x,y)) + geom_point(aes(color=Technology)) +labs(x="Cumulative Capacity (MW)",y="Variable Cost ($/MWh)") + ggtitle("Virginia's Electriciy Supply Curve") + theme(legend.position = c(.25,.65))

ggplot(plotdf_demand, aes(x=loc,y=demand)) + 
  #  geom_point(aes(color=Technology)) +
  #  geom_bar(binwidth = 0,aes(fill=Technology), stat="identity", position="identity") +
  # geom_bar(stat="identity") + 
  geom_line() + theme_bw() +
  labs(x="Hours (cumulative)",y="Demand (GW)") +
  ggtitle("ERCOT 2016 Load Duration Curve") 
ggsave(file = paste0(plotsfol,"loadDurationCurve_ercot_2016.png"), width = 7, height = 5)
  

## plot cumulative demand with supply curve ##
library(cowplot)
maxx = 110000
p1 = ggplot(plotdf_demand, aes(y=loc,x=demand*1000)) + 
  geom_line() + theme_bw() +
  labs(y="Hours (cumulative)",x="Demand (MW)") +
  ggtitle("ERCOT 2016 Inverse Load Duration Curve") +
  scale_x_continuous(limits=c(-10, maxx), breaks=seq(0,120000,10000)) 

p2 = ggplot(plotdf, aes(x=loc,y=cost,width=cap,fill=Technology)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=cbbPalette) + theme_bw() +
  labs(x="Cumulative Capacity (MW)",y="Variable Cost ($/MWh)") + 
  ggtitle(paste("Texas' Electriciy Supply Curve: 2016")) + 
  theme(legend.position = "none")+
  scale_x_continuous(limits=c(-10, maxx), breaks=seq(0,120000,10000)) 

plot_grid(p1, p2, ncol =1, align = "h", axis = "b") 

ggsave(file = paste0(plotsfol,"supply_and_demand_ercot.png"), width = 7, height = 6)



## make same plot but with wind removed
gendatnoRE = gendat[which(gendat$Fuel !="WIND" & gendat$Fuel !="SOLAR"),]
ordered = order(gendatnoRE[,"VCost"]) # gives the indices of VCost in ascending value
sorted = gendatnoRE[,"Capacity"][ordered]
cumsorted = cumsum(sorted)
plotdfnoRE <- data.frame(x=cumsorted, 
                     cost=gendatnoRE[,"VCost"][ordered]/1e3,
                     totcost = (gendatnoRE[,"VCost"][ordered] + gendatnoRE[,"StartCost"][ordered]/264)/1e3, #spread startup costs over 264 hours for full_test
                     Technology=gendatnoRE[,"Fuel"][ordered],
                     cap = gendatnoRE[,"Capacity"][ordered],
                     GENID = gendatnoRE[,"plantUnique"][ordered])
plotdfnoRE$loc <- cumsum(plotdfnoRE$cap) - plotdfnoRE$cap/2
plotdfnoRE$cost[which(plotdfnoRE$cost==0)] = -0.001

maxx = 110000
p1 = ggplot(plotdf_demand, aes(y=loc,x=demand*1000)) + 
  geom_line() + theme_bw() +
  labs(y="Hours (cumulative)",x="Demand (MW)") +
  ggtitle("ERCOT 2016 Inverse Load Duration Curve") +
  scale_x_continuous(limits=c(-10, maxx), breaks=seq(0,120000,10000)) 


p2 = ggplot(plotdfnoRE, aes(x=loc,y=cost,width=cap,fill=Technology)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=cbbPalette) + theme_bw() +
  labs(x="Cumulative Capacity (MW)",y="Variable Cost ($/MWh)") + 
  ggtitle(paste("Texas' Electriciy Supply Curve: 2016")) + 
  theme(legend.position = "none")+
  scale_x_continuous(limits=c(-10, maxx), breaks=seq(0,120000,10000)) 

plot_grid(p1, p2, ncol =1, align = "h", axis = "b") 

ggsave(file = paste0(plotsfol,"supply_and_demand_ercot_noRE.png"), width = 7, height = 6)

