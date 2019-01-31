# load fuel prices from EIA-Form-923 data ####
# turns out this doesn't really help because only 29 rows match, but I use this for avg values
# uses objects from setup_egrid_generators
# Jan 2019
eiafuel = read_excel(path = paste0(datain,"EIA-f923_2017/EIA923_Schedules_2_3_4_5_M_12_2017_Final_Revision.xlsx"), 
                     sheet = "Page 5 Fuel Receipts and Costs", skip=4, na=".")

# subselect eiafuel columns
eiafuel = eiafuel %>%
  select(`Plant Id`,`Plant Name`, `Plant State`, FUEL_COST, FUEL_GROUP,ENERGY_SOURCE,QUANTITY,MONTH)
# only take 1 month of data per Plant Id, 

#or rearrange so that each month is a column.
eiafuelbycode = eiafuel %>%
  #  group_by(`Plant Id`,MONTH) %>%
  group_by(`Plant Id`) %>%
  summarise(quantity = sum(QUANTITY),
            fuel_cost = weighted.mean(FUEL_COST,QUANTITY,na.rm=T), 
            FUEL_GROUP = unique(FUEL_GROUP)[1],
            types_fuel = length(unique(FUEL_GROUP)),
            ENERGY_SOURCE = unique(ENERGY_SOURCE)[1]) #%>%
#  spread(key = MONTH, value = fuel_cost, sep= ".fuelcost.")
#  spread(key = MONTH, value = c(fuel_cost,quantity), sep= ".") #Can't spread on 2 values in this way
# ERROR: this does not work because for some plants there are multiple quantity/cost rows per month
# solve this via grouping by plant ID and year/month
# summarising by quantity = sum and fuel cost = weighted average(how do I do a weighted average in this format?)

# merge with alldat by "Plant Id"
allfuel = merge(alldat, eiafuelbycode, by.x = "ORISPL", by.y = "Plant Id", all.x=F)
# eia has fuel costs for 29 of 383 rows... ugh
# I guess I could just use this for average values...?
avg_fuel_costs = allfuel %>%
  group_by(PLFUELCT) %>%
  summarize(avg_fuel_cost = mean(fuel_cost, na.rm=T),
            fuel_cost_sd = sd(fuel_cost,na.rm=T),
            nobs = sum(!is.nan(fuel_cost)))
ggplot(allfuel,aes(x=fuel_cost)) + geom_histogram() + facet_wrap(~PLFUELCT) #not all that normal, just use mean
