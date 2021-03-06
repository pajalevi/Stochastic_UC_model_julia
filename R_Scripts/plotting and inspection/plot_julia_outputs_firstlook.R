# plot_julia_outputs_firstlook.R
# read in julia outputs downloaded from sherlock
# plot and extract data including:
# * demand and slow commitment timeseries
# * actual production levels and demand for a given scenario
# * sanity check total startup costs by re-calculating them
# * calculate marginal cost electricity at each timestep for a scenario
# Patricia Levi Sept 2018
library(tidyverse)

### filepaths ####
base_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
output_fol_base = "Data/julia_output/"
input_fol = "Data/julia_input/"

# timeseriesID = "7d_12o_periods"
timeseriesID = intputfolID = "7d_12o_periods_10subsel"
periodID = "2_157_324"
periodID2 = "p2_157-324"
stochID = "n3_m1.0_0.2pp"
outputID = "10_int_multioutput"

# default_in_fol = paste0(base_fol,input_fol,"default/")
default_in_fol = paste0(base_fol,input_fol,"ercot_default/")
instance_in_fol = paste0(base_fol,input_fol,timeseriesID,"/")
output_fol = paste0(base_fol,output_fol_base,outputID,"/")

### load data ####
# demand2015 = read_csv(file = paste0(default_in_fol,"demand_2015.csv"), col_names = c("hour","demand"))
demand2015 = read_csv(file = paste0(default_in_fol,"ercot_demand_2016.csv"), col_names = c("hour","demand"), skip=1)

timeperiods = read_csv(file = paste0(instance_in_fol,"periods_",periodID,".csv"), col_names = "hour")
  
# gen_data = read_csv(file = paste0(default_in_fol,"gen_merged_withIDs.csv"))
gen_data = read_csv(file = paste0(default_in_fol,"complete_generator_listing_ERCOT_012019.csv"))

# slow_commit = read_csv(file = paste0(output_fol,"slow_commitment.csv"))
slow_commit = read_csv(file = paste0(output_fol,"slow_commitment_",periodID2,".csv"))
slow_commit$t = timeperiods$hour

slow_prod = read_csv(file = paste0(output_fol,"slow_production_",periodID2,".csv"))
slow_prod$speed = "slow"
fast_prod = read_csv(file = paste0(output_fol,"fast_production_",periodID2,".csv"))
fast_prod$speed = "fast"
prod = rbind(slow_prod, fast_prod)

# identify marginal price and generator of production
prod1 = prod %>%
  gather(key = scenario, value = prodMW, starts_with("o")) %>%
  merge(gen_data[,c("Capacity","PMin","plantUnique","VCost")], by.x = "GEN_IND", by.y = "plantUnique") %>%
  filter(prodMW > 0) 
prod_margprice = prod1 %>%
  group_by(t,scenario) %>%
  summarise(margprice = max(VCost),
            marggen = GEN_IND[which.max(VCost)],
            marggencap = Capacity[which.max(VCost)],
            marggenspd = speed[which.max(VCost)])
ggplot(prod_margprice, aes(x=t, y = margprice, color = scenario)) + 
  geom_line(aes(color = scenario)) + ggtitle("marginal price across scenarios") 


### plot demand and slow commitment ####
# format data
slcmt = slow_commit %>%
  gather(key = "generator", value = "cmt", `BIOMASS-1`:`DR-1`)
slcmt = merge(slcmt,gen_data[,c("Capacity","PMin","plantUnique","VCost")], by.x = "generator", by.y = "plantUnique")
slcmt$min_cmt = slcmt$cmt * slcmt$PMin
slcmt$cap_cmt = slcmt$cmt * slcmt$Capacity
slcmt$VCost[slcmt$generator == "DR-1"] = 1000
# identify marginal price and marginal generator of commitment
slcmt_margprice = slcmt %>%
  group_by(t) %>%
  filter(cmt > 0) %>%
  summarise(margprice = max(VCost),
            marggen = generator[which.max(VCost)],
            marggencap =Capacity[which.max(VCost)])
slcmt_plot = slcmt %>%
  group_by(t) %>%
  summarise(capacity_committed = sum(cap_cmt),
          min_generation = sum(min_cmt))

ggplot(slcmt_plot,aes(y=capacity_committed, x = 1:nrow(slcmt_plot))) + geom_line(color="red") +
  geom_line(aes(y=min_generation,x = 1:nrow(slcmt_plot)), color = "blue") +
  geom_line(data = demand2015[timeperiods$hour,], aes(y = demand, x = 1:nrow(slcmt_plot))) +
  labs(x="hour of simulation", y = "MW", title = "Demand, Min & Max slow committed capacity") +
  ggsave(filename = paste0(output_fol,"demand_and_commitment_",stochID,".png"), width = 7, height = 5)

# identify fraction of generator/time combos where slow commitment was nonbinary
sum(slcmt$cmt != 0 & slcmt$cmt !=1) / nrow(slcmt)
# [1] 0.00997426

### plot actual production for a central scenario ####
# select one scenario
scen_prod = select(prod,o5,GEN_IND,t,speed)

prod_plot = scen_prod %>%
  group_by(t,speed) %>%
  summarise(production = sum(o5)) %>%
  spread(key = speed, value =production) %>%
  mutate(total = fast + slow)
# check that production equals demand
summary(prod_plot$total - demand2015[timeperiods$hour,"demand"])

ggplot(prod_plot, aes(y = slow, x = 1:nrow(prod_plot))) + geom_line(color = "red") +
  geom_line(aes(y=total, x = 1:nrow(prod_plot)), color = "black") +
  geom_line(aes(y=fast, x = 1:nrow(prod_plot)), color = "green") +
  geom_line(data = slcmt_plot, aes(y=capacity_committed, x = 1:nrow(slcmt_plot)), color = "purple", linetype = 3) #+
  # geom_line(data = demand2015[timeperiods$hour,], aes(y = demand, x = 1:nrow(slcmt_plot)), color = "green")
# level of production by fast generators is extremely constant
# it appears that slow generator production exceeds max capacity committed at times! am i getting the assignment of generators wrong?

# inspect slow commitment and production datasets for sameness - 
## TODO: need to filter first for cmt >0 and prod >0 ###
slprod1 = slow_prod %>%
  gather(key = scenario, value = prodMW, starts_with("o")) %>%
  filter(prodMW > 0) 
length(unique(slcmt$generator))
length(unique(slow_prod$GEN_IND))
cbind(sort(unique(slcmt$generator)),sort(unique(slow_prod$GEN_IND)))

# could I be matching capacities to commitment decisions incorrectly? seems unlikely
# could I have made an error in the construction of the constraints? possible
# error is either in the model itself or in the outputs...
# error coult be in pf or u or defn of pmax
  # pf is always >=0, <=1, so it could not lift the max capacity allowed
# could be in assigning w to u - perhaps indexing is working incorrectly. compare slow_commitment to u_commitment.csv
  u = read_csv(file = paste0(output_fol,"u_commitment.csv"))
  u = select(u, o6, GEN_IND, t)
  u$timestep = timeperiods$hour[u$t]
  
  test = merge(x = u, y = slcmt,by.y = c("generator","t"), by.x = c("GEN_IND","timestep"))  
  test$diff = test$cmt - test$o6
  summary(test$diff)  
  # OK, so u is not different from w, we can rule that out
  
  test[test$GEN_IND %in% "DR-1",] # looks like DR is not committed ever... but other outputs show it definitely in play. 
  # this is probably due to the error I just found in the construction of the GENMAXDR constraint
  sum(test$GEN_IND %in% "DR-1") 
  slow_prod[slow_prod$GEN_IND %in% "DR-1",]$o9
  slow_prod[slow_prod$GEN_IND %in% "DR-1",]$o1 # DR production is the same in all scenarios!
  slcmt[slcmt$generator %in% "DR-1",]$cap_cmt # no DR capacity is committed but there is production!
  
  # merge slow_prod and slcmt by t, generator/GEN_IND, and identify all places where slow_prod > slcmt
  slow_prod$timestep = timeperiods$hour[slow_prod$t]
  slow = merge(x=slow_prod, y = slcmt, by.x = c("GEN_IND" ,"timestep"), by.y = c("generator","t"))
  slow$diffo5 = slow$cap_cmt - slow$o5
  which(slow$diffo5 < 0)  
  View(slow[which(slow$diffo5 < 0),])
  unique(slow[which(slow$diffo5 < 0),"GEN_IND"])
  # just DR violates - constant violation of 900
  
  plot(slcmt_plot$capacity_committed - prod_plot$slow, type = "l")
  summary(slcmt_plot$capacity_committed - prod_plot$slow)
  # the maximum negative violation is -865.10
  # so removing the DR violation of 900 would fix this entirely
  plot(slcmt_plot$capacity_committed - prod_plot$slow + 900, type = "l")
  abline(h=0)
  
  ggplot(prod_plot, aes(y = slow , x = 1:nrow(prod_plot))) + geom_line(color = "red") +
    # geom_line(aes(y=total, x = 1:nrow(prod_plot)), color = "black") + 
    geom_line(aes(y=fast, x = 1:nrow(prod_plot)), color = "green") +
    geom_line(data = slcmt_plot, aes(y=capacity_committed + 900, x = 1:nrow(slcmt_plot)), color = "purple", linetype = 3) 
  
  