library(tidyverse)

### filepaths ####
base_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
output_fol_base = "Data/julia_output/"
input_fol = "Data/julia_input/"

intputfolID = "7d_12o_periods_10subsel"
periodID = "2_157_324"
periodID2 = "p2_157-324"
stochID = "n3_m1.0_0.2pp"
outputID = "10_int_multioutput"

# default_in_fol = paste0(base_fol,input_fol,"default/")
default_in_fol = paste0(base_fol,input_fol,"ercot_default/")
instance_in_fol = paste0(base_fol,input_fol,inputfolID,"/")
output_fol = paste0(base_fol,output_fol_base,outputID,"/")

all_periods = list.files(path = instance_in_fol)

for(i in 1:length(all_periods)){
  xx = all_periods[i]
  periodinfo = strsplit(xx,"_")[[1]]
  periodID = paste0(periodinfo[2],"_",periodinfo[3],"_",periodinfo[4])
  makedemandgraphs(default_in_fol, instance_in_fol, output_fol, periodID,stochID,outputID)
}

makedemandgraphs = function(default_in_fol,instance_in_fol,output_fol,periodID,stochID, outputID){
  periodinfo = strsplit(periodID,"_")[[1]]
  periodsave = substr(periodID,1,str_length(periodID)-4)
  
  ### load data ####
  # demand2015 = read_csv(file = paste0(default_in_fol,"demand_2015.csv"), col_names = c("hour","demand"))
  demand2015 = read_csv(file = paste0(default_in_fol,"ercot_demand_2016.csv"), col_names = c("hour","demand"), skip=1)
  
  timeperiods = read_csv(file = paste0(instance_in_fol,"periods_",periodID), col_names = "hour")
  
  # gen_data = read_csv(file = paste0(default_in_fol,"gen_merged_withIDs.csv"))
  gen_data = read_csv(file = paste0(default_in_fol,"complete_generator_listing_ERCOT_012019.csv"))
  
  # slow_commit = read_csv(file = paste0(output_fol,"slow_commitment.csv"))
  slow_commit = read_csv(file = paste0(output_fol,"slow_commitment_p",periodinfo[1],"_",periodinfo[2],"-",periodinfo[3]))
  slow_commit$t = timeperiods$hour
  
  slow_prod = read_csv(file = paste0(output_fol,"slow_production_p",periodinfo[1],"_",periodinfo[2],"-",periodinfo[3]))
  slow_prod$speed = "slow"
  fast_prod = read_csv(file = paste0(output_fol,"fast_production_p",periodinfo[1],"_",periodinfo[2],"-",periodinfo[3]))
  fast_prod$speed = "fast"
  prod = rbind(slow_prod, fast_prod)
  
  ## plot marginal price ##
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
    geom_line(aes(color = scenario)) + ggtitle("marginal price across scenarios") +
    ggsave(filename = paste0(output_fol,"marginal_price_",periodsave,".png"), width = 7, height = 5)
  
  ### plot demand and slow commitment ####
  # format data
  slcmt = slow_commit %>%
    gather(key = "generator", value = "cmt", `BIOMASS-1`:`NUCLEAR-2`)
  slcmt = merge(slcmt,gen_data[,c("Capacity","PMin","plantUnique")], by.x = "generator", by.y = "plantUnique")
  slcmt$min_cmt = slcmt$cmt * slcmt$PMin
  slcmt$cap_cmt = slcmt$cmt * slcmt$Capacity
  
  slcmt_plot = slcmt %>%
    group_by(t) %>%
    summarise(capacity_committed = sum(cap_cmt),
              min_generation = sum(min_cmt))
  
  ggplot(slcmt_plot,aes(y=capacity_committed, x = 1:nrow(slcmt_plot))) + geom_line(color="red") +
    geom_line(aes(y=min_generation,x = 1:nrow(slcmt_plot)), color = "blue") +
    geom_line(data = demand2015[timeperiods$hour,], aes(y = demand, x = 1:nrow(slcmt_plot))) +
    labs(x="hour of simulation", y = "MW", title = "Demand, Min & Max slow committed capacity") +
    ggsave(filename = paste0(output_fol,"demand_and_commitment_",periodsave,".png"), width = 7, height = 5)
  
  
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
    geom_line(data = slcmt_plot, aes(y=capacity_committed, x = 1:nrow(slcmt_plot)), color = "purple", linetype = 3) +
    ggsave(filename = paste0(output_fol,"slow_fast_production_",periodsave,".png"), width = 7, height = 5)
}
