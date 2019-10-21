# collection of functions that assist with analyzing 
# data from model runs after theyve been consolidated
# March 2019


## FILE STRUCTURE ##
# base_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
# output_fol_base = "Data/julia_output/forIAEE_1PMin/"
# input_fol = "Data/julia_input/"

## RESOURCES ##
library(tidyverse)
library(viridis) # for nicer plot colors
library(data.table) # for faster merges
# source(paste0(base_fol,"/Julia_UC_Github/R_Scripts/mergeTimeseriesData.R")) # contains loadTimeseriesData
## 


## Plot DR Production ####
#TODO: add ability to plot multiple periods together
#TODO: have demand, vdem be inputs
if(!SHRLK){
  modelOutputFol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/forIAEE_1PMin/"
  modelInputFol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/"
}else{
  modelOutputFol = "/home/users/pjlevi/dr_stoch_uc/julia_ver/outputs/"
  modelInputFol = "/home/users/pjlevi/dr_stoch_uc/julia_ver/inputs/"
}
plotDRUse = function(runID,runDate,drcommit,
                     inputfolID, outputfolID,
                     scenarios = 1:5, # what scenarios will be graphed
                     overlaplength = 6, #loadTimeseriesData param
                     period = "p2_1020_1140", 
                     model_output_fol = modelOutputFol, 
                     model_input_fol = modelInputFol,
                     SHRLOK = SHRLK,
                     base_fol = baseFol) { 
  # model_output_fol is where model output folder : 
  # instance_in_fol is where model input is
  # plot_fol is where plots should go
  # output_fol: paste0(base_fol,output_fol_base,outputID,"/")
  # plot_fol:   paste0(base_fol,output_fol_base,"plots/")
  # instance_in:paste0(base_fol,input_fol,inputfolID,"/")
  
  plot_fol = paste0(model_output_fol,"plots/")
  if(!dir.exists(plot_fol)){dir.create(plot_fol)}
  instance_in_fol = paste0(model_input_fol,inputfolID,"/")
  if(!dir.exists(instance_in_fol)){stop("julia input file doesnt exist", instance_in_fol)}
  output_fol = paste0(model_output_fol,outputfolID,"/")
  
  # load params
  if(!SHRLK){
    inputs_file = paste0(base_fol,"/Julia_UC_Github/Julia_scripts/inputs_ercot.csv")
  }else{
    inputs_file = paste0(base_fol,"/code/inputs_ercot.csv")
  }
  allinputs = read_csv(inputs_file)
  params = allinputs[,c("input_name",runID)]
  params = spread(params, key = input_name, value = runID)
  ##
  
  # load DR production
  drprod = loadTimeseriesData(output_fol,"DR_production",overlaplength,2, probabilities=F,instance_in_fol,params$nrandp,dist_ID = params$stochID)
  
  # ggplot(drprod, aes(x=t,y=value)) + facet_wrap(~scenario) + geom_point()
  # ggplot(filter(drprod,nperiod=="10"), aes(x=t,y=value)) + facet_wrap(~scenario) + 
  #   geom_line() +
  #   ggtitle(paste(runID, "Period 10 DR production"))#+ 
  # coord_cartesian(xlim=c(5000,6000))
  
  # load and set up demand #
  periodinfo = strsplit(period,"p|_")[[1]] 
  numperiod=as.numeric(periodinfo[2])
  firstperiod = as.numeric(periodinfo[3])
  lastperiod = as.numeric(periodinfo[4])
  dem_base = read_csv(paste0(model_input_fol,"ercot_default/ercot_demand_2016.csv"))
  demchange = read_csv(paste0(instance_in_fol,"demandScenarios_vdem_ARMA26.0_",period,".csv")) 
  demrealo1 = dem_base$demand[firstperiod:lastperiod] * demchange$V1
  demreal = dem_base$demand[firstperiod:lastperiod] * demchange
  demreal$t = firstperiod:lastperiod
  demreal = gather(demreal,key = "scenario",value = "demand",-t) 
  demreal$scenarionum = substr(demreal$scenario,2,4)
  
  # select demand from just one period #
  drprodoneperiod = filter(drprod,nperiod == numperiod)
  drprodoneperiod$scenarionum = substr(drprodoneperiod$scenario,2,4)
  demprod = merge(demreal,drprodoneperiod,by=c("t","scenarionum")) 
  
  # plot demand and DR production
  ggplot(filter(demprod,scenarionum %in% scenarios))+ 
    facet_wrap(~scenario.x) + 
    geom_line(aes(x=t-min(t), y=(value*10)+30000), color="blue")+
    geom_line(aes(x=t-min(t),y=demand)) +
    # scale_color_gradient(low="black",high="red")+
    ggtitle(paste(runIDs[r], "Period 10 demand and DR Production")) +
    ggsave(paste0(plot_fol,runIDs[r],"_period10demand_DRproduction.png"),width = 10, height=7)
  
  # plot dr commitment
  drcomtoneperiod = filter(drcommit, nperiod == numperiod)
  drcomtoneperiod$scenarionum = substr(drcomtoneperiod$scenario,2,4)
  demcomt = merge(demreal, drcomtoneperiod, by=c("t","scenarionum"))
  # print(names(demcomt))
  # print(head(demcomt))
  
  ggplot(filter(demcomt,scenarionum %in% scenarios))+ 
    facet_wrap(~scenario.x) + 
    geom_line(aes(x=t-min(t), y=(value*10000)+30000), color="blue")+
    geom_line(aes(x=t-min(t),y=demand)) +
    # scale_color_gradient(low="black",high="red")+
    ggtitle(paste(runIDs[r], paste("Period",numperiod,"demand and DR Commitment"))) +
    ggsave(paste0(plot_fol,runIDs[r],"_period",numperiod,"demand_DRcommitment.png"),width = 10, height=7)
  
  # plot both together
  demprod = rename(demprod, production = value)
  drcomtoneperiod = rename(drcomtoneperiod, commitment = value)
  dralldata = merge(drcomtoneperiod, demprod, by=c("t","scenarionum"))
  # print(names(dralldata))
  ggplot(filter(dralldata,scenarionum %in% scenarios))+ 
    facet_wrap(~scenario.x) + 
    geom_line(aes(x=t-min(t), y=(production*10)+30000), color="blue")+
    geom_point(aes(x=t-min(t), y=(production*10)+30000, color = commitment), shape=1, size = 0.5) + 
    geom_line(aes(x=t-min(t),y=demand)) +
    scale_color_gradient(low="black",high="red")+
    ggtitle(paste(runIDs[r], paste("Period",numperiod,"demand and DR production with commitment in red"))) +
    ggsave(paste0(plot_fol,runIDs[r],"_period",numperiod,"demand_DRfunction.png"),width = 10, height=7)
  
}

fuelBreakdown = function(prodgendat,plotfol,runName,plots=T){
  # takes production data and calculates expected amount
  # of production by each type of generator
  # returns a dataframe with that information
  # optionally saves a bar chart with that information
  # Patricia Levi March 2019
  
  #prodgenDat should be "prod2" i.e. prod merged with gendat[,c("Fuel")]
  # prod2 = prod %>%
  #   merge(gendat[,c("Capacity","PMin","plantUnique","VCost","Fuel")], by.x = "GEN_IND", by.y = "plantUnique") %>%
  #   filter(MWout > 0) 
  
  prod_expected = prodgendat %>%
    group_by(GEN_IND,t,Fuel) %>%
    summarise(eProd = sum(prob * MWout))
  
  prod_byfuel = prod_expected %>%
    group_by(Fuel) %>%
    summarise(totEprod = sum(eProd))
  totalprod = sum(prod_byfuel$totEprod)
  
  prod_byfuel$prodFrac = prod_byfuel$totEprod/totalprod
  prod_byfuel$lab = paste0(round(100* prod_byfuel$prodFrac, 1),"%")
  
  if(plots){
  ggplot(prod_byfuel,aes(y=prodFrac,x=Fuel)) + geom_col() +
      ggtitle(paste0("Expected generation breakdown for: ", runName))+
      labs(x="Fuel Type",y="Percentage of total generation") +theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::percent) +
      geom_text(data=prod_byfuel,aes(x=Fuel,y=prodFrac,label=lab),vjust=0, nudge_y=0.008,size=3) +
      ggsave(paste0(plotfol,runName,"_fuelBreakdown_bar.png"),width=6,height=4)
  }
  return(prod_byfuel)
}

rampInfo = function(prodgendat,runName){
  # calculates the max and min ramp rates by SLOW/FAST/DR and for all dispatchabe gen
  # Also returns the *time/date* (or at least the period number?) of these ramps
  
  # assign speed to be SLOW/FAST/DR
  drsel = which(prodgendat$Fuel == "DR")
  prodgendat$speed[drsel] = "DR"
  
  # assign RE T/F column
  REsel = which(prodgendat$Fuel %in% c("DR","SOLAR","WIND")) # do I want to include Hydro in here? Nah
  prodgendat$REgen = F
  prodgendat$REgen[REsel] = T
  
  # ramp by SPEED -- not that meaningful since RE is included in there
  # find ramp by scenario, speed, timestep
  # prodrampspeed = prodgendat %>%
  #   group_by(scenario,speed,t) %>%
  #   summarise(MWtot = sum(MWout)) %>%
  #   group_by(scenario,speed) %>%
  #   mutate(ramp = MWtot - lag(MWtot, default=0))%>%
  #   group_by(speed) %>%
  #   summarise(maxramp = max(ramp),
  #             minramp = min(ramp))
  
  # need to weight scenarios to find expected ramp? what is meaningful? max expected ramp or max possible ramp?
  # theyre both meaningful. But for now just doing max possible ramp
  
  # ramp by non-RE
  # find ramp by scenario, RE, timestep
  prodrampRE = prodgendat %>%
    group_by(scenario,REgen,t) %>%
    summarise(MWtot = sum(MWout)) %>%
    group_by(scenario,REgen) %>%
    mutate(ramp = MWtot - lag(MWtot, default=0))%>%
    group_by(REgen) %>%
    summarise(maxramp = max(ramp),
              minramp = min(ramp),
              pct99.9th = quantile(ramp,probs = 0.999,na.rm=T),
              pct99th = quantile(ramp,probs = 0.99,na.rm=T))
  prodrampRE$speed = "all"
  
  # ramp by non-RE and speed
  # find ramp by scenario, RE, timestep
  prodrampREspeed = prodgendat %>%
    group_by(scenario,REgen,speed,t) %>%
    summarise(MWtot = sum(MWout)) %>%
    group_by(scenario,REgen,speed) %>%
    mutate(ramp = MWtot - lag(MWtot, default=0))%>%
    group_by(REgen,speed) %>%
    summarise(maxramp = max(ramp),
              minramp = min(ramp),
              pct99.9th = quantile(ramp,probs = 0.999,na.rm=T),
              pct99th = quantile(ramp,probs = 0.99,na.rm=T))
  
  # consolidate and return
  output = bind_rows(prodrampRE,prodrampREspeed)
  output$runName = runName
  return(output)
}
