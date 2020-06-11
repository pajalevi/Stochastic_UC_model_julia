# plotDispatchAndValue
# makes plots of DR dispatch info vs system value
# function to be used on Sherlock
# April 2020

library(tidyverse)
library(lubridate)
source("./consolidatedAnalysisFns.R")

# source("./plotDispatchAndValue.R")
# plotDispatchAndValue(outputfol = "/home/groups/weyant/plevi_outputs/",desiredRuns = "*o25*keyDays2*",summaryFilenames = "combined_summary.csv")
plotDispatchAndValue = function(outputfol, desiredRuns,
                                summaryFilenames){
#drdat
  # identify all desired run folders
  filelist = list.files(path = outputfol, pattern = glob2rx(desiredRuns))
  
  # load drdat for each one using getDRAllData
  # append name of run, and combine into one DR
  for(i in 1:length(filelist)){
    # check if dralldata exists. if not, create it
    drdatfile = paste0(outputfol,filelist[i],"/",filelist[i],"_dralldata.csv")
    last_loc = as.vector(regexpr("\\_[^\\_]*$", filelist[i]))
    runID = substr(filelist[i], 1, last_loc - 1)
    runDate = substr(filelist[i], last_loc+1,100)
    
    if(file.exists(drdatfile)){
      drdat = read_csv(drdatfile)
    } else {
      drdat = getDRAllData(runID, runDate, output_fol_base = outputfol, savecsv = TRUE)
    }
    
    # filter and sort drdat
    

    drdat$scenarionum = as.numeric(substr(drdat$scenario,2,10))
    drdat2 = drdat %>%
      filter(value.commit >0) %>%
      arrange(scenarionum,t)
    
    # TODO: calculate dispatch stats for DR, following the lead of eventStats()
    # number of events (mean max min)
    # scenarios are in order
    # t is strictly increasing except when we swich to next scenario
    drdat2$tdiffs = c(0,diff(drdat2$t))
    drdat2$eventStart = -1
    drdat2$eventStart[drdat2$tdiffs == 1] = 0
    drdat2$eventStart[drdat2$tdiffs != 1] = 1
    drdat2$eventnum = cumsum(drdat2$eventStart)
    eventnum = drdat2 %>%
      group_by(scenarionum) %>%
      summarise(nevents = length(unique(eventnum))) %>%
      ungroup() %>%
      summarise(meannevents = mean(nevents),
                mediannevents = median(nevents),
                maxnevents = max(nevents),
                minnevents = min(nevents))
    # # A tibble: 1 x 4
    # meannevents mediannevents maxnevents minnevents
    # <dbl>         <int>      <int>      <int>
    #  1775          1775       1775       1775
#TODO: test with a !advNot3 run

    # number of hours of use (mean max min)
    eventhours = drdat2 %>%
      group_by(scenarionum) %>%
      summarise(nhours = length(unique(t)),
                energy = sum(value.prod)) %>%
      ungroup() %>%
      summarise(meannhours = mean(nhours),
                mediannhours = median(nhours),
                maxnhours = max(nhours),
                minnhours = min(nhours),
                meanenergy = mean(energy),
                medianenergy = median(energy),
                maxenergy = max(energy),
                minenergy = min(energy))
    # # A tibble: 1 x 4
    # meannhours mediannhours maxnhours minnhours
    # <dbl>        <int>     <int>     <int>
    #  72           72        72        72
    
    # duration of events  (mean max min)
    eventlength = drdat2 %>%
      group_by(eventnum, scenarionum) %>% 
      summarise(length = n()) %>%
      ungroup() %>%
      summarise(meanlength = mean(length),
                medianlength = median(length),
                maxlength = max(length),
                minlength = min(length))
    # # A tibble: 1 x 4
    # meanlength medianlength maxlength minlength
    # <dbl>        <int>     <int>     <int>
    #  1.01            1         2         1
    
    eventdat = cbind(eventnum, eventhours, eventlength)
    eventdat$runID = runID
    eventdat$runDate = runDate
    
    # combine with previous data
    #TODO: update to use condensed dataframe
    if(i==1){
      alldrdat = eventdat
    } else {
      alldrdat = rbind(alldrdat, eventdat)
    }
    
  } #end filelist loop
  
  
#value data
  # based on compareOldNewNormal
  # identify desired combine_summary files
  if(length(summaryFilenames) == 1){
    # load 
    results = read_csv(paste0(outputfol,summaryFilenames))
  } else {
    print("function is currently not set up to handle multiple summmaryFiles")
    for(f in 1:length(summaryFilenames)){
      # load and combine appropriately
    }
  }
  
  # calculate savings and bounds
  # taken from compareOldNewNormal.R
  noDR_sel = (results$type == "noDR")
  results$dr_varcost = as.factor(results$dr_varcost)
  results$cost_lowbound = results$`expected Total costs` - (results$`expected Total costs` * results$MIPGapParam)
  results$`expected cost reduction hibound` = results$`Expected cost reduction from DR` + (results$`expected Total costs` * results$MIPGapParam) # this should be related to noDR MIPGAP
  results$`expected cost reduction lowbound` = results$`Expected cost reduction from DR` #- (results$`expected Total costs` * results$MIPGapParam[noDR_sel])
  results$`expected cost reduction hibound`[noDR_sel] = results$`Expected cost reduction from DR`[noDR_sel]
  
  
  
#combine event stats and savings dataframes
  #results date is in inconsistent format
  a = mdy(results$date)
  b = ymd(results$date)
  c = c(a[!is.na(a)], b[!is.na(b)])
  results$date = c
  
  # convert drdat2 date
  alldrdat$date = ymd(alldrdat$runDate)
  
  # merge
  alldat = merge(results, alldrdat, by = c("runID"), all = TRUE)
  if(sum(is.na(alldat$dr_varcost))){stop("not all DR dispatch data was successfully matched with a summary row")}
  
####plot####
  # interesting_costs = c(35,70)
  alldat$runlabel = substr(alldat$runID,1,as.vector(regexpr("_",alldat$runID))-1)
  end = regexpr(alldat$runlabel, pattern = "[0-9]")
  end[which(end<0)] = 50
  alldat$runtype =  substr(alldat$runlabel,1,end-1)
  alldat$runlevel = substr(alldat$runlabel,end,50)
  
  # number of hours
  # all
  ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70 | dr_varcost == 10000)), aes(y =`Expected cost reduction from DR`, x = meannhours, color = runtype, shape = dr_varcost)) +
    geom_point() +
    # coord_cartesian(xlim = c(0,750)) +
    geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
    geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`,width=.15))+
    geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minnhours, xmax = maxnhours)) +
    theme_minimal() +
    labs(y="savings relative to noDR scenario", x = "number of hours DR is used",
         title = "DR value vs Hours of use") +
    ggsave(paste0(outputfol,"value_and_DR_dispatch_hours.png"), width = 10, height = 5)
  # faceted by DR cost
  ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70)), aes(y =`Expected cost reduction from DR`, x = meannhours, color = runtype, shape = dr_varcost)) +
    geom_point() +
    facet_wrap(~dr_varcost, scales = "free")+
    # coord_cartesian(xlim = c(0,750)) +
    geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
    geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`,width=.15))+
    geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minnhours, xmax = maxnhours)) +
    theme_bw() +
    labs(y="savings relative to noDR scenario", x = "number of hours DR is used",
         title = "DR value vs Hours of use") +
    ggsave(paste0(outputfol,"value_and_DR_dispatch_hours_costfacet.png"), width = 18, height = 5)
  

  # event length
  # all
  ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70 | dr_varcost == 10000)), aes(y =`Expected cost reduction from DR`, x = meanlength, color = runtype, shape = dr_varcost)) +
    geom_point() +
    geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
    geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`, alpha = 0.2,width=.05))+
    geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minlength, xmax = maxlength, alpha = 0.2)) +
    theme_minimal() +
    labs(y="savings relative to noDR scenario", x = "Mean length of DR events (hours)",
         title = "DR value vs Event length") +
    ggsave(paste0(outputfol,"value_and_DR_event_length.png"), width = 10, height = 5)
  # faceted by DR cost
  ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70)), aes(y =`Expected cost reduction from DR`, x = meanlength, color = runtype, shape = dr_varcost)) +
    geom_point() +
    facet_wrap(~dr_varcost, scales = "free")+
    geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
    geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`, alpha = 0.2,width=.05))+
    geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minlength, xmax = maxlength, alpha = 0.2)) +
    theme_bw() +
    labs(y="savings relative to noDR scenario", x = "Mean length of DR events (hours)",
         title = "DR value vs Event length") +
    ggsave(paste0(outputfol,"value_and_DR_event_length_costfacet.png"), width = 18, height = 5)
  
  
  # event number
  # all
  ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70 | dr_varcost == 10000)), aes(y =`Expected cost reduction from DR`, x = meannevents, color = runtype, shape = dr_varcost)) +
    geom_point() +
    geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
    geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`,width=.1, alpha = 0.2))+
    geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minnevents, xmax = maxnevents, alpha = 0.2)) +
    theme_minimal() +
    labs(y="savings relative to noDR scenario", x = "Mean number of DR events",
         title = "DR value vs Number of events") +
    ggsave(paste0(outputfol,"value_and_DR_event_number.png"), width = 10, height = 5)
  # faceted by DR cost
  ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70)), aes(y =`Expected cost reduction from DR`, x = meannevents, color = runtype, shape = dr_varcost)) +
    geom_point() +
    facet_wrap(~dr_varcost, scales = "free")+
    geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
    geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`,width=.1, alpha = 0.2))+
    geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minnevents, xmax = maxnevents, alpha = 0.2)) +
    theme_bw() +
    labs(y="savings relative to noDR scenario", x = "Mean number of DR events",
         title = "DR value vs Number of events") +
    ggsave(paste0(outputfol,"value_and_DR_event_number_costfacet.png"), width = 18, height = 5)
  
  
  # total energy
  # all
  ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70 | dr_varcost == 10000)), aes(y =`Expected cost reduction from DR`, x = meanenergy, color = runtype, shape = dr_varcost)) +
    geom_point() +
    geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
    geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`,width=.1, alpha = 0.2))+
    geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minenergy, xmax = maxenergy, alpha = 0.2)) +
    theme_minimal() +
    labs(y="savings relative to noDR scenario", x = "Mean energy curtailed from DR (MW)",
         title = "DR value vs Energy curtailed by DR") +
    ggsave(paste0(outputfol,"value_and_DR_energy.png"), width = 10, height = 5)
  # faceted by DR cost
  ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70)), aes(y =`Expected cost reduction from DR`, x = meanenergy, color = runtype, shape = dr_varcost)) +
    geom_point() +
    facet_wrap(~dr_varcost, scales = "free")+
    geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
    geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`,width=.1, alpha = 0.2))+
    geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minenergy, xmax = maxenergy, alpha = 0.2)) +
    theme_bw() +
    labs(y="savings relative to noDR scenario", x = "Mean energy curtailed from DR (MW)",
         title = "DR value vs Energy curtailed by DR") +
    ggsave(paste0(outputfol,"value_and_DR_energy_costfacet.png"), width = 18, height = 5)
  
  
  # total energy without error bars
  # all
  ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70 | dr_varcost == 10000)), aes(y =`Expected cost reduction from DR`, x = meanenergy, color = runtype, shape = dr_varcost)) +
    geom_point() +
    geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
    # geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`,width=.1, alpha = 0.2))+
    # geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minenergy, xmax = maxenergy, alpha = 0.2)) +
    theme_minimal() +
    labs(y="savings relative to noDR scenario", x = "Mean energy curtailed from DR (MW)",
         title = "DR value vs Energy curtailed by DR") +
    ggsave(paste0(outputfol,"value_and_DR_energy_noerrorbar.png"), width = 10, height = 5)
  #faceted by DR cost
  ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70)), aes(y =`Expected cost reduction from DR`, x = meanenergy, color = runtype, shape = dr_varcost)) +
    geom_point() +
    geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
    facet_wrap(~dr_varcost, scales = "free")+
    # geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`,width=.1, alpha = 0.2))+
    # geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minenergy, xmax = maxenergy, alpha = 0.2)) +
    theme_bw() +
    labs(y="savings relative to noDR scenario", x = "Mean energy curtailed from DR (MW)",
         title = "DR value vs Energy curtailed by DR") +
    ggsave(paste0(outputfol,"value_and_DR_energy_noerrorbar_costfacet.png"), width = 18, height = 5)
  
  
  }