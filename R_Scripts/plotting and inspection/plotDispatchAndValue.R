# plotDispatchAndValue
# makes plots of DR dispatch info vs system value
# function to be used on Sherlock
# April 2020

source("./combine_summary_files.R")
source("./consolidatedAnalysisFns.R")
library(tidyverse)
library(lubridate)
library(scales)

# source("./plotDispatchAndValue.R")
# plotDispatchAndValue(outputfol = "/home/groups/weyant/plevi_outputs/",desiredRuns = "*o25*keyDays2*",summaryFilenames = "combined_summary_082420_slowgas_dur_test.csv")
outputfol = "/home/groups/weyant/plevi_outputs/"
desiredRuns = "o25(_|_c2_)keyDays2"
## desiredRuns = *o25*keyDays2*
summaryFilenames = "combined_summary.csv"
summaryFilenames = "combined_summary_05-01_new_gendat_noNAs.csv"
# summaryFilenames = "combined_summary_082420_slowgas_dur_test.csv"
# plotDispatchAndValue(outputfol = "/home/groups/weyant/plevi_outputs/",desiredRuns = "o25(_|_c2_)keyDays2",summaryFilenames = "combined_summary.csv")

plotDispatchAndValue = function(outputfol, desiredRuns,
                                summaryFilenames){
#drdat - dispatch data ####
  # identify all desired run folders
  filelist = list.files(path = outputfol, pattern = desiredRuns)#glob2rx(desiredRuns))
  runIDs = substr(filelist, 1, nchar(filelist)-11)
  runDates = substr(filelist, nchar(filelist)-9,100)
  combineSummaryFiles(runIDs,runDates)
  
  
  # load drdat for each one using getDRAllData
  # append name of run, and combine into one DR
  for(i in 1:length(filelist)){
    # check if dralldata exists. if not, create it
    print(filelist[i])
    drdatfile = paste0(outputfol,filelist[i],"/",filelist[i],"_dralldata.csv")
    last_loc = as.vector(regexpr("\\_[^\\_]*$", filelist[i]))
    runID = substr(filelist[i], 1, last_loc - 1)
    runDate = substr(filelist[i], last_loc+1,100)
    
    if(file.exists(drdatfile)){
      drdat = read_csv(drdatfile)
    } else if(!str_detect(filelist[i],"noDR")) {
      drdat = getDRAllData(runID, runDate, output_fol_base = outputfol, savecsv = TRUE)
    } else {
      # this is a noDR file
      next
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
      dplyr::group_by(eventnum, scenarionum) %>% 
      dplyr::summarise(length = n()) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(scenarionum) %>%
      dplyr::summarise(o_meanlength = mean(length)) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(meanlength = mean(o_meanlength),
                medianlength = median(o_meanlength),
                maxlength = max(o_meanlength),
                minlength = min(o_meanlength))
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
  
  
#value data ####
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
  b[b < mdy("01/01/2018")] = NA
  a[a < mdy("01/01/2018")] = NA
  c = c(a[!is.na(a)], b[!is.na(b)])
  results$date = c
  
  # convert drdat2 date
  alldrdat$date = ymd(alldrdat$runDate)
  
  # merge
  alldat = merge(results, alldrdat, by = c("runID"), all = TRUE) #don't match by date because combined_summary doesn't use filename date for date column! it uses individual run summaries which may differ
  if(sum(is.na(alldat$dr_varcost))){
    warning("not all DR dispatch data was successfully matched with a summary row")
    print("the following runs from desiredRuns were not in the combined summary file:")
    print(alldat[is.na(alldat$dr_varcost), c("runID","date")])
  }
  
####plot####
  basewidth = 10 *0.8
  baseheight = 5 * 0.8
  facetwidth = 16
  facetheight = 5
  # interesting_costs = c(35,70)
  
  alldat$runlabel = substr(alldat$runID,1,as.vector(regexpr("_",alldat$runID))-1)
  end = regexpr(alldat$runlabel, pattern = "[0-9]")
  end[which(end<0)] = 50
  alldat$runtype =  substr(alldat$runlabel,1,end-1)
  alldat$runlevel = substr(alldat$runlabel,end,50)
  
  ##calculate fractional values ##
  noDR_sel = which(alldat$type == "noDR")
  if(length(noDR_sel)>1){
    noDR_sel = noDR_sel[which.max(alldat$date.x[noDR_sel])]
  }
  alldat$`cost reduction lowbound, frac` = alldat$`expected cost reduction lowbound` / alldat$`expected Total costs`[noDR_sel]
  alldat$`cost reduction hibound, frac` = alldat$`expected cost reduction hibound` / alldat$`expected Total costs`[noDR_sel]
  
  ##  relabeling ##
  unconstrained = which(alldat$runlabel == "advNot1")
  alldat$type[unconstrained] = "Unconstrained"
  alldat$runlabel[unconstrained] = "Unconst"
  alldat$type[which(alldat$type == "advNot")] = "Notification"
  alldat$type[which(alldat$type == "avail")] = "Availability"
  alldat$type[which(alldat$type == "energy")] = "Energy"
  alldat$type[which(alldat$type == "hour")] = "Hour"
  alldat$type[which(alldat$type == "noDR")] = "No DR"
  alldat$type[which(alldat$type == "dur")] = "Duration"
  
  alldat$type = fct_relevel(alldat$type, "Notification","Hour","Energy","Duration","Availability","Unconstrained","No DR")
  
  # alldat$dr_varcost = fct_recode(alldat$dr_varcost, none = "10000", `$35/MWh` = "35", `$70/MWh` = "70")
  
  
  # to make 'constraint level' consistent, we need...
  # advNot2 -> 1
  alldat$runlevel[which(alldat$runlabel == "advNot2")] = 1
  # advNot3 -> 2
  alldat$runlevel[which(alldat$runlabel == "advNot3")] = 2
  # hour3 -> 1
  alldat$runlevel[which(alldat$runlabel == "hour3")] = 1
  # hour1 -> 3
  alldat$runlevel[which(alldat$runlabel == "hour1")] = 3
  # energy3 > 1
  alldat$runlevel[which(alldat$runlabel == "energy3")] = 1
  # energy1 > 3
  alldat$runlevel[which(alldat$runlabel == "energy1")] = 3
  # avail 1 (evening) > 2
  alldat$runlevel[which(alldat$runlabel == "avail1")] = 2
  # avail 2 (daytime) > 1
  alldat$runlevel[which(alldat$runlabel == "avail2")] = 1
  # avail 3 (workhours) > 4
  alldat$runlevel[which(alldat$runlabel == "avail3")] = 4
  # avail 4 (after work) > 3
  alldat$runlevel[which(alldat$runlabel == "avail4")] = 3
  
  
  ###### number of hours ####
  modpalette = brewer_pal(palette = "Dark2")(6)
  modpalette[1] = "blue"
  shapepalette = c(1,2,0,5,6,4,8)

  # all
  gglayers = list(
    geom_point(),
    theme_minimal(),
    geom_errorbar(aes(ymin = `cost reduction lowbound, frac`, ymax = `cost reduction hibound, frac`), alpha = 0.3,width=0),
    scale_y_continuous(labels = scales::percent), #TODO: add values = palette for consistent colors, also name= legend name
    scale_color_manual(name="DR Variable\nCost", values = modpalette, labels = c("$35/MWh","$70/MWh","N/A")),
    scale_shape_manual(name="Constraint Type", values = shapepalette),
    labs(y="Savings relative to noDR scenario", 
         color = "DR Variable\nCost") 
  )
  filterdat = filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70 ))#| dr_varcost == 10000 | is.na(dr_varcost)))
  
 # p =  ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70 | dr_varcost == 10000)), 
 #             aes(y =`Expected cost reduction from DR, frac`, x = meannhours, color = dr_varcost, shape = runtype)) +
 #    geom_point() +
 #    # coord_cartesian(xlim = c(0,750)) +
 #    geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
 #    geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`,width=.15))+ #TODO: adjust for frac
 #    geom_errorbarh(aes(y = `Expected cost reduction from DR, frac`, xmin = minnhours, xmax = maxnhours)) + #TODO: adjust to represent interquartile range
 #    theme_minimal() +
 #    scale_y_continuous(labels = scales::percent) + #TODO: add values = palette for consistent colors, also name= legend name
 #    scale_color_manual(name="DR Variable\nCost", values = modpalette) +
 #    scale_shape_discrete(name="Constraint Type") +
 #    labs(y="Savings relative to noDR scenario", x = "Number of hours DR is used",
 #         title = "DR value vs Hours of use") 
 
 ggplot(filterdat,aes(y =`Expected cost reduction from DR, frac`, x = meannhours, color = dr_varcost, shape = type)) +
   gglayers +
   # geom_errorbar(aes(ymin = `cost reduction lowbound, frac`, ymax = `cost reduction hibound, frac`), alpha = 0.3,width=.15)+ 
   geom_errorbarh(aes(y = `Expected cost reduction from DR, frac`, xmin = minnhours, xmax = maxnhours), alpha = 0.3) + #TODO: adjust to represent interquartile range
   labs(x = "Number of hours DR is used",
        title = "DR value vs Hours of use")  +
    ggsave(paste0(outputfol,"value_and_DR_dispatch_hours_08-17.png"), width = basewidth, height = baseheight)
  
  # faceted by DR cost
  pf = ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70)), 
              aes(y =`Expected cost reduction from DR`, x = meannhours, color = runtype, shape = dr_varcost)) +
    geom_point() +
    facet_wrap(~dr_varcost, scales = "free")+
    # coord_cartesian(xlim = c(0,750)) +
    geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
    geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`,width=.15))+
    geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minnhours, xmax = maxnhours)) +
    theme_bw() +
    labs(y="savings relative to noDR scenario", x = "number of hours DR is used",
         title = "DR value vs Hours of use") 
  pf +
    ggsave(paste0(outputfol,"value_and_DR_dispatch_hours_costfacet.png"), width = facetwidth, height = facetheight)
  

  # event length ####
  
  # all
  # ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70 | dr_varcost == 10000)), 
  #        aes(y =`Expected cost reduction from DR`, x = meanlength, color = runtype, shape = dr_varcost)) +
  #   geom_point() +
  #   geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
  #   geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`, alpha = 0.2,width=.05))+
  #   geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minlength, xmax = maxlength, alpha = 0.2)) +
  #   theme_minimal() +
  #   labs(y="savings relative to noDR scenario", x = "Mean length of DR events (hours)",
  #        title = "DR value vs Event length") +
  #   ggsave(paste0(outputfol,"value_and_DR_event_length.png"), width = basewidth, height = baseheight)
  
  ggplot(filterdat, 
         aes(y =`Expected cost reduction from DR, frac`, x = meanlength, color = dr_varcost, shape = type)) +
    gglayers + 
    # geom_errorbar(aes(ymin = `cost reduction lowbound, frac`, ymax = `expected cost reduction hibound, frac`), alpha = 0.3,width=.15)
    geom_errorbarh(aes(y = `Expected cost reduction from DR, frac`, xmin = minlength, xmax = maxlength), alpha = 0.3) +
    labs(x = "Mean length of DR events (hours)",
         title = "DR value vs Event length") +
    ggsave(paste0(outputfol,"value_and_DR_event_length_08-17.png"), width = basewidth, height = baseheight)
  
    
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
    ggsave(paste0(outputfol,"value_and_DR_event_length_costfacet.png"), width = facetwidth, height = facetheight)
  
  
  # event number  ####
  # all
  # ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70 | dr_varcost == 10000)), aes(y =`Expected cost reduction from DR`, x = meannevents, color = runtype, shape = dr_varcost)) +
  #   geom_point() +
  #   geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
  #   geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`,width=.1, alpha = 0.2))+
  #   geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minnevents, xmax = maxnevents, alpha = 0.2)) +
  #   theme_minimal() +
  #   labs(y="savings relative to noDR scenario", x = "Mean number of DR events",
  #        title = "DR value vs Number of events") +
  #   ggsave(paste0(outputfol,"value_and_DR_event_number.png"), width = basewidth, height = baseheight)
  ggplot(filterdat,
         aes(y =`Expected cost reduction from DR, frac`, x = meannevents, color = dr_varcost, shape = type)) +
    gglayers +
    geom_errorbarh(aes(y = `Expected cost reduction from DR, frac`, xmin = minnevents, xmax = maxnevents), alpha = 0.2) +
    labs(x = "Mean number of DR events",
         title = "DR value vs Number of events") +
    ggsave(paste0(outputfol,"value_and_DR_event_number_06-16.png"), width = basewidth, height = baseheight)
  
  
  
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
    ggsave(paste0(outputfol,"value_and_DR_event_number_costfacet.png"), width = facetwidth, height = facetheight)
  
  
  # total energy ####
  # all
  # ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70 | dr_varcost == 10000)), 
  #      aes(y =`Expected cost reduction from DR`, x = meanenergy, color = runtype, shape = dr_varcost)) +
  #   geom_point() +
  #   geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
  #   geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`,width=.1), alpha = 0.5)+
  #   geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minenergy, xmax = maxenergy), alpha = 0.5) +
  #   theme_minimal() +
  #   labs(y="savings relative to noDR scenario", x = "Mean energy curtailed from DR (MWh)",
  #        title = "DR value vs Energy curtailed by DR") +
  #   ggsave(paste0(outputfol,"value_and_DR_energy.png"), width = basewidth, height = baseheight)
  
  ggplot(filterdat,
         aes(y =`Expected cost reduction from DR, frac`, x = meanenergy, color = dr_varcost, shape = type)) +
    gglayers +
    geom_errorbarh(aes(y = `Expected cost reduction from DR, frac`, xmin = minenergy, xmax = maxenergy), alpha = 0.2) +
    labs(x = "Mean energy curtailed from DR (MWh)",
         title = "DR value vs Energy curtailed by DR") +
    ggsave(paste0(outputfol,"value_and_DR_energy_06-16.png"), width = basewidth, height = baseheight)
  
  # faceted by DR cost
  ggplot(filter(alldat,runtype != "start" & (dr_varcost == 35 | dr_varcost == 70)), aes(y =`Expected cost reduction from DR`, x = meanenergy, color = runtype, shape = dr_varcost)) +
    geom_point() +
    facet_wrap(~dr_varcost, scales = "free")+
    geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.2) +
    geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`,width=.1), alpha = 0.5)+
    geom_errorbarh(aes(y = `Expected cost reduction from DR`, xmin = minenergy, xmax = maxenergy), alpha = 0.5) +
    theme_bw() +
    labs(y="savings relative to noDR scenario", x = "Mean energy curtailed from DR (MWh)",
         title = "DR value vs Energy curtailed by DR") +
    ggsave(paste0(outputfol,"value_and_DR_energy_costfacet.png"), width = facetwidth, height = facetheight)
  
  
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
    ggsave(paste0(outputfol,"value_and_DR_energy_noerrorbar.png"), width = basewidth, height = baseheight)
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
    ggsave(paste0(outputfol,"value_and_DR_energy_noerrorbar_costfacet.png"), width = facetwidth, height = facetheight)
  
  
  }