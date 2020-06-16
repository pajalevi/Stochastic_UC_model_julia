#compareOldNewNormal.R
# compare runs before and after oct 1st/2nd 2019
# try to do this in a way that actually makes sense
# Used to make Fig 1, Fig 4
library(plyr) #overwrites arrange, count, desc, failwith, id, mutate, rename, summarise, summarize
library(tidyverse)
library(scales)
modpalette = brewer_pal(palette = "Dark2")(6)
modpalette[1] = "blue"

# results = read_csv(file = file.choose())

outputfol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/"
# oldResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_INFORMSruns_NovAnalysis.csv")
# newResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_postINFORMS_newNormal_all.csv")
# revertResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_preINFORMS_revert_attempt_11-7.csv")
# reprocessedResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_preINFORMS_reprocessed_prod_11-14.csv")
# #new70results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_INFORMS_70DR (1).csv")
# new70results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_postINFORMS_all.csv")

results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/combined_summary_05-01_new_gendat_noNAs.csv") #Fig 1

# slowhydroresults =  read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/combined_summary_slow_hydro_04-05.csv")
# slowhydroresults$hydro  = "slow"
# otherresults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/combined_summary_04-01.csv")
# # otherresults  = filter(otherresults,!str_detect(runID,"advNot1_o25_c2"))
# otherresults$hydro = "fast"
# results = rbind.fill(slowhydroresults,otherresults)

# # oldResults$edition = "old"
# newResults$edition = "new"
# revertResults$edition = "revert"
# # reprocessedResults$edition = "old-reprocess"
# new70results$edition = "new70"

# fixsel = which(results$nrandp == 5 & str_detect(results$runID,pattern="_o25_"))
# results$`expected Total costs`[fixsel] = results$`expected Total costs`[fixsel]/5
# results$`Expected cost reduction from DR`[fixsel] = results$`Expected cost reduction from DR`[fixsel]/5
# results$`Total CO2 emissions`[fixsel] = results$`Total CO2 emissions`[fixsel]/5


# results = rbind.fill(newResults,revertResults,new70results)
noDR_sel = (results$type == "noDR")
results$dr_varcost = as.factor(results$dr_varcost)
results$cost_lowbound = results$`expected Total costs` - (results$`expected Total costs` * results$MIPGapParam)
# results$`expected cost reduction lowbound` = results$`Expected cost reduction from DR` - (results$`expected Total costs` * results$MIPGapParam)
# results$`expected cost reduction hibound` = results$`Expected cost reduction from DR` + (results$`expected Total costs` * results$MIPGapParam[noDR_sel]) # this should be related to noDR MIPGAP
results$`expected cost reduction hibound` = results$`Expected cost reduction from DR` + (results$`expected Total costs` * results$MIPGapParam) # this should be related to noDR MIPGAP
results$`expected cost reduction lowbound` = results$`Expected cost reduction from DR` #- (results$`expected Total costs` * results$MIPGapParam[noDR_sel])

results$`expected cost reduction hibound`[noDR_sel] = results$`Expected cost reduction from DR`[noDR_sel]

# View(results)
# View(results[!rand_sel,c("type","Expected cost reduction from DR","edition","dr_varcost")])
#View(results[,c("MIPGapParam","runID","expected Total costs","Expected cost reduction from DR","expected cost reduction hibound","expected cost reduction lowbound")])

rand_sel = (results$type == "rand")
results$dr_varcost = as.factor(results$dr_varcost)
results$runlabel = substr(results$runID,1,as.vector(regexpr("_",results$runID))-1)
end = regexpr(results$runlabel, pattern = "[0-9]")
end[which(end<0)] = 50
results$runtype =  substr(results$runlabel,1,end-1)
results$runlevel = substr(results$runlabel,end,50)

pd <- position_dodge(0.5)
# p = ggplot(filter(results,dr_varcost != 70, type != "start"), aes(x = type, y = `Expected cost reduction from DR`,
p = ggplot(results, aes(x = type, y = `Expected cost reduction from DR`,
                        color = dr_varcost, shape = dr_varcost)) +
                        # color = hydro, shape = hydro))+
                        # color = dr_varcost))+#, shape = hydro))+
  
  # geom_jitter(width = 0.5) +
  geom_point() +
  geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.6) +
  geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`),#, linetype = hydro),
  # geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`, linetype = hydro), 
                width=.15, position = pd) +
  theme_minimal() +
  scale_color_manual(values = modpalette) +
  labs(y = "Savings relative to no DR scenario") 

p
p + 
  ggsave(paste0(outputfol,"new_results_06-11_Figure1.png"), width = 6, height = 5)



p + aes(y = `Expected cost reduction from DR, frac`, shape = dr_varcost) + scale_y_continuous(labels = scales::percent) 

############ FIGURE 1 ################

pd <- position_dodge(0.25)
# results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/combined_summary_05-01_new_gendat_noNAs.csv") #Fig 1

noDR_sel = (results$type == "noDR")

results$`cost reduction lowbound, frac` = results$`expected cost reduction lowbound` / results$`expected Total costs`[noDR_sel]
results$`cost reduction hibound, frac` = results$`expected cost reduction hibound` / results$`expected Total costs`[noDR_sel]

fig_results = results

##  relabeling ##
unconstrained = which(fig_results$runlabel == "advNot1")
fig_results$type[unconstrained] = "Unconstrained"
fig_results$runlabel[unconstrained] = "Unconst"
fig_results$type[which(fig_results$type == "advNot")] = "Notification"
fig_results$type[which(fig_results$type == "avail")] = "Availability"
fig_results$type[which(fig_results$type == "energy")] = "Energy"
fig_results$type[which(fig_results$type == "hour")] = "Hour"

# to make 'constraint level' consistent, we need...
# advNot2 -> 1
  fig_results$runlevel[which(fig_results$runlabel == "advNot2")] = 1
# advNot3 -> 2
  fig_results$runlevel[which(fig_results$runlabel == "advNot3")] = 2
# hour3 -> 1
  fig_results$runlevel[which(fig_results$runlabel == "hour3")] = 1
# hour1 -> 3
  fig_results$runlevel[which(fig_results$runlabel == "hour1")] = 3
# energy3 > 1
  fig_results$runlevel[which(fig_results$runlabel == "energy3")] = 1
# energy1 > 3
  fig_results$runlevel[which(fig_results$runlabel == "energy1")] = 3
  

pd <- position_dodge(0.4)
p = ggplot(filter(fig_results,runtype != "start" & runtype != "noDR"& (dr_varcost == 35 | dr_varcost == 70 | dr_varcost == 10000)), 
           # aes(x = type, y = `Expected cost reduction from DR`,
           aes(x = type, y = `Expected cost reduction from DR, frac`,
                        color = dr_varcost, shape = runlevel)) +
  geom_point() +
  # geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.6) +
  geom_errorbar(aes(ymin = `cost reduction lowbound, frac`, ymax = `cost reduction hibound, frac`), alpha = 0.5,#, linetype = hydro),
                width=.3, position = pd) +
  theme_minimal() +
  scale_color_manual(values = modpalette) +
  labs(y = "Savings relative to no DR scenario", x = "Constraint Type") +
  scale_color_manual(name="DR Variable\nCost", values = modpalette) +
  scale_shape_discrete(name="Constraint Level") +
  scale_y_continuous(labels = scales::percent, limits = c(-0.00005,0.002)) 
  

p
p + 
  ggsave(paste0(outputfol,"Figure1_06-15.png"), width = 6, height = 5)


############ FIGURE 4 ################
results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/combined_summary_05-01_new_gendat_noNAs.csv") #Fig 1 # the 'new gendat' is just an update of co2 emissions
slowhydroresults =  read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/combined_summary_slow_hydro_04-05.csv")
#this combined_summary has results from a different noDR run (Dec instead of Feb). Use the newer one. 

slowhydroresults$hydro  = "slow"
# otherresults  = filter(otherresults,!str_detect(runID,"advNot1_o25_c2"))
results$hydro = "fast"
fig4_results = rbind.fill(slowhydroresults,results)

noDR_sel = (fig4_results$type == "noDR")
fig4_results$date[noDR_sel]
noDR_sel = which(noDR_sel)[1]
fig4_results$dr_varcost = as.factor(fig4_results$dr_varcost)
fig4_results$cost_lowbound = fig4_results$`expected Total costs` - (fig4_results$`expected Total costs` * fig4_results$MIPGapParam)
# fig4_results$`expected cost reduction lowbound` = fig4_results$`Expected cost reduction from DR` - (fig4_results$`expected Total costs` * fig4_results$MIPGapParam)
# fig4_results$`expected cost reduction hibound` = fig4_results$`Expected cost reduction from DR` + (fig4_results$`expected Total costs` * fig4_results$MIPGapParam[noDR_sel]) # this should be related to noDR MIPGAP
fig4_results$`expected cost reduction hibound` = fig4_results$`Expected cost reduction from DR` + (fig4_results$`expected Total costs` * fig4_results$MIPGapParam) # this should be related to noDR MIPGAP
fig4_results$`expected cost reduction lowbound` = fig4_results$`Expected cost reduction from DR` #- (fig4_results$`expected Total costs` * fig4_results$MIPGapParam[noDR_sel])
fig4_results$`expected cost reduction hibound`[noDR_sel] = fig4_results$`Expected cost reduction from DR`[noDR_sel]
fig4_results$`cost reduction lowbound, frac` = fig4_results$`expected cost reduction lowbound` / fig4_results$`expected Total costs`[noDR_sel]
fig4_results$`cost reduction hibound, frac` = fig4_results$`expected cost reduction hibound` / fig4_results$`expected Total costs`[noDR_sel]


## make categories for runlevel, type ##

rand_sel = (fig4_results$type == "rand")
fig4_results$dr_varcost = as.factor(fig4_results$dr_varcost)
fig4_results$runlabel = substr(fig4_results$runID,1,as.vector(regexpr("_",fig4_results$runID))-1)
end = regexpr(fig4_results$runlabel, pattern = "[0-9]")
end[which(end<0)] = 50
fig4_results$runtype =  substr(fig4_results$runlabel,1,end-1)
fig4_results$runlevel = substr(fig4_results$runlabel,end,50)

##  relabeling ##
unconstrained = which(fig4_results$runlabel == "advNot1")
fig4_results$type[unconstrained] = "Unconstrained"
fig4_results$runlabel[unconstrained] = "Unconst"
fig4_results$type[which(fig4_results$type == "advNot")] = "Notification"
fig4_results$type[which(fig4_results$type == "avail")] = "Availability"
fig4_results$type[which(fig4_results$type == "energy")] = "Energy"
fig4_results$type[which(fig4_results$type == "hour")] = "Hour"

# to make 'constraint level' consistent, we need...
# advNot2 -> 1
fig4_results$runlevel[which(fig4_results$runlabel == "advNot2")] = 1
# advNot3 -> 2
fig4_results$runlevel[which(fig4_results$runlabel == "advNot3")] = 2
# hour3 -> 1
fig4_results$runlevel[which(fig4_results$runlabel == "hour3")] = 1
# hour1 -> 3
fig4_results$runlevel[which(fig4_results$runlabel == "hour1")] = 3
# energy3 > 1
fig4_results$runlevel[which(fig4_results$runlabel == "energy3")] = 1
# energy1 > 3
fig4_results$runlevel[which(fig4_results$runlabel == "energy1")] = 3


pd <- position_dodge(0.5)
# p = ggplot(filter(results,dr_varcost != 70, type != "start"), aes(x = type, y = `Expected cost reduction from DR`,
q = ggplot(filter(fig4_results,runtype != "start" & runtype != "noDR"& (dr_varcost == 35)), 
                  aes(x = type, y = `Expected cost reduction from DR, frac`,
                        # color = dr_varcost, shape = dr_varcost)) +
                          color = hydro, shape = runlevel))+
  # color = dr_varcost))+#, shape = hydro))+
  
  # geom_jitter(width = 0.5) +
  geom_point() +
  # geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.6) +
  geom_errorbar(aes(ymin = `cost reduction lowbound, frac`, ymax = `cost reduction hibound, frac`), alpha = 0.5,#, linetype = hydro),
                width=.3, position = pd) +
  theme_minimal() +
  scale_color_manual(name="Hydro\nFlexibility", values = modpalette[c(1,4)]) +
  scale_shape_discrete(name="Constraint Level") +
  labs(y = "Savings relative to no DR scenario", x = "Constraint Type") +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.002)) 

q
q + 
  ggsave(paste0(outputfol,"Figure4_06-15.png"), width = 6, height = 5)
 