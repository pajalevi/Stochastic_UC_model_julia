#compareOldNewNormal.R
# compare runs before and after oct 1st/2nd 2019
# try to do this in a way that actually makes sense
# Used to make Fig 1, Fig 4
library(plyr) #overwrites arrange, count, desc, failwith, id, mutate, rename, summarise, summarize
library(tidyverse)
library(scales)
modpalette = brewer_pal(palette = "Dark2")(6)
modpalette[1] = "blue"

results = read_csv(file = file.choose())

outputfol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/"
# oldResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_INFORMSruns_NovAnalysis.csv")
# newResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_postINFORMS_newNormal_all.csv")
# revertResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_preINFORMS_revert_attempt_11-7.csv")
# reprocessedResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_preINFORMS_reprocessed_prod_11-14.csv")
# #new70results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_INFORMS_70DR (1).csv")
# new70results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_postINFORMS_all.csv")

results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/combined_summary_05-01_new_gendat_noNAs.csv") #Fig 1
results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/combined_summary_082420_slowgas_dur_test.csv") #Fig 1 - take 2

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
noDR_sel = which(results$type == "noDR")[2] # with august runs
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
results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/combined_summary_08-21.csv")#09-08.csv") #Fig 1 - take 2
results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/combined_summary_09-08.csv") #Fig 1 - take 2

pd <- position_dodge(0.25)
# results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/combined_summary_05-01_new_gendat_noNAs.csv") #Fig 1

noDR_sel = which(results$type == "noDR")[2]
results$cost_lowbound = results$`expected Total costs` - (results$`expected Total costs` * results$MIPGapParam)
# results$`expected cost reduction lowbound` = results$`Expected cost reduction from DR` - (results$`expected Total costs` * results$MIPGapParam)
# results$`expected cost reduction hibound` = results$`Expected cost reduction from DR` + (results$`expected Total costs` * results$MIPGapParam[noDR_sel]) # this should be related to noDR MIPGAP
results$`expected cost reduction hibound` = results$`Expected cost reduction from DR` + (results$`expected Total costs` * results$MIPGapParam) # this should be related to noDR MIPGAP
results$`expected cost reduction lowbound` = results$`Expected cost reduction from DR` #- (results$`expected Total costs` * results$MIPGapParam[noDR_sel])

results$`expected cost reduction hibound`[noDR_sel] = results$`Expected cost reduction from DR`[noDR_sel]

rand_sel = (results$type == "rand")
results$dr_varcost = as.factor(results$dr_varcost)
results$runlabel = substr(results$runID,1,as.vector(regexpr("_",results$runID))-1)
end = regexpr(results$runlabel, pattern = "[0-9]")
end[which(end<0)] = 50
results$runtype =  substr(results$runlabel,1,end-1)
results$runlevel = substr(results$runlabel,end,50)

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
fig_results$type[which(fig_results$type == "dur")] = "Duration"
fig_results = fig_results[!str_detect(fig_results$runID, "slowgas"),]

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
# avail 1 (evening) > 2
  fig_results$runlevel[which(fig_results$runlabel == "avail1")] = 2
# avail 2 (daytime) > 1
  fig_results$runlevel[which(fig_results$runlabel == "avail2")] = 1
# avail 3 (workhours) > 4
  fig_results$runlevel[which(fig_results$runlabel == "avail3")] = 4
# avail 4 (after work) > 3
  fig_results$runlevel[which(fig_results$runlabel == "avail4")] = 3
  

pd <- position_dodge(0.9)
p = ggplot(filter(fig_results,runtype != "start" & runtype != "noDR" & 
                    (dr_varcost == 35 | dr_varcost == 70 | dr_varcost == 10000)), 
           # aes(x = type, y = `Expected cost reduction from DR`,
           aes(x = type, y = `Expected cost reduction from DR, frac`,
                        color = dr_varcost, shape = runlevel)) +
  geom_point(size = 2) +
  # geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.6) +
  geom_errorbar(aes(ymin = `cost reduction lowbound, frac`, ymax = `cost reduction hibound, frac`), alpha = 0.5,#, linetype = hydro),
                width=.3, position = pd) +
  theme_minimal() +
  # scale_color_manual(values = modpalette) +
  labs(y = "Savings relative to no DR scenario", x = "Constraint Type") +
  scale_color_manual(name="DR Variable\nCost", values = modpalette) +
  scale_shape_discrete(name="Constraint Level") +
  scale_y_continuous(labels = scales::percent, limits = c(-0.00005,0.002)) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))
  

p
p + 
  ggsave(paste0(outputfol,"Figure1_08-21.png"), width = 6*0.85, height = 5*0.8)
##### Stuff for rand plat ######
fig_results$type[which(fig_results$type == "rand")] = "Reliability\n(plus notification)"

fig_results$runlevel[which(fig_results$runID == "rand_o1_100mean_keyDays2")] = 5
fig_results$runlevel[which(fig_results$runID == "rand_o2_100mean_keyDays2")] = 1
fig_results$runlevel[which(fig_results$runID == "rand_o2_90mean_keyDays2")] = 2
fig_results$runlevel[which(fig_results$runID == "rand_o2_70mean_keyDays2")] = 3
fig_results$runlevel[which(fig_results$runID == "rand_o2_50mean_keyDays2")] = 4

p = ggplot(filter(fig_results,runtype != "start" & runtype != "noDR" & 
                    (dr_varcost == 35 | dr_varcost == 70 | dr_varcost == 10000)), 
           # aes(x = type, y = `Expected cost reduction from DR`,
           aes(x = type, y = `Expected cost reduction from DR, frac`,
               color = dr_varcost, shape = runlevel)) +
  geom_point(size = 2) +
  # geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.6) +
  geom_errorbar(aes(ymin = `cost reduction lowbound, frac`, ymax = `cost reduction hibound, frac`), alpha = 0.5,#, linetype = hydro),
                width=.3, position = pd) +
  theme_minimal() +
  # scale_color_manual(values = modpalette) +
  labs(y = "Savings relative to no DR scenario", x = "Constraint Type") +
  scale_color_manual(name="DR Variable\nCost", values = modpalette) +
  scale_shape_discrete(name="Constraint Level", labels = c("1","2","3","4","1: One realization")) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.00005,0.002)) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

p

p + 
  ggsave(paste0(outputfol,"fig1_withRand_o1_10-06.png"), width = 6.5, height = 5)


#### INFORMS presentation plots ####
# make group
fig_results$Category = "NA"
fig_results$Category[which(fig_results$type == "Availability")] = "Other Limits" #"Availability Restrictions"
fig_results$Category[which(fig_results$type == "Duration")] = "Other Limits"
fig_results$Category[which(fig_results$type == "Energy")] = "Other Limits"
fig_results$Category[which(fig_results$type == "Hour")] = "Other Limits"
fig_results$Category[which(fig_results$type == "Unconstrained")] = "Unlimited"
fig_results$Category[which(fig_results$type == "Reliability\n(plus notification)")] = "Reliability Limited"
fig_results$Category[which(fig_results$type == "Notification")] = "Other Limits"#"Notification Limited" #
fig_results$Category = factor(fig_results$Category, levels = c("Unlimited","Other Limits","Availability Restrictions","Notification Limited","Reliability Limited","NA"))

modpalette[7] = "black"
fig_results$runlevel = as.character(fig_results$runlevel)
pd <- position_dodge(0.9)
pq = ggplot(filter(fig_results,runtype != "start" & runtype != "noDR" & #runtype != "rand" &
                    (dr_varcost == 35 ) &#| dr_varcost == 70 | dr_varcost == 10000) &
                     runID != "rand_o1_100mean_keyDays2"), 
           # aes(x = type, y = `Expected cost reduction from DR`,
           aes(x = Category, #type,#
               y = `Expected cost reduction from DR, frac`,
               color = Category, #type,#
               alpha = runlevel, shape = dr_varcost)) +
  # facet_wrap(~dr_varcost, ncol=1) +
  geom_point(size = 2, position = position_dodge(0.1)) +
  # geom_text(aes(label = runlabel),size = 2,vjust = 0, hjust  = -0.6) +
  geom_errorbar(aes(ymin = `cost reduction lowbound, frac`, ymax = `cost reduction hibound, frac`),# alpha = 0.5,#, linetype = hydro),
                width=0, position = position_dodge(0.1)) +
  theme_minimal() +
  # scale_color_manual(values = modpalette) +
  labs(y = "Savings relative to no DR scenario", x = "Constraint Type") +
  scale_color_manual(name="DR Constraint\nType", values = modpalette) +
  scale_alpha_manual(name="Constraint Level", labels = c("1","2","3","4","1: One realization"), values = c(1,.7,.5,.3,1)) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.00005,0.002)) + 
  scale_shape_manual(values = c(16,4), name = "Variable cost\n($/MWh)") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
pq
pq + ggsave(paste0(outputfol,"fig3_presentation_10-20.png"), width = 6.5, height = 5)

############ FIGURE 4 ################
results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/combined_summary_05-01_new_gendat_noNAs.csv") #Fig 1 # the 'new gendat' is just an update of co2 emissions
slowhydroresults =  read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/combined_summary_slow_hydro_04-05.csv")
#this combined_summary has results from a different noDR run (Dec instead of Feb). Use the newer one. 

slowhydroresults$hydro  = "slow"
# otherresults  = filter(otherresults,!str_detect(runID,"advNot1_o25_c2"))
results$hydro = "fast"
# results$hydro[str_detect(results$runID,"slowgas")] = "slow"
# fig4_results = results
fig4_results = rbind.fill(slowhydroresults,results)

noDR_sel = (fig4_results$type == "noDR")
fig4_results$date[noDR_sel]
noDR_sel = which(noDR_sel)[2]
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
# avail 1 (evening) > 2
fig4_results$runlevel[which(fig4_results$runlabel == "avail1")] = 2
# avail 2 (daytime) > 1
fig4_results$runlevel[which(fig4_results$runlabel == "avail2")] = 1
# avail 3 (workhours) > 4
fig4_results$runlevel[which(fig4_results$runlabel == "avail3")] = 4
# avail 4 (after work) > 3
fig4_results$runlevel[which(fig4_results$runlabel == "avail4")] = 3



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
  scale_y_continuous(labels = scales::percent, limits = c(0,0.002)) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

q
q + 
  ggsave(paste0(outputfol,"Figure4_08-17.png"), width = 6*0.8, height = 5*0.8)
 