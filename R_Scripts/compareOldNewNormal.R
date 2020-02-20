#compareOldNewNormal.R
# compare runs before and after oct 1st/2nd 2019
# try to do this in a way that actually makes sense
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
# 
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
results$`expected cost reduction lowbound` = results$`Expected cost reduction from DR` - (results$`expected Total costs` * results$MIPGapParam)
results$`expected cost reduction hibound` = results$`Expected cost reduction from DR` + (results$`expected Total costs` * results$MIPGapParam[noDR_sel]) # this should be related to noDR MIPGAP

results$`expected cost reduction hibound`[noDR_sel] = results$`Expected cost reduction from DR`[noDR_sel]

# View(results)
# View(results[!rand_sel,c("type","Expected cost reduction from DR","edition","dr_varcost")])
#View(results[,c("MIPGapParam","runID","expected Total costs","Expected cost reduction from DR","expected cost reduction hibound","expected cost reduction lowbound")])

rand_sel = (results$type == "rand")
results$dr_varcost = as.factor(results$dr_varcost)
pd <- position_dodge(0.5)
p = ggplot(results, aes(x = type, y = `Expected cost reduction from DR`, color = dr_varcost, shape = dr_varcost)) +
  # geom_jitter(width = 0.5) +
  geom_point() +
  # geom_text(aes(label = runID),size = 3) +
  geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `expected cost reduction hibound`), 
                width=.15, position = pd) +
  theme_minimal() +
  scale_color_manual(values = modpalette) +
  labs(y = "Savings from adding DR") 
p
p + 
  ggsave(paste0(outputfol,"new_results.png"), width = 6, height = 5)



p + aes(y = `Expected cost reduction from DR, frac`, shape = dr_varcost) + scale_y_continuous(labels = scales::percent) 

