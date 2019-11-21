#compareOldNewNormal.R
# compare runs before and after oct 1st/2nd 2019
# try to do this in a way that actually makes sense
library(tidyverse)
library(plyr) #overwrites arrange, count, desc, failwith, id, mutate, rename, summarise, summarize

outputfol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/"
oldResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_INFORMSruns_NovAnalysis.csv")
newResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_postINFORMS_newNormal_all.csv")
revertResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_preINFORMS_revert_attempt_11-7.csv")
reprocessedResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_preINFORMS_reprocessed_prod_11-14.csv")
#new70results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_INFORMS_70DR (1).csv")
new70results = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_postINFORMS_all.csv")

# oldResults$edition = "old"
newResults$edition = "new"
revertResults$edition = "revert"
# reprocessedResults$edition = "old-reprocess"
new70results$edition = "new70"

results = rbind.fill(newResults,revertResults,new70results)
results$dr_varcost = as.factor(results$dr_varcost)
results$cost_lowbound = results$`expected Total costs` - (results$`expected Total costs` * results$MIPGap)
results$`expected cost reduction lowbound` = results$`Expected cost reduction from DR` - (results$`expected Total costs` * results$MIPGap)
results$`expected cost reduction hibound` = results$`Expected cost reduction from DR` + (results$`expected Total costs` * results$MIPGap)
noDR_sel = (results$type == "noDR")
results$`expected cost reduction hibound`[noDR_sel] = results$`Expected cost reduction from DR`[noDR_sel]

# View(results)
# View(results[!rand_sel,c("type","Expected cost reduction from DR","edition","dr_varcost")])

rand_sel = (results$type == "rand")
results$dr_varcost = as.factor(results$dr_varcost)
pd <- position_dodge(0.5)
p = ggplot(results[!rand_sel,], aes(x = type, y = `Expected cost reduction from DR`, color = dr_varcost, shape = dr_varcost)) +
  # geom_jitter(width = 0.2) +
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

