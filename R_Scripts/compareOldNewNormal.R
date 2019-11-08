#compareOldNewNormal.R
# compare runs before and after oct 1st/2nd 2019
# try to do this in a way that actually makes sense
library(tidyverse)
library(plyr) #overwrites arrange, count, desc, failwith, id, mutate, rename, summarise, summarize

outputfol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/"
oldResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_INFORMSruns_NovAnalysis.csv")
newResults = read_csv("/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/INFORMS_results/combined_summary_postINFORMS_newNormal_all.csv")

oldResults$edition = "old"
newResults$edition = "new"

results = rbind.fill(oldResults,newResults)
results$dr_varcost = as.factor(results$dr_varcost)
results$cost_lowbound = results$`expected Total costs` - (results$`expected Total costs` * results$MIPGap)
results$`expected cost reduction lowbound` = results$`Expected cost reduction from DR` - (results$`expected Total costs` * results$MIPGap)

View(results)

rand_sel = (results$type == "rand")

pd <- position_dodge(0.5)
p = ggplot(results[!rand_sel,], aes(x = type, y = `Expected cost reduction from DR`, color = edition, shape = dr_varcost)) +
  # geom_jitter(width = 0.2) +
  geom_point() +
  geom_errorbar(aes(ymin = `expected cost reduction lowbound`, ymax = `Expected cost reduction from DR`), 
                width=.15, position = pd) +
  theme_minimal() +
  scale_color_manual(values = modpalette) +
  labs(y = "Savings from adding DR") 
p + 
  ggsave(paste0(outputfol,"old_vs_new_results.png"), width = 6, height = 5)



p + aes(y = `Expected cost reduction from DR, frac`, shape = dr_varcost) + scale_y_continuous(labels = scales::percent) 

