# inspect CO2 distributions
library(tidyverse)
datafolder = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/Production_jan2020/"
run = "advNot1_o25_keyDays2_2019-12-11"
co2emit = read_csv(file = paste0(datafolder,run,"_co2data.csv"))

last_loc = as.vector(regexpr("\\_[^\\_]*$", run))
runID = substr(run, 1, last_loc - 1)

co2emit_sum = co2emit %>%
  group_by(Fuel,scenario) %>%
  summarise(co2tot = sum(co2)) %>%
  group_by(Fuel) %>%
  summarise(co2mean = mean(co2tot),
            co2min = min(co2tot),
            co2max = max(co2tot))

ggplot(co2emit_sum,aes(x=Fuel,y=co2mean)) + 
  theme(axis.text.x =  element_text(angle=90,hjust=1)) +
  geom_errorbar(aes(ymin = co2min,  ymax = co2max), color = "grey") +
  geom_point() + theme_minimal() +
  geom_text(aes(label = round(co2mean,0)),size = 2,vjust=-1, angle = 15) +
  labs(title  = paste("CO2 emissions  by generator type,",runID,"\n error bars give range across scenarios"),
       y = "Tons CO2 emitted")
