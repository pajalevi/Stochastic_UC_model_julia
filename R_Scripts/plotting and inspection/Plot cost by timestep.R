# plots the change in cost by timestep between two runs
# March 2020, Patricia

# TODO make into a fn where the inputs are the two runs you want compared
# TODO add plot labels

library(tidyverse)

# load data - from consolidatedAnalysisFns - costByTimestep()
nodr = read_csv("/home/groups/weyant/plevi_outputs/noDR_o25_keyDays2_2019-12-04/plots/noDR_o25_keyDays2_allcost.csv")
# advnot = read_csv("/home/groups/weyant/plevi_outputs/advNot3_o25_c2_keyDays2_2019-12-12/plots/advNot3_o25_c2_keyDays2_allcost.csv")
advnot = read_csv("/home/groups/weyant/plevi_outputs/advNot3_o25_keyDays2_2019-12-12/plots/advNot3_o25_keyDays2_allcost.csv")
# advnot = read_csv("/home/groups/weyant/plevi_outputs/advNot2_o25_keyDays2_2019-12-11/plots/advNot2_o25_keyDays2_allcost.csv")


# add an ID to each & name cols appropriately
nodr$run="noDR"
advnot$run="advNot"

# merge them
alldat=bind_rows(nodr,advnot)

# find diff
diff = alldat %>%
  # spread(key = run, value = totcost) %>%
  pivot_wider(names_from = run, values_from = c(totcost, totprodcost, totstartcost))%>%
  group_by(t,scenario,nperiod) %>%
  # summarise(deltaCost = sum(totcost_advNot,na.rm=T) - sum(totcost_noDR,na.rm=T))
  summarise(deltaCost =totcost_advNot - totcost_noDR,
            deltaProdCost = totprodcost_advNot - totprodcost_noDR,
            deltaStartCost = totstartcost_advNot - totstartcost_noDR)

# deal with rounding errors
e = 1e-5
diff$deltaCost[diff$deltaCost < e & diff$deltaCost > (-1*e)]  = 0
diff$deltaProdCost[diff$deltaProdCost < e & diff$deltaProdCost > (-1*e)]  = 0
diff$deltaStartCost[diff$deltaStartCost < e & diff$deltaStartCost > (-1*e)]  = 0

expecteddiff = diff %>%
  group_by(t,nperiod) %>%
  summarise(meandeltacost = mean(deltaCost),
            mindeltacost = min(deltaCost),
            maxdeltacost = max(deltaCost),
            meandeltastartcost = mean(deltaStartCost),
            meandeltaprodcost = mean(deltaProdCost))
expecteddiff$rankt = rank(expecteddiff$t)

jumps = which(diff(expecteddiff$t)>1)

ggplot(diff, aes(x = t, y = deltaCost, color = scenario)) +
  facet_wrap(~nperiod, scales="free_x")+
  geom_line() + theme_minimal() +
  # geom_vline(xintercept = jumps+0.5, color = "red", linetype="dotted")+
  # ggsave("/home/groups/weyant/plevi_outputs/advNot3_o25_c2_keyDays2_2019-12-12/plots/advNot3_o25_c2_keyDays2_deltacost.png",
  ggsave("/home/groups/weyant/plevi_outputs/advNot3_o25_keyDays2_2019-12-12/plots/advNot3_o25_keyDays2_deltacost.png",
  # ggsave("/home/groups/weyant/plevi_outputs/advNot2_o25_keyDays2_2019-12-11/plots/advNot2_o25_keyDays2_deltacost.png",
         width = 20, height = 20)

ggplot(expecteddiff, aes(x = rankt, y = meandeltacost)) +
  geom_line()+
  geom_line(aes(x=rankt,  y = mindeltacost, color = "light blue"),linetype = "dotted") + 
  geom_line(aes(x=rankt,  y = maxdeltacost, color = "pink"),linetype = "dotted") +             
  theme_minimal() +
  geom_vline(xintercept = jumps+0.5, color = "blue", linetype="dotted")+
  scale_color_discrete(name = "", labels = c("Min","Max"))+
  # ggsave("/home/groups/weyant/plevi_outputs/advNot3_o25_c2_keyDays2_2019-12-12/plots/advNot3_o25_c2_keyDays2_meanDeltaCost.png",
  ggsave("/home/groups/weyant/plevi_outputs/advNot3_o25_keyDays2_2019-12-12/plots/advNot3_o25_keyDays2_meanDeltaCost.png",
  # ggsave("/home/groups/weyant/plevi_outputs/advNot2_o25_keyDays2_2019-12-11/plots/advNot2_o25_keyDays2_meanDeltaCost.png",
         width = 20, height = 5)

ggplot(expecteddiff[expecteddiff$meandeltacost!=0,],aes(x = meandeltacost))  +
  geom_histogram(breaks = seq(-15000, 15000, by = 60)) +
  theme_minimal()+
  labs(title = "Expected difference in cost due to presence of DR in each hour  (Zeros removed)")+
  # ggsave("/home/groups/weyant/plevi_outputs/advNot3_o25_c2_keyDays2_2019-12-12/plots/advNot3_o25_c2_keyDays2_meanDeltaCost_hist.png",
  # ggsave("/home/groups/weyant/plevi_outputs/advNot3_o25_keyDays2_2019-12-12/plots/advNot3_o25_keyDays2_meanDeltaCost_hist.png",
  ggsave("/home/groups/weyant/plevi_outputs/advNot2_o25_keyDays2_2019-12-11/plots/advNot2_o25_keyDays2_meanDeltaCost_hist.png",
         width = 10, height = 6)

# start
ggplot(expecteddiff[expecteddiff$meandeltastartcost!=0,],aes(x = meandeltastartcost))  +
  geom_histogram(breaks = seq(-15000, 15000, by = 60)) +
  theme_minimal()+
  labs(title = "Expected difference in startup cost due to presence of DR in each hour  (Zeros removed)")+
  # ggsave("/home/groups/weyant/plevi_outputs/advNot3_o25_c2_keyDays2_2019-12-12/plots/advNot3_o25_c2_keyDays2_meanDeltaStartCost_hist.png",
  ggsave("/home/groups/weyant/plevi_outputs/advNot3_o25_keyDays2_2019-12-12/plots/advNot3_o25_keyDays2_meanDeltaStartCost_hist.png",
  # ggsave("/home/groups/weyant/plevi_outputs/advNot2_o25_keyDays2_2019-12-11/plots/advNot2_o25_keyDays2_meanDeltaStartCost_hist.png",
         width = 10, height = 6)

# prod
ggplot(expecteddiff[expecteddiff$meandeltaprodcost!=0,],aes(x = meandeltaprodcost))  +
  geom_histogram(breaks = seq(-15000, 15000, by = 60)) +
  theme_minimal()+
  labs(title = "Expected difference in production cost due to presence of DR in each hour  (Zeros removed)")+
  # ggsave("/home/groups/weyant/plevi_outputs/advNot3_o25_c2_keyDays2_2019-12-12/plots/advNot3_o25_c2_keyDays2_meanDeltaProdCost_hist.png",
  ggsave("/home/groups/weyant/plevi_outputs/advNot3_o25_keyDays2_2019-12-12/plots/advNot3_o25_keyDays2_meanDeltaProdCost_hist.png",
  # ggsave("/home/groups/weyant/plevi_outputs/advNot2_o25_keyDays2_2019-12-11/plots/advNot2_o25_keyDays2_meanDeltaProdCost_hist.png",
         width = 10, height = 6)


# now repeat this for advNot2 (regular cost) for comparison


