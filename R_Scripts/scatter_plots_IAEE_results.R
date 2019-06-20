drstats = read_csv(file = file.choose()) #basic_stats.csv in IAEE presentation folder
names(drstats) = c("Scenario","order","Savings Percent","Savings per MWh","Total MWh",
                   "Total savings","Commitment Hours","Category","Limit Type")
drstats$Category = factor(drstats$Category, levels = c("Unlimited","Notification Limits","Other Limits"))

plotfile = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/forIAEE_1Pmin/plots/"

library(scales)
modpalette = brewer_pal(palette = "Dark2")(6)
modpalette[1] = "blue"

modpalette3 = modpalette[c(6,3,2)]

ggplot(drstats,aes(x=Category,y=`Savings Percent`, color = `Limit Type`)) +
  geom_jitter(width = 0.2) + 
  scale_y_continuous(labels = scales::percent) + theme_minimal() +
  # scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = modpalette) +
  labs(y = "Savings from adding DR") +
  ggtitle("System cost reductions from different DR Types")
ggsave(filename = paste0(plotfile,"scatter_total_savings_limit_type.png"), width = 5, height = 4)

# without color by limit type
ggplot(drstats,aes(x=Category,y=`Savings Percent`, color = Category)) +
  geom_jitter(width = 0.2) + 
  scale_y_continuous(labels = scales::percent) + theme_minimal() +
  # scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = modpalette3) +
  labs(y = "Savings from adding DR") +
  ggtitle("System cost reductions from different DR Types")
ggsave(filename = paste0(plotfile,"scatter_total_savings_category_type.png"), width = 5.3, height = 4)


# By savings per MWh
ggplot(drstats,aes(x=Category,y=`Savings per MWh`, color = Category)) +
  geom_jitter(width = 0.2) + 
  theme_minimal() +
  # scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = modpalette3) +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Value per MWh of DR used") +
  labs(y="Savings per MWh of demand shed")
ggsave(filename = paste0(plotfile,"scatter_savings_perMWh_limit_type.png"), width = 5.3, height = 4)

# by hours of commitment
tothrs = 25*88*24
ggplot(drstats,aes(x=Category,y=`Commitment Hours`/52800, color = `Limit Type`)) +
  geom_jitter(width = 0.2) + 
  scale_y_continuous(labels = scales::percent,breaks = seq(0,1,0.2)) +
  theme_minimal() +
  # scale_color_brewer(palette = "Dark2") +
  # scale_color_viridis(discrete = T, option = "E")+
  scale_color_manual(values = modpalette)+
  ggtitle("Overcommitment by some types of DR") + 
  labs(y="Percent of all hours DR is commited")

ggsave(filename = paste0(plotfile,"scatter_commitent_limit_type.png"), width = 5.3, height = 4)

# by hours of commitment - just unlimited and advance notification
tothrs = 25*88*24
drstats_sub = filter(drstats, Category %in% c("Unlimited","Notification Limits"))
ggplot(drstats_sub,aes(x=Category,y=`Commitment Hours`/52800, color = `Limit Type`)) +
  geom_point() + 
  scale_y_continuous(labels = scales::percent, limits = c(0,NA)) +
  theme_minimal() +
   scale_color_manual(values = modpalette3[c(2,1,3)])+
  ggtitle("Overcommitment by notification-limited DR") + 
  labs(y="Percent of all hours DR is commited")

ggsave(filename = paste0(plotfile,"scatter_commitent_limit_type_notification.png"), width = 5.3, height = 4)



# make marginal cost plot
ggplot(drstats, aes(x = `Total MWh`/25, y = `Savings per MWh`)) + 
  geom_jitter(shape = 20, width = 100, height = 0.5) +
  # geom_point() + 
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar,"Savings per MWh of demand shed", limits = c(0,45)) +
  scale_x_continuous(labels = scales::comma, "Mean MWh shed by DR", limits = c(0,NA)) +
  ggtitle("Marginal value of DR declines quickly")
ggsave(filename = paste0(plotfile,"marginal_value.png"), width = 7, height = 4)


# make marginal cost plot with color by type
ggplot(drstats, aes(x = `Total MWh`/25, y = `Savings per MWh`, color = `Limit Type`)) + 
  geom_jitter(shape = 20, width = 100, height = 0.5) +
  # geom_point(shape = 20) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar,"Savings per MWh of demand shed", limits = c(0,45)) +
  scale_x_continuous(labels = scales::comma, "Mean MWh shed by DR", limits = c(0,NA)) +
  scale_color_manual(values = modpalette) +
  ggtitle("Marginal value of DR declines quickly")
ggsave(filename = paste0(plotfile,"marginal_value_limit_type.png"), width = 6, height = 4)

# make value vs hours committed with color by type
ggplot(drstats, aes(y = `Savings Percent`, x = `Commitment Hours`/52800, color = `Limit Type`)) + 
  # geom_jitter(shape = 20, width = 100, height = 0.5) +
  geom_point(shape = 20) +
  theme_minimal() +
  scale_y_continuous("Savings from adding DR") +
  scale_x_continuous(labels = scales::percent, "Percent of hours committed") +
  scale_color_manual(values = modpalette) 
  ggtitle("Marginal value of DR declines quickly")
# ggsave(filename = paste0(plotfile,"marginal_value_limit_type.png"), width = 6, height = 4)

  ggplot(drstats, aes(y = `Total MWh`/25, x = `Commitment Hours`/52800, color = `Limit Type`)) + 
    geom_jitter(width = 0.005, height = 10) +
    # geom_point(shape = 20) +
    theme_minimal() +
    scale_y_continuous("MWh shed") +
    scale_x_continuous(labels = scales::percent, "Percent of hours committed") +
    scale_color_manual(values = modpalette) 
  ggtitle("Marginal value of DR declines quickly")
  
