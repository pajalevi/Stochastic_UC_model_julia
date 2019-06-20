# demand_forecast_error.R
# load SEED EBA_BA_level.csv and look for demand forecast info
library(tidyverse)
library(lubridate)

data_directory = "/Users/patricia/Documents/Google Drive/stanford/SEED/"
all_dat = read_csv(paste0(data_directory,"EBA_BA_level.csv"))
plots_dir = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/plots/ERCOT_data/"
data_out = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/unformatted data/"

# select PJM-ALL columns
PJM = select(all_dat, contains("PJM-ALL"))
caiso = select(all_dat, contains("CISO-ALL"))
ercot = select(all_dat, X1,contains("ERCO-ALL")) #ERCOT
rm(all_dat)

# select date column 
# this loads as a 'date-time' object aka POSIXct
# manipulable with functions from package 'lubridate'
date = select(all_dat, contains("X1")) 
date$year = year(date$X1)
date$month = month(date$X1)
date$hour = hour(date$X1)
date$yday = yday(date$X1)
date$wday = wday(date$X1, label=TRUE)

#########################
#ERCOT analysis

ercot$error = ercot$`EBA.ERCO-ALL.DF.H` - ercot$`EBA.ERCO-ALL.D.H`
ggplot(ercot,aes(error)) + geom_histogram() 
# looks sorta normal
summary(ercot$error)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -5522.00  -577.00   -67.00   -33.55   470.00  6303.00      639 
sd(ercot$error, na.rm=T)
# [1] 1004.788

# save ERCOT data to be read in by model later
ercot_save = ercot
names(ercot_save) = c("time_UTC","demand_forecast","total_interchange","demand","net_generation","forecast_error")
write_csv(ercot_save, path = paste0(data_out,"/ercot_demand_data.csv"))


# plot with a normal curve
# from https://stackoverflow.com/questions/6967664/ggplot2-histogram-with-normal-curve
#
n = nrow(ercot)
mean = mean(ercot$error, na.rm=T)
sd = sd(ercot$error, na.rm=T)
binwidth = 500 # passed to geom_histogram and stat_function
ggplot(ercot, aes(x = error, mean = mean, sd = sd, binwidth = binwidth, n = n)) +
  theme_bw() +
  geom_histogram(binwidth = binwidth, 
                 colour = "white", fill = "cornflowerblue", size = 0.1) +
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd) * n * binwidth,
                color = "darkred", size = 1) + ggtitle(paste("Error in ercot day-ahead demand forecast \n",
                                                             "Approx. normal, mean", round(mean,2)," stdev", round(sd,2)))
#----------------
# probability by day of week
#----------------
dercot = cbind(ercot, date)
# first check March 11, 2018 for DST shifts
DST_spring = which(dercot$year == 2018 & dercot$month == 4 & day(dercot$X1)==11)
View(dercot[DST_spring,])

dercot$errorfrac = (dercot$`EBA.ERCO-ALL.DF.H` - dercot$`EBA.ERCO-ALL.D.H`)/dercot$`EBA.ERCO-ALL.D.H`
ggplot(dercot,aes(error)) + geom_histogram(binwidth = 500) + facet_wrap(~wday)
ggplot(dercot,aes(y=errorfrac,x=wday, group=wday)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.shape = 1, outlier.size = 0.5, outlier.color = "dark grey") +
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+
  ggtitle("Distribution of day-ahead demand forecast error")+
  geom_hline(yintercept=0, color = "blue")

ggsave(filename = paste0(plots_dir,"demand_boxplot_dayofweek.png"))

# plots of std dev and mean error
stats = dercot %>%
  group_by(wday) %>%
  summarise(stddev = sd(errorfrac, na.rm=T),
            mean = mean(errorfrac,na.rm=T))
ggplot(stats, aes(x=wday, y = stddev)) + geom_point() + # wow this is super useful
  ggtitle("Standard deviation of forecast error by day of the week")+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  geom_hline(yintercept=0, color = "blue")
ggsave(filename = paste0(plots_dir,"stdev_demand_dayofweek.png"))

ggplot(stats, aes(x=wday, y = mean)) + geom_point() + # wow this is super useful
  ggtitle("Mean forecast error fraction by day of the week")+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  geom_hline(yintercept=0, color = "blue")
ggsave(filename = paste0(plots_dir,"mean_demand_dayofweek.png"))

# make col for wkday/wkend
dercot$wday_num = wday(dercot$X1)
dercot$wknd = "weekday"
dercot$wknd[dercot$wday_num == 1 | dercot$wday_num == 7] = "weekend"
ggplot(dercot,aes(y=errorfrac,x=wknd, group=wknd)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.shape = 1, outlier.size = 0.5, outlier.color = "dark grey") +
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  ggtitle("Distribution of day-ahead \n demand forecast error")+
  geom_hline(yintercept=0, color = "blue")

ggsave(filename = paste0(plots_dir,"demand_boxplot_weekend.png"))

#----------------
# probability by month/season
#----------------
ggplot(dercot,aes(y=errorfrac,x=month, group=month)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.shape = 1, outlier.size = 0.5, outlier.color = "dark grey") +
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  ggtitle("Distribution of day-ahead \n demand forecast error")+
  geom_hline(yintercept=0, color = "blue")

ggsave(filename = paste0(plots_dir,"demand_boxplot_month.png"))

# plots of std dev and mean error
stats = dercot %>%
  group_by(month) %>%
  summarise(stddev = sd(errorfrac, na.rm=T),
            mean = mean(errorfrac,na.rm=T))
ggplot(stats, aes(x=month, y = stddev)) + geom_point() + # wow this is super useful
   scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  ggtitle("Standard deviation of day-ahead demand forecast error")+
  geom_hline(yintercept=0, color = "blue")
ggsave(filename = paste0(plots_dir,"stdev_demand_month.png"))

ggplot(stats, aes(x=month, y = mean)) + geom_point() + # wow this is super useful
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  ggtitle("Mean of day-ahead demand forecast error by month")+
  geom_hline(yintercept=0, color = "blue")
ggsave(filename = paste0(plots_dir,"mean_demand_month.png"))

# make col for season
dercot$season = "1_winter"
dercot$season[dercot$month >= 3 & dercot$month <= 5] = "2_spring"
dercot$season[dercot$month >= 6 & dercot$month <= 8] = "3_summer"
dercot$season[dercot$month >= 9 & dercot$month <= 11] = "4_fall"
ggplot(dercot,aes(y=errorfrac,x=season, group=season)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.shape = 1, outlier.size = 0.5, outlier.color = "dark grey") +
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  ggtitle("Distribution of day-ahead \n demand forecast error")+
  geom_hline(yintercept=0, color = "blue")

ggsave(filename = paste0(plots_dir,"demand_boxplot_season.png"))

#----------------
# probability by time of day
#----------------
ggplot(dercot,aes(y=errorfrac,x=hour, group=hour)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.shape = 1, outlier.size = 0.5, outlier.color = "dark grey") +
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  ggtitle("Distribution of day-ahead demand forecast error")+
  geom_hline(yintercept=0, color = "blue")
ggsave(filename = paste0(plots_dir,"demand_boxplot_hour.png"))
# jumps out that error is smaller during morning/afternoon, and increases in the night
# does this pattern hold across seasons?
ggplot(dercot,aes(y=errorfrac,x=hour, group=hour)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.shape = 1, outlier.size = 0.5, outlier.color = "dark grey") +
  facet_wrap(~season)+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  ggtitle("Distribution of day-ahead demand forecast error")+
  geom_hline(yintercept=0, color = "blue")
ggsave(filename = paste0(plots_dir,"demand_boxplot_season_hour.png"))
# seems like in all seasons but winter there is a substantial diurnal pattern. 
# could divide day into night, early morning, late morning/early aft, late aft/early evening

# find SD by hour
stats = dercot %>%
  group_by(hour) %>%
  summarise(stddev = sd(errorfrac, na.rm=T),
            mean = mean(errorfrac,na.rm=T))
ggplot(stats, aes(x=hour, y = stddev)) + geom_point() + # wow this is super useful
  ggtitle("Standard deviation of forecast error by hour") +
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  geom_hline(yintercept=0, color = "blue")
ggsave(filename = paste0(plots_dir,"stdev_demand_hour.png"))

ggplot(stats, aes(x=hour, y = mean)) + geom_point() + # wow this is super useful
  ggtitle("Mean forecast error fraction by hour") +
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  geom_hline(yintercept=0, color = "blue")
ggsave(filename = paste0(plots_dir,"mean_demand_hour.png"))

# sd and mean by hour/season
stats = dercot %>%
  group_by(hour, season) %>%
  summarise(stddev = sd(errorfrac, na.rm=T),
            mean = mean(errorfrac,na.rm=T))
ggplot(stats, aes(x=hour, y = stddev)) + geom_point() + # wow this is super useful
  ggtitle("Standard deviation of forecast error by hour") +
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  facet_wrap(~season)+
  geom_hline(yintercept=0, color = "blue")
ggsave(filename = paste0(plots_dir,"stdev_demand_hour_season.png"))

ggplot(stats, aes(x=hour, y = mean)) + geom_point() + # wow this is super useful
  ggtitle("Mean of forecast error by hour") +
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  facet_wrap(~season)+
  geom_hline(yintercept=0, color = "blue")
ggsave(filename = paste0(plots_dir,"mean_demand_hour_season.png"))

# sd and mean by hour/wday/season
# there are ~16 of each wday per season, so for each point the sample size is 16*3 years = 48
stats = dercot %>%
  group_by(hour, wday,season) %>%
  summarise(stddev = sd(errorfrac, na.rm=T),
            mean = mean(errorfrac,na.rm=T))
ggplot(stats, aes(x=hour, y = stddev)) + geom_line() + # wow this is super useful
  ggtitle("Standard deviation of forecast error by hour") +
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  facet_grid(season~wday)+
  geom_hline(yintercept=0, color = "blue")
ggsave(filename = paste0(plots_dir,"stdev_demand_hour_wday.png"))

ggplot(stats, aes(x=hour, y = mean)) + geom_line() + # wow this is super useful
  ggtitle("Mean of forecast error by hour") +
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+  
  facet_grid(season~wday)+
  geom_hline(yintercept=0, color = "blue")
ggsave(filename = paste0(plots_dir,"mean_demand_hour_wday.png"))

#########################
# Create distribution stats for day/night by season ####
# Try to match mean, sd.


# divide by day/night
dercot = dercot %>%
  mutate(daytime = (hour > 5 & hour < 19))

#summarise
dercot2 = dercot %>%
  group_by(daytime, season) %>%
  summarise(stddev = sd(errorfrac, na.rm=T),
            mean = mean(errorfrac,na.rm=T))

p_ctr = 0.73
p_side = (1-p_ctr)/2
diff_side = 2 #multiply by SD

dercot2$high = dercot2$mean + diff_side*dercot2$stddev
dercot2$low = dercot2$mean - diff_side*dercot2$stddev

# calculate stddev of created dist
dercot2 = dercot2 %>%
  mutate(sim_sd = sqrt((mean^2 * p_ctr - mean^2) + (high^2*p_side - mean^2) + (low^2*p_side - mean^2)),
         sd_error = stddev - sim_sd)
View(dercot2)

dercot2$mean_prob = p_ctr

write_csv(dercot2, path = paste0(data_out,"probabilities/symmetric_demandForecastError_daytime_season.csv"))
#########################
#CAISO analysis
caiso$error = caiso$`EBA.CISO-ALL.DF.H` - caiso$`EBA.CISO-ALL.D.H`
ggplot(caiso,aes(error)) + geom_histogram() 
# looks sorta normal
summary(caiso$error)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -5522.00  -577.00   -67.00   -33.55   470.00  6303.00      639 
sd(caiso$error, na.rm=T)
# [1] 1004.788

# plot with a normal curve
# from https://stackoverflow.com/questions/6967664/ggplot2-histogram-with-normal-curve
#
n = nrow(caiso)
mean = mean(caiso$error, na.rm=T)
sd = sd(caiso$error, na.rm=T)
binwidth = 500 # passed to geom_histogram and stat_function
ggplot(caiso, aes(x = error, mean = mean, sd = sd, binwidth = binwidth, n = n)) +
  theme_bw() +
  geom_histogram(binwidth = binwidth, 
                 colour = "white", fill = "cornflowerblue", size = 0.1) +
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd) * n * binwidth,
                color = "darkred", size = 1) + ggtitle(paste("Error in CAISO day-ahead demand forecast \n",
                                                             "Approx. normal, mean", round(mean,2)," stdev", round(sd,2)))
  
# looks pretty good!

#PJM analysis
pjm$error = pjm$`EBA.PJM-ALL.DF.H` - pjm$`EBA.PJM-ALL.D.H`
ggplot(pjm,aes(error)) + geom_histogram()
# this seems to be pretty bimodal. whats going on?
ggplot(pjm,aes(error)) + geom_histogram() + scale_x_continuous(limits = c(-15000, 15000))#coord_cartesian(xlim = c(-15000, 15000))
# sort of normal
ggplot(pjm,aes(error)) + geom_histogram() + scale_x_continuous(limits = c(-90000, -30000))#coord_cartesian(xlim = c(-90000, -30000))
# not particularly normal, but also much less frequent
summary(pjm$error)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  -93635   -2796   -1119   -2669     330   14777    1324 
sd(pjm$error, na.rm=T)
# [1] 10406.94

# what if we just look at the stuff centered at 0?

# also, look at D and DF to see whats happening
plot(y = pjm$`EBA.PJM-ALL.DF.H`, x = 1:nrow(pjm), type = "l", ylim = c(20000,150000), xlim = c(0,2000))
# par(new=TRUE)
# plot(y = pjm$`EBA.PJM-ALL.D.H`, x = 1:nrow(pjm), type = "l", col="green", ylim = c(20000,150000) , xlim = c(0,1000))
abline(h = 55000, col = "red")
abline(v = start)
# looks like theres some sort of error in the forecast in the early data

start = max(which(pjm$`EBA.PJM-ALL.DF.H` < 56000), na.rm=T) + 1000
pjm_new = pjm[start:nrow(pjm),]
ggplot(pjm_new,aes(error)) + geom_histogram()
summary(pjm_new$error)
# so there's still a low outlier but for the most part seems OK

# plot with a normal curve
# from https://stackoverflow.com/questions/6967664/ggplot2-histogram-with-normal-curve
#
n = nrow(pjm_new)
mean = mean(pjm_new$error[which(pjm_new$error >-20000)], na.rm=T)
sd = sd(pjm_new$error[which(pjm_new$error >-20000)], na.rm=T)
binwidth = 500 # passed to geom_histogram and stat_function
ggplot(pjm_new, aes(x = error, mean = mean, sd = sd, binwidth = binwidth, n = n)) +
  theme_bw() +
  geom_histogram(binwidth = binwidth, 
                 colour = "white", fill = "cornflowerblue", size = 0.1) +
  stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd) * n * binwidth,
                color = "darkred", size = 1)+
  xlim(-10000,10000)+ ggtitle(paste("Error in PJM day-ahead demand forecast \n",
                                    "Approx. normal, mean", round(mean,2)," stdev", round(sd,2)))
# also very normal! YES I LOVE NORMAL DATA WOOHOO

#---------------------------------------------
### autoregressive properties ####
# for (lag in 1:5){
lag = 2
plot(x = caiso$error[(1+lag):nrow(caiso)], y = caiso$error[1:(nrow(caiso)-lag)])
title(main = paste("lag of", lag))
# }
# looks pretty linear - good candidate for an autoregressive model
# linear relationship seems to disappear by lag = 4

for (lag in 1:5){
# lag = 1
plot(x = pjm_new$error[(1+lag):nrow(pjm_new)], y = pjm_new$error[1:(nrow(pjm_new)-lag)])
title(main = paste("PJM: lag of", lag))
}
# got just a few outliers in there but pretty linear