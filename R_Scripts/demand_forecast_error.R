# demand_forecast_error.R
# load SEED EBA_BA_level.csv and look for demand forecast info
library(tidyverse)

data_directory = "/Users/patricia/Documents/Google Drive/stanford/SEED/"
all_dat = read_csv(paste0(data_directory,"EBA_BA_level.csv"))

# select PJM-ALL columns
PJM = select(all_dat, contains("PJM-ALL"))
caiso = select(all_dat, contains("CISO-ALL"))
rm(all_dat)


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