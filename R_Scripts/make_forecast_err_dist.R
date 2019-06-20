library(tidyverse)
library(lubridate)

data_directory = "/Users/patricia/Documents/Google Drive/stanford/SEED/"
all_dat = read_csv(paste0(data_directory,"EBA_BA_level.csv"))
plots_dir = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/plots/ERCOT_data/"
data_out = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/unformatted data/"

# select PJM-ALL columns
ercot = select(all_dat, X1,contains("ERCO-ALL")) #ERCOT

# select date column 
# this loads as a 'date-time' object aka POSIXct
# manipulable with functions from package 'lubridate'
date = select(all_dat, contains("X1")) 
date$year = year(date$X1)
date$month = month(date$X1)
date$hour = hour(date$X1)
date$yday = yday(date$X1)
date$wday = wday(date$X1, label=TRUE)

rm(all_dat)

ercot$error = ercot$`EBA.ERCO-ALL.DF.H` - ercot$`EBA.ERCO-ALL.D.H`

dercot = cbind(ercot, date[,2:6])
# make col for season
dercot$season = "1_winter"
dercot$season[dercot$month >= 3 & dercot$month <= 5] = "2_spring"
dercot$season[dercot$month >= 6 & dercot$month <= 8] = "3_summer"
dercot$season[dercot$month >= 9 & dercot$month <= 11] = "4_fall"

dercot$errorfrac = (dercot$`EBA.ERCO-ALL.DF.H` - dercot$`EBA.ERCO-ALL.D.H`)/dercot$`EBA.ERCO-ALL.D.H`

# plot autocorrelation over time
erracf = acf(dercot[!is.na(dercot[,"errorfrac"]) & dercot$year==2016,c("errorfrac")])
plot(erracf,main="Autocorrellation of forecast error", ci.type="ma")

# Create distribution stats for day/night by season, 3 points ####
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
diff_side = 2 #multiply by SD

p_side = (1-p_ctr)/2
dercot2$high = dercot2$mean + diff_side*dercot2$stddev
dercot2$low = dercot2$mean - diff_side*dercot2$stddev

# calculate stddev of created dist
dercot2 = dercot2 %>%
  mutate(sim_sd = sqrt((mean^2 * p_ctr - mean^2) + (high^2*p_side - mean^2) + (low^2*p_side - mean^2)),
         sd_error = stddev - sim_sd)
View(dercot2)

dercot2$mean_prob = p_ctr

## double SD of original ##
p_ctr = 0.74
diff_side = 4 #multiply by SD
p_side = (1-p_ctr)/2
dercot2$high = dercot2$mean + diff_side*dercot2$stddev
dercot2$low = dercot2$mean - diff_side*dercot2$stddev

# calculate stddev of created dist
dercot2 = dercot2 %>%
  mutate(sim_sd = sqrt((mean^2 * p_ctr - mean^2) + (high^2*p_side - mean^2) + (low^2*p_side - mean^2)),
         sd_error = stddev - sim_sd,
         sd_ratio = sim_sd/stddev)
View(dercot2)

# make 3-point approximation of first row
p = c(p_side, p_ctr, p_side)
v = c(1+dercot2$low[1], 1, 1+dercot2$high[1])
output = matrix(data = c(p,v),nrow=2,ncol=length(p), 
                dimnames = list(c("p","v"),c("o1","o2","o3")), byrow=TRUE)
output
write_csv(as.data.frame(round(output,6)), path = paste0(data_out,"probabilities/dist_input_n3_doubleSD_ercot_nd.csv"))


## 5 point approx ##
dercot2 = dercot %>%
  group_by(daytime, season) %>%
  summarise(stddev = sd(errorfrac, na.rm=T),
            mean = 0)#mean(errorfrac,na.rm=T))

p_ctr1 = 0.72
p_ctr2 = p_ctr1 + 0.23
if(p_ctr2 >= 1){ stop("pctr2 is too high!")}
diff_side1 = 1 #multiply by SD
diff_side2 = 3.795 #multiply by SD

p_side1 = (1-p_ctr1)/2
p_side2 = (1-p_ctr2)/2
dercot2$v1 = dercot2$mean - diff_side2*dercot2$stddev
dercot2$v2 = dercot2$mean - diff_side1*dercot2$stddev
dercot2$v4 = dercot2$mean + diff_side1*dercot2$stddev
dercot2$v5 = dercot2$mean + diff_side2*dercot2$stddev


# calculate stddev of created dist
dercot2 = dercot2 %>%
  mutate(sim_sd = sqrt((mean^2 * p_ctr1 - mean^2) + (v2^2*p_side1 - mean^2) + (v4^2*p_side1 - mean^2) +
          (v1^2*p_side2 - mean^2) + (v5^2*p_side2 - mean^2)), 
         sd_error = stddev - sim_sd,
         sd_ratio = sim_sd/stddev)
View(dercot2)

# save 5-point approximation of first row
p = c(p_side2, p_side1, p_ctr1, p_side1, p_side2)
v = c(1+dercot2$v1[1],1+dercot2$v2[1], 1, 1+dercot2$v4[1],1+dercot2$v5[1])
output = matrix(data = c(p,v),nrow=2,ncol=length(p), 
               dimnames = list(c("p","v"),c("o1","o2","o3","o4","o5")), byrow=TRUE)
output
write_csv(as.data.frame(round(output,6)), path = paste0(data_out,"probabilities/dist_input_n5_longtail_ercot_nd.csv"))

## 7 point approx ##
dercot2 = dercot %>%
  group_by(daytime, season) %>%
  summarise(stddev = sd(errorfrac, na.rm=T),
            mean = 0)#mean(errorfrac,na.rm=T))

p_ctr1 = 0.72
p_ctr2 = p_ctr1 + 0.2
p_ctr3 = p_ctr2 + 0.1
if(p_ctr3 >= 1){ stop("pctr2 is too high!")}
diff_side1 = 1 #multiply by SD
diff_side2 = 2 #multiply by SD
diff_side3 = 4 #multiply by SD

p_side1 = (1-p_ctr1)/2
p_side2 = (1-p_ctr2)/2
p_side3 = (1-p_ctr3)/2
dercot2$v1 = dercot2$mean - diff_side3*dercot2$stddev
dercot2$v2 = dercot2$mean - diff_side2*dercot2$stddev
dercot2$v3 = dercot2$mean - diff_side1*dercot2$stddev

dercot2$v5 = dercot2$mean + diff_side1*dercot2$stddev
dercot2$v6 = dercot2$mean + diff_side2*dercot2$stddev
dercot2$v7 = dercot2$mean + diff_side3*dercot2$stddev


# calculate stddev of created dist
dercot2 = dercot2 %>%
  mutate(sim_sd = sqrt((mean^2 * p_ctr1 - mean^2) + (v2^2*p_side1 - mean^2) + (v4^2*p_side1 - mean^2) + 
                         (v2^2*p_side2 - mean^2) + (v6^2*p_side2 - mean^2) +  # WRONG
                         (v1^2*p_side3 - mean^2) + (v7^2*p_side3 - mean^2)),
         sd_error = stddev - sim_sd,
         sd_ratio = sim_sd/stddev)
View(dercot2)

# save 7-point approximation of first row
p = c(p_side2, p_side1, p_ctr1, p_side1, p_side2)
v = c(1+dercot2$v1[1],1+dercot2$v2[1], 1, 1+dercot2$v4[1],1+dercot2$v5[1])
output = matrix(data = c(p,v),nrow=2,ncol=length(p), 
                dimnames = list(c("p","v"),c("o1","o2","o3","o4","o5")), byrow=TRUE)
output
write_csv(as.data.frame(round(output,6)), path = paste0(data_out,"probabilities/dist_input_n5_ercot_nd.csv"))

