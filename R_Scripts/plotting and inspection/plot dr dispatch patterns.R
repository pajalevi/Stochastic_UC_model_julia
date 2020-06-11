# In SHERLOCK
# plot variation in DR dispatch across scenarios

source("./consolidatedAnalysisFns.R")

# get advNot2_c2
yy = list.files(path = "/home/groups/weyant/plevi_outputs/", pattern = "*_o25_c2_keyDays2")
yy
runIDs23 = substr(yy, 1, nchar(yy)-11)
runDates23 = substr(yy, nchar(yy)-9,100)
runID = runIDs23[2]
runDate = runDates23[2]
outputFolBase = "/home/groups/weyant/plevi_outputs/"

# get dralldat
dat = getDRAllData(runID,runDate)
head(dat)
names(dat)
# [1] "t"            "scenario"     "nperiod"      "GEN_IND"      "value.commit"
# [6] "DR_IND"       "value.prod"   "scenarionum" 

datmean = dat %>%
  group_by(nperiod,t,GEN_IND)%>%
  summarise(meanprod = mean(value.prod),
            medianprod = median(value.prod),
            maxprod = max(value.prod),
            minprod = min(value.prod))

datmeanlong = datmean %>%
  pivot_longer(cols = ends_with("prod"),names_to = "datatype",values_to = "obs")
datmeanlong$rankt = rank(datmeanlong$t)

ggplot(datmeanlong,aes(x=t, y=obs, color=datatype)) + theme_minimal() +
  geom_line() +
  facet_wrap(~nperiod,scales = "free_x")+
  labs(y = "dispatch MW")+
  ggsave(file = paste0("/home/groups/weyant/plevi_outputs/",runID,"_",runDate,"/plots/all_dispatch_dat.png"),
         width = 12, height = 10)

ggplot(filter(datmeanlong,datatype=="meanprod" | datatype=="medianprod"),aes(x=t, y=obs, color = datatype)) + 
  theme_minimal() +
  geom_line() +
  facet_wrap(~nperiod,scales = "free_x")+
  labs(y = "mean dispatch MW")+
  ggsave(file = paste0("/home/groups/weyant/plevi_outputs/",runID,"_",runDate,"/plots/mean_median_dispatch.png"),
         width = 12, height = 10)

#NEXT -
# load up advNot3_c2 and plot it with the above data to compare
#yy[3]
# is it more similar to mean or median?
runID = runIDs23[3]
runDate = runDates23[3]
# get dralldat
datadvNot3 = getDRAllData(runID,runDate)
datmeanadvNot3 = datadvNot3 %>%
  group_by(nperiod,t,GEN_IND)%>%
  summarise(meanprod = mean(value.prod),
            medianprod = median(value.prod),
            maxprod = max(value.prod),
            minprod = min(value.prod))

datlongadvNot3 = datmean %>%
  pivot_longer(cols = ends_with("prod"),names_to = "datatype",values_to = "obs")

#merge with datmeanlong
datmeanlong$run = "advNot2_c2"
datlongadvNot3$run = "advNot3_c2"
alldatlong = bind_rows(datmeanlong, datlongadvNot3)

ggplot(filter(alldatlong,datatype=="meanprod"),aes(x=t, y=obs, color = run, linetype = run)) + 
  theme_minimal() +
  geom_line() +
  facet_wrap(~nperiod,scales = "free_x")+
  labs(y = "mean dispatch MW")+
  ggsave(file = paste0("/home/groups/weyant/plevi_outputs/",runID,"_",runDate,"/plots/mean_run_dispatch.png"),
         width = 12, height = 10)

####
# repeat analysis with $35 DR
xx = list.files(path = output_fol_base, pattern = glob2rx("*_o25_*keyDays2*"))
runIDs22 = substr(xx, 1, nchar(xx)-11)
runDates22 = substr(xx, nchar(xx)-9,100)
# [4] : advNot2
# [6] : advNot3
runID = runIDs22[4]
runDate = runDates22[4]
dat = getDRAllData(runID,runDate)
datmean = dat %>%
  group_by(nperiod,t,GEN_IND)%>%
  summarise(meanprod = mean(value.prod),
            medianprod = median(value.prod),
            maxprod = max(value.prod),
            minprod = min(value.prod))

datmeanlong = datmean %>%
  pivot_longer(cols = ends_with("prod"),names_to = "datatype",values_to = "obs")
ggplot(datmeanlong,aes(x=t, y=obs, color=datatype)) + theme_minimal() +
  geom_line() +
  facet_wrap(~nperiod,scales = "free_x")+
  labs(y = "dispatch MW")+
  ggsave(file = paste0("/home/groups/weyant/plevi_outputs/",runID,"_",runDate,"/plots/",runID,"_",runDate,"all_dispatch_dat.png"),
         width = 12, height = 10)

ggplot(filter(datmeanlong,datatype=="meanprod" | datatype=="medianprod"),aes(x=t, y=obs, color = datatype)) + 
  theme_minimal() +
  geom_line() +
  facet_wrap(~nperiod,scales = "free_x")+
  labs(y = "mean dispatch MW")+
  ggsave(file = paste0("/home/groups/weyant/plevi_outputs/",runID,"_",runDate,"/plots/",runID,"_",runDate,"mean_median_dispatch.png"),
         width = 12, height = 10)


runID = runIDs22[6]
runDate = runDates22[6]
# get dralldat
datadvNot3 = getDRAllData(runID,runDate)
datmeanadvNot3 = datadvNot3 %>%
  group_by(nperiod,t,GEN_IND)%>%
  summarise(meanprod = mean(value.prod),
            medianprod = median(value.prod),
            maxprod = max(value.prod),
            minprod = min(value.prod))

datlongadvNot3 = datmean %>%
  pivot_longer(cols = ends_with("prod"),names_to = "datatype",values_to = "obs")

#merge with datmeanlong
datmeanlong$run = "advNot2"
datlongadvNot3$run = "advNot3"
alldatlong = bind_rows(datmeanlong, datlongadvNot3)

ggplot(filter(alldatlong,datatype=="meanprod"),aes(x=t, y=obs, color = run, linetype = run)) + 
  theme_minimal() +
  geom_line() +
  facet_wrap(~nperiod,scales = "free_x")+
  labs(y = "mean dispatch MW")+
  ggsave(file = paste0("/home/groups/weyant/plevi_outputs/",runID,"_",runDate,"/plots/mean_run_dispatch.png"),
         width = 12, height = 10)

# There are duplicated ts! each period that overlaps retains an extra t...
# where does this error come from?
# it could come from loadTimeseriesData :(
# it definitely exists in dat
# allcomt has it too... when loaded as called from getDRAllDat which uses loadTimeseriesData :(
length(unique(datmean$t))
which(duplicated(datmean$t))
# [1]  691  903 1000 1097 1654 1751
datmean[685:695,]
# # A tibble: 11 x 5
# # Groups:   nperiod, t [11]
# nperiod     t GEN_IND meanprod medianprod
# <dbl> <dbl> <chr>      <dbl>      <dbl>
#   1       6  4900 DR-1        0         0    
# 2       6  4901 DR-1        0         0    
# 3       6  4902 DR-1        0         0    
# 4       6  4903 DR-1        0         0    
# 5       6  4904 DR-1        0         0    
# 6       6  4905 DR-1        0         0    
# 7       7  4905 DR-1        0         0    
# 8       7  4906 DR-1        0         0    
# 9       7  4907 DR-1        0         0    
# 10       7  4908 DR-1        3.48      1.000
# 11       7  4909 DR-1       22.8       1.000
datmean[900:907,]
# # A tibble: 8 x 5
# # Groups:   nperiod, t [8]
# nperiod     t GEN_IND meanprod medianprod
# <dbl> <dbl> <chr>      <dbl>      <dbl>
#   1       8  5143 DR-1           0          0
# 2       8  5144 DR-1           0          0
# 3       8  5145 DR-1           0          0
# 4       9  5145 DR-1           0          0
# 5       9  5146 DR-1           0          0
# 6       9  5147 DR-1           0          0
# 7       9  5148 DR-1           0          0
# 8       9  5149 DR-1           0          0
