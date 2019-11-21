# match and organize wind availability data
# output: each col is a unique wind plant
#         with availability factor for each hour of the year 2016
# follows setup_egrid_generators.R
# Jan 2019


library(tidyverse)
library(readxl)
library(lubridate)
library(geosphere)
dataout = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_ercot_input/"
datain = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/unformatted data/"
ercotwinddata =  paste0(datain,"ERCOT data/ERCOT_WindPatterns_1980-2017_ExistingSites_6-26-2018/")
gendat = read_csv(paste0(datain,"complete_generator_listing_ERCOT_012019.csv"))


#windsel = which(gendat$PLFUELCT == "WIND")
windsel = which(gendat$Fuel == "WIND")

#-------------------------------#
# match wind plants to siteID  ####
#-------------------------------#

# Load wind site key ID data
# and match it to generator data
windkey = read_xlsx(path = paste0(datain,"ERCOT data/ERCOT_WindPatterns_1980-2017_ExistingSites_6-26-2018/Existing Wind Site ID Key_formatching.xlsx"),
                    range = "A1:F142")

# 'matching' gives the index in windkey that coresponds to gendat
matching = pmatch(x = tolower(word(gendat$PNAME[windsel],1,2)), table = tolower(word(windkey$`ERCOT Unit Name(s)`,1,2)),
                  duplicates.ok = TRUE)
matches = !is.na(matching)

# check match quality
sum(matches)
# 111
sort((gendat$PNAME[windsel])[!matches]) #unmatched wind
# [1] "Harbor Wind LLC"       "Post Wind Farm LP"     "Scurry County Wind II" "Scurry County Wind LP"

matchlist = rep(NA,nrow(windkey))
matchlist[matching[matches]] = (gendat$PNAME[windsel])[matches]
# View(cbind(windkey$`ERCOT Unit Name(s)`,matchlist))

## identify site ID for each wind plant ##
gendat$windsiteID = NA
gendat$windsiteID[windsel] = windkey$`AWST SITE ID #`[matching]

## match unmatched plants ##
## on the basis of lat/lon? ##
matchedlatlons = cbind(gendat$LON[windsel[which(matches)]],gendat$LAT[windsel[which(matches)]])
for(i in which(!matches)){
  distances = distHaversine(p1 = c(gendat$LON[windsel[i]],gendat$LAT[windsel[i]]), p2 = matchedlatlons)
  minind = which.min(distances)
  newid = (windkey$`AWST SITE ID #`[which(matches)[minind]])[1]
  gendat$windsiteID[windsel[i]] =  newid
}

#----------------------------#
# match wind data to plants ####
#----------------------------#

# load wind data
windgen = read_csv(file = paste0(ercotwinddata,"ERCOT_existing_1980-2017_20180625.CSV"))
# extract 2016
sel2016 = windgen$YYYYMMDD >= 20160000 & windgen$YYYYMMDD < 20170000
windgen16 = windgen[sel2016,]
rm(windgen)

# extract desired sites
sitesdesired = gendat$windsiteID[windsel] #this is ordered with increasing unique "WIND-##" ID

# make wind16 long and skinny, col name -> column
# split columnname colum to site# and capacity
# convert data to availability factor
# select desired site #s
intermediate = windgen16 %>%
  gather(-c(YYYYMMDD,HHMM_CST) , key = "sitecolname", value = "generation") %>%
  mutate(sitenumber = as.integer(substr(sitecolname, 6,10)),
         capacity = as.double(substr(sitecolname, 21,30)),
         availfactor = generation/capacity,
         month = as.integer(substr(YYYYMMDD,5,6)),
         day = as.integer(substr(YYYYMMDD,7,8)),
         hour = as.integer(substr(HHMM_CST,1,2)),
         time_CST = make_datetime(year = 2016, month, day, hour)) %>%
  filter(sitenumber %in% unique(sitesdesired)) 

newwind = intermediate%>%
  select(-generation, -sitecolname, -capacity) %>%
  spread(key = sitenumber, value = availfactor)

# translate to colname = plantUnique name

# arrange for colname to be unique wind names
# map from unique Wind names to desired sites.
windavailfactors16 = as.tibble(matrix(data = NA, nrow = 8760+24, ncol = length(windsel), dimnames = list(c(), gendat$plantUnique[windsel])))
colnamesref = names(newwind)
for(i in 1:length(windsel)){
  col = which(colnamesref == as.character(sitesdesired[i]))
  windavailfactors16[,i] = newwind[,col]
}

#-----------------#
# add dttm col ####
#-----------------#
windavailfactors16$time_CST = newwind$time_CST


#---------#
# check ####
#---------#
# to check: WIND-1 corresponds to site 2112, which is column 93 in newwind
head(windavailfactors16$`WIND-1`)
head(newwind$`2112`)

gendat[windsel,c("PNAME","plantUnique","windsiteID")]
# # A tibble: 1 x 3
# PNAME                          plantUnique windsiteID
# <chr>                          <chr>            <dbl>
#   1 Big Spring Wind Power Facility WIND-1            2112

windkey[windkey$`AWST SITE ID #` == 2112,]
# # A tibble: 1 x 6
# `AWST SITE NAME` `AWST SITE ID #` `FINAL MW` `ERCOT Unit Name(s)` `ERCOT Unit Name(s)__1` X__1 
# <chr>                       <dbl>      <dbl> <chr>                <chr>                   <chr>
#   1 SGMTN_SIGNAL                 2112       34.3 BIG SPRING WIND a    TEXAS BIG SPRING WIND b NA   


#---------#
# save ####
#---------#
write_csv(windavailfactors16,path = paste0(dataout,"wind_availability_factors_2016.csv"))
