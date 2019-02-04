# solar_avail_data.R
# ingest solar production data and make availability profiles
# match solar plants to nearest data point
# follows setup_egrid_generators.R
# output: each col is a unique solar plant
#         with availability factor for each hour of the year 2016
# Jan 2018, Patricia Levi

library(tidyverse)
library(geosphere)
dataout = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_ercot_input/"
datain = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/unformatted data/"
solardatfol = paste0(datain,"texas_solar/")
gendat = read_csv(paste0(datain,"complete_generator_listing_ERCOT_012019.csv"))

# format solar data
solar_dat_files = list.files(solardatfol)
solar_metadata = matrix(unlist(strsplit(solar_dat_files,split="_")),ncol = 5,byrow=T,
                        dimnames = list(c(),c("x1","x2","lat","lon","x5")))
solar_index = data_frame(index = 1:nrow(solar_metadata),
                         lat = as.numeric(solar_metadata[,"lat"]),
                         lon = as.numeric(solar_metadata[,"lon"]),
                         filename = solar_dat_files)

# set up holder for output, and useful indices
solarsel = (gendat$Fuel =="SOLAR")
solardf = data.frame(names = gendat$plantUnique[solarsel],
                     solarind = which(solarsel)) #bookkeeping
solar_avail = as.tibble(matrix(nrow = 8760+24, ncol = nrow(solardf), 
                     dimnames = list(c(),solardf$names))) #final output

# identify nearest solar_index measurement site
solardf$solarsiteID = NA
for(i in 1:nrow(solardf)){
  distances = distHaversine(p1 = c(gendat$LON[solardf$solarind[i]],gendat$LAT[solardf$solarind[i]]), p2 = solar_index[,c("lon","lat")])
  minind = which.min(distances)
  newid = solar_index$index[minind] #should be the same as minind, but just to be safe
  solardf$solarsiteID[i] =  newid
}

# Load the datasets that are wanted
solar_load = unique(solardf$solarsiteID)
availdata = as_data_frame(matrix(NA, nrow = 8760+24, ncol = length(solar_load),
                              dimnames = list(c(),paste0("file",solar_load))))
for(f in solar_load){
  index_row = which(f==solar_index$index)
  temp = read_csv(file = paste0(solardatfol,solar_index$filename[index_row]), skip=2)
  maxprod = max(temp$kW)
  temp$avail = temp$kW/maxprod
  
  #NEED TO ADJUST UTC-> CST! I will work in CST (6 hours behind UTC)
  # can find first row that is in 2016, and fill the remaining hours at the end of 2016 with 0... since its night
  # this should be fine...
  # what time zone is wind or demand given in?
  availdata[,paste0("file",f)] = c(temp$avail[7:nrow(temp)],rep(0,6))
}
solar_avail$time_UTC = as_datetime(rep(0,8784))
solar_avail$time_UTC[1:(nrow(temp)-6)] = temp$UTC[7:nrow(temp)]
solar_avail$time_UTC[(nrow(temp)-5):nrow(temp)] = temp$UTC[nrow(temp)]+hours(1:6)


# fill in solar_avail
for(c in 1:nrow(solardf)){
  # ID correct dataset for current col
  avail_file = solardf$solarsiteID[solardf$names == dimnames(solar_avail)[[2]][c]]
  
  # copy over relevant column from availdata to solar_avail
  solar_avail[,c] = availdata[,paste0("file",avail_file)]
}

# save
write_csv(solar_avail,path = paste0(dataout,"solar_availability_factors_2016.csv"))
