# DR_avail_data.R
# create availability data
# copied from write_gams_input.R
# separated out for easier management
# Julia model
library(lubridate)

source('/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/R_scripts/hour_of_year.R')

### for daytime: 7am to 11pm
dr_input_name = "dr_availability_workhours_2016.csv"
output_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/ercot_default/"

days_in_year = 8760+24

starthour = 9
endhour = 17
#PJM onpeak = >=16, <23

pfdr = matrix(rep(0,days_in_year), nrow = days_in_year, ncol = 2, dimnames = list(c(),c("hour","DR-1")))
pfdr[,"hour"] = paste0("h",1:days_in_year)
MOO_hours = c()
for(i in 1:days_in_year){
  thedate = date_from_yearhour(year = 2016, year_hour = i)
  thehour = hour(thedate)
  # themonth = month(thedate)
  if(thehour >=starthour & thehour < endhour  ){ 
    MOO_hours = c(MOO_hours, i)
  }
}
pfdr[MOO_hours,"DR-1"] = 1

#save
write.table(pfdr, file = paste0(output_fol,dr_input_name), row.names = F, col.names = T, sep=",", quote = F)
