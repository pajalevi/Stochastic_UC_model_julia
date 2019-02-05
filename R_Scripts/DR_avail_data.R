# DR_avail_data.R
# create availability data
# copied from write_gams_input.R
# separated out for easier management
# Julia model


source('/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/R_scripts/hour_of_year.R')

### for daytime: 7am to 11pm
dr_input_name = "dr_availability_daytime_2016.csv"
output_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_ercot_input/"

days_in_year = 8760+24


pfdr = matrix(rep(0,days_in_year), nrow = days_in_year, ncol = 1, dimnames = list(c(paste0("h",1:days_in_year)),c("DR-1")))
MOO_hours = c()
for(i in 1:days_in_year){
  thedate = date_from_yearhour(year = 2015, year_hour = i)
  thehour = hour(thedate)
  # themonth = month(thedate)
  if(thehour >=7 & thehour < 23  ){ # PJM on peak
    MOO_hours = c(MOO_hours, i)
  }
}
pfdr[MOO_hours,] = 1

#save
write.table(pfdr, file = paste0(output_fol,dr_input_name), row.names = T, col.names = T, sep=",")
