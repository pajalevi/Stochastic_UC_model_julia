# Create availability factors for nuclear and hydro based on historical data
# Aug 2019
# Abandoned because not sure if this is useful ultimately

library(tidyverse)
library(lubridate)
library(readxl)

base_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
input_fol = "Data/julia_input/"
default_in_fol = paste0(base_fol,input_fol,"ercot_default/")

#read in historical generation data - iterate over all months
months = c("Jan16","Feb16","Mar16","Apr16","May16","Jun16","Jul16","Aug16","Sep16","Oct16","Nov16","Dec16")
for(i in 1:12) {
  month = months[i]
  fuel_report = read_excel(path = paste0(base_fol,"Data/primary documents/FuelMixReport_PreviousYears/IntGenbyFuel2016.xlsx"),
                           sheet = month)
  #create column names
  hour_names = rep(seq(0,23,1), each=4)
  hour_names = as.character(c(hour_names[2:length(hour_names)],hour_names[1]))
  minutes = rep(c("00","15","30","45"), 24)
  minutes = c(minutes[2:length(minutes)],minutes[1])
  # time_names = hour_names + minutes
  time_names =str_c(as.character(hour_names), as.character(minutes), sep=":")
  column_names = c("Date-Fuel","Total", as.character(time_names))
  
  if(length(names(fuel_report))> length(column_names)){
    # indicates that there are DST columns
    column_names = c(column_names, c("24:00","24:15","24:30","24:45")) # NEED TO DEAL WITH DST COLUMNS UGH
  }
  
  names(fuel_report) = column_names 
  
  # parse fuel, date
  fuel_report$Date = str_split(fuel_report$`Date-Fuel`,"_", simplify=T)[,1]
  fuel_report$Fuel = str_split(fuel_report$`Date-Fuel`,"_", simplify=T)[,2]
  
  # gather time
  fuel_long_i = fuel_report %>%
    gather(key = "time", value = "MWH",contains(":"))  
  fuel_long_i$hour = str_split(fuel_long_i$time,":",simplify=T)[,1]
  
  if(i==1){fuel_long = fuel_long_i
  } else {
    fuel_long = rbind(fuel_long, fuel_long_i)
  }
}

fuel_hourly = fuel_long %>%
  group_by(Fuel,Date,hour) %>%
  summarise(totMWH = sum(MWH))
fuel_hourly$datetime = mdy_h(str_c(fuel_hourly$Date,fuel_hourly$hour,sep="_"))

# convert totMWH into an availability factor

## Use this as a test for interactive plots.
# https://www.littlemissdata.com/blog/interactiveplots
install.packages("plotly")
install.packages("htmlwidgets")
library(plotly)
library(htmlwidgets)

# here is the originial plot:
fuel_hourly_stack = fuel_hourly
fuel_hourly_stack$Fuel = factor(fuel_hourly_stack$Fuel)
fuel_hourly_stack$Fuel = factor(fuel_hourly_stack$Fuel, levels = levels(fuel_hourly_stack$Fuel)[c(7,9,1,4,3,2,8,5,6)])


stackplot = ggplot(fuel_hourly_stack,aes(x=datetime,y=totMWH,fill=Fuel))  + geom_area(color = "black", size = 0.05) + 
  scale_x_datetime(limits = c(mdy_hm("05/01/2016 12:00 UTC"), mdy_hm("06/01/2016 12:00 UTC")))

ggplotly(stackplot)
# doesnt work

# lets just plot gas generation and see if we can explore that...
gasgen = ggplot(fuel_hourly[fuel_hourly$Fuel == "Gas-CC",],aes(y=totMWH, x = 1:sum(fuel_hourly$Fuel == "Gas-CC"))) + geom_line()
ggplotly(gasgen)
# THIS IS FUCKIN SWEET

# lets try this with historical prices
histprice = read_csv(file = paste0(base_fol,"Data/primary documents/20160101-20161231 ERCOT Real-time Price.csv"))
# col_types = cols(Date = col_datetime(), Price = col_double(), Zone = col_character()))
histprice$Date = mdy_hms(histprice$Date, tz = "US/Central")
priceplot = ggplot(histprice,aes(x=Date, y = Price, color = Zone)) + geom_line() +
  scale_color_viridis(discrete=T) + theme_bw() +
  labs(title = "2016 ERCOT energy prices ($/MWh)")
ggplotly(priceplot)

medprice = histprice %>%
  group_by(Date) %>%
  summarise(medprice = median(Price))
medpriceplot = ggplot(medprice,aes(x=Date, y = medprice)) + geom_line() +
  theme_bw() +
  # coord_cartesian(ylim=c(0,100)) +
  labs(title = "MEDIAN 2016 ERCOT energy prices ($/MWh)")
ggplotly(medpriceplot)

# would be great to add demand to this, or my historical energy prices...
modeledprice = read_csv(file = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_output/forIAEE_1Pmin/base_noDRfullyear_2019-05-02/prod_margprice_o1.csv")

# NEXT: combine modeledprice with histprice and/or medprice.
