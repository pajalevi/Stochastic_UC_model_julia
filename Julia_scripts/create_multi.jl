#create_multi.jl
# makes the inputs for ercot_multi.jl
# followed by submit_multi.jl
# Patricia Feb 2019

using DataFrames
using CSV

function create_multi(periodhrs, overlaphrs, runID,
    # drID, stochID,
    headfol;
    daysinyear = 366#,
    # defaultname = "ercot_default/",
    # genID = "complete_generator_listing_ERCOT_012019",
    # wind_avail = "wind_availability_factors_2016.csv",
    # solar_avail = "solar_availability_factors_2016.csv",
    # ercot_demand = "ercot_demand_2016.csv"
    )
    #daysinyear = 366 #2016
    # periodhrs = how many hours in a run
    # overlaphrs = hours that runs overlap

    # create one folder with all periods_*.csv for each sub-period
    starts = collect(1:(periodhrs-overlaphrs):(daysinyear*24))

    input_fol = string(headfol,runID,"/")
    if !isdir(input_fol)
        mkdir(input_fol)
    end

    for i in 1:size(starts)[1]
        firstp = starts[i]
        lastp = starts[i] + periodhrs - 1
        hours = collect(firstp:lastp)
        CSV.write(string(input_fol,"/periods_",i,"_",firstp,"_",lastp,".csv"),DataFrame(hour=hours),header=false)
    end

    # copy over other folders from default folder
    # default_fol = string(headfol,"/",defaultname)
    # cp(string(default_fol,genID,".csv"), string(output_fol,genID,".csv"))
    # cp(string(default_fol,"dist_input_",stochID,".csv"), string(output_fol,"dist_input_",stochID,".csv"))
    # cp(string(default_fol,drID,".csv"), string(output_fol,drID,".csv"))
    # cp(string(default_fol,wind_avail), string(output_fol, wind_avail))
    # cp(string(default_fol,solar_avail), string(output_fol, solar_avail))
    # cp(string(default_fol,ercot_demand), string(output_fol, ercot_demand))

end
#create_multi(7*24,12,"7d_12o_periods",
# "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/")

create_multi(5*24,6,"5d_6o_periods","/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/")

create_multi(7*24,6,"7d_6o_periods",
"/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/")

create_multi(7*24,24,"7d_24o_periods",
"/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/")

create_multi(7*24,36,"7d_36o_periods",
"/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/")

create_multi(10*24,12,"10d_12o_periods",
"/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/")

create_multi(14*24,12,"14d_12o_periods",
"/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/Data/julia_input/")
