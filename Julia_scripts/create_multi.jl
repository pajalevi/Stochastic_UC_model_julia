#create_multi.jl
# makes the inputs for ercot_multi.jl
# followed by submit_multi.jl
# Patricia Feb 2019

using DataFrames
using CSV

function create_multi(periodhrs, overlaphrs, runID, drID, stochID, headfol;
    daysinyear = 366, defaultname = "ercot_default/",
    genID = "complete_generator_listing_ERCOT_012019",
    wind_avail = "wind_availability_factors_2016.csv",
    solar_avail = "solar_availability_factors_2016.csv"
    ercot_demand = "ercot_demand_2016.csv")
    #daysinyear = 366 #2016

    # create one folder with all periods_*.csv for each sub-period
    starts = collect(1:(periodhrs-overlaphrs):(daysinyear*24))

    input_fol = string(headfol,"outputs/",runID)
    if !isdir(input_fol)
        mkdir(input_fol)
    end

    for i in 1:size(starts)[1]
        firstp = starts[i]
        lastp = starts[i] + periodhrs - 1
        hours = collect(firstp:lastp)
        CSV.write(string(outputfol,"/periods_",i,"_",firstp,"-",lastp,".csv"),DataFrame(hour=hours),header=false)
    end

    # copy over other folders from default folder
    default_fol = string(headfol,"/",defaultname)
    cp(string(default_fol,genID,".csv"), string(output_fol,genID,".csv"))
    cp(string(default_fol,"dist_input_",stochID,".csv"), string(output_fol,"dist_input_",stochID,".csv"))
    cp(string(default_fol,drID,".csv"), string(output_fol,drID,".csv"))
    cp(string(default_fol,wind_avail), string(output_fol, wind_avail))
    cp(string(default_fol,solar_avail), string(output_fol, solar_avail))
    cp(string(default_fol,ercot_demand), string(output_fol, ercot_demand))

end
