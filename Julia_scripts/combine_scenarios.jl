#combine_scenarios.jl
#=
Sept 2019

Takes two sets of scenarios: demand and DR reliability - and combines them.
Creates pro, vdr, and vdem

=#

function combine_scenarios(dr_scenarios, dr_p, nd_scenarios, nd_p)

    if size(dr_scenarios,1) != size(nd_scenarios,1)
        error("DR and Demand scenarios must cover same number of timesteps")
    end

    timesteps = size(dr_scenarios,1)
    n_new_scenarios = size(dr_scenarios,2) * size(nd_scenarios,2)
    print("number of new scenarios is",n_new_scenarios)

    vdr = Array{Float64}(timesteps,n_new_scenarios)
    vnd = Array{Float64}(timesteps,n_new_scenarios)
    prob_array = Array{Float64}(n_new_scenarios)

    dr_ind_array = repeat(1:size(dr_scenarios,2),outer = size(nd_scenarios,2))
    nd_ind_array = repeat(1:size(nd_scenarios,2),inner = size(dr_scenarios,2))
    for i in 1:n_new_scenarios
        println("i is ",i)
        #ID which dr scenario belongs here
        dr_ind = dr_ind_array[i]
        println("dr ind is ", dr_ind)
        #ID which nd scenario belongs here
        nd_ind = nd_ind_array[i]
        println("nd_ind is ", nd_ind)
        #
        vdr[:,i] = dr_scenarios[:,dr_ind]
        vnd[:,i] = nd_scenarios[:,nd_ind]
        prob_array[i] = dr_p[dr_ind] * nd_p[nd_ind]

    end

    return(vdr,vnd,prob_array)
end

#test
