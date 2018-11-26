#make_scenarios_int.jl
#=
branched from make_scenarios.jl Nov 15

Takes raw data about probability distribution and time intervals, and creates
output usable in the model representing the different scenarios available
by timestep, such that hours in the same specified interval have the same random
outcome. Handles both DR and Net Demand uncertainty

inputs:
- n of timesteps
- length of interval for both net demand and DR
- prob distribution of performance and outcomes, vdr, p (original)

outputs:
- vdr[t,o]
- p[o]
- vnd[t,o]
=#

function make_scenarios_int(n_timesteps, dr_int_length, nd_int_length, dr_v, dr_p, nd_v, nd_p)
    #vdr_og,p_og,ndv,ndp)
    #=
    number of timesteps in simulation
    interval length for DR
    interval length for Net demand (ND)
    variable options for DR
    probability of options for DR
    variable options for ND
    probability of options for ND
    =#

    # test the lenght of intervals, identify number of intervals
    # are intervals a multiple of each other?
    # set Tp, the number of intervals
    if dr_int_length > nd_int_length
        if dr_int_length%nd_int_length != 0
            error("interval lengths must be a multiple of each other")
        else
            Tp = convert(Int,ceil(n_timesteps / nd_int_length))
        end
    else #if nd_int_length >= dr_int_length
        if nd_int_length%dr_int_length != 0
            error("interval lengths must be a multiple of each other")
        else
            Tp = convert(Int,ceil(n_timesteps / dr_int_length))
        end
    end

    # identify the number of simple scenarios (within each interval)
    # and the number of scenarios across the modeled time period
    n_omega = length(dr_p) * length(nd_p)
    n_new_scenarios = n_omega^Tp

    if n_new_scenarios > 1e9
        error("more than a million scenarios, too big")
    end

    # combine probabilities of DR and ND scenarios
    # output: n_omegax3 matrix
    # cols: prob, dr_v, nd_v
    tot_prob = DataFrame([Float64,Float64,Float64],[:prob,:dr_v,:nd_v], n_omega)
    for i in 1:length(dr_p)
        for j in 1:length(nd_p)
            index = (length(nd_p)*(i-1) + j)
            # print(index)
            tot_prob[index, :prob] = dr_p[i] * nd_p[j]
            tot_prob[index, :nd_v] = nd_v[j]
            tot_prob[index, :dr_v] = dr_v[i]
        end
    end
    if sum(tot_prob[:prob]) != 1.0
        error("probabilities in tot_prob do not sum to 1")
    end

    # create array of probabilities
    # - each row is a combination scenario
    # - each column is a time period
    # - data is the probability of the base scenario
    #prob_array = Array{Rational}(n_new_scenarios,Tp) #rational type avoids rounding error, but creates overflow error when calculating p
    prob_array = Array{BigFloat}(n_new_scenarios,Tp)
    #iterate through columns (time periods)
    for i in 1:Tp
        prob_array[:,i] = repeat(vec(tot_prob[:prob]),inner = n_omega^(Tp-i),outer = n_omega^(i-1))
    end
    # can use prod() to get row-wise products.
    p = prod(prob_array,2)
    #p = round.(p, 10) #reduce effect of rounding errors
    p = convert(Array{Float64,2},p) #this rounds effectively

    # create vdr[t,o]
    # - each row is a timestep
    # - each column is an outcome
    #vdr = Array{Float64}(n_timesteps,n_new_scenarios) #this fills array with random small values
    vdr = fill(0.0,(n_timesteps,n_new_scenarios))
    # iterate through columns
    # need to ensure that this matrix is constructed EXACTLY the same way as p above
    int_length = min(nd_int_length, dr_int_length)
    tot_dr_v = reshape(tot_prob[:dr_v],1,size(tot_prob,1))
    for i in 1:Tp
        # identify which rows belong to this time period
        Tp_timesteps = ((i-1)*int_length +1):(i*int_length)
        # println(Tp_timesteps)

        # fill the given time period(row) with the appropriate repetition
        # of possible values in the same way as p is constructed, above
        ## IMPORTANT: the vector that repeats should be a ROW VECTOR ie a 1xn array ##
        vdr[Tp_timesteps,:] = repeat(tot_dr_v,
                    inner = (length(Tp_timesteps),n_omega^(Tp-i)),outer = (1,n_omega^(i-1)))
    end

    # create vnd in the same way
    vnd = fill(0.0,(n_timesteps,n_new_scenarios))
    tot_nd_v = reshape(tot_prob[:nd_v],1,size(tot_prob,1))
    for i in 1:Tp
        # identify which rows belong to this time period
        Tp_timesteps = ((i-1)*int_length +1):(i*int_length)
        # println(Tp_timesteps)

        # fill the given time period(row) with the appropriate repetition
        # of possible values in the same way as p is constructed, above
        vnd[Tp_timesteps,:] = repeat(tot_nd_v,inner = (length(Tp_timesteps),n_omega^(Tp-i)),outer = (1,n_omega^(i-1)))
    end


    return(vdr,vnd,p)
end

#---- NOTE ----
# if I want to make an arbitrary number of periods independent
# but assume the same distribution within each period
# the period is identified by t_firsts. Everything after a t_first entry
# and before the next one is one period
# I will use this and the read-in distribution to create a new set of stoch
# inputs which are expanded to have a scenario for each time period...
# vdr will be indexed vdr[t,o]
# and pro will be accordingly lengthened (because there will be more scenarios)

# if there are 3 periods and 2 scenarios there will be
# 2*2*2 = 2^3 = 8 different combinations
# number of scenarios is now n_omega^Tp
# each probability is just the product of the probabilities of each sub-scenario

# this new dataset [o,Tp] can be created in blocks of Tp, o
# I create an array with the probabilities and find the row-wise product
# To create this array -- in the first col, each outcome is
# listed n_omega^(Tp-1) times, in the second n_omega^(Tp-2) etc.

# to make the new vdr[t,o]... time is rows, scenario is columns
# The array is filled in blocks of rows corresponding to each time period
# the logic for doing this mirrors that of the probabilities
# proceeding row-wise across the vdr (by block - a block is a 'row')
# and repeating each outcome n_omega^(Tp-1) times in the first block
# and n_omega^(Tp-2) in the second block,etc.
