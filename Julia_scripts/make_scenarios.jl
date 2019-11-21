#make_scenarios.jl
# inputs:
# n of timesteps
# position of beginning of new time periods (t_firsts) NOT USED
# prob distribution of performance and outcomes, vdr, p (original)

# outputs:
# vdr[t,o]
# p[o]
# this is to make the performance of DR in different
# time periods independent.

using StatsBase
function make_scenarios(n_timesteps,v_og,p_og,int_length; randsel = true, nrand = 200)
    #=
    number of timesteps in simulation
    X timesteps at beginning of a discrete timeperiod
    value options
    probabilities for each value
    interval length
    TF should we pick a subselection of the scenarios
    how many scenarios if so
    =#

    Tp = convert(Int64,floor(n_timesteps/int_length))
    n_omega = length(p_og)
    n_new_scenarios = convert(Int64,n_omega^Tp)

    if randsel & (nrand < n_new_scenarios)
        s = sample(1:n_new_scenarios,nrand,replace=false)
    else
        s = 1:n_new_scenarios
    end


    # create array of probabilities prob_array[o,T]
    # - each row is a combination scenario
    # - each column is a time period
    # - data is the probability of the base scenario


    # prob_array = Array{Rational}(n_new_scenarios,Tp) #rational type avoids rounding error
    # prob_array = Array{Float64}(n_new_scenarios,Tp)
    #iterate through columns
    # for i in 1:Tp
    #     prob_array[:,i] = repeat(vec(p_og),inner = n_omega^(Tp-i),outer = n_omega^(i-1))
    # end

    #iterate through rows
    prob_array2 = Array{Float64}(length(s),Tp)
    for i in 1:length(s) #row
        for j in 1:Tp #col
            ind = mod(fld(s[i]-1,n_omega^(Tp-j)),n_omega)+1
            prob_array2[i,j] = p_og[ind]
        end
    end

    # can use prod() to get row-wise products.
    ptemp = prod(prob_array2,2)
    p = ptemp/sum(ptemp)
    # p = round.(p, 10)

    # create vdr[t,o]
    # - each row is a timestep
    # - each column is an outcome
    # vdr = Array{Float64}(n_timesteps,n_new_scenarios) #this fills array with random small values
    # vdr = fill(0.0,(n_timesteps,n_new_scenarios))
    # # iterate through columns
    # # need to ensure that this matrix is constructed EXACTLY the same way as p above
    # for i in 1:Tp
    #     # identify which rows belong to this time period
    #     if i == Tp
    #         last_period = n_timesteps
    #     else
    #         last_period = int_length*i #t_firsts[i+1]-1
    #     end
    #     Tp_timesteps = (int_length*(i-1) + 1):last_period
    #
    #     # fill the given time period(row) with the appropriate repetition
    #     # of possible values in the same way as p is constructed, above
    #     vdr[Tp_timesteps,:] = repeat(v_og,inner = (length(Tp_timesteps),n_omega^(Tp-i)),outer = (1,n_omega^(i-1)))
    # end

    vdr = fill(0.0,(n_timesteps,length(s)))
    for j in 1:length(s) #cols
        for i in 1:Tp #row chunk
            # rows = ((i-1)*int_length +1):(i*int_length)
            # identify which rows belong to this time period
            if i == Tp
                last_period = n_timesteps
            else
                last_period = int_length*i #t_firsts[i+1]-1
            end
            rows = (int_length*(i-1) + 1):last_period

            ind = mod(fld(s[j]-1,n_omega^(Tp-i)),n_omega)+1
            println(ind)
            vdr[rows,j] = v_og[ind]
        end
    end

    # if randsel & (nrand < n_new_scenarios)
    #     scenarios = sample(1:n_new_scenarios,nrand,replace=false)
    #     vdr = vdr[:,scenarios]
    #     ptemp = p[scenarios]
    #     p = ptemp/sum(ptemp) #adjust probabilities so they sum to 1
    # end

    return(vdr,p)
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
