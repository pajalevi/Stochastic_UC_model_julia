#make_scenarios.jl
# inputs:
# n of timesteps
# position of beginning of new time periods (t_firsts)
# prob distribution of performance and outcomes, vdr, p (original)

# outputs:
# vdr[t,o]
# p[o]
# this is to make the performance of DR in different
# time periods independent.

#---- NOTE ----
#if I want to make an arbitrary number of periods independent
# but assume the same distribution within each period
# the period is identified by t_firsts. Everything after a t_first entry
# and before the next one is one period
# I will use this and the read-in distribution to create a new set of stoch
# inputs which are expanded to have a scenario for each time period...
# vdr will be indexed vdr[t,o]
# and pro will be accordingly lengthened (because there will be more scenarios)
# if Tp is the number of periods, the new prob of each outcome is...
# P = sum( ,t in 1:Tp, o in base_scenarios_in_this_scenario)

# if there are 3 periods and 2 scenarios there will be
# 2*2*2 = 2^3 = 8 different combinations
# number of scenarios is now n_omega^Tp

# each probability is just the product of the probabilities of each sub-scenario

# this new dataset [o,Tp] can be created in blocks of Tp, o
# could create an array with the probabilities and find the row-wise product
# this could be created rather efficiently. in the first col, each outcome is
# listed n_omega^(Tp-1) times, in the second n_omega^(Tp-2) etc.

# to make the new vdr[t,o]... time is rows, scenario is column
# fill in columns in blocks (Tp blocks)
# the logic for doing this could mirror that of the probabilities
# proceeding row-wise across the vdr (by block - a block is a 'row')
# and repeating each outcome n_omega^(Tp-1) times in the first block
# and n_omega^(Tp-2) in the second block,etc.

# so now I'm iterating by block.

function make_scenarios(n_timesteps,t_firsts,vdr_og,p_og)
    Tp = length(t_firsts)

    # can use prod() to get row-wise products. might need to iterate thru rows
    return(vdr,p)
end
