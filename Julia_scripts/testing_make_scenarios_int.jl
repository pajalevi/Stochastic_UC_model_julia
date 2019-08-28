# test dataset for make scenarios_int

dr_p = [0.5 0.5]
dr_v = [0.8 1]

nd_p = [0.1 0.80 0.1]
nd_v = [0.85 1 1.15]

n_timesteps = 24
dr_int_length = 24
nd_int_length = 6

#make_scenarios_int(n_timesteps, dr_int_length, nd_int_length, dr_v, dr_p, nd_v, nd_p)
output = make_scenarios_int(n_timesteps, dr_int_length, nd_int_length, dr_v, dr_p, nd_v, nd_p)
CSV.write("make_scenarios_test_vdr.csv", convert(DataFrame,output[1]))
CSV.write(string("make_scenarios_test_vnd.csv"), convert(DataFrame,output[2]))


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


# Even these moderate params result in an ENORMOUS NUMBER OF SCENARIOS!!
# what do I DOOOOOOOOOOO
# I need to sample
# I was hoping I wouldnt have to address this quite yet
# oh well this has been an informative exercise
# time to drink and deal with it later.

# combine probabilities of DR and ND scenarios
# output: n_omega x 3 matrix
# cols: prob, dr_v, nd_v
n_omega = length(dr_p) * length(nd_p)
# can just do this with a nested for loop

# number of scenarios:
n_new_scenarios = n_omega^Tp # at n_omega = 6 and Tp = 18, this is ~1e14...

# https://stackoverflow.com/questions/28666466/preallocate-a-data-frame-of-known-size-in-julia
#tot_prob = zeros(n_omega,3)
tot_prob = DataFrame([Float64,Float64,Float64],[:prob,:dr_v,:nd_v], n_omega)
for i in 1:length(dr_p)
    for j in 1:length(nd_p)
        index = (length(nd_p)*(i-1) + j)
        print(index)
        tot_prob[index, :prob] = dr_p[i] * nd_p[j]
        tot_prob[index, :nd_v] = nd_v[j]
        tot_prob[index, :dr_v] = dr_v[i]
    end
end


# create array of probabilities
# - each row is a combination scenario
# - each column is a time period
# - data is the probability of the base scenario
prob_array = Array{Rational}(n_new_scenarios,Tp) #rational type avoids rounding error
#iterate through columns (time periods)
for i in 1:Tp
    prob_array[:,i] = repeat(vec(p_og),inner = n_omega^(Tp-i),outer = n_omega^(i-1))
end


# creation of vdr

dr_p = [0.4 0.6]
dr_v = [0.8 1]

nd_p = [1]
nd_v = [1]

n_timesteps = 12
dr_int_length = 2
nd_int_length = 12

Tp = convert(Int,n_timesteps / min(dr_int_length,nd_int_length))
n_omega =length(dr_p) * length(nd_p)

n_new_scenarios =n_omega^Tp # at n_omega = 6 and Tp = 18, this is ~1e14...
tot_prob = DataFrame([Float64,Float64,Float64],[:prob,:dr_v,:nd_v], n_omega)
for i in 1:length(dr_p)
    for j in 1:length(nd_p)
        index = (length(nd_p)*(i-1) + j)
        print(index)
        tot_prob[index, :prob] = dr_p[i] * nd_p[j]
        tot_prob[index, :nd_v] = nd_v[j]
        tot_prob[index, :dr_v] = dr_v[i]
    end
end

# modeled after:
prob_array = Array{Float64}(n_new_scenarios,Tp)
#iterate through columns (intervals)
for i in 1:Tp
    prob_array[:,i] = repeat(vec(tot_prob[:prob]),inner = n_omega^(Tp-i),outer = n_omega^(i-1))
end

# using pro_in as loaded in full model
repeat(vec(pro_in),inner = n_omega^(Tp-i),outer = n_omega^(i-1))


vdr = fill(0.0,(n_timesteps,n_new_scenarios))
# iterate through columns
# need to ensure that this matrix is constructed EXACTLY the same way as p above
# - each row is a combination scenario
# - each column is a time period
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


repeat([1 2 3 4], inner = (2,3), outer = (1,3))
