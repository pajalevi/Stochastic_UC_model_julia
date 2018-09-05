# full_universe_stoch.jl
# solves the full universe version of a stochastic two stage
# unit-commitment model in which slow generators are committed in the
# first stage and fast generators can be committed in real time.
# Demand response functions as a slow generator with an advance commmitment
# not only of startup but also to a generation schedule
# The only uncertainty modeled is that of the actual DR generation
# Written under Julia 0.6.4
# Patricia Levi
# pjlevi@stanford.edu

# TODO --------------------------------------
# X change filepath for output writing
# ----- change file structure in sherlock to match below
# X ----- file structure: home folder(outputs(output ID), inputs(base, timeseriesID))
# X better file org for scenario input files
# X check if output folder exists before creating it
# X set up tests for runtime on sherlock (different # time periods, omegas)
# think about benders, binary relaxation, how to reduce problem size
# develop better way to differentiate between slow and fast generators
#       papavasiliou denotes any generator <300 MW(?) as fast, all others slow
# look at output to sanity check.
#       graph output levels along with demand levels
#       look at marginal prices over time (cannot just look at shadow price
#            because the binary problem means theres no useful dual variable.
#            Instead I can look at the max price of dispatched generators at
#            each timestep)
# X make sherlock = true an argument that can be passed in when calling file
# X make debug switch that can be used to run everything up to model solve
# -------------------------------------------


### handle command line args ###
# cmd line format should be include("full_universe_stoch.jl") <timeseriesID> <stochID> <outputID>
######### USER CONTROLS ##########
debug = true # stops execution before solving model

# Parse ARGS #
defaultARGS = ["144h2groups","n3_m1.0_0.2pp","base_testing"]
localARGS = length(ARGS) > 0 ? ARGS : defaultARGS
nargs = length(localARGS)
@show localARGS

if nargs == 3
    timeseriesID = localARGS[1]
    stochID = localARGS[2]
    outputID = localARGS[3]
elseif nargs > 3
    error("ERROR: Too many arguments supplied. Need <timeseriesID> <stochID> <outputID>")
else
    warn("not enough arguments supplied. Need <timeseriesID> <stochID> <outputID>")
end
n_periods = parse(Int64,split(timeseriesID,"h")[1]) # Must be the same as the first number in timeseriesID
n_omega=parse(Int64,split(stochID,r"n|_";keep=false)[1]) #number of realizations


sherlock_fol = "/home/users/pjlevi/dr_stoch_uc/julia_ver/"
sherlock_input_file = "inputs/"
sherlock_output_file = "outputs/"
laptop_fol = "/Users/patricia/Documents/Google Drive/stanford/second year paper/Tutorial II/Data/"
laptop_input_file = "julia_input/"
laptop_output_file = "julia_output/"

if split(pwd(),"/")[2] == "Users"
    Sherlock = false
else
    Sherlock = true # on sherlock? where are folders?
end

# OTHER PARAMS #
dr_override = true # set to true for below value to be used
dr_varcost = 10000 #for overriding variable cost to test things

####### END USER CONTROLS ##########

# For SHERLOCK:
if Sherlock
    # Pkg.update()
    # Pkg.add("JuMP")
    # #Pkg.add("Clp")
    # Pkg.add("Gurobi")
    # Pkg.add("DataFrames")
    # Pkg.pin("DataFrames",v"0.11.7")
    # Pkg.add("CSV")
end

using JuMP
#using Clp
using Gurobi
using DataFrames
using CSV
include("convert3dto2d.jl")
include("make_scenarios.jl")

test = Gurobi.Env() # test that gurobi is working

### FILE PATHS ###
if Sherlock
    base_fol = sherlock_fol
    input_fol = string(sherlock_fol,sherlock_input_file)
    output_fol = string(sherlock_fol, sherlock_output_file, outputID,"/")
else
    base_fol = laptop_fol
    input_fol = string(laptop_fol,laptop_input_file)
    output_fol = string(laptop_fol, laptop_output_file, outputID,"/")
end
default_data_fol = string(input_fol,"default/")
subsel_data_fol = string(input_fol,timeseriesID,"/")

### READ IN DATA ###
first_periods = CSV.read(string(subsel_data_fol,"first_periods_",timeseriesID,".csv"),datarow=1)[1]
notfirst_periods = CSV.read(string(subsel_data_fol,"notfirst_periods_",timeseriesID,".csv"),datarow=1)[1]
hours = CSV.read(string(subsel_data_fol,"periods_",timeseriesID,".csv"),datarow=1)[1]
if length(hours)!=n_periods
    error("n_periods does not match the length of hours")
end

# convert first and notfirst periods to indices
t_firsts = findin(hours, first_periods)
t_notfirst = findin(hours, notfirst_periods)

dem2 = CSV.read(string(default_data_fol, "demand_2015.csv"),datarow=1)
# subselect for just the rows corresponding to 'hours'
dem = dem2[hours,2]


# STOCHASTIC PARAMS #
# vdr = [0.9,1,1.1]
# pro = [0.25,0.5,0.25]
probs = CSV.read(string(default_data_fol , "dist_input_n",stochID,".csv"))
vdr_in = convert(Array,probs[1,:]) # converts the first row of probs to an Array
pro_in = rationalize.(convert(Array,probs[2,:])) #to avoid rounding issues later
    # if this becomes a problem, look into https://github.com/JuliaMath/DecFP.jl

test = make_scenarios(n_periods,t_firsts,vdr_in, pro_in)
vdr = test[1]
pro = test[2]
if sum(pro) != 1
    error("sum of probabilities is not one")
end
n_omega = length(pro) #redefine for new number of scenarios

# to check vdr and p are constructed properly
# writecsv("vdr_test.csv",vdr)
# writecsv("p_test.csv",pro')

genset = CSV.read(string(default_data_fol,"gen_merged_withIDs.csv"), missingstring ="NA")
# genset[Symbol("Plant Name")] # this is how to access by column name if there are spaces
# names(genset) # this is how to get the column names
# anscombe[:,[:X3, :Y1]]  #how to grab several columns by colname

# how do I select which rows match one of a set of strings?
# in("c",["a","b","c"]) # ask if "c" is contained in set of interest
# useful: https://cbrownley.wordpress.com/2015/07/26/intro-to-julia-filtering-rows-with-r-python-and-julia/
#          data_frame_value_in_set =
#           data_frame[findin(data_frame[:quality], set_of_interest), :]

# x = ["a","b","c"]
# set_interest = ["c"]# for this to work must have the []
# findin(x,set_interest)

slow_gens = ["COAL","NUCLEAR","DR", "MUNICIPAL_SOLID_WASTE","LANDFILL_GAS",
            "BIOMASS","GAS","GAS_CC",
            "IMPORT_COAL","IMPORT_GAS"]
fast_gens = ["HYDRO","GAS_CT","OIL","SOLAR","WIND","IMPORT_HYDRO"]
dr_gens = ["DR"]
notdr_gens = ["COAL","NUCLEAR", "MUNICIPAL_SOLID_WASTE","LANDFILL_GAS",
            "BIOMASS","GAS","GAS_CC",
            "IMPORT_COAL","IMPORT_GAS",
            "HYDRO","GAS_CT","OIL","SOLAR","WIND","IMPORT_HYDRO"]

dr_ind = findin(genset[:Fuel],dr_gens)
slow_ind = findin(genset[:Fuel],slow_gens)
fast_ind = findin(genset[:Fuel],fast_gens)

#generator min and max
pmin = genset[:PMin]
pmax = genset[:Capacity]
startup = genset[:StartCost]
varcost = genset[:VCost]
# for manual override of DR variable cost
if dr_override
    varcost[dr_ind] = dr_varcost
end

# Generator PARAMS ###
n_gsl= length(slow_ind)# number of slow generators
n_g =nrow(genset)# number of generators
n_gf = length(fast_ind)
n_gdr = length(dr_ind) #number of DR generators
n_t = n_periods # number of timesteps

## generator availability ##
pf = repeat([1], inner = [n_g, n_t])
pf = convert(Array{Float64},pf)


# load wind, solar info
solar_avail = CSV.read(string(default_data_fol,"solar_input_8760.txt"))
wind_avail = CSV.read(string(default_data_fol,"wind_input_8760.txt"))
# remove first col of each
solar_avail = solar_avail[:,2:ncol(solar_avail)]
wind_avail = wind_avail[:,2:ncol(wind_avail)]
# loop through all colnames, use findin(genset[:plantUnique],XX) to get row
# sub in new info
for i in 1:length(names(solar_avail))
    col = names(solar_avail)[i]
    ind = findin(genset[:plantUnique],[convert(String, col)])
    pf[ind,:] = solar_avail[hours,i]
end

for i in 1:length(names(wind_avail))
    col = names(wind_avail)[i]
    ind = findin(genset[:plantUnique],[convert(String, col)])
    pf[ind,:] = wind_avail[hours,i]
end


### SETS ###
TIME = 1:n_t
SCENARIOS = 1:n_omega
GENERATORS = 1:n_g #all generators
GEN_NODR = findin(genset[:Fuel],notdr_gens)
GF = fast_ind
GSL = slow_ind #slow generators
GDR = dr_ind #DR generators

### MODEL ###
# m = Model(solver = ClpSolver())
m = Model(solver=GurobiSolver(Presolve=0))

### VARIABLES ###
# @variable(m, 0 <= x <= 2 )
@variable(m, z[1:n_gsl,1:n_t]) # slow generator startup
# @variable(m, 0 <= w[1:n_gsl,1:n_t] <= 1) # slow generator commitment RELAXED BINARY
@variable(m, w[1:n_gsl,1:n_t], Bin) # slow generator commitment TRUE BINARY

#real-time commitment, startup
@variable(m, v[1:n_g,1:n_t,1:n_omega]) # generator startup
# @variable(m, 0 <= u[1:n_g,1:n_t,1:n_omega] <= 1) # generator commitment RELAXED BINARY
@variable(m, u[1:n_g,1:n_t,1:n_omega], Bin) # generator commitment TRUE BINARY

#production variables
@variable(m, p[1:n_g,1:n_t,1:n_omega] >= 0) #generator production
@variable(m, p_dr[1:n_gdr, 1:n_t] >= 0) # DR day-ahead production commitment

@variable(m, start_cost[1:n_g, 1:n_t, 1:n_omega] >= 0)


### CONSTRAINTS ###
# @constraint(m, 1x + 5y <= 3.0 )

#SUPPLY-DEMAND
@constraint(m,supplydemand[t=1:n_t, o=1:n_omega],
    sum( p[g,t,o] for g=1:n_g) == dem[t])
#GENMIN
@constraint(m, [g= 1:n_g, t= 1:n_t, o=1:n_omega ],
    p[g,t,o] >= pmin[g] * u[g,t,o])
#GENMAX
@constraint(m,[g = GEN_NODR, t = 1:n_t, o=1:n_omega],
    p[g,t,o] <= pmax[g] * u[g,t,o] * pf[g,t] )
#GENMAXDR
@constraint(m,[g = 1:n_gdr, t = 1:n_t],
    p_dr[g,t] <= pmax[GDR[g]] * w[g,t] * pf[g,t]) #needed for p_da
#START_S
# @constraint(m,[g = 1:n_gsl, t=1:(n_t-1)],
    # z[g,t+1] == w[g,t+1] - w[g,t])
@constraint(m,[g = 1:n_gsl, t=t_notfirst],
    z[g,t] == w[g,t] - w[g,t-1])
#START_F
# @constraint(m,[g=GENERATORS ,t=1:(n_t-1), o = SCENARIOS],
#     v[g,t+1,o] == u[g,t+1,o] - u[g,t,o])
@constraint(m,[g=GENERATORS ,t=t_notfirst, o = SCENARIOS],
    v[g,t,o] == u[g,t,o] - u[g,t-1,o])
#INIT_S
@constraint(m,[g=1:n_gsl,t=t_firsts],
    z[g,t] == w[g,t])
#INIT_F
@constraint(m,[g=GENERATORS,t=t_firsts, o = SCENARIOS],
    v[g,t,o] == u[g,t,o])
#NAN_ST
@constraint(m,[g = 1:n_gsl, t = TIME, o = SCENARIOS],
    v[GSL[g],t,o] == z[g,t])
#NAN_CM
@constraint(m,[g = 1:n_gsl, t = TIME, o = SCENARIOS],
    u[GSL[g],t,o] == w[g,t])

#DR_RAND
#think carefully about how to index into p and p_dr
#e.g. need to get from [5,7] for p to [1,2] for p_dr
# one workaround is p[g=GDR,t,o] being its own variable.
# would need to change SUPPLY-DEMAND, GENMAX, and potentially startup/commitment too,
# since these rely on the GENERATORS index...

# another workaround could be to list DR resources first
# so that the indices are the same for both.
# this is kinda hack-y but functional and perhaps cleaner
# for the equations. Lets go with this for now
@constraint(m, [g=1:n_gdr,t = TIME, o = SCENARIOS],
     p[GDR[g],t,o] == p_dr[g,t] * vdr[o])

#STARTUP COSTS
@constraint(m, [g=GENERATORS, t = TIME, o = SCENARIOS],
    start_cost[g,t,o] >= v[g,t,o] * startup[g])

### OBJECTIVE ###
# @objective(m, Max, 5x + 3*y )
@objective(m, Min, sum(pro[o] *
    sum(start_cost[g,t,o] + p[g,t,o]*varcost[g] for g = GENERATORS, t = TIME)
    for o = SCENARIOS))

# Check model
# print(m)
if debug
    error("just testing model so we are stopping here")
end

# Solve the model
@printf("\nSolving:\n")
status = solve(m)
@printf("Status: %s\n", status)

if !isdir(output_fol)
    mkdir(output_fol)
end

# check production
print("schedule of DR")
x = getvalue(p_dr)
x_df = DataFrame(transpose(x))
names!(x_df,[Symbol("$input") for input in genset[dr_ind,:plantUnique]])
CSV.write(string(output_fol,"/DR_schedule.csv"), x_df)

# display(x)
print("production of DR:")
y = getvalue(p[GDR,:,:])

y_out = convert3dto2d(y,1, 3,  2,
    vcat([String("o$i") for i in 1:n_omega],"DR_IND","t"),
     genset[dr_ind,:plantUnique])
CSV.write(string(output_fol,"/DR_production.csv"), y_out)

# display(y)
print("production of slow generators:")
zs = getvalue(p[GSL,:,:])

y_out = convert3dto2d(zs,1, 3, 2,
    vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
     genset[slow_ind,:plantUnique])
CSV.write(string(output_fol,"/slow_production.csv"), y_out)

# display(zs)
print("production of fast generators:")
zf = getvalue(p[GF,:,:])
y_out = convert3dto2d(zf,1, 3, 2,
    vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
     genset[fast_ind,:plantUnique])
CSV.write(string(output_fol,"/fast_production.csv"), y_out)
# display(zf)

# check commitment
print("commitment of slow generators:")
# display(getvalue(w))
w_out = getvalue(w)
wdf = DataFrame(transpose(w_out))
names!(wdf,[Symbol("$input") for input in genset[slow_ind,:plantUnique]])
CSV.write(string(output_fol,"/slow_commitment.csv"), wdf)

print("startup of slow generators:")
z_out = getvalue(z)
zdf = DataFrame(transpose(z_out))
names!(zdf,[Symbol("$input") for input in genset[slow_ind,:plantUnique]])
CSV.write(string(output_fol,"/slow_startup.csv"), zdf)

# display(getvalue(z))

print("commitment of all generators")
# display(getvalue(u))
u_out = getvalue(u)
y_out = convert3dto2d(u_out,1, 3, 2,
    vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
     genset[:plantUnique])
CSV.write(string(output_fol,"/u_commitment.csv"), y_out)

print("startup of all generators")
# display(getvalue(v))
v_out = getvalue(v)
y_out = convert3dto2d(v_out,1, 3, 2,
    vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
     genset[:plantUnique])
CSV.write(string(output_fol,"/v_startup.csv"), y_out)

# check costs
print("total cost")
totcost = getvalue(sum(pro[o] *
    sum(start_cost[g,t,o] + p[g,t,o]*varcost[g] for g = GENERATORS, t = TIME)
    for o = SCENARIOS))
display(totcost)
print("startup cost")
display("text/plain",getvalue(start_cost))
print("fraction of total costs that are startup costs")
totstartupcost = getvalue(sum(pro[o] *
    sum(start_cost[g,t,o] for g = GENERATORS, t = TIME)
    for o = SCENARIOS))
display("text/plain",totstartupcost/totcost)
print("fraction of total costs that are var cost")
totvarcost = getvalue(sum(pro[o] *
    sum(p[g,t,o]*varcost[g] for g = GENERATORS, t = TIME)
    for o = SCENARIOS))
display("text/plain",totvarcost/totcost)

output_summary = DataFrame(TotalCost = totcost, TotStartupCst = totstartupcost,
                            TotVarCst = totvarcost,
                            FracStartupCost = totstartupcost/totcost,
                            FracVarCost = totvarcost/totcost)

CSV.write(string(output_fol,"/summary_stats.csv"), output_summary)
