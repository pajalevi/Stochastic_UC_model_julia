 # full_universe_stoch.jl
# solves the full universe version of a stochastic two stage
# unit-commitment model in which slow generators are committed in the
# first stage and fast generators can be committed in real time.
# Demand response functions as a slow generator with an advance commmitment
# not only of startup but also to a generation schedule
# The only uncertainty modeled is that of the actual DR generation
# The availability of DR can be restricted by the last four command line args

# _relibility.jl version adds ability to model the response of DR to
# first-stage directions (assuming DR is type 3)

# cmd line format should be:
# include("full_universe_stoch.jl") <date> <inputs_file_name> <multi-runTF> <period_name>
#
# last four are "0" if switch is not used

# Written under Julia 0.6.4
# Patricia Levi
# pjlevi@stanford.edu
# NOV 2018

# TODO --------------------------------------
# TRIPLECHECK UNITS - MW VS GW
# X change filepath for output writing
# X ----- change file structure in sherlock to match below
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
# 'keep' does not seem to work for Julia 1.0
# upgrade code to Julia 0.7 (which has depracation warnings for 1.0)
# X upload newest version with submission scripts to sherlock
# save workspace to output folder using JLD2
# save probabilities and characteristics of new scenarios in output folder
# -------------------------------------------
### Packages ###
using JuMP
#using Clp
using Gurobi
using DataFrames
using CSV
using JLD2
include("convert3dto2d.jl")
include("make_scenarios_rejection_sample.jl")
include("writecsvmulti.jl")
include("combine_scenarios.jl")

test = Gurobi.Env() # test that gurobi is working


# -------------------------------------------
# USER CONTROLS & CMD LINE ARGS ##########
# -------------------------------------------

# USER PARAMS #
no_vars = false #stops execution before making variables
# debug = true  # stops execution before solving model

sherlock_fol = "/home/users/pjlevi/dr_stoch_uc/julia_ver/"
sherlock_input_file = "inputs/"
sherlock_output_file = "outputs/"
sherlock_params_fol = "code/"
laptop_fol = "/Users/patricia/Documents/Google Drive/stanford/Value of DR Project/"
laptop_input_file = "Data/julia_input/"
laptop_output_file = "Data/julia_output/"
laptop_params_fol = "Julia_UC_Github/Julia_scripts/"

## VIRGINIA DATASET
# slow_gens = ["HYDRO",
#             "COAL","NUCLEAR","DR", "MUNICIPAL_SOLID_WASTE","LANDFILL_GAS",
#             "BIOMASS","GAS","GAS_CC",
#             "IMPORT_COAL","IMPORT_GAS"]
#             # NB: if DR is changed to a fast gen, constraint on hourlim and startlim needs to be changed
# fast_gens = ["GAS_CT","OIL","SOLAR","WIND","IMPORT_HYDRO"]
# dr_gens = ["DR"]
# notdr_gens = ["COAL","NUCLEAR", "MUNICIPAL_SOLID_WASTE","LANDFILL_GAS",
#             "BIOMASS","GAS","GAS_CC",
#             "IMPORT_COAL","IMPORT_GAS",
#             "HYDRO","GAS_CT","OIL","SOLAR","WIND","IMPORT_HYDRO"]

## ERCOT DATASET
## DR is added to slow/fast categories depending on value of
## DRtype (in inputs csv)
slow_gens = ["COAL","NUCLEAR","LANDFILL_GAS",
            "BIOMASS","GAS","GAS_ST","GAS_CC"]
            # NB: if DR is changed to a fast gen, constraint on hourlim and startlim needs to be changed
fast_gens = ["GAS_CT","GAS_ICE","OIL","SOLAR","WIND","HYDRO"]
dr_gens = ["DR"]
notdr_gens = ["BIOMASS","COAL","GAS","GAS_CC","GAS_CT","GAS_ICE","GAS_ST","HYDRO",
            "LANDFILL_GAS","NUCLEAR","OIL","SOLAR","WIND"    ]

####### END USER CONTROLS ##########

# --------------------------------------------------------------------------------------
# FILEPATH SETUP
# -------------------------------------------
if split(pwd(),"/")[2] == "Users"
    Sherlock = false
else
    Sherlock = true # on sherlock? where are folders?
end
@show Sherlock

if Sherlock
    base_fol = sherlock_fol
    input_fol = string(sherlock_fol,sherlock_input_file)
    params_fol = string(sherlock_fol, sherlock_params_fol)
else
    base_fol = laptop_fol
    input_fol = string(laptop_fol,laptop_input_file)
    params_fol = string(laptop_fol, laptop_params_fol)
end
default_data_fol = string(input_fol,"ercot_default/")


# PARSE CMD LINE ARGS #
ARGNAMES = ["date" ,"inputs_file_name","input_verion" ,"multi-runTF", "period_name" ]
defaultARGS = [Dates.format(Dates.now(),"Y-m-d"),"inputs_ercot.csv","rand_o25_00pp_keyDays2","true","periods_1_468_588.csv"]
localARGS = length(ARGS) > 0 ? ARGS : defaultARGS #if ARGS supplied, use those. otherwise, use default
nargs = length(localARGS)
@show localARGS

if nargs == 5
    submitdate = localARGS[1]
    input_file_name = localARGS[2]
    input_version = localARGS[3]
    multiTF = parse(Bool,lowercase(localARGS[4]))
    periodID = localARGS[5]
# elseif nargs == 3
#     submitdate = localARGS[1]
#     input_file_name = localARGS[2]
#     multiTF = parse(Bool,lowercase(localARGS[3]))
#     if !multiTF
#         error("If doing a multi-period run, need period ID")
#     end
elseif nargs > 5
    error(string("Too many arguments supplied. Need ",join(ARGNAMES[1,:]," ")))
elseif nargs <5
    warn("not enough arguments supplied. Need ", join(ARGNAMES[1,:]," "))
end


read_inputs = CSV.read(string(params_fol,input_file_name))
read_inputs = read_inputs[:,[:input_name, Symbol(input_version)]]
# add cmd line args to read_inputs file
newdf = DataFrame(input_name = ARGNAMES[1:length(localARGS)], args = localARGS)
names!(newdf.colindex,map(parse,["input_name",input_version]))
read_inputs = vcat(read_inputs, newdf)
@show read_inputs
# ~ transpose read_inputs so that values can be referenced by name
read_inputs[:,:rowkey] = 1
inputs = unstack(read_inputs,:rowkey, :input_name, Symbol(input_version))

# parse out non-string inputs
startlim = parse(Float64,inputs[1,:startlim])
hourlim = parse(Float64,inputs[1,:hourlim])
energylim = parse(Float64,inputs[1,:energylim])
ramplims = parse(Float64,inputs[1,:ramplims])
durationlim = parse(Float64, inputs[1,:durationlim])
dr_override = parse(Bool,lowercase(inputs[1,:dr_override]))
dr_varcost = parse(Float64,inputs[1,:dr_varcost])
randScenarioSel = parse(Bool,lowercase(inputs[1,:randScenarioSel]))
trueBinaryStartup = parse(Bool,lowercase(inputs[1,:trueBinaryStartup]))
DRtype = parse(Int64,inputs[1,:DRtype])
DRrand = parse(Bool,lowercase(inputs[1,:DRrand]))
nd_nrand_o = parse(Int64,inputs[1,:nd_nrand_o])
int_length = parse(Int64,inputs[1,:intlength])
debug = !parse(Bool, lowercase(inputs[1,:solve_model]))
MIPFocusParam = parse(Int64,inputs[1,:MIPFocusParam])
MIPGapParam = parse(Float64,inputs[1,:MIPGapParam])
NodefileStartParam = parse(Float64,inputs[1,:NodefileStartParam])
MethodParam = parse(Int64,inputs[1,:MethodParam])
ThreadsParam = parse(Int64,inputs[1,:ThreadsParam])

if Sherlock
    output_fol = string(sherlock_fol, sherlock_output_file, input_version,"_",submitdate,"/")
else
    output_fol = string(laptop_fol, laptop_output_file, input_version,"_",submitdate,"/")
end
subsel_data_fol = string(input_fol,inputs[1,:timeseriesID],"/")

# setup unique ID for period identification in output
if multiTF
    pbits = split(periodID,r"_|\.|-")
    periodnum = pbits[2]
    periodfirst = pbits[3]
    periodlast = pbits[4]
    periodsave = string("p",periodnum,"_",periodfirst,"_",periodlast)
end

# --------------------------------------------------------------------------------------
### TIME PERIOD DATA
# -------------------------------------------
# first_periods = CSV.read(string(subsel_data_fol,"first_periods_",inputs[1,:timeseriesID],".csv"),datarow=1)[1]
# notfirst_periods = CSV.read(string(subsel_data_fol,"notfirst_periods_",inputs[1,:timeseriesID],".csv"),datarow=1)[1]
if multiTF
    hours = CSV.read(string(subsel_data_fol,periodID),datarow=1,types=[Int])[1]
else
    hours = CSV.read(string(subsel_data_fol,"periods_",timeseriesID,".csv"),datarow=1,types=[Int])[1]
end
n_days = convert(Int32,ceil(length(hours)/24))
n_periods = length(hours)

# convert first and notfirst periods to indices
t_firsts = 1
t_notfirst = collect(2:length(hours))

dem2 = CSV.read(string(default_data_fol, inputs[1,:demandFile]),datarow=2,missingstring="NA")
# subselect for just the rows corresponding to 'hours'
dem = dem2[hours,2]

# check that t_firsts fall on multiples of 24+1
# ie that each period encompasses entire days
# because this simplifies computation later on
# for i in 1:length(t_firsts)
#     if rem(t_firsts[i],24) != 1
#         error(string("period ",i," does not begin at the beginning of a day"))
#     end
# end
# this gets messed up by daylight savings time

# --------------------------------------------------------------------------------------
# STOCHASTIC VARIABLE DATA
# -------------------------------------------
# vdr = [0.9,1,1.1]
# pro = [0.25,0.5,0.25]
# probs = CSV.read(string(default_data_fol , "dist_input_",stochID,".csv"))
# vdr_in = convert(Array,probs[1,:]) # converts the first row of probs to an Array
# pro_in = rationalize.(convert(Array,probs[2,:])) #to avoid rounding issues later
    # if this becomes a problem, look into https://github.com/JuliaMath/DecFP.jl

## Net Demand uncertainty ##
# if _vdem files do not exist already, assume that the distribution data
# is given by a file describing the probability distribution of deviations
if !isfile(string(subsel_data_fol,"demandScenarios_vdem","_",inputs[1,:stochID],"_",periodsave,".csv"))
    ndprobs = CSV.read(string(default_data_fol , "dist_input_",inputs[1,:stochID],"_nd.csv"))
    ndv_in = convert(Array,ndprobs[1,:]) # converts the first row of probs to an Array
    ndpro_in = rationalize.(convert(Array,ndprobs[2,:])) #to avoid rounding issues later

    demandScenarios = make_scenarios(n_periods, ndv_in, ndpro_in, int_length; randsel = randScenarioSel, nrand = nd_nrand_o)
    vdem_in = demandScenarios[1]
    vdem_p = demandScenarios[2]
    writecsvmulti(DataFrame(vdem_in),subsel_data_fol,string("demandScenarios_vdem","_",inputs[1,:stochID]),multiTF,periodsave)
    writecsvmulti(DataFrame(vdem_p),subsel_data_fol,string("demandScenarios_prob","_",inputs[1,:stochID]),multiTF,periodsave)
else
    vdem_in = convert(Array,CSV.read(string(subsel_data_fol,"demandScenarios_vdem","_",inputs[1,:stochID],"_",periodsave,".csv")))
    vdem_p = convert(Array,CSV.read(string(subsel_data_fol,"demandScenarios_prob","_",inputs[1,:stochID],"_",periodsave,".csv")))
end

## DR Response uncertainty ##
if !isfile(string(subsel_data_fol,"drResponseScenarios_vdr","_",inputs[1,:DRrand_ID],"_",periodsave,".csv"))
    dr_int_length = parse(Int64,inputs[1,:dr_int_length])
    dr_nrand_o = parse(Int64,inputs[1,:dr_nrand_o])

    drprobs = CSV.read(string(default_data_fol , "dist_input_",inputs[1,:DRrand_ID],"_dr.csv"))
    drv_in = convert(Array,drprobs[2,:]) # converts the first row of probs to an Array
    drpro_in = rationalize.(convert(Array,drprobs[1,:])) #to avoid rounding issues later

    demandScenarios = make_scenarios(n_periods, drv_in, drpro_in, dr_int_length; randsel = randScenarioSel, nrand = dr_nrand_o)
    vdr_in = demandScenarios[1]
    vdr_p = demandScenarios[2]
    writecsvmulti(DataFrame(vdr_in),subsel_data_fol,string("drResponseScenarios_vdr","_",inputs[1,:DRrand_ID]),multiTF,periodsave)
    writecsvmulti(DataFrame(vdr_p),subsel_data_fol,string("drResponseScenarios_vdr_p","_",inputs[1,:DRrand_ID]),multiTF,periodsave)
else
    vdr_in = convert(Array,CSV.read(string(subsel_data_fol,"drResponseScenarios_vdr_",inputs[1,:DRrand_ID],"_",periodsave,".csv")))
    vdr_p = convert(Array,CSV.read(string(subsel_data_fol,"drResponseScenarios_vdr_p_",inputs[1,:DRrand_ID],"_",periodsave,".csv")))
end


## Create new vdr, vdem, pro representing combined scenarios from demand
## and DR reliability
println(vdr_p)
println(vdem_p)
scenario_info = combine_scenarios(vdr_in,vdr_p,vdem_in,vdem_p)
vdr = scenario_info[1];
vnd = scenario_info[2];
pro = scenario_info[3];
writecsvmulti(DataFrame(vdr),subsel_data_fol,string("vdr","_",inputs[1,:DRrand_ID]),multiTF,periodsave)
writecsvmulti(DataFrame(vnd),subsel_data_fol,string("vnd","_",inputs[1,:DRrand_ID]),multiTF,periodsave)
writecsvmulti(DataFrame(pro),subsel_data_fol,string("pro","_",inputs[1,:DRrand_ID]),multiTF,periodsave) ## WARNING: passing columns argument with non-AbstractVector entries is deprecated



println("sum of probabilities is ", sum(pro))
n_omega = length(pro) #redefine for new number of scenarios

# to check vdr and p are constructed properly
# writecsv("vdr_test.csv",vdr)
# writecsv("p_test.csv",pro')

# Set up demand realizations matrix dem_real[t,o] #
dem_real = dem .* vnd


# --------------------------------------------------------------------------------------
# GENERATOR DATA
# -------------------------------------------

genset = CSV.read(string(default_data_fol,inputs[1,:genFile]), missingstring ="NA")
# genset[Symbol("Plant Name")] # this is how to access by column name if there are spaces
# names(genset) # this is how to get the column names
# anscombe[:,[:X3, :Y1]]  #how to grab several columns by colname

dr_ind = findin(genset[:Fuel],dr_gens)
slow_ind = findin(genset[:Speed],["SLOW"])
fast_ind = findin(genset[:Speed],["FAST"])

# adjust slow/fast defn based on DRtype
if DRtype == 1
    fast_gens = vcat(fast_gens, dr_gens)
    fast_ind = vcat(fast_ind_, dr_ind)
elseif (DRtype == 2) | (DRtype == 3)
    slow_gens = vcat(slow_gens, dr_gens)
    slow_ind = vcat(slow_ind, dr_ind)
else
    error("DRtype must be 1, 2 or 3")
end

# how do I select which rows match one of a set of strings?
# in("c",["a","b","c"]) # ask if "c" is contained in set of interest
# useful: https://cbrownley.wordpress.com/2015/07/26/intro-to-julia-filtering-rows-with-r-python-and-julia/
#          data_frame_value_in_set =
#           data_frame[findin(data_frame[:quality], set_of_interest), :]

# x = ["a","b","c"]
# set_interest = ["c"]# for this to work must have the []
# findin(x,set_interest)

# dr_ind = findin(genset[:Fuel],dr_gens)
# slow_ind = findin(genset[:Fuel],slow_gens)
# fast_ind = findin(genset[:Fuel],fast_gens)

#generator min and max
pmin = genset[:PMin]
pmax = genset[:Capacity]
startup = genset[:StartCost]
varcost = genset[:VCost]
rampmax = genset[:ramprate]

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

## GENERATOR AVAILABILITY ##
pf = repeat([1.0], inner = [n_g, n_t])

### Wind and solar
# load wind, solar info
solar_avail = CSV.read(string(default_data_fol,"solar_availability_factors_2016.csv"))
wind_avail = CSV.read(string(default_data_fol,"wind_availability_factors_2016.csv"))
# remove last col of each
solar_avail = solar_avail[:,1:(ncol(solar_avail)-1)]
wind_avail = wind_avail[:,1:(ncol(wind_avail)-1)]
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

### Demand Response
if parse(inputs[1,:availID])!=0
    dr_avail = CSV.read(string(default_data_fol,inputs[1,:availID],".csv"))
    # remove first col
    dr_avail = dr_avail[:,2:ncol(dr_avail)]
    # sub in new info
    for i in 1:length(names(dr_avail))
        col = names(dr_avail)[i]
        ind = findin(genset[:plantUnique],[convert(String, col)])
        if length(ind) > 0
            pf[ind,:] = dr_avail[hours,i]
        else
            error("No corresponding DR unit found for ", names(dr_avail)[i])
        end
    end
end


# --------------------------------------------------------------------------------------
# MODEL SETUP
# -------------------------------------------
### SETS ###
TIME = 1:n_t
SCENARIOS = 1:n_omega
GENERATORS = 1:n_g #all generators
GEN_NODR = findin(genset[:Fuel],notdr_gens)
GF = fast_ind
GSL = slow_ind #slow generators
GDR = dr_ind #DR generators
# need an index for where the DR is in the slow generators
GDR_SL_ind = findin(GSL,GDR)

# -------------------------------------------
### MODEL ###
# m = Model(solver = ClpSolver())
m = Model(solver=GurobiSolver(Threads = ThreadsParam,ImproveStartGap = 0.0002, Method=MethodParam,#1,
                              MIPFocus=MIPFocusParam,#3,
                              MIPGap=MIPGapParam,#0.0003,
                              NodefileStart = NodefileStartParam,#0.05,
                              NodefileDir = "/scratch/users/pjlevi/gurobi_solving_outputs/",
                              Seed = convert(Int64,abs(floor(rand(Float64)*2000000000)))))
                # can also try reducing threadcount, reducing nodefilestart
if no_vars
    error("just testing model so we are stopping here")
else
    println("begin building model")
end

# -----------------------------------------------------------------------------------------------------
### VARIABLES ###
# @variable(m, 0 <= x <= 2 )
@variable(m, z[1:n_gsl,1:n_t]) # slow generator startup
if trueBinaryStartup
    @variable(m, w[1:n_gsl,1:n_t], Bin) # slow generator commitment TRUE BINARY
else
    @variable(m, 0 <= w[1:n_gsl,1:n_t] <= 1) # slow generator commitment RELAXED BINARY
end

#real-time commitment, startup
@variable(m, v[1:n_g,1:n_t,1:n_omega]) # generator startup
if trueBinaryStartup
    @variable(m, u[1:n_g,1:n_t,1:n_omega], Bin) # generator commitment TRUE BINARY
else
    @variable(m, 0 <= u[1:n_g,1:n_t,1:n_omega] <= 1) # generator commitment RELAXED BINARY
end

#production variables
@variable(m, p[1:n_g,1:n_t,1:n_omega] >= 0) #generator production

# if DR requires advance production schedule
if DRtype == 3
    @variable(m, p_dr[1:n_gdr, 1:n_t] >= 0) # DR day-ahead production commitment
end

# track number of startups to determine costs
@variable(m, start_num[1:n_g, 1:n_t, 1:n_omega] >= 0)


# -----------------------------------------------------------------------------------------------------
### CONSTRAINTS ###
# @constraint(m, 1x + 5y <= 3.0 )

#SUPPLY-DEMAND
@constraint(m,supplydemand[t=1:n_t, o=1:n_omega],
    sum( p[g,t,o] for g=1:n_g) >= dem_real[t,o])
#DEMAND RAND
# @constraint(m, dem_rand[t=TIME, o = SCENARIOS],
#     dem_real[t,o] = dem[t] * vnd[t,o]) #THIS DOESNT NEED TO BE A CONSTRAINT?

#GENMIN
@constraint(m, mingen[g= GEN_NODR, t= 1:n_t, o=1:n_omega ],
    p[g,t,o] >= pmin[g] * u[g,t,o])
#GENMAX - only for GEN_NODR if dr is random
@constraint(m,[g = GEN_NODR, t = 1:n_t, o=1:n_omega],
    p[g,t,o] <= pmax[g] * u[g,t,o] * pf[g,t] )

if DRtype == 3
#GENMAXDR
    @constraint(m,[g = 1:n_gdr, t = 1:n_t],
        p_dr[g,t] <= pmax[GDR[g]] * w[GDR_SL_ind[g],t] * pf[GDR_SL_ind[g],t]) #needed for p_dr
#GENMINDR
    @constraint(m,[g = 1:n_gdr, t = 1:n_t],
        p_dr[g,t] >= pmin[GDR[g]] * w[GDR_SL_ind[g],t])
#TYPE3DR
    # @constraint(m,type3dr[g=1:n_gdr,t = TIME, o = SCENARIOS],
    #      p[GDR[g],t,o] == p_dr[g,t])
end

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
if DRtype == 3 && DRrand
    @constraint(m, dr_rand[g=1:n_gdr,t = TIME, o = SCENARIOS],
         p[GDR[g],t,o] == p_dr[g,t] * vdr[t,o])
elseif DRtype != 3 && DRrand
    error("DRrand can only be used with a DRtype of 3")
end
#TODO: make an elseif for DRype ==3 and !DRrand

#STARTUP COUNT
@constraint(m, [g=GENERATORS, t = TIME, o = SCENARIOS],
    start_num[g,t,o] >= v[g,t,o])

#RAMP RATE -
if ramplims !=0
    @constraint(m,ramplim[g=GENERATORS,t=t_notfirst, o = SCENARIOS],
        p[g,t,o] - p[g,t-1,o] <= (rampmax[g] * pmax[g]))
end

# ------ DR USAGE LIMITS
# Require that each period starts at the beginning of a new day
# to simplify computation

#STARTLIM
# number of startups per period
if startlim !=0
    # # per day
    # @constraint(m,[g = 1:n_gdr,d = 1:n_days, o = SCENARIOS],
    #     sum(v[GDR[g],t,o] for t = (24*(d-1)+1):(24*d)) <= startlim)
    # per period
    @constraint(m,limstart[g = 1:n_gdr, o = SCENARIOS],
        sum(start_num[GDR[g],t,o] for t = TIME) <= startlim)
end
#uses z, the first stage startup var - corresponds to slow DR

#HOURLIM
#number of hours used per period
if hourlim != 0
    # # per day
    # @constraint(m,[g = 1:n_gdr,d = 1:n_days, o = SCENARIOS],
    #     sum(u[GDR[g],t,o] for t = (24*(d-1)+1):(24*d)) <= hourlim)
    # per period
    @constraint(m,limhrs[g = 1:n_gdr, o = SCENARIOS],
        sum(u[GDR[g],t,o] for t = TIME) <= hourlim)
end
#uses w, the first stage startup var - corresponds to slow DR

#ENERGYLIM
# amount of energy used per period
if energylim != 0
    # per day
    # @constraint(m,[g = 1:n_gdr,d = 1:n_days ,o=SCENARIOS],
    #     sum(p[GDR[g],t,o] for t = (24*(d-1)+1):(24*d)) <= energylim)
    # per period
    @constraint(m,limenergy[g = 1:n_gdr, o=SCENARIOS],
        sum(p[GDR[g],t,o] for t = TIME) <= energylim)
end

#DURATIONLIM
# number of hours in a row that DR can be committed
if durationlim != 0
    # make a list of lists of 24-h timestep groupings
    l = Int(durationlim + 1)
    s = 1
    WINDOWS = [TIME[i:i+l-1] for i in range(1,s,Int(length(TIME)-l+1))]
    for i in range(1,s,Int(length(TIME)-l+1))
        print(TIME[i:i+l-1])
    end
    for W in WINDOWS
        @constraint(m,[g = 1:n_gdr, o = SCENARIOS],
            sum(u[GDR[g],t,o] for t = W) <= durationlim)
    end
end

### OBJECTIVE ###
# @objective(m, Max, 5x + 3*y )
@objective(m, Min, sum(pro[o] *
    sum(start_num[g,t,o]*startup[g] + p[g,t,o]*varcost[g] for g = GENERATORS, t = TIME)
    for o = SCENARIOS))

# Check model
# print(m)
if debug
    error("just testing model so we are stopping here")
end

# -------------------------------------------
# SOLVE MODEL
# -------------------------------------------
@printf("\nSolving:\n")
status = solve(m)
@printf("Status: %s\n", status)

if !isdir(output_fol)
    mkdir(output_fol)
end

## save a copy of inputs & demand scenarios to the output file
# inputscsv = DataFrame(hcat(ARGNAMES[1,:],localARGS))
# inputscsv = DataFrame(input_type = ARGNAMES[1,:], value = localARGS)
# CSV.write(string(output_fol,"run_inputs.csv"), inputscsv)
writecsvmulti(read_inputs,output_fol,"inputfile",multiTF,periodsave)

# --------------------------------------------------------------------------------------
# SAVE OUTPUT
# -------------------------------------------

# save workspace: m and certain inputs
# vdr, pro, pf, varcost, various indices...
# try
#     @save string(output_fol,"workspace.jld2") m vdr pro p
# end

# check production
# print("schedule of DR")
# x = getvalue(p)
# x_df = DataFrame(transpose(x))
# names!(x_df,[Symbol("$input") for input in genset[:plantUnique]])
# CSV.write(string(output_fol,"production_schedule.csv"), x_df)

# display(x)
# print("production of DR:")
y = getvalue(p[GDR,:,:])

y_out = convert3dto2d(y,1, 3,  2,
    vcat([String("o$i") for i in 1:n_omega],"DR_IND","t"),
     genset[dr_ind,:plantUnique])
writecsvmulti(y_out,output_fol,"DR_production",multiTF,periodsave)

# display(y)
# print("production of slow generators:")
zs = getvalue(p[GSL,:,:])

y_out = convert3dto2d(zs,1, 3, 2,
    vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
     genset[slow_ind,:plantUnique])
writecsvmulti(y_out,output_fol,"slow_production",multiTF,periodsave)


# display(zs)
# print("production of fast generators:")
zf = getvalue(p[GF,:,:])
y_out = convert3dto2d(zf,1, 3, 2,
    vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
     genset[fast_ind,:plantUnique])
writecsvmulti(y_out,output_fol,"fast_production",multiTF,periodsave)

# display(zf)

# check commitment
# print("commitment of slow generators:")
# display(getvalue(w))
w_out = getvalue(w)
wdf = DataFrame(transpose(w_out))
names!(wdf,[Symbol("$input") for input in genset[slow_ind,:plantUnique]])
writecsvmulti(wdf,output_fol,"slow_commitment",multiTF,periodsave)

# Redundant with startup of all generators --
# print("startup of slow generators:")
# z_out = getvalue(z)
# zdf = DataFrame(transpose(z_out))
# names!(zdf,[Symbol("$input") for input in genset[slow_ind,:plantUnique]])
# writecsvmulti(zdf,output_fol,"slow_startup",multiTF,periodsave)

# display(getvalue(z))

# print("commitment of all generators")
# display(getvalue(u))
u_out = getvalue(u)
y_out = convert3dto2d(u_out,1, 3, 2,
    vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
     genset[:plantUnique])
writecsvmulti(y_out,output_fol,"u_commitment",multiTF,periodsave)

# print("startup of all generators")
# display(getvalue(v))
v_out = getvalue(v)
y_out = convert3dto2d(v_out,1, 3, 2,
    vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
     genset[:plantUnique])
writecsvmulti(y_out,output_fol,"v_startup",multiTF,periodsave)

# check costs
# print("total cost")
totcost = getvalue(sum(pro[o] *
    sum(start_num[g,t,o]*startup[g] + p[g,t,o]*varcost[g] for g = GENERATORS, t = TIME)
    for o = SCENARIOS))
# display(totcost)
# print("startup cost")
# display("text/plain",getvalue(start_cost))
# print("fraction of total costs that are startup costs")
totstartupcost = getvalue(sum(pro[o] *
    sum(start_num[g,t,o]*startup[g] for g = GENERATORS, t = TIME)
    for o = SCENARIOS))
# display("text/plain",totstartupcost/totcost)
# print("fraction of total costs that are var cost")
totvarcost = getvalue(sum(pro[o] *
    sum(p[g,t,o]*varcost[g] for g = GENERATORS, t = TIME)
    for o = SCENARIOS))
# display("text/plain",totvarcost/totcost)

output_summary = DataFrame(TotalCost = totcost, TotStartupCst = totstartupcost,
                            TotVarCst = totvarcost,
                            FracStartupCost = totstartupcost/totcost,
                            FracVarCost = totvarcost/totcost,
                            timeseries = inputs[1,:timeseriesID],
                            stoch_scenario = inputs[1,:stochID])

writecsvmulti(output_summary,output_fol,"summary_stats",multiTF,periodsave)

# Get dual variables and save
if !trueBinaryStartup
    #supplydemand #2D
    sd_shadow = DataFrame(getdual(supplydemand))
    writecsvmulti(sd_shadow,output_fol,"supplydemand_shadow",multiTF,periodsave)

    ### mingen ###
    mingen_shadow = getdual(mingen)
    mingen_sf = convert3dto2d(mingen_shadow,1, 3,  2,
        vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
         genset[:,:plantUnique])
    # only save rows where there is a nonzero shadow price
    shadowsum = sum(convert(Array{Float64},mingen_sf[:,1:n_omega]),2)
    saveind = find(shadowsum .!= 0)
    writecsvmulti(mingen_sf[saveind,:],output_fol,"mingen_shadow",multiTF,periodsave)

    ### type3dr ###
    ## TODO: This does not work.
    if DRtype == 3
        type3dr_shadow = getdual(type3dr)
        type3dr_sf = convert3dto2d(type3dr_shadow,1, 3,  2,
            vcat([String("o$i") for i in 1:n_omega],"DR_IND","t"),
             genset[dr_ind,:plantUnique])
        # only save rows where there is a nonzero shadow price
        shadowsum = sum(convert(Array{Float64},type3dr_sf[:,1:n_omega]),2)
        saveind = find(shadowsum .!= 0)
        writecsvmulti(type3dr_sf[saveind,:],output_fol,"type3dr_shadow",multiTF,periodsave)
    end
end

### ramplim ###
# rampl_shadow = getdual(ramplim)
# rampl_sf = convert3dto2d(rampl_shadow,1, 3,  2,
#     vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
#      genset[:,:plantUnique])
# # only save rows where there is a nonzero shadow price
# shadowsum = sum(convert(Array{Float64},rampl_sf[:,1:n_omega]),2)
# saveind = find(shadowsum .!= 0)
# writecsvmulti(rampl_sf[saveind,:],output_fol,"ramplimit_shadow",multiTF,periodsave)
#
# ### limstart ###
# if startlim !=0
#     limstart_shadow = getdual(limstart)
#     limstart_sf = convert3dto2d(limstart_shadow,1, 3,  2,
#         vcat([String("o$i") for i in 1:n_omega],"DR_IND","t"),
#          genset[dr_ind,:plantUnique])
#     # only save rows where there is a nonzero shadow price
#     shadowsum = sum(convert(Array{Float64},limstart_sf[:,1:n_omega]),2)
#     saveind = find(shadowsum .!= 0)
#     writecsvmulti(limstart_sf[saveind,:],output_fol,"startlimit_shadow",multiTF,periodsave)
# end
# ### limhrs ###
# if hourlim != 0
#     limhrs_shadow = getdual(limhrs)
#     limhrs_sf = convert3dto2d(limhrs_shadow,1, 3,  2,
#         vcat([String("o$i") for i in 1:n_omega],"DR_IND","t"),
#          genset[dr_ind,:plantUnique])
#     # only save rows where there is a nonzero shadow price
#     shadowsum = sum(convert(Array{Float64},limhrs_sf[:,1:n_omega]),2)
#     saveind = find(shadowsum .!= 0)
#     writecsvmulti(limhrs_sf[saveind,:],output_fol,"hourlimit_shadow",multiTF,periodsave)
# end
# ### limenergy ###
# if energylim !=0
#     limenergy_shadow = getdual(limenergy)
#     limenergy_sf = convert3dto2d(limenergy_shadow,1, 3,  2,
#         vcat([String("o$i") for i in 1:n_omega],"DR_IND","t"),
#          genset[dr_ind,:plantUnique])
#     # only save rows where there is a nonzero shadow price
#     shadowsum = sum(convert(Array{Float64},limenergy_sf[:,1:n_omega]),2)
#     saveind = find(shadowsum .!= 0)
#     writecsvmulti(limenergy_sf[saveind,:],output_fol,"energylimit_shadow",multiTF,periodsave)
# end
