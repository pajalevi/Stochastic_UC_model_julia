# ercot_stoch.jl
# solves the full universe version of a stochastic two stage
# unit-commitment model in which slow generators are committed in the
# first stage and fast generators can be committed in real time.
# Demand response functions as a slow generator with an advance commmitment
# not only of startup but also to a generation schedule
# The only uncertainty modeled is that of the actual DR generation
# The availability of DR can be restricted by the last four command line args

# cmd line format should be:
# include("ercot_stoch.jl") <date> <inputs_file_name> <multi-runTF> <period_name>
# last four are "0" if switch is not used

# Written under Julia 0.6.4
# Patricia Levi
# pjlevi@stanford.edu
# NOV 2018

### Packages ###
using JuMP
using Gurobi
using DataFrames
using CSV
using Dates
# using JLD2
include("convert3dto2d.jl")
include("make_scenarios.jl")
include("writecsvmulti.jl")

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
defaultARGS = [Dates.format(Dates.now(),"Y-m-d"),"inputs_ercot.csv","tiny","true","periods_1_5353_5400.csv"]
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


read_inputs = CSV.File(string(params_fol,input_file_name)) |> DataFrame
read_inputs = read_inputs[!,[:input_name, Symbol(input_version)]]
# add cmd line args to read_inputs file
newdf = DataFrame(input_name = ARGNAMES[1:length(localARGS)], args = localARGS)
rename!(newdf,Symbol.(["input_name",input_version])) #rename column 2
read_inputs = vcat(read_inputs, newdf)
@show read_inputs
# ~ transpose read_inputs so that values can be referenced by name
read_inputs.rowkey = ones(size(read_inputs)[1])
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
nrandp = parse(Int64,inputs[1,:nrandp])
int_length = parse(Int64,inputs[1,:intlength])
debug = !parse(Bool, lowercase(inputs[1,:solve_model]))
MIPFocusParam = parse(Int64,inputs[1,:MIPFocusParam])
MIPGapParam = parse(Float64,inputs[1,:MIPGapParam])
NodefileStartParam = parse(Float64,inputs[1,:NodefileStartParam])
MethodParam = parse(Int64,inputs[1,:MethodParam])

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
if multiTF
    hours = CSV.File(string(subsel_data_fol,periodID),datarow=1,types=[Int]) |> DataFrame
else
    hours = CSV.File(string(subsel_data_fol,"periods_",timeseriesID,".csv"),datarow=1,types=[Int]) |> DataFrame
end
hours = vec(hours[!,1])
n_days = convert(Int32,ceil(length(hours)/24))
n_periods = length(hours)

# convert first and notfirst periods to indices
t_firsts = 1
t_notfirst = collect(2:length(hours))

dem2 = CSV.File(string(default_data_fol, inputs[1,:demandFile]),datarow=2,missingstring="NA") |> DataFrame
# subselect for just the rows corresponding to 'hours'
dem = dem2[hours,2]

# --------------------------------------------------------------------------------------
# STOCHASTIC VARIABLE DATA
# -------------------------------------------
# Test data:
# vdr = [0.9,1,1.1]
# pro = [0.25,0.5,0.25]
# probs = CSV.read(string(default_data_fol , "dist_input_",stochID,".csv"))
# vdr_in = convert(Array,probs[1,:]) # converts the first row of probs to an Array
# pro_in = rationalize.(convert(Array,probs[2,:])) #to avoid rounding issues later
    # if this becomes a problem, look into https://github.com/JuliaMath/DecFP.jl

## Net Demand uncertainty ##

if !isfile(string(subsel_data_fol,"demandScenarios_vdem","_",inputs[1,:stochID],"_",periodsave,".csv")) #haven't checked the below section for 1.0 compatibility
    ndprobs = CSV.read(string(default_data_fol , "dist_input_",inputs[1,:stochID],"_nd.csv"),DataFrame)
    ndv_in = convert(Array,ndprobs[1,:]) # converts the first row of probs to an Array
    ndpro_in = rationalize.(convert(Array,ndprobs[2,:])) #to avoid rounding issues later

    demandScenarios = make_scenarios(n_periods, ndv_in, ndpro_in, int_length; randsel = randScenarioSel, nrand = nrandp)
    vdem = demandScenarios[1]
    pro = demandScenarios[2]
    writecsvmulti(DataFrame(vdem),subsel_data_fol,string("demandScenarios_vdem","_",inputs[1,:stochID]),multiTF,periodsave)
    writecsvmulti(DataFrame(pro),subsel_data_fol,string("demandScenarios_prob","_",inputs[1,:stochID]),multiTF,periodsave)
else
    vdem = convert(Array,CSV.read(string(subsel_data_fol,"demandScenarios_vdem","_",inputs[1,:stochID],"_",periodsave,".csv"),DataFrame))
    pro = convert(Array,CSV.read(string(subsel_data_fol,"demandScenarios_prob","_",inputs[1,:stochID],"_",periodsave,".csv"),DataFrame))
end

# testing:
# if sum(pro) != 1
    # error("sum of probabilities is not one, it is ", sum(pro))
# end
println("sum of probabilities is ", sum(pro))
n_omega = length(pro) #redefine for new number of scenarios

# to check vdr and p are constructed properly
# writecsv("vdr_test.csv",vdr)
# writecsv("p_test.csv",pro')

# Set up demand realizations matrix dem_real[t,o]
dem_real = dem .* vdem

# --------------------------------------------------------------------------------------
# GENERATOR DATA
# -------------------------------------------

genset = CSV.read(string(default_data_fol,inputs[1,:genFile]), missingstring ="NA",DataFrame)

# Tips for manipulating genset data:
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

dr_ind = findall((in)(["DR"]),genset[!,:Speed])
slow_ind = findall((in)(["SLOW"]),genset[!,:Speed])
fast_ind = findall((in)(["FAST"]),genset[!,:Speed])

# adjust slow/fast defn based on DRtype
if DRtype == 1
    fast_ind = vcat(fast_ind,dr_ind)
elseif (DRtype == 2) | (DRtype == 3)
    slow_ind = vcat(slow_ind,dr_ind)
elseif DRtype == 0
    println("No DR in this run")
else
    error("DRtype must be 0, 1, 2 or 3")
end

#generator min and max
pmin = genset.PMin
pmax = genset.Capacity
startup = genset.StartCost
varcost = genset.VCost
rampmax = genset.ramprate

# for manual override of DR variable cost
if dr_override
    varcost[dr_ind] .= dr_varcost
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
solar_avail = CSV.read(string(default_data_fol,"solar_availability_factors_2016.csv"),DataFrame)
wind_avail = CSV.read(string(default_data_fol,"wind_availability_factors_2016.csv"),DataFrame)
# remove last col of each
solar_avail = solar_avail[:,1:(ncol(solar_avail)-1)]
wind_avail = wind_avail[:,1:(ncol(wind_avail)-1)]
# loop through all colnames, use findin(genset[:plantUnique],XX) to get row
# sub in new info
for i in 1:length(names(solar_avail))
    col = names(solar_avail)[i]
    ind = findall((in)([convert(String, col)]),genset[!,:plantUnique])
    pf[ind,:] = solar_avail[hours,i]
end

for i in 1:length(names(wind_avail))
    col = names(wind_avail)[i]
    ind = findall((in)([convert(String, col)]),genset[!,:plantUnique])
    pf[ind,:] = wind_avail[hours,i]
end

### Demand Response
if !ismissing(inputs[1,:availID]) &  DRtype != 0
    dr_avail = CSV.read(string(default_data_fol,inputs[1,:availID],".csv"),DataFrame)
    # remove first col
    dr_avail = dr_avail[:,2:ncol(dr_avail)]
    # sub in new info
    for i in 1:length(names(dr_avail))
        col = names(dr_avail)[i]
        ind = findall((in)([convert(String, col)]),genset[!,:plantUnique])
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
GEN_NODR = findall((in)(notdr_gens),genset[!,:Fuel])
GF = fast_ind
GSL = slow_ind #slow generators
GDR = dr_ind #DR generators
# need an index for where the DR is in the slow generators
GDR_SL_ind = findall((in)(GDR),GSL)

# -------------------------------------------
### MODEL ###
# m = Model(solver = ClpSolver())
m = Model(optimizer_with_attributes(
                Gurobi.Optimizer,"Method" => MethodParam,
                "MIPFocus" => MIPFocusParam,
                "MIPGap" => MIPGapParam,
                "NodefileStart" => NodefileStartParam,
                "NodeFileDir" => "/scratch/users/pjlevi/gurobi_solving_outputs/",
                "Seed" => convert(Int64,abs(floor(rand(Float64)*2000000000)))))
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

#SUPPLY-DEMAND
@constraint(m,supplydemand[t=1:n_t, o=1:n_omega],
    sum( p[g,t,o] for g=1:n_g) >= dem_real[t,o])
#DEMAND RAND
# @constraint(m, dem_rand[t=TIME, o = SCENARIOS],
#     dem_real[t,o] = dem[t] * vnd[t,o]) #THIS DOESNT NEED TO BE A CONSTRAINT?

#GENMIN
@constraint(m, mingen[g= 1:n_g, t= 1:n_t, o=1:n_omega ],
    p[g,t,o] >= pmin[g] * u[g,t,o])
#GENMAX - only for GEN_NODR if dr is random
@constraint(m,[g = 1:n_g, t = 1:n_t, o=1:n_omega],
    p[g,t,o] <= pmax[g] * u[g,t,o] * pf[g,t] )

if DRtype == 3
#GENMAXDR
    @constraint(m,[g = 1:n_gdr, t = 1:n_t],
        p_dr[g,t] <= pmax[GDR[g]] * w[GDR_SL_ind[g],t] * pf[GDR_SL_ind[g],t]) #needed for p_da
    @constraint(m,type3dr[g=1:n_gdr,t = TIME, o = SCENARIOS],
         p[GDR[g],t,o] == p_dr[g,t])
end

#START_S
@constraint(m,[g = 1:n_gsl, t=t_notfirst],
    z[g,t] == w[g,t] - w[g,t-1])
#START_F
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

# @constraint(m, dr_rand[g=1:n_gdr,t = TIME, o = SCENARIOS],
#      p[GDR[g],t,o] == p_dr[g,t] * vdr[t,o])

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
    # make a list of lists of duration+1 timestep groupings
    l = Int(durationlim + 1) #length of window
    s = 1 #stepsize between windows
    WINDOWS = [TIME[i:i+l-1] for i in range(1,Int(length(TIME)-l+1),step=s)]
    # #for testing logic of WINDOWS construction:
    # for i in range(1,Int(length(TIME)-l+1),step=s)
    #     println(TIME[i:i+l-1])
    # end
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
println("\nSolving:\n")
JuMP.optimize!(m)
println(string("Status: ",termination_status(m)))

if !isdir(output_fol)
    mkdir(output_fol)
end

## save a copy of inputs & demand scenarios to the output file
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
# x = value.(p)
# x_df = DataFrame(transpose(x))
# rename!(x_df,[Symbol("$input") for input in genset[:plantUnique]])
# CSV.write(string(output_fol,"production_schedule.csv"), x_df)

# display(x)
# print("production of DR:")
y = value.(p[GDR,:,:])

if DRtype != 0
    y_out = convert3dto2d(y,1, 3,  2,
        vcat([String("o$i") for i in 1:n_omega],"DR_IND","t"),
         genset[dr_ind,:plantUnique])
    writecsvmulti(y_out,output_fol,"DR_production",multiTF,periodsave)
end

# display(y)
# print("production of slow generators:")
zs = value.(p[GSL,:,:])

y_out = convert3dto2d(zs,1, 3, 2,
    vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
     genset[slow_ind,:plantUnique])
writecsvmulti(y_out,output_fol,"slow_production",multiTF,periodsave)


# display(zs)
# print("production of fast generators:")
zf = value.(p[GF,:,:])
y_out = convert3dto2d(zf,1, 3, 2,
    vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
     genset[fast_ind,:plantUnique])
writecsvmulti(y_out,output_fol,"fast_production",multiTF,periodsave)

# display(zf)

# check commitment
# print("commitment of slow generators:")
# display(value.(w))
w_out = value.(w)
wdf = DataFrame(transpose(w_out))
rename!(wdf,[Symbol("$input") for input in genset[slow_ind,:plantUnique]])
writecsvmulti(wdf,output_fol,"slow_commitment",multiTF,periodsave)

# print("commitment of all generators")
# display(value.(u))
u_out = value.(u)
y_out = convert3dto2d(u_out,1, 3, 2,
    vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
     genset[!,:plantUnique])
writecsvmulti(y_out,output_fol,"u_commitment",multiTF,periodsave)

# print("startup of all generators")
# display(value.(v))
v_out = value.(v)
y_out = convert3dto2d(v_out,1, 3, 2,
    vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
     genset[!,:plantUnique])
writecsvmulti(y_out,output_fol,"v_startup",multiTF,periodsave)

# check costs
# print("total cost")
costexpr = @expression(m,sum(pro[o] *
    sum(start_num[g,t,o]*startup[g] + p[g,t,o]*varcost[g] for g = GENERATORS, t = TIME)
    for o = SCENARIOS))
totcost = value.(costexpr)
# display(totcost)
# print("startup cost")
# display("text/plain",value.(start_cost))
# print("fraction of total costs that are startup costs")
startupexpr = @expression(m,sum(pro[o] *
    sum(start_num[g,t,o]*startup[g] for g = GENERATORS, t = TIME)
    for o = SCENARIOS))
totstartupcost = value.(startupexpr)
# totstartupcost = value.(sum(pro[o] *
#     sum(start_num[g,t,o]*startup[g] for g = GENERATORS, t = TIME)
#     for o = SCENARIOS))
# display("text/plain",totstartupcost/totcost)
# print("fraction of total costs that are var cost")
varcostexpr = @expression(m,sum(pro[o] *
    sum(p[g,t,o]*varcost[g] for g = GENERATORS, t = TIME)
    for o = SCENARIOS))
totvarcost = value.(varcostexpr)
# display("text/plain",totvarcost/totcost)

output_summary = DataFrame(TotalCost = totcost, TotStartupCst = totstartupcost,
                            TotVarCst = totvarcost,
                            FracStartupCost = totstartupcost/totcost,
                            FracVarCost = totvarcost/totcost,
                            timeseries = inputs[1,:timeseriesID],
                            stoch_scenario = inputs[1,:stochID])

writecsvmulti(output_summary,output_fol,"summary_stats",multiTF,periodsave)

# Get dual variables and save
## Currently does not work - mingen_subset[saveind,:] doesn't work because saveind is not of type AbstractVector{Bool} https://dataframes.juliadata.org/stable/lib/indexing/
if has_duals(m)
    #supplydemand #2D
    sd_shadow = DataFrame(JuMP.shadow_price.(supplydemand))
    writecsvmulti(sd_shadow,output_fol,"supplydemand_shadow",multiTF,periodsave)

    ### mingen ###
    mingen_shadow = JuMP.shadow_price.(mingen)
    mingen_sf = convert3dto2d(mingen_shadow,1, 3,  2,
        vcat([String("o$i") for i in 1:n_omega],"GEN_IND","t"),
         genset[!,:plantUnique])
    # only save rows where there is a nonzero shadow price
    mingen_subset = mingen_sf[!,1:n_omega]
    shadowsum = sum(convert(Array{Float64},mingen_subset),dims=2)
    saveind = find(shadowsum .!= 0)
    writecsvmulti(mingen_subset[saveind,:],output_fol,"mingen_shadow",multiTF,periodsave)

    ### type3dr ###
    ## TODO: This does not work.
    if DRtype == 3
        type3dr_shadow = JuMP.shadow_price.(type3dr)
        type3dr_sf = convert3dto2d(type3dr_shadow,1, 3,  2,
            vcat([String("o$i") for i in 1:n_omega],"DR_IND","t"),
             genset[dr_ind,:plantUnique])
        # only save rows where there is a nonzero shadow price
        shadowsum = sum(convert(Array{Float64},type3dr_sf[:,1:n_omega]),2)
        saveind = find(shadowsum .!= 0)
        writecsvmulti(type3dr_sf[saveind,:],output_fol,"type3dr_shadow",multiTF,periodsave)
    end
end
