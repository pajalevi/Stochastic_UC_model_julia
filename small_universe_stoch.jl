# Patricia Levi
# pjlevi@stanford.edu

#Pkg.add("JuMP")
using JuMP
using Clp #this is a solver
using Gurobi
using DataFrames
using CSV

### FILE PATHS ###
data_fol = "/Users/patricia/Documents/Google Drive/stanford/second year paper/Tutorial II/Data/gams_input/simple"
unformat_data_fol = "/Users/patricia/Documents/Google Drive/stanford/second year paper/Tutorial II/Data/unformatted data"
n_periods = 5

### READ IN DATA ###
dem = CSV.read(string(data_fol , "/demand_2015_" , n_periods , ".csv"),
                datarow=1)
dem = dem[2]
genset = CSV.read(string(unformat_data_fol,"/gen_data_merged_simple.csv"))
# genset[Symbol("Plant Name")] # this is how to access by column name if there are spaces
# names(genset) # this is how to get the column names
# anscombe[:,[:X3, :Y1]]  #how to grab several columns by colname

# how do I select which rows match one of a set of strings?
# i.e. to pull out the slow, fast generators...
# in("c",["a","b","c"]) # ask if "c" is contained in set of interest
# useful: https://cbrownley.wordpress.com/2015/07/26/intro-to-julia-filtering-rows-with-r-python-and-julia/
#          data_frame_value_in_set =
#           data_frame[findin(data_frame[:quality], set_of_interest), :]

slow_gens = ["COAL","NUCLEAR","DR"]
fast_gens = ["GAS","HYDRO"]
dr_gens = ["DR"]


# PARAMETERS ###
n_t = n_periods# number of timesteps
n_gsl= 5# number of slow generators
n_g =10# number of generators
n_gf = 5
n_gdr = 1 #number of DR generators
n_omega=3 #number of realizations
t_firsts=1 # index of timesteps that are 'first' of a timeblock
t_notfirsts=23 # timesteps that follow

### INPUT DATA ###
dem = [collect(1:(n_t/2)); collect((n_t/2):-1:1);1] * 18

### SETS ###
TIME = 1:n_t
SCENARIOS = 1:n_omega
GENERATORS = 1:n_g #all generators
GEN_NODR = (n_gdr + 1):n_g
GF = 6:10
GSL = 1:5 #slow generators
GDR = 1 #DR generators MUST BE LISTED FIRST

### STOCHASTIC PARAMS ###
vdr = [0.9,1,1.1]
pro = [0.25,0.5,0.25]
#TODO: insert a check that vdr, pro are same length as n_omega


#generator min and max
pmin = repeat([1],inner = n_g)
pmax = repeat([10],outer = n_g)
startup = [0;repeat([2],inner = n_gsl -n_gdr);repeat([1], inner = n_gf)]
#varcost = [0;repeat([1],inner = n_gsl -n_gdr);repeat([2], inner = n_gf)]
varcost = [0;collect(1:0.1:(1+0.1*(n_gsl-n_gdr)));collect(2:0.1:(2+0.1*(n_gf)))]

#generator availability
pf = repeat([1], inner = [n_g, n_t])

### MODEL ###
m = Model(solver = ClpSolver())
# m = Model(solver=GurobiSolver(Presolve=0))

### VARIABLES ###
# @variable(m, 0 <= x <= 2 )
@variable(m, z[1:n_gsl,1:n_t]) # slow generator startup
@variable(m, 0 <= w[1:n_gsl,1:n_t] <= 1) # slow generator commitment RELAXED BINARY
#@variable(m, w[1:n_gsl,1:n_t], Bin) # slow generator commitment TRUE BINARY

#real-time commitment, startup
@variable(m, v[1:n_g,1:n_t,1:n_omega]) # generator startup
@variable(m, 0 <= u[1:n_g,1:n_t,1:n_omega] <= 1) # generator commitment RELAXED BINARY
#@variable(m, u[1:n_g,1:n_t,1:n_omega], Bin) # generator commitment TRUE BINARY

#production variables
@variable(m, p[1:n_g,1:n_t,1:n_omega] >= 0) #generator production
@variable(m, p_dr[1:n_gdr, 1:n_t] >= 0) # DR day-ahead production commitment

@variable(m, start_cost[1:n_g, 1:n_t, 1:n_omega] >= 0)


### CONSTRAINTS ###
# @constraint(m, 1x + 5y <= 3.0 )

#SUPPLY-DEMAND
@constraint(m,[t=1:n_t, o=1:n_omega],
    sum( p[g,t,o] for g=1:n_g) == dem[t])
#GENMIN
@constraint(m, [g= 1:n_g, t= 1:n_t, o=1:n_omega ],
    p[g,t,o] >= pmin[g] * u[g,t,o])
#GENMAX
@constraint(m,[g = GEN_NODR, t = 1:n_t, o=1:n_omega],
    p[g,t,o] <= pmax[g] * u[g,t,o] * pf[g,t] )
#GENMAXDR
@constraint(m,[g = 1:n_gdr, t = 1:n_t],
    p_dr[g,t] <= pmax[g] * w[g,t] * pf[g,t]) #needed for p_da
#START_S
@constraint(m,[g = GSL, t=1:(n_t-1)],
    z[g,t+1] == w[g,t+1] - w[g,t])
#START_F
@constraint(m,[g=GF ,t=1:(n_t-1), o = SCENARIOS],
    v[g,t+1,o] == u[g,t+1,o] - u[g,t,o])
#INIT_S
@constraint(m,[g=GSL,t=t_firsts],
    z[g,t] == w[g,t])
#INIT_F
@constraint(m,[g=GF,t=t_firsts, o = SCENARIOS],
    v[g,t,o] == u[g,t,o])
#NAN_ST
@constraint(m,[g = GSL, t = TIME, o = SCENARIOS],
    v[g,t,o] == z[g,t])
#NAN_CM
@constraint(m,[g = GSL, t = TIME, o = SCENARIOS],
    u[g,t,o] == w[g,t])

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
@constraint(m, [g=GDR,t = TIME, o = SCENARIOS],
     p[g,t,o] == p_dr[g,t] * vdr[o])

#STARTUP COSTS
@constraint(m, [g=GENERATORS, t = TIME, o = SCENARIOS],
    start_cost[g,t,o] >= v[g,t,o] * startup[g])

### OBJECTIVE ###
# @objective(m, Max, 5x + 3*y )
@objective(m, Min, sum(pro[o] *
    sum(start_cost[g,t,o] + p[g,t,o]*varcost[g] for g = GENERATORS, t = TIME)
    for o = SCENARIOS))

# Check model
print(m)

# Solve the model
@printf("\nSolving:\n")
status = solve(m)
@printf("Status: %s\n", status)

# ok, it solves. Now I just need to figure out how to reasonably
# plot the output so I can check it -- refer to R files to get a
# point of comparison for how I manipulated stuff...

# check production
print("schedule of DR")
x = getvalue(p_dr)
display(x)
print("production of DR:")
y = getvalue(p[1,:,:])
display(y)
print("production of slow generators:")
zs = getvalue(p[2:5,:,:])
display(zs)
print("production of fast generators:")
zf = getvalue(p[6:10,:,:])
display(zf)

# check commitment
print("commitment of slow generators:")
display(getvalue(w))
print("startup of slow generators:")
display(getvalue(z))
print("commitment of all generators")
display(getvalue(u))
print("startup of all generators")
display(getvalue(v))

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
