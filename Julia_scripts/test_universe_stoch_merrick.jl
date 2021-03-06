#@ Model: Stochastic Unit Commitment
#@ Description: Adapted from model of Patricia Levi, pjlevi@stanford.edu . Find the dispatch schedule, p, for generators g, that minimizes costs across time t, and scenarios o, subject to technical constraints, a sample of which are below. $$\min\sum_{g,t,o}(start\_cost_{g,t,o}+p_{g,t,o} varcost_g)$$$$\sum_g p_{g,t,o}={dem}_t\quad\forall t,o$$$$start\_cost_{g,t,o}\geq v_{g,t,o}startup_g\forall g,t,o$$$$pmin_g u_{g,t,o} \leq p_{g,t,o} \leq pmax_g u_{g,t,o}$$$$v_{g,t+1,o}=u_{g,t+1,o}-u_{g,t,o}$$$$u,v\in(0,1),p\in\mathbb{R}$$


using JuMP, Clp, JSON


# PARAMETERS ###
#@ Helper object: n_t
#@ Description: number of timesteps
n_t = 10


#@ Input Object: n_g
#@ Description: number of generators
n_g = 10

#@ Input Object: n_gsl
#@ Description: number of slow generators
n_gsl = 5


#@ Helper object: n_gf
#@ Description: number of fast generators
n_gf = n_g - n_gsl

#@ Helper object: n_gdr
#@ Description: number of DR generators
n_gdr = 1 

#@ Helper Object: n_omega
#@ Description: number of realizations
n_omega=3 

#@ Helper object: t_firsts
#@ Description: index of timesteps that are 'first' of a timeblock
t_firsts=1

### SETS ###
#@ Helper object: TIME
TIME = 1:n_t

#@ Helper object: SCENARIOS
SCENARIOS = 1:n_omega

#@ Helper object: GENERATORS
#@ Description: all generators
GENERATORS = 1:n_g

#@ Helper object: GEN_NODR
#@ Description: all generators excluding DR
GEN_NODR = (n_gdr + 1):n_g

#@ Helper object: GF
#@ Description: index of fast generators
GF = n_gsl+1:n_g

#@ Helper object: GSL
#@ Description: index of slow generators
GSL = 1:n_gsl

#@ Helper object: GDR
#@ Description: index of DR generator
GDR = 1

### STOCHASTIC PARAMS ###
#@ Helper object: vdr
#@ Description: stochastic parameter vdr
vdr = [0.9,1,1.1]

#@ Helper object: pro
#@ Description: stochastic parameter pro
pro = [0.25,0.5,0.25]
#TODO: insert a check that vdr, pro are same length as n_omega

### INPUT DATA ###
#@ Helper object: dem
#@ Description: demand
dem = [collect(1:(n_t/2)); collect((n_t/2):-1:1)] * 18

#generator min and max
#@ Helper object: pmin
#@ Description: minimum generator
pmin = repeat([1],inner = n_g)

#@ Helper object: pmax
#@ Description: maximum generator
pmax = repeat([10],outer = n_g)

#@ Helper object: startup
#@ Description: startup costs
startup = [0;repeat([2],inner = n_gsl -n_gdr);repeat([1], inner = n_gf)]

#@ Helper object: varcost
#@ Description: variable costs
varcost = [0;collect(1:0.1:(1+0.1*(n_gsl-n_gdr)));collect(2:0.1:(2+0.1*(n_gf)))]

#@ Helper object: pf
#@ Description: generator availability
pf = repeat([1], inner = [n_g, n_t])


### Model ###
#@ Solver: solver
solver = Clp.Optimizer

#@ Problem: m
m = Model(optimizer_with_attributes(solver))


### VARIABLES ###
#@ Variable: z
#@ Description: slow generator startup
@variable(m, z[1:n_gsl,1:n_t])

#@ Variable: w
#@ Description: slow generator commitment (relaxed binary)
@variable(m, 0 <= w[1:n_gsl,1:n_t] <= 1)

#@ Variable: v
#@ Description: real-time commitment, startup
@variable(m, v[1:n_g,1:n_t,1:n_omega])

#@ Variable: u
#@ Description: generator commitment RELAXED BINARY
@variable(m, 0 <= u[1:n_g,1:n_t,1:n_omega] <= 1) 

#@ Variable: p
#@ Description: generator production
@variable(m, p[1:n_g,1:n_t,1:n_omega] >= 0)

#@ Variable: p_dr
#@ Description: day-ahead production commitment
@variable(m, p_dr[1:n_gdr, 1:n_t] >= 0) 


#@ Variable: start_cost
#@ Description: start_cost
@variable(m, start_cost[1:n_g, 1:n_t, 1:n_omega] >= 0)


### CONSTRAINTS ###

#@ Constraint: SD
#@ Description: Supply from the sum of generators must equal demand $$\sum_g P_{g,t,o}={dem}_t\quad\forall t,o$$
@constraint(m,SD[t=1:n_t, o=1:n_omega],
    sum( p[g,t,o] for g=1:n_g) == dem[t])

#@ Constraint: GENMIN
#@ Description: Minimum generation $$p_{g,t,o}\geq pmin_g u_{g,t,o}$$
@constraint(m, GENMIN[g= 1:n_g, t= 1:n_t, o=1:n_omega ],
    p[g,t,o] >= pmin[g] * u[g,t,o])

#@ Constraint: GENMAX 
#@ Description: Maximum generation $$p_{g,t,o)\leq pmax_g u_{g,t,o} pf_{g,t}$$
@constraint(m,GENMAX[g = GEN_NODR, t = 1:n_t, o=1:n_omega],
    p[g,t,o] <= pmax[g] * u[g,t,o] * pf[g,t] )

#@ Constraint: GENMAXDR
#@ Description: Maximum generation, demand resonse
@constraint(m,GENMAXDR[g = 1:n_gdr, t = 1:n_t],
    p_dr[g,t] <= pmax[g] * w[g,t] * pf[g,t]) #needed for p_da

#@ Constraint: START_S
#@ Description: Generator (slow) start up n
@constraint(m,START_S[g = GSL, t=1:(n_t-1)],
    z[g,t+1] == w[g,t+1] - w[g,t])

#@ Constraint: START_F
#@ Description: Generator (fast) start up 
@constraint(m,START_F[g=GF ,t=1:(n_t-1), o = SCENARIOS],
    v[g,t+1,o] == u[g,t+1,o] - u[g,t,o])

#@ Constraint: INIT_S
#@ Description: First period commitment for slow generators
@constraint(m,INIT_S[g=GSL,t=t_firsts],
    z[g,t] == w[g,t])

#@ Constraint: INIT_F
#@ Description: First period commitment for fast generators
@constraint(m,INIT_F[g=GF,t=t_firsts, o = SCENARIOS],
    v[g,t,o] == u[g,t,o])

#@ Constraint: NAN_ST
#@ Description: 
@constraint(m,NAN_ST[g = GSL, t = TIME, o = SCENARIOS],
    v[g,t,o] == z[g,t])

#@ Constraint: NAN_CM
#@ Description: 
@constraint(m,NAN_CM[g = GSL, t = TIME, o = SCENARIOS],
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
#@ Constraint: DR_RAND
@constraint(m, DR_RAND[g=GDR,t = TIME, o = SCENARIOS],
     p[g,t,o] == p_dr[g,t] * vdr[o])

#@ Constraint: STARTCOST
#@ Description: Total startup costs are a product of startup costs by generator and the number of startups of that generator $$start\_cost_{g,t,o}\geq v_{g,t,o}startup_g\forall g,t,o$$
@constraint(m, STARTCOST[g=GENERATORS, t = TIME, o = SCENARIOS],
    start_cost[g,t,o] >= v[g,t,o] * startup[g])

#@ Function: objectivefn
#@ Description: minimize costs across time and scenarios $$\min\sum_{g,t,o}(start\_cost_{g,t,o}+p_{g,t,o} varcost_g)$$
@expression(m, objectivefn, sum(pro[o] *
    sum(start_cost[g,t,o] + p[g,t,o]*varcost[g] for g = GENERATORS, t = TIME)   for o = SCENARIOS))


# Solve the model
@objective(m, Min, objectivefn)
println("\nSolving:\n")
JuMP.optimize!(m)
println(string("Status: ",termination_status(m)))



#@ Helper object: startupcost_fraction
#@ Description: Fraction of total costs that are startup costs
startupcost_fraction = JuMP.value.(sum(pro[o] *
    sum(start_cost[g,t,o] for g = GENERATORS, t = TIME)
    for o = SCENARIOS)) / JuMP.value.(objectivefn)

#@ Helper object: varcost_fraction
#@ Description: Fraction of total costs that are var costs
varcost_fraction = JuMP.value.(sum(pro[o] *
    sum(p[g,t,o]*varcost[g] for g = GENERATORS, t = TIME)
    for o = SCENARIOS)) / JuMP.value.(objectivefn)


# ok, it solves. Now I just need to figure out how to reasonably
# plot the output so I can check it -- refer to R files to get a
# point of comparison for how I manipulated stuff...

#=
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
=#
