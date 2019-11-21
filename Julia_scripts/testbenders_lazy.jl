# testbenders_lazy.jl
# trying to see if I can make a benders decomp properly
# based on https://www.juliaopt.org/notebooks/Shuvomoy%20-%20Benders%20decomposition.html

using JuMP
using Gurobi

# Data for the problem
# which is of the form :
# min c1*x + c2*v
# s.t.
# A1*x + A2*v <= b
# x >= 0, v >= 0
 #---------------------
c1=[-1;-4]
c2=[-2; -3]
dimX=length(c1)
dimU=length(c2)
b=[-2;-3]
A1=[
    1 -3;
   -1 -3
   ]
A2=[
    1 -2;
   -1 -1
   ]
M=1000

#---------------------
# MODERN APPROACH
# using lazy constraints
#--------------------
masterProblemModel = Model(solver=GurobiSolver(Heuristics=0, Cuts = 0))

# Variable Definition
# ----------------------------------------------------------------

# ***************ALTERNATIVE VARIABLE DEFINITION FOR GUROBI************
#If we replace the two lines above with the follwoing:
@variable(masterProblemModel,  0<= x[1:dimX] <= 1e6 , Int)
@variable(masterProblemModel, t <= 1e6)
# then all the solvers give the expected solution
#**********************************************************************

# objective
@objective(masterProblemModel,Max, t)
print(masterProblemModel)

stringOfBenderCuts = String[] #this is an array to hold the
#Benders custs to be printed later

#we dont add any constraints to start - we add them all thru benders.

# copied from link above, and updated to latest julia conventions
function addBendersCut(cb)
    #***************************************************************************
    # First we store the master problem solution in conventional data structures
    println("----------------------------")
    println("ITERATION NUMBER = ", length(stringOfBenderCuts)+1)
    println("---------------------------\n")

    fmCurrent = getvalue(t)
    xCurrent=Float64[]
    for i in 1:dimX
        push!(xCurrent,getvalue(x[i]))
    end

    # Display the current solution of the master problem
    println("MASTERPROBLEM INFORMATION")
    println("-------------------------")
    println("The master problem that was solved was:")
    print(masterProblemModel)
    println("with ", length(stringOfBenderCuts), " added lazy constraints")
    println(stringOfBenderCuts)
    println("Current Value of x is: ", xCurrent)
    println("Current objective value of master problem, fmCurrent is: ", fmCurrent)
    println("\n")

    #************************************************************************

    # ========================================================================
    #                         Now we solve the subproblem

    # subProblemModel=Model(solver=CplexSolver())

     subProblemModel = Model(solver=GurobiSolver(Presolve=0))

    cSub=b-A1*xCurrent

    @variable(subProblemModel, u[1:dimU]>=0)

    @constraint(subProblemModel, constrRefSubProblem[j=1:size(A2,2)],
        sum(A2[i,j]*u[i] for i in 1:size(A2,1)) >= c2[j])

    @objective(subProblemModel, Min, dot(c1, xCurrent) + sum(cSub[i]*u[i] for i in 1:dimU))

    println("The subproblem is being solved")

    statusSubProblem = solve(subProblemModel)

    # We store the results achieved from the subproblem in conventional data structures

    fsxCurrent = getobjectivevalue(subProblemModel)

    uCurrent = Float64[]
    for i in 1:dimU
        push!(uCurrent, getvalue(u[i]))
    end

    # Display the solution corresponding to the subproblem

    println("SUBPROBLEM INFORMATION")
    println("----------------------")
    println("The subproblem that was solved was: ")
    print(subProblemModel)
    println("Current status of the subproblem is ", statusSubProblem)
    println("Current Value of u is: ", uCurrent) # JuMP will return an extreme ray
    # automatically (if the solver supports it), so we do not need to change the syntax
    println("Current Value of fs(xCurrent) is: ", fsxCurrent)
    println("\n")

    # ==========================================================================
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Now we check the status of the algorithm and add Benders cut when necessary
    sgamma=dot(b,uCurrent)

    if statusSubProblem == :Optimal &&  fsxCurrent==fmCurrent # we are done
        println("OPTIMAL SOLUTION OF THE ORIGINAL PROBLEM FOUND :-)")
        println("The optimal objective value t is ", fmCurrent)
        println("The optimal x is ", xCurrent)
        println("The optimal v is ", getDual(constrRefSubProblem))
        println("\n")
        return
    end

    println("-------------------ADDING LAZY CONSTRAINT----------------")
        if statusSubProblem == :Optimal && fsxCurrent < fmCurrent
        println("\nThere is a suboptimal vertex, add the corresponding constraint")
        cv= A1'*uCurrent - c1
        @lazyconstraint(cb, t+sum(cv[i]*x[i] for i in 1:dimX) <= sgamma )
        println("t + ", cv, "ᵀ x <= ", sgamma)
        push!(stringOfBenderCuts, string("t+", cv, "'x <=", sgamma))
    end

    if statusSubProblem == :Unbounded
        println("\nThere is an  extreme ray, adding the corresponding constraint")
        ce = A1'*uCurrent
        @lazyconstraint(cb, sum(ce[i]*x[i] for i in 1:dimX) <= sgamma)
        println(ce, "x <= ", sgamma)
        push!(stringOfBenderCuts, string(ce, "ᵀ x <= ", sgamma))
    end
    println("\n")
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

end

# now we tell the solver to use this as the callback function
addlazycallback(masterProblemModel, addBendersCut)

solve(masterProblemModel)
