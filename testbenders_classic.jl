# testbenders.jl
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
# CLASSICAL APPROACH
#--------------------
masterProblemModel = Model(solver=GurobiSolver())

# Variable Definition
# ----------------------------------------------------------------
@variable(masterProblemModel, 0<= x[1:dimX]<= 1e6  , Int)
@variable(masterProblemModel, t<=1e6)

# Objective Setting
# -----------------
@setObjective(masterProblemModel, Max, t)
iC=1 # iC is the iteration counter

print(masterProblemModel)

while(true)
    println("\n-----------------------")
    println("Iteration number = ", iC)
    println("-----------------------\n")
    println("The current master problem is")
    print(masterProblemModel)
    statusMasterProblem = solve(masterProblemModel)

    if statusMasterProblem == :Infeasible
        println("The problem is infeasible :-(")
        break
    end

    if statusMasterProblem == :Unbounded
        fmCurrent = M
        xCurrent=M*ones(dimX) #I think this just sets x to be a large value...
    end

    if statusMasterProblem == :Optimal
        fmCurrent = getvalue(t)
        xCurrent=Float64[]
        for i in 1:dimX
            push!(xCurrent,getvalue(x[i]))
        end
    end

    println("Status of the master problem is ", statusMasterProblem,
        "\nwith fmCurrent = ", fmCurrent,
        "\nxCurrent = ", xCurrent)

#--------- begin subproblem

    subProblemModel = Model(solver=GurobiSolver())

    cSub = b- A1*xCurrent #for the master-provide part of subproblem
    @variable(subProblemModel, u[1:dimU]>=0)
    @constraint(subProblemModel, constRefSubProblem[j=1:size(A2,2)],
                sum(A2[i,j]*u[i] for i in 1:size(A2,1)) >= c2[j]) #just dotting u with A
    @objective(subProblemModel, Min, dot(c1, xCurrent) + sum(cSub[i]*u[i] for i in 1:dimU))
    print("\nThe current subproblem model is \n", subProblemModel)

    #solve subproblem
    statusSubProblem = solve(subProblemModel)
    fsxCurrent = getobjectivevalue(subProblemModel)

    # get variable values for subproblem
    uCurrent = Float64[]
    for i in 1:dimU
        push!(uCurrent, getvalue(u[i]))
    end

    sgamma=dot(b,uCurrent)

    println("Status of the subproblem is ", statusSubProblem,
     "\nwith fsxCurrent= ", fsxCurrent,
     "\nand fmCurrent= ", fmCurrent)

     if statusSubProblem == :Optimal &&  fsxCurrent == fmCurrent # we are done
         println("\n################################################")
         println("Optimal solution of the original problem found")
         println("The optimal objective value t is ", fmCurrent)
         println("The optimal x is ", xCurrent)
         println("The optimal v is ", getdual(constRefSubProblem))
         println("################################################\n")
         break
     end

     # add a contraint to the master model
     if statusSubProblem == :Optimal && fsxCurrent < fmCurrent
         println("\nThere is a suboptimal vertex, add the corresponding constraint")
         cv= A1'*uCurrent - c1
         @constraint(masterProblemModel, t+sum(cv[i]*x[i] for i in 1:dimX) <= sgamma )
         println("t + ", cv, "ᵀ x <= ", sgamma)
    end

    if statusSubProblem == :Unbounded
         println("\nThere is an  extreme ray, adding the corresponding constraint")
         ce = A1'*uCurrent
         @constraint(masterProblemModel, sum(ce[i]*x[i] for i in 1:dimX) <= sgamma)
         println(ce, "ᵀ x <= ", γ)
    end


    iC=iC+1

    if iC>50
        break
    end

end
