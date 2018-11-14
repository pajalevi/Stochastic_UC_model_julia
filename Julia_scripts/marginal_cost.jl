#=
marginal_cost.jl
Get marginal cost of generation at each time step
Currently (for universe version of problem) get for every scenario
based on the following files:
* fast_production.csv (model output)
* gen_merged_withIDs.csv (model input)
# demand_2015.csv (input)
* periods_<identifier>.csv (run-specific input)

outputs:
* a matrix with time as rows,
  columns are marginal generator and marginal cost
* plot over time showing marginal cost over time for a subset of scenarios
  with demand over time for reference
* a plot of the supply curve in gen_merged_withIDs.csv

Patricia Levi September 2018
=#

using CSV
using DataFrames

# load files
laptop_fol = "/Users/patricia/Documents/Google Drive/stanford/second year paper/Tutorial II/Data/"
laptop_input_file = "julia_input/"
laptop_output_file = "julia_output/"
outputID = "base_testing_full"

input_file = string(laptop_fol,laptop_input_file)
output_file = string(laptop_fol, laptop_output_file, outputID,"/")

gendata = CSV.read(string(input_file,"default/gen_merged_withIDs.csv"),
        rows_for_type_detect = 200)
production = CSV.read(string(output_file,"fast_production.csv"))


#---------------------------------------------------------
# Find marginal generator & cost per timestep and scenario
#---------------------------------------------------------
# simplify and rename columns for join
gendata = gendata[:,[:plantUnique,:VCost]]
rename!(gendata, :plantUnique => :GEN_IND)
# join - kind = left. (first arg is production)
prod2 = join(production, gendata, on = :GEN_IND, kind = :left)

NUM_SCENARIOS = 9
marginal_costs = DataFrame(t = 1:maximum(prod2[:t]))
for i in 1:NUM_SCENARIOS
    colname = convert(Symbol, string("o",i))
    newprod = prod2[:,[colname, :t, :VCost, :GEN_IND]]

    # select rows with max VCost per timestep using 'by()'
    # newprod = by(newprod, :t, df -> maximum(df[:VCost]))
    omega_marg_cost = by(newprod, :t) do df
        DataFrame(marg_cost =  maximum(df[:VCost]),
                  marg_gen = df[findmax(df[:VCost])[2],:GEN_IND])
    end

    #add this to existing dataframe
    marginal_costs = join(marginal_costs, omega_marg_cost, on = :t, kind = :left)
    rename!(marginal_costs, :marg_cost => colname)

end
