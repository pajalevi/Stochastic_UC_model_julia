#plot_stoch.jl
#plot outputs of test_universe_stoch.jl

Pkg.update()
Pkg.add("Gadfly")
using Gadfly

include("test_universe_stoch.jl")
