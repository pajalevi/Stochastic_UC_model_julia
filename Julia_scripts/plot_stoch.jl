#plot_stoch.jl
#plot outputs of test_universe_stoch.jl

#Pkg.update()
#Pkg.add("Gadfly")
using Gadfly

include("test_universe_stoch.jl")

plot(y=dem, x=1:length(dem), Geom.line, Geom.point,
        Guide.ylabel("MW Demand"),
        Guide.xlabel("Timestep"),
        Guide.title("Demand during simulation"))

# now add to plot the amount of capacity that is committed.
# first need to make a timeseries of capacity that is committed
# this is actually sort of complicated and not very necessary right now
# but for the future I should reerence the R script gdx.readin.timeseries.R (line 150 and before)
# and work on my data manipulation chops
capcommit = getvalue()
