#testing_gadfly.jl
# just messing around with gadfly
# Patricia Levi July 2018

using Gadfly
Pkg.add("RDatasets")
using RDatasets

iris = dataset("datasets","iris")

p = plot(iris, x=:SepalLength, y = :SepalWidth, color = :Species, Geom.point)
display(p)

plot(layer(x=rand(10), y = rand(10), Theme(default_color=color("red")), Geom.point),
    layer(x = rand(10),y=rand(10), Theme(default_color=color("blue")), Geom.line))
plot(x=rand(10), y = rand(10), Theme(default_color=colorant"red"), Geom.line)
display(p)s
