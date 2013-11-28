#
# Correctness Tests
#

using Base.Test
using DataArrays

my_tests = ["abstractarray.jl",
            "booleans.jl",
            "constructors.jl",
            "containers.jl",
            "conversions.jl",
            "data.jl",
            "dataarray.jl",
            "datamatrix.jl",
            "datavector.jl",
            "linalg.jl",
            #"test/nas.jl",
            "operators.jl",
            "padding.jl",
            "pooleddataarray.jl",
            "extras.jl",
            "statistics.jl",
            "literals.jl"]

println("Running tests:")

for my_test in my_tests
    @printf " * %s\n" my_test
    include(my_test)
end
