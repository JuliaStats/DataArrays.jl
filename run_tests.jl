#
# Correctness Tests
#

using Base.Test
using DataArrays

my_tests = ["test/abstractarray.jl",
            "test/booleans.jl",
            "test/constructors.jl",
            #"test/containers.jl",
            "test/conversions.jl",
            "test/data.jl",
            "test/dataarray.jl",
            "test/datamatrix.jl",
            "test/linalg.jl",
            #"test/nas.jl",
            #"test/operators.jl",
            "test/padding.jl",
            "test/pooleddataarray.jl"]
            #"test/statistics.jl"]

println("Running tests:")

for my_test in my_tests
    println(" * $(my_test)")
    include(my_test)
end
