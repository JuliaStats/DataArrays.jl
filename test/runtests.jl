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
            "indexing.jl",
            "linalg.jl",
            "nas.jl",
            "operators.jl",
            "reduce.jl",
            "reducedim.jl",
            "broadcast.jl",
            "padding.jl",
            "pooleddataarray.jl",
            "extras.jl",
            "sort.jl",
            "statistics.jl",
            "literals.jl",
            "newtests/dataarray.jl",
            "newtests/datamatrix.jl",
            "newtests/datavector.jl"]

for test in my_tests
    include(test)
end

@testset "Ambiguities" begin
    @test_broken isempty(detect_ambiguities(DataArrays, Base, Core))
end
