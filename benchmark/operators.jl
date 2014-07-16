module DataArraysBenchmark
using DataArrays, Benchmark

# seed rng for more consistent timings
srand(1776)

const TEST_NAMES = [
    "Vector",
    "DataVector No NA",
    "DataVector Half NA",
    "Matrix",
    "DataMatrix No NA",
    "DataMatrix Half NA"
]

function make_test_types(genfunc, sz)
    mat = genfunc(abs2(sz))
    na = shuffle!([trues(ifloor(abs2(sz)/2)), falses(iceil(abs2(sz)/2))])
    (
        mat,
        DataArray(mat),
        DataArray(mat, na),
        reshape(mat, sz, sz),
        DataArray(reshape(mat, sz, sz)),
        DataArray(reshape(mat, sz, sz), reshape(na, sz, sz))
    )
end

make_bools{N}(x::NTuple{N}) = convert(Array{Bool, N}, randbool(x...))
make_bools(x::Integer...) = make_bools(x)

macro perf(fn, replications, idx...)
    name = fn.args[1]
    idx = isempty(idx) ? (1:length(TEST_NAMES)) : idx[1]
    quote
        println($name)
        gc_disable()
        df = compare([let i=i; ()->$fn; end for i=$idx], $replications)
        gc_enable()
        gc()
        df[:Function] = TEST_NAMES[$idx]
        df[:Relative] = df[:Average]./df[1, :Average]
        println(df)
    end
end

const Float1 = make_test_types(rand, 1000)
const Float2 = make_test_types(rand, 1000)
const Bool1 = make_test_types(make_bools, 1000)
const Bool2 = make_test_types(make_bools, 1000)

# Unary operators
@perf -Float1[i] 100
@perf transpose(Float1[i]) 100
@perf abs(Float1[i]) 100
@perf sin(Float1[i]) 25
@perf round(Float1[i]) 25
@perf any(Bool1[i]) 10000
@perf all(Bool1[i]) 10000

# Binary operators
@perf isequal(Float1[i], Float2[i]) 10000
@perf .==(Float1[i], Float2[i]) 100
@perf +(Float1[i], Float2[i]) 100
@perf .+(Float1[i], Float2[i]) 100
@perf .*(Float1[i], Float2[i]) 100
@perf ./(Float1[i], Float2[i]) 50
@perf *(Float1[i], Float2[i]) 10 div(length(Float1), 2)+1:length(Float1)
@perf Bool1[i] & Bool2[i] 100
@perf Bool1[i] | Bool2[i] 100
@perf Bool1[i] $ Bool2[i] 100

# Vector operators
@perf diff(Float1[i]) 50 1:div(length(Float1), 2)
@perf cumsum(Float1[i]) 50 1:div(length(Float1), 2)
end
