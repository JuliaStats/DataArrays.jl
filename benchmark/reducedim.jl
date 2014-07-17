module ReducedimBenchmark
using DataArrays, Benchmark

# seed rng for more consistent timings
srand(1776)

const TEST_NAMES = [
    "Matrix",
    "DataMatrix No NA skipna=false",
    "DataMatrix No NA skipna=true",
    "DataMatrix Half NA skipna=false",
    "DataMatrix Half NA skipna=true"
]

function make_test_types(genfunc, sz)
    mat = genfunc(abs2(sz))
    na = shuffle!([trues(ifloor(abs2(sz)/2)), falses(iceil(abs2(sz)/2))])
    (
        reshape(mat, sz, sz),
        DataArray(reshape(mat, sz, sz)),
        DataArray(reshape(mat, sz, sz), reshape(na, sz, sz))
    )
end

const Data = make_test_types(rand, 10000)

macro perf(fn, dim, replications)
    quote
        println($fn, " (region = ", $dim, ")")
        fns = [()->$fn(Data[1], $dim),
               ()->$fn(Data[2], $dim),
               ()->$fn(Data[2], $dim; skipna=true),
               ()->$fn(Data[3], $dim),
               ()->$fn(Data[3], $dim; skipna=true)]
        gc_disable()
        df = compare(fns, $replications)
        gc_enable()
        gc()
        df[:Function] = TEST_NAMES
        df[:Relative] = df[:Average]./df[1, :Average]
        println(df)
    end
end

@perf sum 1 10
@perf sum 2 10
@perf maximum 1 10
@perf maximum 2 10
@perf mean 1 10
@perf mean 2 10
@perf var 1 10
@perf var 2 10
end
