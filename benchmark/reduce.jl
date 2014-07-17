module ReduceBenchmark
using DataArrays, Benchmark

# seed rng for more consistent timings
srand(1776)

const TEST_NAMES = [
    "Vector",
    "DataVector No NA skipna=false",
    "DataVector No NA skipna=true",
    "DataVector Half NA skipna=false",
    "DataVector Half NA skipna=true"
]

function make_test_types(genfunc, sz)
    mat = genfunc(sz)
    na = shuffle!([trues(ifloor(sz/2)), falses(iceil(sz/2))])
    (
        mat,
        DataArray(mat),
        DataArray(mat, na)
    )
end

const Data = make_test_types(rand, 100000000)

macro perf(fn, replications)
    quote
        println($fn)
        fns = [()->$fn(Data[1]),
               ()->$fn(Data[2]),
               ()->$fn(Data[2]; skipna=true),
               ()->$fn(Data[3]),
               ()->$fn(Data[3]; skipna=true)]
        gc_disable()
        df = compare(fns, $replications)
        gc_enable()
        gc()
        df[:Function] = TEST_NAMES
        df[:Relative] = df[:Average]./df[1, :Average]
        println(df)
    end
end

@perf sum 10
@perf maximum 10
@perf mean 10
@perf var 10
end
