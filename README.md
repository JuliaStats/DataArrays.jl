DataArrays.jl
=============

[![Build Status](https://travis-ci.org/JuliaStats/DataArrays.jl.svg?branch=master)](https://travis-ci.org/JuliaStats/DataArrays.jl)
[![Coverage Status](https://coveralls.io/repos/JuliaStats/DataArrays.jl/badge.svg?branch=master)](https://coveralls.io/r/JuliaStats/DataArrays.jl?branch=master)

Latest release:
[![DataArrays](http://pkg.julialang.org/badges/DataArrays_0.6.svg)](http://pkg.julialang.org/?pkg=DataArrays)

Documentation:
[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://JuliaStats.github.io/DataArrays.jl/stable)
[![](https://img.shields.io/badge/docs-latest-blue.svg)](https://JuliaStats.github.io/DataArrays.jl/latest)

The DataArrays package provides the `DataArray` type for working efficiently with [missing data](https://en.wikipedia.org/wiki/Missing_data)
in Julia, based on the `missing` value from the [Missings.jl](https://github.com/JuliaData/Missings.jl) package.

Most Julian arrays cannot contain `missing` values: only `Array{Union{T, Missing}}` and more generally `Array{>:Missing}` can contain `missing` values.

The generic use of heterogeneous `Array` is discouraged in Julia versions below 0.7 because it is inefficient: accessing any value requires dereferencing a pointer. The `DataArray` type allows one to work around this inefficiency by providing tightly-typed arrays that can contain values of exactly one type, but can also contain `missing` values.

For example, a `DataArray{Int}` can contain integers and `missing` values. We can construct one as follows:

	da = @data([1, 2, missing, 4])

This package used to provide the `PooledDataArray` type, a variant of `DataArray{T}` optimized for representing arrays that contain many repetitions of a small number of unique values. `PooledDataArray` has been deprecated in favor of [`CategoricalArray`](https://github.com/JuliaData/CategoricalArrays.jl) or [`PooledArray`](https://github.com/JuliaComputing/PooledArrays.jl).