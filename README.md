DataArrays.jl
=============

[![Build Status](https://travis-ci.org/JuliaStats/DataArrays.jl.svg?branch=master)](https://travis-ci.org/JuliaStats/DataArrays.jl)
[![Coverage Status](https://coveralls.io/repos/JuliaStats/DataArrays.jl/badge.svg?branch=master)](https://coveralls.io/r/JuliaStats/DataArrays.jl?branch=master)

Latest release:
[![DataArrays](http://pkg.julialang.org/badges/DataArrays_0.6.svg)](http://pkg.julialang.org/?pkg=DataArrays)

Documentation:
[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://JuliaStats.github.io/DataArrays.jl/stable)
[![](https://img.shields.io/badge/docs-latest-blue.svg)](https://JuliaStats.github.io/DataArrays.jl/latest)

The DataArrays package provides array types for working efficiently with [missing data](https://en.wikipedia.org/wiki/Missing_data)
in Julia, based on the `null` value from the [Nulls.jl](https://github.com/JuliaData/Nulls.jl) package.
In particular, it provides the following:

* `DataArray{T}`: An array-like data structure that can contain values of type `T`, but can also contain missing values.
* `PooledDataArray{T}`: A variant of `DataArray{T}` optimized for representing arrays that contain many repetitions of a small number of unique values -- as commonly occurs when working with categorical data.

# DataArray's

Most Julian arrays cannot contain `null` values: only `Array{Union{T, Null}}` and more generally `Array{>:Null}` can contain `null` values.

The generic use of heterogeneous `Array` is discouraged in Julia versions below 0.7 because it is inefficient: accessing any value requires dereferencing a pointer. The `DataArray` type allows one to work around this inefficiency by providing tightly-typed arrays that can contain values of exactly one type, but can also contain `null` values.

For example, a `DataArray{Int}` can contain integers and `null` values. We can construct one as follows:

	da = @data([1, 2, null, 4])

# PooledDataArray's

As noted above, the `DataArray` type provides an efficient array-like data structure that contain potentially missing values. When working with categorical data sets in which a large number of data points occur, but only take on a limited set of unique values, we provide an analog to `DataArray` that is optimized for efficient memory usage: `PooledDataArray`.

For example, we can use a PooledDataArray to represent the following data efficiently:

	d = repeat(["Group A", "Group B"], inner = [1000, 1000])
	pda = PooledDataArray(d)

Instead of maintaining an array of 2,000 distinct strings, the PooledDataArray will maintain an array of 2,000 distinct references to the 2 unique values: `"Group A"` and `"Group B"`.

In addition to using less memory, PooledDataArray's also maintain a cached record of the unique values in an array: for applications, like the creation of dummy variables, in which the unique values are frequently needed, this allows computations to avoid recalculating the unique values.
