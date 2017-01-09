DataArrays.jl
=============

[![Build Status](https://travis-ci.org/JuliaStats/DataArrays.jl.svg?branch=master)](https://travis-ci.org/JuliaStats/DataArrays.jl)
[![Coverage Status](https://coveralls.io/repos/JuliaStats/DataArrays.jl/badge.svg?branch=master)](https://coveralls.io/r/JuliaStats/DataArrays.jl?branch=master)

[![DataArrays](http://pkg.julialang.org/badges/DataArrays_0.4.svg)](http://pkg.julialang.org/?pkg=DataArrays)
[![DataArrays](http://pkg.julialang.org/badges/DataArrays_0.5.svg)](http://pkg.julialang.org/?pkg=DataArrays)
[![DataArrays](http://pkg.julialang.org/badges/DataArrays_0.6.svg)](http://pkg.julialang.org/?pkg=DataArrays)

The DataArrays package extends Julia by introducing data structures that can contain missing data. In particular, the package introduces three new data types to Julia:

* `NA`: A singleton type that represents a single missing value.
* `DataArray{T}`: An array-like data structure that can contain values of type `T`, but can also contain missing values.
* `PooledDataArray{T}`: A variant of `DataArray{T}` optimized for representing arrays that contain many repetitions of a small number of unique values -- as commonly occurs when working with categorical data.

# The `NA` Value

Many languages represent missing values using a reserved value like `NULL` or `NA`. A missing integer value, for example, might be represented as a `NULL` value in SQL or as an `NA` value in R.

Julia takes its conception of `NA` from R, where `NA` denotes missingness based on lack of information. If, for example, we were to measure people's heights as integers, an `NA` might reflect our ignorance of a specific person's height.

Conceptualizing the use of `NA` as a signal of uncertainty will help you understand how `NA` interacts with other values. For example, it explains why `NA + 1` is `NA`, but `NA & false` is `false`. In general, `NA` corrupts any computation whose results cannot be determined without knowledge of the value that is `NA`.

# DataArray's

Most Julian arrays cannot contain `NA` values: only `Array{NAtype}` and heterogeneous Arrays can contain `NA` values. Of these, only heterogeneous arrays could contain values of any type other than `NAtype`.

The generic use of heterogeneous Arrays is discouraged in Julia because it is inefficient: accessing any value requires dereferencing a pointer. The `DataArray` type allows one to work around this inefficiency by providing tightly-typed arrays that can contain values of exactly one type, but can also contain `NA` values.

For example, a `DataArray{Int}` can contain integers and NA values. We can construct one as follows:

	da = @data([1, 2, NA, 4])

# PooledDataArray's

As noted above, the `DataArray` type provides an efficient array-like data structure that contain potentially missing values. When working with categorical data sets in which a large number of data points occur, but only take on a limited set of unique values, we provide an analog to `DataArray` that is optimized for efficient memory usage: `PooledDataArray`.

For example, we can use a PooledDataArray to represent the following data efficiently:

	d = repeat(["Group A", "Group B"], inner = [1000, 1000])
	pda = PooledDataArray(d)

Instead of maintaining an array of 2,000 distinct strings, the PooledDataArray will maintain an array of 2,000 distinct references to the 2 unique values: `"Group A"` and `"Group B"`.

In addition to using less memory, PooledDataArray's also maintain a cached record of the unique values in an array: for applications, like the creation of dummy variables, in which the unique values are frequently needed, this allows computations to avoid recalculating the unique values.
