# DataArrays.jl

This package provides array types for working efficiently with [missing data](https://en.wikipedia.org/wiki/Missing_data)
in Julia, based on the `missing` value from the [Missings.jl](https://github.com/JuliaData/Missings.jl) package.
In particular, it provides the following:

* `DataArray{T}`: An array type that can house both values of type `T` and missing values (of type `Missing`)
* `PooledDataArray{T}`: An array type akin to `DataArray` but optimized for arrays with a smaller set of unique
  values, as commonly occurs with categorical data. This type is now deprecated in favor of [`CategoricalArray`](https://github.com/JuliaData/CategoricalArrays.jl) or [`PooledArray`](https://github.com/JuliaComputing/PooledArrays.jl).

## Installation

To install the package, run `Pkg.add("DataArrays")` from the Julia REPL, then run `using DataArrays` to load it.

## Contents

```@contents
Pages = ["da.md", "util.md"]
```
