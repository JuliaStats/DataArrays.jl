# DataArrays.jl

This package provides functionality for working with [missing data](https://en.wikipedia.org/wiki/Missing_data)
in Julia.
In particular, it provides the following:

* `NA`: A singleton representing a missing value
* `DataArray{T}`: An array type that can house both values of type `T` and missing values
* `PooledDataArray{T}`: An array type akin to `DataArray` but optimized for arrays with a smaller set of unique
  values, as commonly occurs with categorical data

## Installation

To install the package, run `Pkg.add("DataArrays")` from the Julia REPL, then run `using DataArrays` to load it.

## Contents

```@contents
Pages = ["da.md", "util.md"]
```
