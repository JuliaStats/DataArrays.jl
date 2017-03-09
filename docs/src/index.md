# DataArrays.jl

This package provides functionality for working with [missing data](https://en.wikipedia.org/wiki/Missing_data)
in Julia.
In particular, it provides the following:

* `NA`: A singleton representing a missing value
* `DataArray{T}`: An array type that can house both values of type `T` and missing values
* `PooledDataArray{T}`: An array type akin to `DataArray` but optimized for arrays with a smaller set of unique
  values, as commonly occurs with categorical data

## Installation

This package is available for Julia versions 0.6 and up.
To install it, run

```julia
Pkg.add("DataArrays")
```

from the Julia REPL.

## Contents

```@contents
```
