var documenterSearchIndex = {"docs": [

{
    "location": "index.html#",
    "page": "Home",
    "title": "Home",
    "category": "page",
    "text": ""
},

{
    "location": "index.html#DataArrays.jl-1",
    "page": "Home",
    "title": "DataArrays.jl",
    "category": "section",
    "text": "This package provides functionality for working with missing data in Julia. In particular, it provides the following:NA: A singleton representing a missing value\nDataArray{T}: An array type that can house both values of type T and missing values\nPooledDataArray{T}: An array type akin to DataArray but optimized for arrays with a smaller set of unique values, as commonly occurs with categorical data"
},

{
    "location": "index.html#Installation-1",
    "page": "Home",
    "title": "Installation",
    "category": "section",
    "text": "To install the package, run Pkg.add(\"DataArrays\") from the Julia REPL, then run using DataArrays to load it."
},

{
    "location": "index.html#Contents-1",
    "page": "Home",
    "title": "Contents",
    "category": "section",
    "text": "Pages = [\"da.md\", \"util.md\"]"
},

{
    "location": "da.html#",
    "page": "Missing Data and Arrays",
    "title": "Missing Data and Arrays",
    "category": "page",
    "text": ""
},

{
    "location": "da.html#DataArrays.NA",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.NA",
    "category": "Constant",
    "text": "NA\n\nA value denoting missingness within the domain of any type.\n\n\n\n"
},

{
    "location": "da.html#DataArrays.NAtype",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.NAtype",
    "category": "Type",
    "text": "NAtype\n\nThe type of a missing value, NA.\n\n\n\n"
},

{
    "location": "da.html#Representing-missing-data-1",
    "page": "Missing Data and Arrays",
    "title": "Representing missing data",
    "category": "section",
    "text": "CurrentModule = DataArraysNA\nNAtype"
},

{
    "location": "da.html#DataArrays.AbstractDataArray",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.AbstractDataArray",
    "category": "Constant",
    "text": "AbstractDataArray{T, N}\n\nAn N-dimensional AbstractArray whose entries can take on values of type T or the value NA.\n\n\n\n"
},

{
    "location": "da.html#DataArrays.AbstractDataVector",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.AbstractDataVector",
    "category": "Constant",
    "text": "AbstractDataVector{T}\n\nA 1-dimensional AbstractDataArray with element type T.\n\n\n\n"
},

{
    "location": "da.html#DataArrays.AbstractDataMatrix",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.AbstractDataMatrix",
    "category": "Constant",
    "text": "AbstractDataMatrix{T}\n\nA 2-dimensional AbstractDataArray with element type T.\n\n\n\n"
},

{
    "location": "da.html#DataArrays.DataArray",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.DataArray",
    "category": "Constant",
    "text": "DataArray{T,N}(d::Array{T,N}, m::AbstractArray{Bool} = falses(size(d)))\n\nConstruct a DataArray, an N-dimensional array with element type T that allows missing values. The resulting array uses the data in d with m as a bitmask to signify missingness. That is, for each index i in d, if m[i] is true, the array contains NA at index i, otherwise it contains d[i].\n\nDataArray(T::Type, dims...)\n\nConstruct a DataArray with element type T and dimensions specified by dims. All elements default to NA.\n\nExamples\n\njulia> DataArray([1, 2, 3], [true, false, true])\n3-element DataArrays.DataArray{Int64,1}:\n  NA\n 2\n  NA\n\njulia> DataArray(Float64, 3, 3)\n3×3 DataArrays.DataArray{Float64,2}:\n NA  NA  NA\n NA  NA  NA\n NA  NA  NA\n\n\n\n"
},

{
    "location": "da.html#DataArrays.DataVector",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.DataVector",
    "category": "Constant",
    "text": "DataVector{T}\n\nA 1-dimensional DataArray with element type T.\n\n\n\n"
},

{
    "location": "da.html#DataArrays.DataMatrix",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.DataMatrix",
    "category": "Constant",
    "text": "DataMatrix{T}\n\nA 2-dimensional DataArray with element type T.\n\n\n\n"
},

{
    "location": "da.html#DataArrays.@data",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.@data",
    "category": "Macro",
    "text": "@data expr\n\nCreate a DataArray based on the given expression.\n\nExamples\n\njulia> @data [1, NA, 3]\n3-element DataArrays.DataArray{Int64,1}:\n 1\n  NA\n 3\n\njulia> @data hcat(1:3, 4:6)\n3×2 DataArrays.DataArray{Int64,2}:\n 1  4\n 2  5\n 3  6\n\n\n\n"
},

{
    "location": "da.html#DataArrays.isna",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.isna",
    "category": "Function",
    "text": "isna(a::AbstractArray) -> BitArray\n\nDetermine whether each element of a is missing, i.e. NA.\n\nExamples\n\njulia> isna(@data [1, 2, NA])\n3-element BitArray{1}:\n false\n false\n  true\n\n\n\nisna(a::AbstractArray, i) -> Bool\n\nDetermine whether the element of a at index i is missing, i.e. NA.\n\nExamples\n\njulia> X = @data [1, 2, NA];\n\njulia> isna(X, 2)\nfalse\n\njulia> isna(X, 3)\ntrue\n\n\n\n"
},

{
    "location": "da.html#DataArrays.allna",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.allna",
    "category": "Function",
    "text": "allna(a::AbstractArray) -> Bool\n\nDetermine whether all elements of a are NA.\n\nExamples\n\njulia> allna(@data [NA, NA])\ntrue\n\njulia> allna(@data [1, 2, NA])\nfalse\n\n\n\n"
},

{
    "location": "da.html#DataArrays.anyna",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.anyna",
    "category": "Function",
    "text": "anyna(a::AbstractArray) -> Bool\n\nDetermine whether any of the entries of a are NA.\n\nExamples\n\njulia> anyna([1, 2, 3])\nfalse\n\njulia> anyna(@data [1, 2, NA])\ntrue\n\n\n\n"
},

{
    "location": "da.html#DataArrays.dropna",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.dropna",
    "category": "Function",
    "text": "dropna(v::AbstractVector) -> AbstractVector\n\nReturn a copy of v with all NA elements removed.\n\nExamples\n\njulia> dropna(@data [NA, 1, NA, 2])\n2-element Array{Int64,1}:\n 1\n 2\n\njulia> dropna([4, 5, 6])\n3-element Array{Int64,1}:\n 4\n 5\n 6\n\n\n\n"
},

{
    "location": "da.html#DataArrays.padNA",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.padNA",
    "category": "Function",
    "text": "padNA(dv::AbstractDataVector, front::Integer, back::Integer) -> DataVector\n\nPad dv with NA values. front is an integer number of NAs to add at the beginning of the array and back is the number of NAs to add at the end.\n\nExamples\n\njulia> padNA(@data([1, 2, 3]), 1, 2)\n6-element DataArrays.DataArray{Int64,1}:\n  NA\n 1\n 2\n 3\n  NA\n  NA\n\n\n\n"
},

{
    "location": "da.html#DataArrays.levels",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.levels",
    "category": "Function",
    "text": "levels(da::DataArray) -> DataVector\n\nReturn a vector of the unique values in da, excluding any NAs.\n\nlevels(a::AbstractArray) -> Vector\n\nEquivalent to unique(a).\n\nExamples\n\njulia> levels(@data [1, 2, NA])\n2-element DataArrays.DataArray{Int64,1}:\n 1\n 2\n\n\n\n"
},

{
    "location": "da.html#Arrays-with-possibly-missing-data-1",
    "page": "Missing Data and Arrays",
    "title": "Arrays with possibly missing data",
    "category": "section",
    "text": "AbstractDataArray\nAbstractDataVector\nAbstractDataMatrix\nDataArray\nDataVector\nDataMatrix\n@data\nisna\nallna\nanyna\ndropna\npadNA\nlevels"
},

{
    "location": "da.html#DataArrays.PooledDataArray",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.PooledDataArray",
    "category": "Constant",
    "text": "PooledDataArray(data::AbstractArray{T}, [pool::Vector{T}], [m::AbstractArray{Bool}], [r::Type])\n\nConstruct a PooledDataArray based on the unique values in the given array. PooledDataArrays are useful for efficient storage of categorical data with a limited set of unique values. Rather than storing all length(data) values, it stores a smaller set of values (typically unique(data)) and an array of references to the stored values.\n\nOptional arguments\n\npool: The possible values of data. Defaults to unique(data).\nm: A missingness indicator akin to that of DataArray. Defaults to falses(size(d)).\nr: The integer subtype used to store pool references. Defaults to UInt32.\n\nExamples\n\njulia> d = repeat([\"A\", \"B\"], outer=4);\n\njulia> p = PooledDataArray(d)\n8-element DataArrays.PooledDataArray{String,UInt32,1}:\n \"A\"\n \"B\"\n \"A\"\n \"B\"\n \"A\"\n \"B\"\n \"A\"\n \"B\"\n\nPooledDataArray(T::Type, [R::Type=UInt32], [dims...])\n\nConstruct a PooledDataArray with element type T, reference storage type R, and dimensions dims. If the dimensions are specified and nonzero, the array is filled with NA values.\n\nExamples\n\njulia> PooledDataArray(Int, 2, 2)\n2×2 DataArrays.PooledDataArray{Int64,UInt32,2}:\n NA  NA\n NA  NA\n\n\n\n"
},

{
    "location": "da.html#DataArrays.@pdata",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.@pdata",
    "category": "Macro",
    "text": "@pdata expr\n\nCreate a PooledDataArray based on the given expression.\n\nExamples\n\njulia> @pdata [\"Hello\", NA, \"World\"]\n3-element DataArrays.PooledDataArray{String,UInt32,1}:\n \"Hello\"\n NA\n \"World\"\n\n\n\n"
},

{
    "location": "da.html#DataArrays.compact",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.compact",
    "category": "Function",
    "text": "compact(d::PooledDataArray)\n\nReturn a PooledDataArray with the smallest possible reference type for the data in d.\n\nnote: Note\nIf the reference type is already the smallest possible for the data, the input array is returned, i.e. the function aliases the input.\n\nExamples\n\njulia> p = @pdata(repeat([\"A\", \"B\"], outer=4))\n8-element DataArrays.PooledDataArray{String,UInt32,1}:\n \"A\"\n \"B\"\n \"A\"\n \"B\"\n \"A\"\n \"B\"\n \"A\"\n \"B\"\n\njulia> compact(p) # second type parameter compacts to UInt8 (only need 2 unique values)\n8-element DataArrays.PooledDataArray{String,UInt8,1}:\n \"A\"\n \"B\"\n \"A\"\n \"B\"\n \"A\"\n \"B\"\n \"A\"\n \"B\"\n\n\n\n"
},

{
    "location": "da.html#DataArrays.setlevels",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.setlevels",
    "category": "Function",
    "text": "setlevels(x::PooledDataArray, newpool::Union{AbstractVector, Dict})\n\nCreate a new PooledDataArray based on x but with the new value pool specified by newpool. The values can be replaced using a mapping specified in a Dict or with an array, since the order of the levels is used to identify values. The pool can be enlarged to contain values not present in the data, but it cannot be reduced to exclude present values.\n\nExamples\n\njulia> p = @pdata repeat([\"A\", \"B\"], inner=3)\n6-element DataArrays.PooledDataArray{String,UInt32,1}:\n \"A\"\n \"A\"\n \"A\"\n \"B\"\n \"B\"\n \"B\"\n\njulia> p2 = setlevels(p, [\"C\", \"D\"]) # could also be Dict(\"A\"=>\"C\", \"B\"=>\"D\")\n6-element DataArrays.PooledDataArray{String,UInt32,1}:\n \"C\"\n \"C\"\n \"C\"\n \"D\"\n \"D\"\n \"D\"\n\njulia> p3 = setlevels(p2, [\"C\", \"D\", \"E\"])\n6-element DataArrays.PooledDataArray{String,UInt32,1}:\n \"C\"\n \"C\"\n \"C\"\n \"D\"\n \"D\"\n \"D\"\n\njulia> p3.pool # the pool can contain values not in the array\n3-element Array{String,1}:\n \"C\"\n \"D\"\n \"E\"\n\n\n\n"
},

{
    "location": "da.html#DataArrays.setlevels!",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.setlevels!",
    "category": "Function",
    "text": "setlevels!(x::PooledDataArray, newpool::Union{AbstractVector, Dict})\n\nSet the value pool for the PooledDataArray x to newpool, modifying x in place. The values can be replaced using a mapping specified in a Dict or with an array, since the order of the levels is used to identify values. The pool can be enlarged to contain values not present in the data, but it cannot be reduced to exclude present values.\n\nExamples\n\njulia> p = @pdata repeat([\"A\", \"B\"], inner=3)\n6-element DataArrays.PooledDataArray{String,UInt32,1}:\n \"A\"\n \"A\"\n \"A\"\n \"B\"\n \"B\"\n \"B\"\n\njulia> setlevels!(p, Dict(\"A\"=>\"C\"));\n\njulia> p # has been modified\n6-element DataArrays.PooledDataArray{String,UInt32,1}:\n \"C\"\n \"C\"\n \"C\"\n \"B\"\n \"B\"\n \"B\"\n\n\n\n"
},

{
    "location": "da.html#DataArrays.replace!",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.replace!",
    "category": "Function",
    "text": "replace!(x::PooledDataArray, from, to)\n\nReplace all occurrences of from in x with to, modifying x in place.\n\n\n\n"
},

{
    "location": "da.html#DataArrays.PooledDataVecs",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.PooledDataVecs",
    "category": "Function",
    "text": "PooledDataVecs(v1, v2) -> (pda1, pda2)\n\nReturn a tuple of PooledDataArrays created from the data in v1 and v2, respectively, but sharing a common value pool.\n\n\n\n"
},

{
    "location": "da.html#DataArrays.getpoolidx",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.getpoolidx",
    "category": "Function",
    "text": "getpoolidx(pda::PooledDataArray, val)\n\nReturn the index of val in the value pool for pda. If val is not already in the value pool, pda is modified to include it in the pool.\n\n\n\n"
},

{
    "location": "da.html#DataArrays.reorder",
    "page": "Missing Data and Arrays",
    "title": "DataArrays.reorder",
    "category": "Function",
    "text": "reorder(x::PooledDataArray) -> PooledDataArray\n\nReturn a PooledDataArray containing the same data as x but with the value pool sorted.\n\n\n\n"
},

{
    "location": "da.html#Pooled-arrays-1",
    "page": "Missing Data and Arrays",
    "title": "Pooled arrays",
    "category": "section",
    "text": "PooledDataArray\n@pdata\ncompact\nsetlevels\nsetlevels!\nreplace!\nPooledDataVecs\ngetpoolidx\nreorder"
},

{
    "location": "util.html#",
    "page": "Utilities",
    "title": "Utilities",
    "category": "page",
    "text": ""
},

{
    "location": "util.html#DataArrays.cut",
    "page": "Utilities",
    "title": "DataArrays.cut",
    "category": "Function",
    "text": "cut(x::AbstractVector, breaks::Vector) -> PooledDataArray\ncut(x::AbstractVector, ngroups::Integer) -> PooledDataArray\n\nDivide the range of x into intervals based on the cut points specified in breaks, or into ngroups intervals of approximately equal length.\n\nExamples\n\njulia> cut([1, 2, 3, 4], [1, 3])\n4-element DataArrays.PooledDataArray{String,UInt32,1}:\n \"[1,3]\"\n \"[1,3]\"\n \"[1,3]\"\n \"(3,4]\"\n\n\n\n"
},

{
    "location": "util.html#DataArrays.gl",
    "page": "Utilities",
    "title": "DataArrays.gl",
    "category": "Function",
    "text": "gl(n::Integer, k::Integer, l::Integer = n*k) -> PooledDataArray\n\nGenerate a PooledDataArray with n levels and k replications, optionally specifying an output length l. If specified, l must be a multiple of n*k.\n\nExamples\n\njulia> gl(2, 1)\n2-element DataArrays.PooledDataArray{Int64,UInt8,1}:\n 1\n 2\n\njulia> gl(2, 1, 4)\n4-element DataArrays.PooledDataArray{Int64,UInt8,1}:\n 1\n 2\n 1\n 2\n\n\n\n"
},

{
    "location": "util.html#DataArrays.xtab",
    "page": "Utilities",
    "title": "DataArrays.xtab",
    "category": "Constant",
    "text": "xtab(x::AbstractArray) -> xtab\n\nConstruct a cross-tabulation table from the unique values in x. Currently only one-way tables are supported. Returns an xtab object.\n\n\n\n"
},

{
    "location": "util.html#DataArrays.xtabs",
    "page": "Utilities",
    "title": "DataArrays.xtabs",
    "category": "Function",
    "text": "xtabs(x::AbstractArray) -> Dict\n\nConstruct a cross-tabulation table from the unique values in x, returning a Dict. Currently only one-way tables are supported.\n\n\n\n"
},

{
    "location": "util.html#DataArrays.reldiff",
    "page": "Utilities",
    "title": "DataArrays.reldiff",
    "category": "Function",
    "text": "reldiff(v::Vector) -> Vector\n\nFor each element in v, compute the relative difference from the previous element.\n\nExamples\n\njulia> reldiff([1.0, 2.0, 3.0, 4.0])\n3-element Array{Float64,1}:\n 2.0\n 1.5\n 1.33333\n\n\n\n"
},

{
    "location": "util.html#DataArrays.percent_change",
    "page": "Utilities",
    "title": "DataArrays.percent_change",
    "category": "Function",
    "text": "percent_change(v::Vector) -> Vector\n\nFor each element in v, compute the percent change from the previous element.\n\nExamples\n\njulia> percent_change([1.0, 2.0, 3.0, 4.0])\n3-element Array{Float64,1}:\n 1.0\n 0.5\n 0.333333\n\n\n\n"
},

{
    "location": "util.html#Utility-functions-1",
    "page": "Utilities",
    "title": "Utility functions",
    "category": "section",
    "text": "CurrentModule = DataArrayscut\ngl\nxtab\nxtabs\nreldiff\npercent_change"
},

]}
