__precompile__()

module DataArrays
    using Base: promote_op
    using Base.Cartesian, Reexport
    @reexport using StatsBase
    @reexport using Missings
    using SpecialFunctions

    const DEFAULT_POOLED_REF_TYPE = UInt32

    import Base: ==, !=, >, <, >=, <=, +, -, *, !, &, |, ⊻, ^, /

    import StatsBase: autocor, inverse_rle, rle

    export @data,
           @pdata,
           AbstractDataArray,
           AbstractDataMatrix,
           AbstractDataVector,
           array,
           autocor,
           compact,
           cut,
           data,
           DataArray,
           DataMatrix,
           DataVector,
           FastPerm,
           getpoolidx,
           gl,
           padmissing,
           pdata,
           PooledDataArray,
           PooledDataMatrix,
           PooledDataVecs,
           PooledDataVector,
           reorder,
           rep,
           replace!,
           setlevels!,
           setlevels

    include("utils.jl")
    include("abstractdataarray.jl")
    include("dataarray.jl")
    include("pooleddataarray.jl")
    include("datavector.jl")
    include("indexing.jl")
    include("datamatrix.jl")
    include("linalg.jl")
    include("operators.jl")
    include("reduce.jl")
    include("reducedim.jl")
    include("broadcast.jl")
    include("sort.jl")
    include("extras.jl")
    include("grouping.jl")
    include("statistics.jl")
    include("literals.jl")
    include("deprecated.jl")
end
