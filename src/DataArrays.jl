using StatsBase

module DataArrays
    using StatsBase, Base.Cartesian, Compat

    const DEFAULT_POOLED_REF_TYPE = Uint32

    export @data,
           @pdata,
           AbstractDataArray,
           AbstractDataMatrix,
           AbstractDataVector,
           allna,
           anyna,
           array,
           autocor,
           compact,
           cut,
           data,
           DataArray,
           DataMatrix,
           DataVector,
           dropna,
           each_failna,
           each_dropna,
           each_replacena,
           EachFailNA,
           EachDropNA,
           EachReplaceNA,
           FastPerm,
           getpoolidx,
           gl,
           head,
           isna,
           levels,
           NA,
           NAException,
           NAtype,
           padNA,
           pdata,
           percent_change,
           PooledDataArray,
           PooledDataMatrix,
           PooledDataVecs,
           PooledDataVector,
           reldiff,
           reorder,
           rep,
           replace!,
           setlevels!,
           setlevels,
           tail,
           xtab,
           xtabs

    include("utils.jl")
    include("natype.jl")
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
    include("predicates.jl")
    include("literals.jl")
    include("deprecated.jl")

    Base.@deprecate removeNA dropna
    Base.@deprecate each_failNA each_failna
    Base.@deprecate each_replaceNA each_replacena
    Base.@deprecate set_levels setlevels
    Base.@deprecate set_levels! setlevels!
end
