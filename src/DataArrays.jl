using Stats

module DataArrays
    using Stats

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
           DataArray,
           databool,
           datafloat,
           dataint,
           DataMatrix,
           DataVector,
           each_failNA,
           each_dropna,
           each_replaceNA,
           EachFailNA,
           EachDropNA,
           EachReplaceNA,
           failNA,
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
           pdatabool,
           pdatafloat,
           pdataint,
           percent_change,
           Perm,
           PooledDataArray,
           PooledDataMatrix,
           PooledDataVecs,
           PooledDataVector,
           reldiff,
           dropna,
           reorder,
           rep,
           replace!,
           replaceNA,
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
    include("datamatrix.jl")
    include("linalg.jl")
    include("operators.jl")
    include("extras.jl")
    include("grouping.jl")
    include("statistics.jl")
    include("predicates.jl")
    include("literals.jl")

    Base.@deprecate removeNA dropna
    Base.@deprecate set_levels setlevels
    Base.@deprecate set_levels! setlevels!
end
