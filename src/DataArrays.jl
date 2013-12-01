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
           each_removeNA,
           each_replaceNA,
           EachFailNA,
           EachRemoveNA,
           EachReplaceNA,
           failNA,
           FastPerm,
           get_indices,
           getpoolidx,
           gl,
           head,
           index_to_level,
           isna,
           level_to_index,
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
           removeNA,
           reorder,
           rep,
           replace!,
           replaceNA,
           set_levels!,
           set_levels,
           tail,
           xtab,
           xtabs

    include("utils.jl")
    include("natype.jl")
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
end
