##############################################################################
##
## PooledDataArray type definition
##
## An AbstractDataArray with efficient storage when values are repeated. A
## PDA wraps an array of unsigned integers, which are used to index into a
## compressed pool of values. NA's are 0's in the refs array.  The unsigned
## integer type used for the refs array defaults to Uint32.  The `compact`
## function converts to a smallest integer size that will index the entire pool.
##
## TODO: Allow ordering of factor levels
## TODO: Add metadata for dummy conversion
##
##############################################################################

# This is used as a wrapper during PooledDataArray construction only, to distinguish
# arrays of pool indices from normal arrays
type RefArray{R<:Integer,N}
    a::Array{R,N}
end

type PooledDataArray{T, R<:Integer, N} <: AbstractDataArray{T, N}
    refs::Array{R, N}
    pool::Vector{T}

    function PooledDataArray(rs::RefArray{R, N},
                             p::Vector{T})
        # refs mustn't overflow pool
        if length(rs.a) > 0 && maximum(rs.a) > prod(size(p))
            error("Reference array points beyond the end of the pool")
        end
        new(rs.a,p)
    end
end
typealias PooledDataVector{T,R} PooledDataArray{T,R,1}
typealias PooledDataMatrix{T,R} PooledDataArray{T,R,2}

##############################################################################
##
## PooledDataArray constructors
##
# How do you construct a PooledDataArray from an Array?
# From the same sigs as a DataArray!
#
# Algorithm:
# * Start with:
#   * A null pool
#   * A pre-allocated refs
#   * A hash from T to Int
# * Iterate over d
#   * If value of d in pool already, set the refs accordingly
#   * If value is new, add it to the pool, then set refs
##############################################################################

# Echo inner constructor as an outer constructor
function PooledDataArray{T,R<:Integer,N}(refs::RefArray{R, N},
                                         pool::Vector{T})
    PooledDataArray{T,R,N}(refs, pool)
end

# A no-op constructor
PooledDataArray(d::PooledDataArray) = d

# Constructor from array, w/ pool, missingness, and ref type
function PooledDataArray{T,R<:Integer,N}(d::AbstractArray{T, N},
                                         pool::Vector{T},
                                         m::AbstractArray{Bool, N},
                                         r::Type{R} = DEFAULT_POOLED_REF_TYPE)
    if length(pool) > typemax(R)
        error("Cannot construct a PooledDataVector with type $R with a pool of size $(length(pool))")
    end

    newrefs = Array(R, size(d))
    poolref = Dict{T, R}()

    # loop through once to fill the poolref dict
    for i = 1:length(pool)
        poolref[pool[i]] = i
    end

    # fill in newrefs
    for i = 1:length(d)
        if m[i]
            newrefs[i] = 0
        else
            newrefs[i] = get(poolref, d[i], 0)
        end
    end
    return PooledDataArray(RefArray(newrefs), pool)
end

# Constructor from array, w/ missingness and ref type
function PooledDataArray{T,R<:Integer,N}(d::AbstractArray{T, N},
                                         m::AbstractArray{Bool, N},
                                         r::Type{R} = DEFAULT_POOLED_REF_TYPE)
    pool = sort(unique(d[!m]))
    PooledDataArray(d, pool, m, r)
end

# Construct an all-NA PooledDataVector of a specific type
PooledDataArray(t::Type, dims::(Int...)) = PooledDataArray(Array(t, dims), trues(dims))
PooledDataArray(t::Type, dims::Int...) = PooledDataArray(Array(t, dims), trues(dims))
PooledDataArray{R<:Integer}(t::Type, r::Type{R}, dims::(Int...)) = PooledDataArray(Array(t, dims), trues(dims), r)
PooledDataArray{R<:Integer}(t::Type, r::Type{R}, dims::Int...) = PooledDataArray(Array(t, dims), trues(dims), r)

# Construct an empty PooledDataVector of a specific type
PooledDataArray(t::Type) = PooledDataArray(similar(Array(t,1),0), trues(0))
PooledDataArray{R<:Integer}(t::Type, r::Type{R}) = PooledDataArray(similar(Array(t,1),0), trues(0), r)

# Convert a BitArray to an Array{Bool} (m = missingness)
# For some reason an additional method is needed but even that doesn't work
# For a BitArray a refs type of Uint8 will always be sufficient as the size of the pool is 0, 1 or 2
PooledDataArray{N}(d::BitArray{N}) = PooledDataArray(bitunpack(d), falses(size(d)), Uint8)
PooledDataArray{N}(d::BitArray{N}, m::AbstractArray{Bool, N}) = PooledDataArray(bitunpack(d), m, Uint8)

# Convert a DataArray to a PooledDataArray
PooledDataArray{T,R<:Integer}(da::DataArray{T},
                              r::Type{R} = DEFAULT_POOLED_REF_TYPE) = PooledDataArray(da.data, da.na, r)
PooledDataArray{T,R<:Integer}(da::DataArray{T},
                              pool::Vector{T},
                              r::Type{R} = DEFAULT_POOLED_REF_TYPE) = PooledDataArray(da.data, pool, da.na, r)

# Convert a Array{T} to a PooledDataArray
PooledDataArray{T,R<:Integer}(d::Array{T},
                              r::Type{R} = DEFAULT_POOLED_REF_TYPE) = PooledDataArray(d, falses(size(d)), r)
PooledDataArray{T,R<:Integer}(d::Array{T},
                              pool::Vector{T},
                              r::Type{R} = DEFAULT_POOLED_REF_TYPE) = PooledDataArray(d, pool, falses(size(d)), r)

# Explicitly convert Ranges into a PooledDataVector
PooledDataArray{R<:Integer}(rs::Ranges,
                            r::Type{R} = DEFAULT_POOLED_REF_TYPE) = PooledDataArray([rs], falses(length(rs)), r)

# Initialized constructors with 0's, 1's
for (f, basef) in ((:pdatazeros, :zeros), (:pdataones, :ones))
    @eval begin
        ($f)(dims::Int...) = PooledDataArray(($basef)(dims...), falses(dims...))
        ($f)(t::Type, dims::Int...) = PooledDataArray(($basef)(t, dims...), falses(dims...))
        ($f){R<:Integer}(t::Type, r::Type{R}, dims::Int...) = PooledDataArray(($basef)(t, dims...), falses(dims...), r)
    end
end

##############################################################################
##
## Basic size properties of all Data* objects
##
##############################################################################

Base.size(pda::PooledDataArray) = size(pda.refs)
Base.length(pda::PooledDataArray) = length(pda.refs)
Base.endof(pda::PooledDataArray) = endof(pda.refs)

##############################################################################
##
## Copying Data* objects
##
##############################################################################

Base.copy(pda::PooledDataArray) = PooledDataArray(RefArray(copy(pda.refs)),
                                                  copy(pda.pool))
# TODO: Implement copy_to()

function Base.resize!{T,R}(pda::PooledDataArray{T,R,1}, n::Int)
    oldn = length(pda.refs)
    resize!(pda.refs, n)
    pda.refs[oldn+1:n] = zero(R)
    pda
end

function Base.append!{T,R1,R2}(x::PooledDataArray{T,R1,1}, y::PooledDataArray{T,R2,1})
    pool = myunique(vcat(x.pool, y.pool))
    xn, yn = length(x), length(y)
    resize!(x.refs, xn + yn)

    xtidx::Array{R1} = findat(pool, x.pool)
    for i in 1:xn
        if x.refs[i] != 0
            x.refs[i] = xtidx[x.refs[i]]
        end
    end

    ytidx::Array{R1} = findat(pool, y.pool)
    for i in 1:yn
        x.refs[xn + i] = y.refs[i] == 0 ? 0 : ytidx[y.refs[i]]
    end

    x.pool = pool
    return x
end


function Base.append!{T,R}(x::PooledDataArray{T,R,1}, y::AbstractArray{T,1})
    poolref = Dict{T, R}()
    for i in 1:length(x.pool)
        poolref[x.pool[i]] = i
    end
    for v in y
        if !isna(v) && !haskey(poolref, v)
            poolref[v] = length(poolref) + 1
        end
    end

    pool = Array(T, length(poolref))
    for (v, i) in poolref
        pool[i] = v
    end

    xn, yn = length(x), length(y)
    resize!(x.refs, xn + yn)

    xtidx::Array{R} = findat(pool, x.pool)
    for i in 1:xn
        if x.refs[i] != 0
            x.refs[i] = xtidx[x.refs[i]]
        end
    end

    for i in 1:yn
        if !isna(y[i])
            x.refs[i+xn] = poolref[y[i]]
        end
    end
    x.pool = pool
    return x
end



##############################################################################
##
## Predicates, including the new isna()
##
##############################################################################

function Base.isnan(pda::PooledDataArray)
    PooledDataArray(RefArray(copy(pda.refs)), isnan(pda.pool))
end

function Base.isfinite(pda::PooledDataArray)
    PooledDataArray(RefArray(copy(pda.refs)), isfinite(pda.pool))
end

isna(pda::PooledDataArray) = pda.refs .== 0

#' @description
#'
#' Safe and type-stable way to determine if element `i` of an
#' PooledDataArray is `NA`.
#'
#' @param a::PooledDataArray The PooledDataArray whose missingness will
#'        be assessed.
#' @param i::Integer The index of the element to be checked for `NA`.
#'
#' @returns na::Bool Is the element `NA` or not?
#'
#' @examples
#'
#' a = @pdata([1, 2, 3])
#' isna(a, 1)
isna(pda::PooledDataArray, i::Real) = pda.refs[i] == 0 # -> Bool

##############################################################################
##
## PooledDataArray utilities
##
## TODO: Add methods with these names for DataArray's
##
##############################################################################

function compact{T,R<:Integer,N}(d::PooledDataArray{T,R,N})
    sz = length(d.pool)

    REFTYPE = sz <= typemax(Uint8)  ? Uint8 :
              sz <= typemax(Uint16) ? Uint16 :
              sz <= typemax(Uint32) ? Uint32 :
                                      Uint64

    if REFTYPE == R
        return d
    end

    newrefs = convert(Array{REFTYPE, N}, d.refs)
    PooledDataArray(RefArray(newrefs), d.pool)
end

#' @description
#'
#' Return a DataVector containing the unique values of a `PooledDataArray`,
#' in the order they appear in the data, including `NA` if any missing entries
#' are encountered. For `PooledDataArray`s, this function is much less efficient
#' than `levels`, which does not return the values in the same order.
#'
#' @param da::DataArray{T} `DataArray` whose unique values are desired.
#'
#' @returns dv::DataVector{T} `DataVector` containing the unique values
#'          from `pda`, in the order they appear, including `NA` if there are
#'          any missing entries in `pda`.
#'
#' @examples
#'
#' pdv = @pdata [1, -2, 1, NA, 4]
#' distinct_values = unique(pdv)
function Base.unique{T}(pda::PooledDataArray{T})
    n = length(pda)
    nlevels = length(pda.pool)
    unique_values = Array(T, 0)
    sizehint(unique_values, nlevels)
    seen = Set{eltype(pda.refs)}()

    firstna = 0
    for i in 1:n
        if isna(pda, i)
            if firstna == 0
                firstna = length(unique_values) + 1
            end
        elseif !in(pda.refs[i], seen)
            push!(seen, pda.refs[i])
            push!(unique_values, pda.refs[i])
        else
            continue
        end

        if firstna > 0 && length(unique_values) == nlevels
            break
        end
    end

    if firstna > 0
        res = DataArray(Array(T, nlevels + 1))
        i = 0
        for val in unique_values
            i += 1
            if i == firstna
                res.na[i] = true
                i += 1
            end
            res.data[i] = pda.pool[val]
        end

        return res
    else
        return DataArray(unique_values)
    end
end

#' @description
#'
#' Return a DataVector containing the levels of a `PooledDataArray`,
#' excluding `NA`. For `PooledDataArray`s, this function is much more
#' efficient than `unique`, and it returns the levels in the order used
#' when calling `setlevels!` rather than in the order of appearance.
#' Contrary to `unique`, it also returns levels which are not present in
#' the vector.
#'
#'
#' @param pda::PooledDataArray{T} PooledDataArray whose levels values are
#' desired.
#'
#' @returns dv::DataVector{T} DataVector containing the levels
#'          of `da`, excluding `NA`.
#'
#' @examples
#'
#' pdv = @pdata [1, -2, 1, NA, 4]
#' distinct_values = levels(pdv)
levels{T}(pda::PooledDataArray{T}) = copy(pda.pool)

function PooledDataArray{S,R,N}(x::PooledDataArray{S,R,N},
                                newpool::Vector{S})
    # QUESTION: should we have a ! version of this? If so, needs renaming?
    tidx::Array{R} = findat(newpool, x.pool)
    refs = zeros(R, length(x))
    for i in 1:length(refs)
        if x.refs[i] != 0
            refs[i] = tidx[x.refs[i]]
        end
    end
    PooledDataArray(RefArray(refs), newpool)
end

myunique(x::AbstractVector) = x[sort(unique(findat(x, x)))]  # gets the ordering right
myunique(x::AbstractDataVector) = myunique(dropna(x))   # gets the ordering right; removes NAs

function setlevels{T,R}(x::PooledDataArray{T,R}, newpool::AbstractVector)
    pool = myunique(newpool)
    refs = zeros(R, length(x))
    tidx::Array{R} = findat(pool, newpool)
    tidx[isna(newpool)] = 0
    for i in 1:length(refs)
        if x.refs[i] != 0
            refs[i] = tidx[x.refs[i]]
        end
    end
    return PooledDataArray(RefArray(refs), pool)
end

function setlevels!{T,R}(x::PooledDataArray{T,R}, newpool::AbstractVector{T})
    if newpool == myunique(newpool) # no NAs or duplicates
        x.pool = newpool
        return x
    else
        x.pool = myunique(newpool)
        tidx::Array{R} = findat(x.pool, newpool)
        tidx[isna(newpool)] = 0
        for i in 1:length(x.refs)
            if x.refs[i] != 0
                x.refs[i] = tidx[x.refs[i]]
            end
        end
        return x
    end
end

function setlevels(x::PooledDataArray, d::Dict)
    newpool = copy(DataArray(x.pool))
    # An NA in `v` is put in the pool; that will cause it to become NA
    for (k,v) in d
        idx = findin(newpool, [k])
        if length(idx) == 1
            newpool[idx[1]] = v
        end
    end
    setlevels(x, newpool)
end

function setlevels!{T,R}(x::PooledDataArray{T,R}, d::Dict{T,T})
    for (k,v) in d
        idx = findin(x.pool, [k])
        if length(idx) == 1
            x.pool[idx[1]] = v
        end
    end
    x
end

function setlevels!{T,R}(x::PooledDataArray{T,R}, d::Dict{T,Any}) # this version handles NAs in d's values
    newpool = copy(DataArray(x.pool))
    # An NA in `v` is put in the pool; that will cause it to become NA
    for (k,v) in d
        idx = findin(newpool, [k])
        if length(idx) == 1
            newpool[idx[1]] = v
        end
    end
    setlevels!(x, newpool)
end

reorder(x::PooledDataArray) = PooledDataArray(x, sort(levels(x)))  # just re-sort the pool

reorder(x::PooledDataArray, y::AbstractVector...) = reorder(mean, x, y...)

### FIXME: this can't work because we don't know about DataFrames
# reorder(fun::Function, x::PooledDataArray, y::AbstractVector...) =
#     reorder(fun, x, DataFrame({y...}))

Base.reverse(x::PooledDataArray) = PooledDataArray(RefArray(reverse(x.refs)), x.pool)

##############################################################################
##
## similar()
##
##############################################################################

function Base.similar{T,R}(pda::PooledDataArray{T,R}, S::Type, dims::Dims)
    PooledDataArray(RefArray(zeros(R, dims)), S[])
end

##############################################################################
##
## find()
##
##############################################################################

Base.find(pdv::PooledDataVector{Bool}) = find(array(pdv, false))

##############################################################################
##
## getindex()
##
##############################################################################

# pda[SingleItemIndex]
function Base.getindex(pda::PooledDataArray, i::Real)
    if pda.refs[i] == 0
        return NA
    else
        return pda.pool[pda.refs[i]]
    end
end

# pda[MultiItemIndex]
function Base.getindex(pda::PooledDataArray, inds::AbstractDataVector{Bool})
    inds = find(inds)
    return PooledDataArray(RefArray(pda.refs[inds]), copy(pda.pool))
end
function Base.getindex(pda::PooledDataArray, inds::AbstractDataVector)
    inds = dropna(inds)
    return PooledDataArray(RefArray(pda.refs[inds]), copy(pda.pool))
end
function Base.getindex(pda::PooledDataArray, inds::Union(Vector, BitVector, Ranges))
    return PooledDataArray(RefArray(pda.refs[inds]), copy(pda.pool))
end

# pdm[SingleItemIndex, SingleItemIndex)
function Base.getindex(pda::PooledDataArray, i::Real, j::Real)
    if pda.refs[i, j] == 0
        return NA
    else
        return pda.pool[pda.refs[i, j]]
    end
end

# pda[SingleItemIndex, MultiItemIndex]
function Base.getindex(pda::PooledDataArray, i::Real, col_inds::AbstractDataVector{Bool})
    getindex(pda, i, find(col_inds))
end
function Base.getindex(pda::PooledDataArray, i::Real, col_inds::AbstractDataVector)
    getindex(pda, i, dropna(col_inds))
end
# TODO: Make inds::AbstractVector
function Base.getindex(pda::PooledDataArray,
             i::Real,
             col_inds::Union(Vector, BitVector, Ranges))
    error("not yet implemented")
    PooledDataArray(RefArray(pda.refs[i, col_inds]), pda.pool[i, col_inds])
end

# pda[MultiItemIndex, SingleItemIndex]
function Base.getindex(pda::PooledDataArray, row_inds::AbstractDataVector{Bool}, j::Real)
    getindex(pda, find(row_inds), j)
end
function Base.getindex(pda::PooledDataArray, row_inds::AbstractVector, j::Real)
    getindex(pda, dropna(row_inds), j)
end
# TODO: Make inds::AbstractVector
function Base.getindex(pda::PooledDataArray,
             row_inds::Union(Vector, BitVector, Ranges),
             j::Real)
    error("not yet implemented")
    PooledDataArray(RefArray(pda.refs[row_inds, j]), pda.pool[row_inds, j])
end

# pda[MultiItemIndex, MultiItemIndex]
function Base.getindex(pda::PooledDataArray,
             row_inds::AbstractDataVector{Bool},
             col_inds::AbstractDataVector{Bool})
    getindex(pda, find(row_inds), find(col_inds))
end
function Base.getindex(pda::PooledDataArray,
             row_inds::AbstractDataVector{Bool},
             col_inds::AbstractDataVector)
    getindex(pda, find(row_inds), dropna(col_inds))
end
# TODO: Make inds::AbstractVector
function Base.getindex(pda::PooledDataArray,
             row_inds::AbstractDataVector{Bool},
             col_inds::Union(Vector, BitVector, Ranges))
    getindex(pda, find(row_inds), col_inds)
end
function Base.getindex(pda::PooledDataArray,
             row_inds::AbstractDataVector,
             col_inds::AbstractDataVector{Bool})
    getindex(pda, dropna(row_inds), find(col_inds))
end
function Base.getindex(pda::PooledDataArray,
             row_inds::AbstractDataVector,
             col_inds::AbstractDataVector)
    getindex(pda, dropna(row_inds), dropna(col_inds))
end
# TODO: Make inds::AbstractVector
function Base.getindex(pda::PooledDataArray,
             row_inds::AbstractDataVector,
             col_inds::Union(Vector, BitVector, Ranges))
    getindex(pda, dropna(row_inds), col_inds)
end
# TODO: Make inds::AbstractVector
function Base.getindex(pda::PooledDataArray,
             row_inds::Union(Vector, BitVector, Ranges),
             col_inds::AbstractDataVector{Bool})
    getindex(pda, row_inds, find(col_inds))
end
# TODO: Make inds::AbstractVector
function Base.getindex(pda::PooledDataArray,
             row_inds::Union(Vector, BitVector, Ranges),
             col_inds::AbstractDataVector)
    getindex(pda, row_inds, dropna(col_inds))
end
# TODO: Make inds::AbstractVector
function Base.getindex(pda::PooledDataArray,
                  row_inds::Union(Vector, BitVector, Ranges),
                  col_inds::Union(Vector, BitVector, Ranges))
    error("not yet implemented")
    PooledDataArray(RefArray(pda.refs[row_inds, col_inds]), pda.pool[row_inds, col_inds])
end

##############################################################################
##
## setindex!() definitions
##
##############################################################################

function getpoolidx{T,R<:Union(Uint8, Uint16, Int8, Int16)}(pda::PooledDataArray{T,R}, val::Any)
    val::T = convert(T,val)
    pool_idx = findfirst(pda.pool, val)
    if pool_idx <= 0
        push!(pda.pool, val)
        pool_idx = length(pda.pool)
        if pool_idx > typemax(R)
            error("You're using a PooledDataArray with ref type $R, which can only hold $(int(typemax(R))) values,\n",
                  "and you just tried to add the $(typemax(R)+1)th reference.  Please change the ref type\n",
                  "to a larger int type, or use the default ref type ($DEFAULT_POOLED_REF_TYPE).")
        end
    end
    return pool_idx
end

function getpoolidx{T,R}(pda::PooledDataArray{T,R}, val::Any)
    val::T = convert(T,val)
    pool_idx = findfirst(pda.pool, val)
    if pool_idx <= 0
        push!(pda.pool, val)
        pool_idx = length(pda.pool)
    end
    return pool_idx
end

getpoolidx{T,R<:Union(Uint8, Uint16, Int8, Int16)}(pda::PooledDataArray{T,R}, val::NAtype) = zero(R)
getpoolidx{T,R}(pda::PooledDataArray{T,R}, val::NAtype) = zero(R)

# x[SingleIndex] = NA
# TODO: Delete values from pool that no longer exist?
# Not a good idea.  Add another function called drop_unused_levels to do this.
# R has the convention that if f is a factor then factor(f) drops unused levels
function Base.setindex!(x::PooledDataArray, val::NAtype, ind::Real)
    x.refs[ind] = 0
    return x
end

# x[SingleIndex] = Single Item
# TODO: Delete values from pool that no longer exist?
function Base.setindex!{T,R}(x::PooledDataArray{T,R}, val::Any, ind::Real)
    val = convert(T, val)
    x.refs[ind] = getpoolidx(x, val)
    return x
end

# x[MultiIndex] = NA
# TODO: Find a way to delete the next four methods
function Base.setindex!(x::PooledDataArray{NAtype},
                val::NAtype,
                inds::AbstractVector{Bool})
    error("Don't use PooledDataVector{NAtype}'s")
end
function Base.setindex!(x::PooledDataArray{NAtype},
                val::NAtype,
                inds::AbstractVector)
    error("Don't use PooledDataVector{NAtype}'s")
end
function Base.setindex!(x::PooledDataArray, val::NAtype, inds::AbstractVector{Bool})
    inds = find(inds)
    x.refs[inds] = 0
    return x
end
function Base.setindex!(x::PooledDataArray, val::NAtype, inds::AbstractVector)
    x.refs[inds] = 0
    return x
end

# pda[MultiIndex] = Multiple Values
function Base.setindex!(pda::PooledDataArray,
                   vals::AbstractVector,
                   inds::AbstractVector{Bool})
    setindex!(pda, vals, find(inds))
end
function Base.setindex!(pda::PooledDataArray,
                   vals::AbstractVector,
                   inds::AbstractVector)
    for (val, ind) in zip(vals, inds)
        pda[ind] = val
    end
    return pda
end

# pda[SingleItemIndex, SingleItemIndex] = NA
function Base.setindex!{T,R}(pda::PooledDataMatrix{T,R}, val::NAtype, i::Real, j::Real)
    pda.refs[i, j] = zero(R)
    return pda
end
# pda[SingleItemIndex, SingleItemIndex] = Single Item
function Base.setindex!{T,R}(pda::PooledDataMatrix{T,R}, val::Any, i::Real, j::Real)
    val = convert(T, val)
    pda.refs[i, j] = getpoolidx(pda, val)
    return pda
end

##############################################################################
##
## show() and similar methods
##
##############################################################################

function Base.string(x::PooledDataVector)
    tmp = join(x, ", ")
    return "[$tmp]"
end

# # Need setindex!()'s to make this work
# This is broken now because the inner show returns to the outer show.
# function show(io::IO, pda::PooledDataArray)
#     invoke(show, (Any, AbstractArray), io, pda)
#     print(io, "\nlevels: ")
#     print(io, levels(pda))
# end

##############################################################################
##
## Replacement operations
##
##############################################################################

function replace!(x::PooledDataArray{NAtype}, fromval::NAtype, toval::NAtype)
    NA # no-op to deal with warning
end
function replace!(x::PooledDataArray, fromval::NAtype, toval::NAtype)
    NA # no-op to deal with warning
end
function replace!{S, T}(x::PooledDataArray{S}, fromval::T, toval::NAtype)
    fromidx = findfirst(x.pool, fromval)
    if fromidx == 0
        error("can't replace a value not in the pool in a PooledDataVector!")
    end

    x.refs[x.refs .== fromidx] = 0

    return NA
end
function replace!{S, T}(x::PooledDataArray{S}, fromval::NAtype, toval::T)
    toidx = findfirst(x.pool, toval)
    # if toval is in the pool, just do the assignment
    if toidx != 0
        x.refs[x.refs .== 0] = toidx
    else
        # otherwise, toval is new, add it to the pool
        push!(x.pool, toval)
        x.refs[x.refs .== 0] = length(x.pool)
    end

    return toval
end
function replace!{R, S, T}(x::PooledDataArray{R}, fromval::S, toval::T)
    # throw error if fromval isn't in the pool
    fromidx = findfirst(x.pool, fromval)
    if fromidx == 0
        error("can't replace a value not in the pool in a PooledDataArray!")
    end

    # if toval is in the pool too, use that and remove fromval from the pool
    toidx = findfirst(x.pool, toval)
    if toidx != 0
        x.refs[x.refs .== fromidx] = toidx
        #x.pool[fromidx] = None    TODO: what to do here??
    else
        # otherwise, toval is new, swap it in
        x.pool[fromidx] = toval
    end

    return toval
end

##############################################################################
##
## Sorting can use the pool to speed things up
##
##############################################################################

# TODO handle sortperm for non-sorted keys
Base.sortperm(pda::PooledDataArray) = groupsort_indexer(pda)[1]
function Base.sortperm(pda::PooledDataArray)
    if issorted(pda.pool)
        return groupsort_indexer(pda)[1]
    else
        return sortperm(reorder!(copy(pda)))
    end
end

Base.sortperm(pda::PooledDataArray, ::Base.Sort.ReverseOrdering) = reverse(sortperm(pda))
Base.sort(pda::PooledDataArray) = pda[sortperm(pda)]
Base.sort(pda::PooledDataArray, ::Base.Sort.ReverseOrdering) = pda[reverse(sortperm(pda))]
type FastPerm{O<:Base.Sort.Ordering,V<:AbstractVector} <: Base.Sort.Ordering
    ord::O
    vec::V
end
FastPerm{O<:Base.Sort.Ordering,V<:AbstractVector}(o::O,v::V) = FastPerm{O,V}(o,v)
Base.sortperm{V}(x::AbstractVector, a::Base.Sort.Algorithm, o::FastPerm{Base.Sort.ForwardOrdering,V}) = x[sortperm(o.vec)]
Base.sortperm{V}(x::AbstractVector, a::Base.Sort.Algorithm, o::FastPerm{Base.Sort.ReverseOrdering,V}) = x[reverse(sortperm(o.vec))]
Perm{O<:Base.Sort.Ordering}(o::O, v::PooledDataVector) = FastPerm(o, v)



##############################################################################
##
## PooledDataVecs: EXPLANATION SHOULD GO HERE
##
##############################################################################


function PooledDataVecs{S,Q<:Integer,R<:Integer,N}(v1::PooledDataArray{S,Q,N},
                                                   v2::PooledDataArray{S,R,N})
    pool = sort(unique([v1.pool, v2.pool]))
    sz = length(pool)

    REFTYPE = sz <= typemax(Uint8)  ? Uint8 :
              sz <= typemax(Uint16) ? Uint16 :
              sz <= typemax(Uint32) ? Uint32 :
                                      Uint64

    tidx1 = convert(Vector{REFTYPE}, findat(pool, v1.pool))
    tidx2 = convert(Vector{REFTYPE}, findat(pool, v2.pool))
    refs1 = zeros(REFTYPE, length(v1))
    refs2 = zeros(REFTYPE, length(v2))
    for i in 1:length(refs1)
        if v1.refs[i] != 0
            refs1[i] = tidx1[v1.refs[i]]
        end
    end
    for i in 1:length(refs2)
        if v2.refs[i] != 0
            refs2[i] = tidx2[v2.refs[i]]
        end
    end
    return (PooledDataArray(RefArray(refs1), pool),
            PooledDataArray(RefArray(refs2), pool))
end

function PooledDataVecs{S,R<:Integer,N}(v1::PooledDataArray{S,R,N},
                                        v2::AbstractArray{S,N})
    return PooledDataVecs(v1,
                          PooledDataArray(v2))
end
function PooledDataVecs{S,R<:Integer,N}(v1::PooledDataArray{S,R,N},
                                        v2::AbstractArray{S,N})
    return PooledDataVecs(v1,
                          PooledDataArray(v2))
end

####
function PooledDataVecs{S,R<:Integer,N}(v1::AbstractArray{S,N},
                                        v2::PooledDataArray{S,R,N})
    return PooledDataVecs(PooledDataArray(v1),
                          v2)
end

function PooledDataVecs(v1::AbstractArray,
                        v2::AbstractArray)

    ## Return two PooledDataVecs that share the same pool.

    ## TODO: allow specification of REFTYPE
    refs1 = Array(DEFAULT_POOLED_REF_TYPE, size(v1))
    refs2 = Array(DEFAULT_POOLED_REF_TYPE, size(v2))
    poolref = Dict{promote_type(eltype(v1), eltype(v2)), DEFAULT_POOLED_REF_TYPE}()
    maxref = 0

    # loop through once to fill the poolref dict
    for i = 1:length(v1)
        if !isna(v1[i])
            poolref[v1[i]] = 0
        end
    end
    for i = 1:length(v2)
        if !isna(v2[i])
            poolref[v2[i]] = 0
        end
    end

    # fill positions in poolref
    pool = sort(collect(keys(poolref)))
    i = 1
    for p in pool
        poolref[p] = i
        i += 1
    end

    # fill in newrefs
    zeroval = zero(DEFAULT_POOLED_REF_TYPE)
    for i = 1:length(v1)
        if isna(v1[i])
            refs1[i] = zeroval
        else
            refs1[i] = poolref[v1[i]]
        end
    end
    for i = 1:length(v2)
        if isna(v2[i])
            refs2[i] = zeroval
        else
            refs2[i] = poolref[v2[i]]
        end
    end

    return (PooledDataArray(RefArray(refs1), pool),
            PooledDataArray(RefArray(refs2), pool))
end

Base.convert{S,T,R1<:Integer,R2<:Integer,N}(::Type{PooledDataArray{S,R1,N}}, pda::PooledDataArray{T,R2,N}) =
    PooledDataArray(RefArray(convert(Array{R1,N}, pda.refs)), convert(Vector{S}, pda.pool))
Base.convert{S,T,R<:Integer,N}(::Type{PooledDataArray{S,R,N}}, pda::PooledDataArray{T,R,N}) =
    PooledDataArray(RefArray(copy(pda.refs)), convert(Vector{S}, pda.pool))
Base.convert{T,R<:Integer,N}(::Type{PooledDataArray{T,R,N}}, pda::PooledDataArray{T,R,N}) = pda
Base.convert{S,T,R1<:Integer,R2<:Integer,N}(::Type{PooledDataArray{S,R1}}, pda::PooledDataArray{T,R2,N}) =
    convert(PooledDataArray{S,R1,N}, pda)
Base.convert{S,T,R<:Integer,N}(::Type{PooledDataArray{S}}, pda::PooledDataArray{T,R,N}) =
    convert(PooledDataArray{S,R,N}, pda)
Base.convert{T,R<:Integer,N}(::Type{PooledDataArray}, pda::PooledDataArray{T,R,N}) = pda

Base.convert{T,R<:Integer,N}(::Type{PooledDataArray{T,R,N}}, a::DataArray{T,N}) =
    PooledDataArray(a.data, a.na, R)
Base.convert{S,T,R<:Integer,N}(::Type{PooledDataArray{S,R,N}}, a::DataArray{T,N}) =
    convert(PooledDataArray{S,R,N}, PooledDataArray(a.data, a.na, R))
Base.convert{S,T,R<:Integer,N}(::Type{PooledDataArray{S,R}}, a::DataArray{T,N}) =
    convert(PooledDataArray{S,R,N}, PooledDataArray(a.data, a.na, R))
Base.convert{S,T,N}(::Type{PooledDataArray{S}}, a::DataArray{T,N}) =
    convert(PooledDataArray{S}, PooledDataArray(a.data, a.na))
Base.convert(::Type{PooledDataArray}, a::DataArray) =
    PooledDataArray(a.data, a.na)

Base.convert{S,T,R<:Integer,N}(::Type{PooledDataArray{S,R,N}}, a::AbstractArray{T,N}) =
    PooledDataArray(convert(Array{S,N}, a), falses(size(a)), R)
Base.convert{S,T,R<:Integer,N}(::Type{PooledDataArray{S,R}}, a::AbstractArray{T,N}) =
    PooledDataArray(convert(Array{S,N}, a), falses(size(a)), R)
Base.convert{S,T,N}(::Type{PooledDataArray{S}}, a::AbstractArray{T,N}) =
    PooledDataArray(convert(Array{S,N}, a), falses(size(a)))
Base.convert(::Type{PooledDataArray}, a::AbstractArray) =
    PooledDataArray(a, falses(size(a)))

function Base.convert{S,T,R<:Integer,N}(::Type{DataArray{S,N}},
                                        pda::PooledDataArray{T,R,N})
    res = DataArray(Array(S, size(pda)), BitArray(size(pda)))
    for i in 1:length(pda)
        r = pda.refs[i]
        if r == 0 # TODO: Use zero(R)
            res.na[i] = true
        else
            res.na[i] = false
            res.data[i] = pda.pool[r]
        end
    end
    return res
end
Base.convert{S,T,R<:Integer,N}(::Type{DataArray{S}}, pda::PooledDataArray{T,R,N}) =
    convert(DataArray{S,N}, pda)
Base.convert{T,R<:Integer,N}(::Type{DataArray}, pda::PooledDataArray{T,R,N}) =
    convert(DataArray{T,N}, pda)

pdata(a::AbstractArray) = convert(PooledDataArray, a)

# Turn a PooledDataArray into an Array. Fail on NA
function array{T, R}(da::PooledDataArray{T, R})
    n = length(da)
    res = Array(T, size(da))
    for i in 1:n
        if da.refs[i] == zero(R)
            error(NAException())
        else
            res[i] = da.pool[da.refs[i]]
        end
    end
    return res
end

function array{T, R}(da::PooledDataArray{T, R}, replacement::T)
    n = length(da)
    res = Array(T, size(da))
    for i in 1:n
        if da.refs[i] == zero(R)
            res[i] = replacement
        else
            res[i] = da.pool[da.refs[i]]
        end
    end
    return res
end

function dropna{T}(pdv::PooledDataVector{T})
    n = length(pdv)
    res = Array(T, n)
    total = 0
    for i in 1:n
        if pdv.refs[i] > 0
            total += 1
            res[total] = pdv.pool[pdv.refs[i]]
        end
    end
    return res[1:total]
end
