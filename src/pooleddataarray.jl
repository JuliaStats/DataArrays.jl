##############################################################################
##
## PooledDataArray type definition
##
## An AbstractDataArray with efficient storage when values are repeated. A
## PDA wraps an array of unsigned integers, which are used to index into a
## compressed pool of values. missings are 0's in the refs array.  The unsigned
## integer type used for the refs array defaults to UInt32.  The `compact`
## function converts to a smallest integer size that will index the entire pool.
##
## TODO: Allow ordering of factor levels
## TODO: Add metadata for dummy conversion
##
##############################################################################

# This is used as a wrapper during PooledDataArray construction only, to distinguish
# arrays of pool indices from normal arrays
struct RefArray{R<:Integer,N}
    a::Array{R,N}
end

"""
    PooledDataArray(data::AbstractArray{T}, [pool::Vector{T}], [m::AbstractArray{Bool}], [r::Type])

Construct a `PooledDataArray` based on the unique values in the given array. `PooledDataArray`s are
useful for efficient storage of categorical data with a limited set of unique values. Rather than
storing all `length(data)` values, it stores a smaller set of values (typically `unique(data)`) and
an array of references to the stored values.

### Optional arguments

* `pool`: The possible values of `data`. Defaults to `unique(data)`.
* `m`: A missingness indicator akin to that of [`DataArray`](@ref). Defaults to `falses(size(d))`.
* `r`: The integer subtype used to store pool references. Defaults to `$DEFAULT_POOLED_REF_TYPE`.

# Examples

```jldoctest
julia> d = repeat(["A", "B"], outer=4);

julia> p = PooledDataArray(d)
8-element DataArrays.PooledDataArray{String,UInt32,1}:
 "A"
 "B"
 "A"
 "B"
 "A"
 "B"
 "A"
 "B"
```

    PooledDataArray(T::Type, [R::Type=$DEFAULT_POOLED_REF_TYPE], [dims...])

Construct a `PooledDataArray` with element type `T`, reference storage type `R`, and dimensions
`dims`. If the dimensions are specified and nonzero, the array is filled with `missing` values.

# Examples

```jldoctest
julia> PooledDataArray(Int, 2, 2)
2×2 DataArrays.PooledDataArray{Int64,UInt32,2}:
 missing  missing
 missing  missing
```
"""
mutable struct PooledDataArray{T, R<:Integer, N} <: AbstractDataArray{T, N}
    refs::Array{R, N}
    pool::Vector{T}

    function PooledDataArray{T,R,N}(rs::RefArray{R, N}, p::Vector{T}) where {T,R,N}
        Base.depwarn("PooledDataArray is deprecated, use CategoricalArray " *
                     "from the CategoricalArrays package or PooledArray " *
                     "from the PooledArrays package instead.", :PooledDataArray)
        # refs mustn't overflow pool
        if length(rs.a) > 0 && maximum(rs.a) > prod(size(p))
            throw(ArgumentError("Reference array points beyond the end of the pool"))
        end
        new(rs.a,p)
    end
end
const PooledDataVector{T,R} = PooledDataArray{T,R,1}
const PooledDataMatrix{T,R} = PooledDataArray{T,R,2}

##############################################################################
##
## PooledDataArray constructors
##
# How do you construct a PooledDataArray from an Array?
# From the same sigs as a DataArray!
#
# Algorithm:
# * Start with:
#   * A missing pool
#   * A pre-allocated refs
#   * A hash from T to Int
# * Iterate over d
#   * If value of d in pool already, set the refs accordingly
#   * If value is new, add it to the pool, then set refs
##############################################################################

# Echo inner constructor as an outer constructor
function PooledDataArray(refs::RefArray{R, N},
                         pool::Vector{T}) where {T,R<:Integer,N}
    PooledDataArray{T,R,N}(refs, pool)
end

# A no-op constructor
PooledDataArray(d::PooledDataArray) = d

# Constructor from array, w/ pool, missingness, and ref type
function PooledDataArray(d::AbstractArray{<:Union{T,Missing}, N},
                         pool::Vector{T},
                         m::AbstractArray{<:Union{Bool,Missing}, N},
                         r::Type{R} = DEFAULT_POOLED_REF_TYPE) where {T,R<:Integer,N}
    if length(pool) > typemax(R)
        throw(ArgumentError("Cannot construct a PooledDataVector with type $R with a pool of size $(length(pool))"))
    end

    newrefs = Array{R,N}(size(d))
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
function PooledDataArray(d::AbstractArray{T, N},
                         m::AbstractArray{Bool, N},
                         r::Type{R} = DEFAULT_POOLED_REF_TYPE) where {T,R<:Integer,N}
    pool = convert(Vector{T}, unique(d[.!m]))
    if method_exists(isless, (T, T))
        sort!(pool)
    end
    PooledDataArray(d, pool, m, r)
end

# Construct an all-missing PooledDataVector of a specific type
PooledDataArray(t::Type, dims::Tuple{Vararg{Int}}) = PooledDataArray(Array{t}(dims), trues(dims))
PooledDataArray(t::Type, dims::Int...) = PooledDataArray(Array{t}(dims), trues(dims))
PooledDataArray(t::Type, r::Type{R}, dims::Tuple{Vararg{Int}}) where {R<:Integer} = PooledDataArray(Array{t}(dims), trues(dims), r)
PooledDataArray(t::Type, r::Type{R}, dims::Int...) where {R<:Integer} = PooledDataArray(Array(t, dims), trues(dims), r)

# Construct an empty PooledDataVector of a specific type
PooledDataArray(t::Type) = PooledDataArray(similar(Vector{t}(1),0), trues(0))
PooledDataArray(t::Type, r::Type{R}) where {R<:Integer} = PooledDataArray(similar(Vector{t}(1),0), trues(0), r)

# Convert a BitArray to an Array{Bool} (m = missingness)
# For some reason an additional method is needed but even that doesn't work
# For a BitArray a refs type of UInt8 will always be sufficient as the size of the pool is 0, 1 or 2
PooledDataArray(d::BitArray{N}) where {N} = PooledDataArray(Array(d), falses(size(d)), UInt8)
PooledDataArray(d::BitArray{N}, m::AbstractArray{Bool, N}) where {N} = PooledDataArray(Array(d), m, UInt8)

# Convert a DataArray to a PooledDataArray
PooledDataArray(da::DataArray{T},
                r::Type{R} = DEFAULT_POOLED_REF_TYPE) where {T,R<:Integer} = PooledDataArray(da.data, da.na, r)
PooledDataArray(da::DataArray{T},
                pool::Vector{T},
                r::Type{R} = DEFAULT_POOLED_REF_TYPE) where {T,R<:Integer} = PooledDataArray(da.data, pool, da.na, r)

# Convert a Array{T} to a PooledDataArray
PooledDataArray(d::Array{T},
                r::Type{R} = DEFAULT_POOLED_REF_TYPE) where {T,R<:Integer} = PooledDataArray(d, falses(size(d)), r)
PooledDataArray(d::Array{T},
                pool::Vector{T},
                r::Type{R} = DEFAULT_POOLED_REF_TYPE) where {T,R<:Integer} = PooledDataArray(d, pool, falses(size(d)), r)

# Explicitly convert Ranges into a PooledDataVector
PooledDataArray(rs::Range,
                r::Type{R} = DEFAULT_POOLED_REF_TYPE) where {R<:Integer} = PooledDataArray(collect(rs), falses(length(rs)), r)

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

function Base.resize!(pda::PooledDataArray{T,R,1}, n::Int) where {T,R}
    oldn = length(pda.refs)
    resize!(pda.refs, n)
    pda.refs[oldn+1:n] = zero(R)
    pda
end

##############################################################################
##
## Predicates, including the new ismissing()
##
##############################################################################

function Base.isnan(pda::PooledDataArray)
    PooledDataArray(RefArray(copy(pda.refs)), isnan(pda.pool))
end

function Base.isfinite(pda::PooledDataArray)
    PooledDataArray(RefArray(copy(pda.refs)), isfinite(pda.pool))
end

Base.broadcast(::typeof(ismissing), pda::PooledDataArray) = pda.refs .== 0
Missings.ismissing(pda::PooledDataArray, i::Real) = pda.refs[i] == 0 # -> Bool

function Base.any(::typeof(ismissing), pda::PooledDataArray)
    for ref in pda.refs
        ref == 0 && return true
    end
    return false
end

function Base.all(::typeof(ismissing), pda::PooledDataArray)
    for ref in pda.refs
        ref == 0 || return false
    end
    return true
end

##############################################################################
##
## PooledDataArray utilities
##
## TODO: Add methods with these names for DataArray's
##
##############################################################################

function compactreftype(sz)
    REFTYPE = sz <= typemax(UInt8)  ? UInt8 :
              sz <= typemax(UInt16) ? UInt16 :
              sz <= typemax(UInt32) ? UInt32 :
                                      UInt64
end

"""
    compact(d::PooledDataArray)

Return a [`PooledDataArray`](@ref) with the smallest possible reference type for the
data in `d`.

!!! note

    If the reference type is already the smallest possible for the data, the input
    array is returned, i.e. the function *aliases* the input.

# Examples

```jldoctest
julia> p = @pdata(repeat(["A", "B"], outer=4))
8-element DataArrays.PooledDataArray{String,UInt32,1}:
 "A"
 "B"
 "A"
 "B"
 "A"
 "B"
 "A"
 "B"

julia> compact(p) # second type parameter compacts to UInt8 (only need 2 unique values)
8-element DataArrays.PooledDataArray{String,UInt8,1}:
 "A"
 "B"
 "A"
 "B"
 "A"
 "B"
 "A"
 "B"
```
"""
function compact(d::PooledDataArray{T,R,N}) where {T,R<:Integer,N}
    REFTYPE = compactreftype(length(d.pool))

    if REFTYPE == R
        return d
    end

    newrefs = convert(Array{REFTYPE, N}, d.refs)
    PooledDataArray(RefArray(newrefs), d.pool)
end

function Base.unique(pda::PooledDataArray{T}) where T
    n = length(pda)
    nlevels = length(pda.pool)
    unique_values = Vector{T}(0)
    sizehint!(unique_values, nlevels)
    seen = Set{eltype(pda.refs)}()

    firstmissing = 0
    for i in 1:n
        if ismissing(pda, i)
            if firstmissing == 0
                firstmissing = length(unique_values) + 1
            end
        elseif !in(pda.refs[i], seen)
            push!(seen, pda.refs[i])
            push!(unique_values, pda.pool[pda.refs[i]])
        else
            continue
        end

        if firstmissing > 0 && length(unique_values) == nlevels
            break
        end
    end

    if firstmissing > 0
        n = length(unique_values)
        res = DataArray(Vector{T}(n + 1))
        i = 0
        for val in unique_values
            i += 1
            if i == firstmissing
                res.na[i] = true
                i += 1
            end
            res.data[i] = val
        end

        if firstmissing == n + 1
            res.na[n + 1] = true
        end

        return res
    else
        return DataArray(unique_values)
    end
end

Missings.levels(pda::PooledDataArray{T}) where {T} = copy(pda.pool)

function PooledDataArray(x::PooledDataArray{S,R,N},
                         newpool::Vector{S}) where {S,R,N}
    # QUESTION: should we have a ! version of this? If so, needs renaming?
    tidx::Array{R} = indexin(x.pool, newpool)
    refs = zeros(R, length(x))
    for i in 1:length(refs)
        if x.refs[i] != 0
            refs[i] = tidx[x.refs[i]]
        end
    end
    PooledDataArray(RefArray(refs), newpool)
end

myunique(x::AbstractVector) = unique(x)
myunique(x::AbstractDataVector) = unique(skipmissing(x))

"""
    setlevels(x::PooledDataArray, newpool::Union{AbstractVector, Dict})

Create a new [`PooledDataArray`](@ref) based on `x` but with the new value pool
specified by `newpool`. The values can be replaced using a mapping specified in a
`Dict` or with an array, since the order of the levels is used to identify values.
The pool can be enlarged to contain values not present in the data, but it cannot
be reduced to exclude present values.

# Examples

```jldoctest
julia> p = @pdata repeat(["A", "B"], inner=3)
6-element DataArrays.PooledDataArray{String,UInt32,1}:
 "A"
 "A"
 "A"
 "B"
 "B"
 "B"

julia> p2 = setlevels(p, ["C", "D"]) # could also be Dict("A"=>"C", "B"=>"D")
6-element DataArrays.PooledDataArray{String,UInt32,1}:
 "C"
 "C"
 "C"
 "D"
 "D"
 "D"

julia> p3 = setlevels(p2, ["C", "D", "E"])
6-element DataArrays.PooledDataArray{String,UInt32,1}:
 "C"
 "C"
 "C"
 "D"
 "D"
 "D"

julia> p3.pool # the pool can contain values not in the array
3-element Array{String,1}:
 "C"
 "D"
 "E"
```
"""
function setlevels(x::PooledDataArray{T,R}, newpool::AbstractVector) where {T,R}
    pool = myunique(newpool)
    refs = zeros(R, length(x))
    tidx::Array{R} = indexin(newpool, pool)
    tidx[ismissing.(newpool)] = 0
    for i in 1:length(refs)
        if x.refs[i] != 0
            refs[i] = tidx[x.refs[i]]
        end
    end
    return PooledDataArray(RefArray(refs), pool)
end

"""
    setlevels!(x::PooledDataArray, newpool::Union{AbstractVector, Dict})

Set the value pool for the [`PooledDataArray`](@ref) `x` to `newpool`, modifying
`x` in place. The values can be replaced using a mapping specified in a `Dict` or
with an array, since the order of the levels is used to identify values. The pool
can be enlarged to contain values not present in the data, but it cannot be reduced
to exclude present values.

# Examples

```jldoctest
julia> p = @pdata repeat(["A", "B"], inner=3)
6-element DataArrays.PooledDataArray{String,UInt32,1}:
 "A"
 "A"
 "A"
 "B"
 "B"
 "B"

julia> setlevels!(p, Dict("A"=>"C"));

julia> p # has been modified
6-element DataArrays.PooledDataArray{String,UInt32,1}:
 "C"
 "C"
 "C"
 "B"
 "B"
 "B"
```
"""
function setlevels!(x::PooledDataArray{T,R}, newpool::AbstractVector) where {T,R}
    if newpool == myunique(newpool) # no missings or duplicates
        x.pool = newpool
        return x
    else
        x.pool = myunique(newpool)
        tidx::Array{R} = indexin(newpool, x.pool)
        tidx[ismissing.(newpool)] = 0
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
    # An missing in `v` is put in the pool; that will cause it to become missing
    for (k,v) in d
        idx = findin(newpool, [k])
        if length(idx) == 1
            newpool[idx[1]] = v
        end
    end
    setlevels(x, newpool)
end

function setlevels!(x::PooledDataArray{T,R}, d::Dict{T,T}) where {T,R}
    for (k,v) in d
        idx = findin(x.pool, [k])
        if length(idx) == 1
            x.pool[idx[1]] = v
        end
    end
    x
end

function setlevels!(x::PooledDataArray{T,R}, d::Dict{T,Any}) where {T,R} # this version handles missings in d's values
    newpool = copy(DataArray(x.pool))
    # An missing in `v` is put in the pool; that will cause it to become missing
    for (k,v) in d
        idx = findin(newpool, [k])
        if length(idx) == 1
            newpool[idx[1]] = v
        end
    end
    setlevels!(x, newpool)
end

"""
    reorder(x::PooledDataArray) -> PooledDataArray

Return a `PooledDataArray` containing the same data as `x` but with the value pool sorted.
"""
reorder(x::PooledDataArray) = PooledDataArray(x, sort(levels(x)))  # just re-sort the pool

### FIXME: this calls a method that doesn't exist
# reorder(x::PooledDataArray, y::AbstractVector...) = reorder(mean, x, y...)

### FIXME: this can't work because we don't know about DataFrames
# reorder(fun::Function, x::PooledDataArray, y::AbstractVector...) =
#     reorder(fun, x, DataFrame({y...}))

Base.reverse(x::PooledDataArray) = PooledDataArray(RefArray(reverse(x.refs)), x.pool)

function Base.permute!!(x::PooledDataArray, p::AbstractVector{T}) where T<:Integer
    Base.permute!!(x.refs, p)
    x
end

function Base.ipermute!!(x::PooledDataArray, p::AbstractVector{T}) where T<:Integer
    Base.ipermute!!(x.refs, p)
    x
end

##############################################################################
##
## similar()
##
##############################################################################


function Base.similar(pda::PooledDataArray{T,R}, S::Type, dims::Dims) where {T,R}
    PooledDataArray(RefArray(zeros(R, dims)), Missings.T(S)[])
end

##############################################################################
##
## find()
##
##############################################################################

Base.find(pdv::PooledDataVector{Bool}) = find(convert(Vector{Bool}, pdv, false))

##############################################################################
##
## setindex!() definitions
##
##############################################################################

"""
    getpoolidx(pda::PooledDataArray, val)

Return the index of `val` in the value pool for `pda`. If `val` is not already
in the value pool, `pda` is modified to include it in the pool.
"""
function getpoolidx(pda::PooledDataArray{T,R}, val::Any) where {T,R}
    val::T = convert(T,val)
    pool_idx = findfirst(pda.pool, val)
    if pool_idx <= 0
        push!(pda.pool, val)
        pool_idx = length(pda.pool)
        if pool_idx > typemax(R)
            throw(ArgumentError(
                "You're using a PooledDataArray with ref type $R,\n" *
                "which can only hold $(Int(typemax(R))) values, and you just tried to add the $(Int(typemax(R))+1)th\n" *
                "reference. Please change the ref type to a larger int type, or use the\n" *
                "default ref type ($DEFAULT_POOLED_REF_TYPE)."
            ))
        end
    end
    return pool_idx
end

getpoolidx(pda::PooledDataArray{T,R}, val::Missing) where {T,R} = zero(R)

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
#     invoke(show, Tuple{Any,AbstractArray}, io, pda)
#     print(io, "\nlevels: ")
#     print(io, levels(pda))
# end

##############################################################################
##
## Replacement operations
##
##############################################################################

"""
    replace!(x::PooledDataArray, from, to)

Replace all occurrences of `from` in `x` with `to`, modifying `x` in place.
"""
function replace!(x::PooledDataArray{Missing}, fromval::Missing, toval::Missing)
    missing # no-op to deal with warning
end
function replace!(x::PooledDataArray, fromval::Missing, toval::Missing)
    missing # no-op to deal with warning
end
function replace!(x::PooledDataArray{S}, fromval::T, toval::Missing) where {S, T}
    fromidx = findfirst(x.pool, fromval)
    if fromidx == 0
        throw(ErrorException("can't replace a value not in the pool in a PooledDataVector!"))
    end

    x.refs[x.refs .== fromidx] = 0

    return missing
end
function replace!(x::PooledDataArray{S}, fromval::Missing, toval::T) where {S, T}
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
function replace!(x::PooledDataArray{R}, fromval::S, toval::T) where {R, S, T}
    # throw error if fromval isn't in the pool
    fromidx = findfirst(x.pool, fromval)
    if fromidx == 0
        throw(ErrorException("can't replace a value not in the pool in a PooledDataArray!"))
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

function Base.sortperm(pda::PooledDataArray; alg::Base.Sort.Algorithm=Base.Sort.DEFAULT_UNSTABLE,
                       lt::Function=isless, by::Function=identity,
                       rev::Bool=false, order=Base.Sort.Forward)
    order = Base.ord(lt, by, rev, order)

    # TODO handle custom ordering efficiently
    if !isa(order, Base.Order.ForwardOrdering) && !isa(order, Base.Order.ReverseOrdering)
        return sort!(collect(1:length(pda)), alg, Base.Order.Perm(order,pda))
    end

    # TODO handle non-sorted keys without copying
    perm = issorted(pda.pool) ? groupsort_indexer(pda, true)[1] : sortperm(reorder(pda))
    isa(order, Base.Order.ReverseOrdering) && reverse!(perm)
    perm
end

Base.sort(pda::PooledDataArray; kw...) = pda[sortperm(pda; kw...)]

struct FastPerm{O<:Base.Sort.Ordering,V<:AbstractVector} <: Base.Sort.Ordering
    ord::O
    vec::V
end
Base.sortperm(x::AbstractVector, a::Base.Sort.Algorithm, o::FastPerm{Base.Sort.ForwardOrdering,V}) where {V} = x[sortperm(o.vec)]
Base.sortperm(x::AbstractVector, a::Base.Sort.Algorithm, o::FastPerm{Base.Sort.ReverseOrdering,V}) where {V} = x[reverse(sortperm(o.vec))]
Perm(o::O, v::PooledDataVector) where {O<:Base.Sort.Ordering} = FastPerm(o, v)



##############################################################################
##
## PooledDataVecs: EXPLAmissingTION SHOULD GO HERE
##
##############################################################################

"""
    PooledDataVecs(v1, v2) -> (pda1, pda2)

Return a tuple of `PooledDataArray`s created from the data in `v1` and `v2`,
respectively, but sharing a common value pool.
"""
function PooledDataVecs(v1::PooledDataArray{S,Q,N},
                        v2::PooledDataArray{S,R,N}) where {S,Q<:Integer,R<:Integer,N}
    pool = sort(unique([v1.pool; v2.pool]))
    REFTYPE = compactreftype(length(pool))

    tidx1 = convert(Vector{REFTYPE}, indexin(v1.pool, pool))
    tidx2 = convert(Vector{REFTYPE}, indexin(v2.pool, pool))
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

function PooledDataVecs(v1::PooledDataArray{S,R,N},
                        v2::AbstractArray{S,N}) where {S,R<:Integer,N}
    return PooledDataVecs(v1,
                          PooledDataArray(v2))
end

####
function PooledDataVecs(v1::AbstractArray{S,N},
                        v2::PooledDataArray{S,R,N}) where {S,R<:Integer,N}
    return PooledDataVecs(PooledDataArray(v1),
                          v2)
end

function PooledDataVecs(v1::AbstractArray,
                        v2::AbstractArray)

    ## Return two PooledDataVecs that share the same pool.

    ## TODO: allow specification of REFTYPE
    refs1 = Array{DEFAULT_POOLED_REF_TYPE}(size(v1))
    refs2 = Array{DEFAULT_POOLED_REF_TYPE}(size(v2))
    poolref = Dict{promote_type(eltype(v1), eltype(v2)), DEFAULT_POOLED_REF_TYPE}()
    maxref = 0

    # loop through once to fill the poolref dict
    for i = 1:length(v1)
        if !ismissing(v1[i])
            poolref[v1[i]] = 0
        end
    end
    for i = 1:length(v2)
        if !ismissing(v2[i])
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
        if ismissing(v1[i])
            refs1[i] = zeroval
        else
            refs1[i] = poolref[v1[i]]
        end
    end
    for i = 1:length(v2)
        if ismissing(v2[i])
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
    res = DataArray(Array{S}(size(pda)), BitArray(size(pda)))
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

function Base.convert{S, T, R, N}(
    ::Type{Array{S, N}},
    pda::PooledDataArray{T, R, N})

    res = Array{S}(size(pda))
    for i in 1:length(pda)
        if pda.refs[i] == zero(R)
            throw(MissingException("cannot convert PooledDataArray with missing values to $(typeof(res))"))
        else
            res[i] = pda.pool[pda.refs[i]]
        end
    end
    return res
end

function Base.convert{T, R}(::Type{Vector}, pdv::PooledDataVector{T, R})
    return convert(Array{T, 1}, pdv)
end

function Base.convert{T, R}(::Type{Matrix}, pdm::PooledDataMatrix{T, R})
    return convert(Array{T, 2}, pdm)
end

function Base.convert{T, R, N}(::Type{Array}, pda::PooledDataArray{T, R, N})
    return convert(Array{T, N}, pda)
end

function Base.convert{S, T, R, N}(
    ::Type{Array{S, N}},
    pda::PooledDataArray{T, R, N},
    replacement::Any)

    res = Array{S}(size(pda))
    replacementS = convert(S, replacement)
    for i in 1:length(pda)
        if pda.refs[i] == zero(R)
            res[i] = replacementS
        else
            res[i] = pda.pool[pda.refs[i]]
        end
    end
    return res
end

function Base.convert{T, R}(::Type{Vector}, pdv::PooledDataVector{T, R}, replacement::Any)
    return convert(Array{T, 1}, pdv, replacement)
end

function Base.convert{T, R}(::Type{Matrix}, pdm::PooledDataMatrix{T, R}, replacement::Any)
    return convert(Array{T, 2}, pdm, replacement)
end

function Base.convert{T, R, N}(::Type{Array}, pda::PooledDataArray{T, R, N}, replacement::Any)
    return convert(Array{T, N}, pda, replacement)
end

function Base.collect(itr::EachDropMissing{<:PooledDataVector{T}}) where T
    pdv = itr.da
    n = length(pdv)
    res = Array{T}(n)
    total = 0
    for i in 1:n
        if pdv.refs[i] > 0
            total += 1
            res[total] = pdv.pool[pdv.refs[i]]
        end
    end
    resize!(res, total)
    return res
end

function Base.vcat(p1::PooledDataArray{T,R,N}, p2::PooledDataArray...) where {T,R,N}
    pa = (p1, p2...)
    pool = unique(T[[p.pool for p in pa]...;])

    idx = [indexin(p.pool, pool)[p.refs] for p in pa]

    refs = Array{DEFAULT_POOLED_REF_TYPE,N}([idx...;])
    PooledDataArray(RefArray(refs), pool)
end
