"""
    AbstractDataArray{T, N}

An `N`-dimensional `AbstractArray` whose entries can take on values of type
`T` or the value `NA`.
"""
abstract type AbstractDataArray{T, N} <: AbstractArray{T, N} end

"""
    AbstractDataVector{T}

A 1-dimensional [`AbstractDataArray`](@ref) with element type `T`.
"""
const AbstractDataVector{T} = AbstractDataArray{T, 1}

"""
    AbstractDataMatrix{T}

A 2-dimensional [`AbstractDataArray`](@ref) with element type `T`.
"""
const AbstractDataMatrix{T} = AbstractDataArray{T, 2}

Base.eltype{T, N}(d::AbstractDataArray{T, N}) = T

# Generic iteration over AbstractDataArray's

Base.start(x::AbstractDataArray) = 1
Base.next(x::AbstractDataArray, state::Integer) = (x[state], state + 1)
Base.done(x::AbstractDataArray, state::Integer) = state > length(x)

Base.broadcast{T}(::typeof(isna), a::AbstractArray{T}) =
    NAtype <: T ? BitArray(map(x->isa(x, NAtype), a)) : falses(size(a)) # -> BitArray

"""
    isna(a::AbstractArray, i) -> Bool

Determine whether the element of `a` at index `i` is missing, i.e. `NA`.

# Examples

```jldoctest
julia> X = @data [1, 2, NA];

julia> isna(X, 2)
false

julia> isna(X, 3)
true
```
"""
isna{T}(a::AbstractArray{T}, i::Real) = NAtype <: T ? isa(a[i], NAtype) : false # -> Bool

"""
    dropna(v::AbstractVector) -> AbstractVector

Return a copy of `v` with all `NA` elements removed.

# Examples

```jldoctest
julia> dropna(@data [NA, 1, NA, 2])
2-element Array{Int64,1}:
 1
 2

julia> dropna([4, 5, 6])
3-element Array{Int64,1}:
 4
 5
 6
```
"""
dropna(v::AbstractVector) = copy(v) # -> AbstractVector

# Iterators
# TODO: Use values()
#       Use DataValueIterator type?

struct EachFailNA{T}
    da::AbstractDataArray{T}
end
each_failna{T}(da::AbstractDataArray{T}) = EachFailNA(da)
Base.length(itr::EachFailNA) = length(itr.da)
Base.start(itr::EachFailNA) = 1
Base.done(itr::EachFailNA, ind::Integer) = ind > length(itr)
function Base.next(itr::EachFailNA, ind::Integer)
    if isna(itr.da[ind])
        throw(NAException())
    else
        (itr.da[ind], ind + 1)
    end
end

struct EachDropNA{T}
    da::AbstractDataArray{T}
end
each_dropna{T}(da::AbstractDataArray{T}) = EachDropNA(da)
function _next_nonna_ind{T}(da::AbstractDataArray{T}, ind::Int)
    ind += 1
    while ind <= length(da) && isna(da, ind)
        ind += 1
    end
    ind
end
Base.length(itr::EachDropNA) = length(itr.da) - sum(itr.da.na)
Base.start(itr::EachDropNA) = _next_nonna_ind(itr.da, 0)
Base.done(itr::EachDropNA, ind::Int) = ind > length(itr.da)
function Base.next(itr::EachDropNA, ind::Int)
    (itr.da[ind], _next_nonna_ind(itr.da, ind))
end

struct EachReplaceNA{S, T}
    da::AbstractDataArray{S}
    replacement::T
end
function each_replacena(da::AbstractDataArray, replacement::Any)
    EachReplaceNA(da, convert(eltype(da), replacement))
end
function each_replacena(replacement::Any)
    x -> each_replacena(x, replacement)
end
Base.length(itr::EachReplaceNA) = length(itr.da)
Base.start(itr::EachReplaceNA) = 1
Base.done(itr::EachReplaceNA, ind::Integer) = ind > length(itr)
function Base.next(itr::EachReplaceNA, ind::Integer)
    item = isna(itr.da, ind) ? itr.replacement : itr.da[ind]
    (item, ind + 1)
end
