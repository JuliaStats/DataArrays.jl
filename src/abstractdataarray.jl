"""
    AbstractDataArray{T, N}

An `N`-dimensional `AbstractArray` whose entries can take on values of type
`T` or the value `null`.
"""
abstract type AbstractDataArray{T, N} <: AbstractArray{Union{T,Null}, N} end

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

Base.eltype(d::AbstractDataArray{T, N}) where {T, N} = Union{T,Null}

# Generic iteration over AbstractDataArray's

Base.start(x::AbstractDataArray) = 1
Base.next(x::AbstractDataArray, state::Integer) = (x[state], state + 1)
Base.done(x::AbstractDataArray, state::Integer) = state > length(x)

# FIXME: type piracy
"""
    isnull(a::AbstractArray, i) -> Bool

Determine whether the element of `a` at index `i` is missing, i.e. `null`.

# Examples

```jldoctest
julia> X = @data [1, 2, null];

julia> isnull(X, 2)
false

julia> isnull(X, 3)
true
```
"""
Base.isnull(a::AbstractArray{T}, i::Real) where {T} = Null <: T ? isa(a[i], Null) : false # -> Bool

# Iterators
# TODO: Use values()
#       Use DataValueIterator type?

struct EachFailNull{T<:AbstractDataArray}
    da::T
end
Nulls.fail(da::AbstractDataArray) = EachFailNull(da)
Base.length(itr::EachFailNull) = length(itr.da)
Base.start(itr::EachFailNull) = 1
Base.done(itr::EachFailNull, ind::Integer) = ind > length(itr)
Base.eltype(itr::EachFailNull) = Nulls.T(eltype(itr.da))
function Base.next(itr::EachFailNull, ind::Integer)
    if itr.da.na[ind]
        throw(NullException())
    else
        (itr.da.data[ind], ind + 1)
    end
end

struct EachDropNull{T<:AbstractDataArray}
    da::T
end
Nulls.skip(da::AbstractDataArray) = EachDropNull(da)
function _next_nonna_ind(da::AbstractDataArray, ind::Int)
    ind += 1
    @inbounds while ind <= length(da) && da.na[ind]
        ind += 1
    end
    ind
end
Base.length(itr::EachDropNull) = length(itr.da) - sum(itr.da.na)
Base.start(itr::EachDropNull) = _next_nonna_ind(itr.da, 0)
Base.done(itr::EachDropNull, ind::Int) = ind > length(itr.da)
Base.eltype(itr::EachDropNull) = Nulls.T(eltype(itr.da))
function Base.next(itr::EachDropNull, ind::Int)
    (itr.da.data[ind], _next_nonna_ind(itr.da, ind))
end

struct EachReplaceNull{S<:AbstractDataArray, T}
    da::S
    replacement::T
end
Nulls.replace(da::AbstractDataArray, replacement::Any) =
    EachReplaceNull(da, replacement)
Base.length(itr::EachReplaceNull) = length(itr.da)
Base.start(itr::EachReplaceNull) = 1
Base.done(itr::EachReplaceNull, ind::Integer) = ind > length(itr)
Base.eltype(itr::EachReplaceNull) = Nulls.T(eltype(itr.da))
function Base.next(itr::EachReplaceNull, ind::Integer)
    item = itr.da.na[ind] ? itr.replacement : itr.da.data[ind]
    (item, ind + 1)
end
