"""
    AbstractDataArray{T, N}

An `N`-dimensional `AbstractArray` whose entries can take on values of type
`T` or the value `missing`.
"""
abstract type AbstractDataArray{T, N} <: AbstractArray{Union{T,Missing}, N} end

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

Base.eltype(d::AbstractDataArray{T, N}) where {T, N} = Union{T,Missing}

# Generic iteration over AbstractDataArray's

function Base.iterate(x::AbstractDataArray, st=1)
    st > length(x) && return nothing
    return (x[st], st + 1)
end

# FIXME: type piracy
"""
    ismissing(a::AbstractArray, i) -> Bool

Determine whether the element of `a` at index `i` is missing, i.e. `missing`.

# Examples

```jldoctest
julia> X = @data [1, 2, missing];

julia> ismissing(X, 2)
false

julia> ismissing(X, 3)
true
```
"""
Missings.ismissing(a::AbstractArray{T}, i::Real) where {T} = Missing <: T ? isa(a[i], Missing) : false # -> Bool
