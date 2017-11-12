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
