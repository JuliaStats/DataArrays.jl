##############################################################################
##
## NA's via the NAtype
##
## Inspirations:
##  * R's NA's
##  * Panda's discussion of NA's:
##    http://pandas.pydata.org/pandas-docs/stable/missing_data.html
##  * NumPy's analysis of the issue:
##    https://github.com/numpy/numpy/blob/master/doc/neps/missing-data.rst
##
## NAtype is a composite type representing missingness:
## * An object of NAtype can be generated by writing NA
##
##############################################################################

"""
    NAtype

The type of a missing value, `NA`.
"""
struct NAtype
end

"""
    NA

A value denoting missingness within the domain of any type.
"""
const NA = NAtype()

const Data{T} = Union{T,NAtype}

Base.show(io::IO, x::NAtype) = print(io, "NA")

struct NAException <: Exception
    msg::String
end
NAException() = NAException("NA found")

# Restrict to Number to avoid infinite recursion
# Might be possible to get rid of these restrictions if the promotion in base gets changed.
## Numbers
Base.promote_rule(::Type{Data{T}}, ::Type{Data{S}}) where {T<:Number,S<:Number} =
    Union{promote_type(T, S),NAtype}
Base.promote_rule(::Type{Data{T}}, ::Type{S}) where {T<:Number,S<:Number} =
    Union{promote_type(T, S),NAtype}
## Dates
Base.promote_rule(::Type{Data{T}}, ::Type{Data{S}}) where {T<:Dates.AbstractTime,S<:Dates.AbstractTime} =
    Union{promote_type(T, S),NAtype}
Base.promote_rule(::Type{Data{T}}, ::Type{S}) where {T<:Dates.AbstractTime,S<:Dates.AbstractTime} =
    Union{promote_type(T, S),NAtype}

Base.promote_rule(::Type{NAtype}, ::Type{T}) where {T} = Union{T,NAtype}

# Restrict to Number to avoid maching everything
Base.convert(::Type{Data{T}}, x::Number)             where {T<:Number}             = convert(T, x)
Base.convert(::Type{Data{T}}, x::Dates.AbstractTime) where {T<:Dates.AbstractTime} = convert(T, x)

Base.length(x::NAtype) = 1
Base.size(x::NAtype) = ()
Base.size(x::NAtype, i::Integer) = i < 1 ? throw(BoundsError()) : 1
Base.ndims(x::NAtype) = 0
Base.getindex(x::NAtype, i) = i == 1 ? NA : throw(BoundsError())

# extractT(::Type{Data{T}}) where {T} = T
extractT(::Type{Union{T,NAtype}}) where {T} = T
extractT(::Type{T}) where {T} = T
extractT(::Type{NAtype}) = NAtype

Base.zero(::Type{Data{T}}) where {T} = zero(T)

"""
    isna(x) -> Bool

Determine whether `x` is missing, i.e. `NA`.

# Examples

```jldoctest
julia> isna(1)
false

julia> isna(NA)
true
```
"""
isna(x::NAtype) = true
isna(x::Any) = false

Base.isnan(::NAtype) = NA
