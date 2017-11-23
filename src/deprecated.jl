using Base: @deprecate, @deprecate_binding, depwarn

# Deprecate in Julia 0.6 cycle
function Base.isnan(da::DataArray)
    depwarn("vectorized method isnan(da) is deprecated, use isnan.(da) instead", :isnan)
    return isnan.(da)
end

@deprecate isna(x::AbstractArray) ismissing.(x)
@deprecate anyna(x) any(ismissing, x)
@deprecate allna(x) all(ismissing, x)

function reldiff(v::Vector{T}) where T
    depwarn("reldiff is deprecated.", :reldiff)
    n = length(v)
    res = Array(T, n - 1)
    for i in 2:n
        res[i - 1] = v[i] / v[i - 1]
    end
    return res
end

function percent_change(v::Vector{T}) where T
    depwarn("percent_change is deprecated.", :percent_change)
    n = length(v)
    res = Array(T, n - 1)
    for i in 2:n
        res[i - 1] = (v[i] - v[i - 1]) / v[i - 1]
    end
    return res
end

mutable struct xtab{T}
    vals::Array{T}
    counts::Vector{Int}
end

function xtab(x::AbstractArray{T}) where T
    depwarn("xtab is deprecated; use the FreqTables package instead.", :xtab)
    d = Dict{T, Int}()
    for el in x
        d[el] = get(d, el, 0) + 1
    end
    kk = sort(keys(d))
    cc = Array(Int, length(kk))
    for i in 1:length(kk)
        cc[i] = d[kk[i]]
    end
    return xtab(kk, cc)
end

function xtabs(x::AbstractArray{T}) where T
    depwarn("xtabs is deprecated; use the FreqTables package instead.", :xtabs)
    d = Dict{T, Int}()
    for el in x
        d[el] = get(d, el, 0) + 1
    end
    return d
end

# Implicitly vectorized bitwise operations

import Base: &, |, xor

for f in [:(&), :(|), :(xor)]
    for T in [:(BitArray), :(Range{<:Integer}), :(AbstractArray{<:Integer}), :(Integer)]
        @eval begin
            @deprecate ($f)(a::DataArray{<:Integer}, b::$T) ($f).(a, b)
            @deprecate ($f)(a::$T, b::DataArray{<:Integer}) ($f).(a, b)
        end
    end
    @eval begin
        @deprecate ($f)(a::DataArray{<:Integer}, b::DataArray{<:Integer}) ($f).(a, b)
    end
end

@deprecate_binding NAtype Missing
@deprecate_binding NA missing
@deprecate_binding NAException MissingException
@deprecate isna ismissing
@deprecate dropna(x) collect(skipmissing(x))
@deprecate padna padmissing
@deprecate each_failna Missings.fail
@deprecate each_dropna skipmissing
@deprecate each_replacena Missings.replace
@deprecate_binding EachFailNA DataArrays.EachFailMissing
@deprecate_binding EachDropNA DataArrays.EachDropMissing
@deprecate_binding EachReplaceNA DataArrays.EachReplaceMissing
import SpecialFunctions: digamma, erf, erfc
@deprecate digamma(x::Missing) ismissing(x) ? missing : SpecialFunctions.digamma(x)
@deprecate erf(x::Missing) ismissing(x) ? missing : SpecialFunctions.erf(x)
@deprecate erfc(x::Missing) ismissing(x) ? missing : SpecialFunctions.erfc(x)
