using Base: @deprecate, @deprecate_binding, depwarn

# Deprecate in Julia 0.6 cycle
function Base.isnan(da::DataArray)
    depwarn("vectorized method isnan(da) is deprecated, use isnan.(da) instead.", :isnan)
    return isnan.(da)
end
@deprecate isna(da::AbstractArray) isna.(da)

@deprecate allna(x) all(isna, x)
@deprecate anyna(x) any(isna, x)

@deprecate_binding padNA padna

@deprecate_binding NAtype NAType

# Old-style array constructors
@deprecate DataArray{T,N}(::Type{T}, dims::NTuple{N,Int}) DataArray{T,N}(dims)
@deprecate DataArray{T}(::Type{T}, dims::Integer...)      DataArray{T}(dims...)

function reldiff{T}(v::Vector{T})
    depwarn("reldiff is deprecated.", :reldiff)
    n = length(v)
    res = Vector{T}(n - 1)
    for i in 2:n
        res[i - 1] = v[i] / v[i - 1]
    end
    return res
end

function percent_change{T}(v::Vector{T})
    depwarn("percent_change is deprecated.", :percent_change)
    n = length(v)
    res = Vector{T}(n - 1)
    for i in 2:n
        res[i - 1] = (v[i] - v[i - 1]) / v[i - 1]
    end
    return res
end

type xtab{T}
    vals::Array{T}
    counts::Vector{Int}
end

function xtab{T}(x::AbstractArray{T})
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

function xtabs{T}(x::AbstractArray{T})
    depwarn("xtabs is deprecated; use the FreqTables package instead.", :xtabs)
    d = Dict{T, Int}()
    for el in x
        d[el] = get(d, el, 0) + 1
    end
    return d
end
