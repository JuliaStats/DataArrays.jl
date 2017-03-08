# This is multiplicative analog of diff
"""
    reldiff(v::Vector) -> Vector

For each element in `v`, compute the relative difference from the previous element.

# Examples

```jldoctest
julia> reldiff([1.0, 2.0, 3.0, 4.0])
3-element Array{Float64,1}:
 2.0
 1.5
 1.33333
```
"""
function reldiff{T}(v::Vector{T})
    n = length(v)
    res = Array(T, n - 1)
    for i in 2:n
        res[i - 1] = v[i] / v[i - 1]
    end
    return res
end

# Diff scaled by previous value
"""
    percent_change(v::Vector) -> Vector

For each element in `v`, compute the percent change from the previous element.

# Examples

```jldoctest
julia> percent_change([1.0, 2.0, 3.0, 4.0])
3-element Array{Float64,1}:
 1.0
 0.5
 0.333333
```
"""
function percent_change{T}(v::Vector{T})
    n = length(v)
    res = Array(T, n - 1)
    for i in 2:n
        res[i - 1] = (v[i] - v[i - 1]) / v[i - 1]
    end
    return res
end

autocor{T}(dv::DataVector{T}, lag::Int) = cor(dv[1:(end - lag)], dv[(1 + lag):end])
autocor{T}(dv::DataVector{T}) = autocor(dv, 1)

"""
    gl(n::Integer, k::Integer, l::Integer = n*k) -> PooledDataArray

Generate a [`PooledDataArray`](@ref) with `n` levels and `k` replications, optionally
specifying an output length `l`. If specified, `l` must be a multiple of `n*k`.

# Examples

```jldoctest
julia> gl(2, 1)
2-element DataArrays.PooledDataArray{Int64,UInt8,1}:
 1
 2

julia> gl(2, 1, 4)
4-element DataArrays.PooledDataArray{Int64,UInt8,1}:
 1
 2
 1
 2
```
"""
function gl(n::Integer, k::Integer, l::Integer)
    nk = n * k
    d, r = divrem(l, nk)
    r == 0 || throw(ArgumentError("length out must be a multiple of n * k"))
    aa = Array(Int, l)
    for j = 0:(d - 1), i = 1:n
        aa[j * nk + (i - 1) * k + (1:k)] = i
    end
    compact(PooledDataArray(aa))
end

gl(n::Integer, k::Integer) = gl(n, k, n*k)

"""
    xtab(x::AbstractArray) -> xtab

Construct a cross-tabulation table from the unique values in `x`.
Currently only one-way tables are supported. Returns an `xtab` object.
"""
type xtab{T}
    vals::Array{T}
    counts::Vector{Int}
end

function xtab{T}(x::AbstractArray{T})
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

"""
    xtabs(x::AbstractArray) -> Dict

Construct a cross-tabulation table from the unique values in `x`,
returning a `Dict`. Currently only one-way tables are supported.
"""
function xtabs{T}(x::AbstractArray{T})
    d = Dict{T, Int}()
    for el in x
        d[el] = get(d, el, 0) + 1
    end
    return d
end
