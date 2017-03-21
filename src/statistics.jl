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
