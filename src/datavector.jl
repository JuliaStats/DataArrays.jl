# Container operations

# TODO: Macroize these definitions

function Base.push!(dv::DataVector, v::Missing)
    resize!(dv.data, length(dv.data) + 1)
    push!(dv.na, true)
    return v
end

function Base.push!(dv::DataVector{S}, v::T) where {S, T}
    push!(dv.data, v)
    push!(dv.na, false)
    return v
end

function Base.pop!(dv::DataVector)
    d, m = pop!(dv.data), pop!(dv.na)
    if m
        return missing
    else
        return d
    end
end

function Base.unshift!(dv::DataVector{T}, v::Missing) where T
    ccall(:jl_array_grow_beg, Void, (Any, UInt), dv.data, 1)
    unshift!(dv.na, true)
    return v
end

function Base.unshift!(dv::DataVector{S}, v::T) where {S, T}
    unshift!(dv.data, v)
    unshift!(dv.na, false)
    return v
end

function Base.shift!(dv::DataVector{T}) where T
    d, m = shift!(dv.data), shift!(dv.na)
    if m
        return missing
    else
        return d
    end
end

function Base.splice!(dv::DataVector, inds::Union{Integer, UnitRange{Int}})
    v = dv[inds]
    deleteat!(dv.data, inds)
    deleteat!(dv.na, inds)
    v
end

function Base.splice!(dv::DataVector, inds::Union{Integer, UnitRange{Int}}, ins::AbstractVector)
    # We cannot merely use the implementation in Base because this
    # needs to handle missing in the replacement vector
    v = dv[inds]
    m = length(ins)
    a = dv.data
    if m == 0
        deleteat!(a, inds)
        deleteat!(dv.na, inds)
        return v
    end

    n = length(a)
    f = first(inds)
    l = last(inds)
    d = length(inds)

    if m < d
        delta = d - m
        i = (f - 1 < n - l) ? f : (l - delta + 1)
        Base._deleteat!(a, i, delta)
    elseif m > d
        delta = m - d
        i = (f - 1 < n - l) ? f : (l + 1)
        Base._growat!(a, i, delta)
    end

    for k = 1:m
        if !ismissing(ins, k)
            if isa(ins, DataVector)
                a[f+k-1] = ins.data[k]
            elseif isa(ins, PooledDataVector)
                a[f+k-1] = ins.pool[ins.refs[k]]
            else
                a[f+k-1] = ins[k]
            end
        end
    end

    splice!(dv.na, inds, ismissing.(ins))
    v
end

function Base.deleteat!(dv::DataVector, inds)
    deleteat!(dv.data, inds)
    deleteat!(dv.na, inds)
    dv
end

function Base.push!(pdv::PooledDataVector{T,R}, v::Missing) where {T,R}
    push!(pdv.refs, zero(R))
    return v
end

function Base.push!(pdv::PooledDataVector{S,R}, v::T) where {S,R,T}
    v = convert(S,v)
    push!(pdv.refs, getpoolidx(pdv, v))
    return v
end

Base.pop!(pdv::PooledDataVector) = pdv.pool[pop!(pdv.refs)]

function Base.unshift!(pdv::PooledDataVector{T,R}, v::Missing) where {T,R}
    unshift!(pdv.refs, zero(R))
    return v
end

function Base.unshift!(pdv::PooledDataVector{S,R}, v::T) where {S,R,T}
    v = convert(S,v)
    unshift!(pdv.refs, getpoolidx(pdv, v))
    return v
end

Base.shift!(pdv::PooledDataVector) = pdv.pool[shift!(pdv.refs)]

Base.reverse(x::AbstractDataVector) = x[end:-1:1]

function Base.splice!(pdv::PooledDataVector, inds::Union{Integer, UnitRange{Int}})
    v = pdv[inds]
    deleteat!(pdv.refs, inds)
    v
end

function Base.splice!(pdv::PooledDataVector, inds::Union{Integer, UnitRange{Int}}, ins::AbstractVector)
    v = pdv[inds]
    splice!(pdv.refs, inds, [getpoolidx(pdv, v) for v in ins])
    v
end

Base.deleteat!(pdv::PooledDataVector, inds) = (deleteat!(pdv.refs, inds); pdv)

function Base.append!(da::AbstractDataVector, items::AbstractVector)
    oldn = length(da)
    itn = length(items)
    resize!(da, oldn+itn)
    da[oldn+1:end] = items[1:itn]
    da
end

function Base.sizehint!(da::DataVector, newsz::Integer)
    sizehint!(da.data, newsz)
    sizehint!(da.na, newsz)
end
Base.sizehint!(pda::PooledDataVector, newsz::Integer) =
    sizehint!(pda.refs, newsz)

# Pad a vector with missings
"""
    padmissing(dv::AbstractDataVector, front::Integer, back::Integer) -> DataVector

Pad `dv` with `missing` values. `front` is an integer number of `missing`s to add at the
beginning of the array and `back` is the number of `missing`s to add at the end.

# Examples

```jldoctest
julia> padmissing(@data([1, 2, 3]), 1, 2)
6-element DataArrays.DataArray{Int64,1}:
  missing
 1
 2
 3
  missing
  missing
```
"""
function padmissing(dv::AbstractDataVector,
                 front::Integer,
                 back::Integer)
    n = length(dv)
    res = similar(dv, front + n + back)
    for i in 1:n
        res[i + front] = dv[i]
    end
    return res
end
