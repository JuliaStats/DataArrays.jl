# dv[SingleItemIndex, SingleItemIndex)
function Base.getindex(d::DataVector,
                       i::SingleIndex,
                       j::SingleIndex)
    if j != 1
        throw(ArgumentError("Second index must be 1"))
    end
    if d.na[i]
        return NA
    else
        return d.data[i]
    end
end

head(dv::AbstractDataVector) = dv[1:min(6, length(dv))]
tail(dv::AbstractDataVector) = dv[max(length(dv) - 6, 1):length(dv)]

# Container operations

# TODO: Macroize these definitions

function Base.push!(dv::DataVector, v::NAtype)
    resize!(dv.data, length(dv.data) + 1)
    push!(dv.na, true)
    return v
end

function Base.push!{S, T}(dv::DataVector{S}, v::T)
    push!(dv.data, v)
    push!(dv.na, false)
    return v
end

function Base.pop!(dv::DataVector)
    d, m = pop!(dv.data), pop!(dv.na)
    if m
        return NA
    else
        return d
    end
end

function Base.unshift!{T}(dv::DataVector{T}, v::NAtype)
    ccall(:jl_array_grow_beg, Void, (Any, Uint), dv.data, 1)
    unshift!(dv.na, true)
    return v
end

function Base.unshift!{S, T}(dv::DataVector{S}, v::T)
    unshift!(dv.data, v)
    unshift!(dv.na, false)
    return v
end

function Base.shift!{T}(dv::DataVector{T})
    d, m = shift!(dv.data), shift!(dv.na)
    if m
        return NA
    else
        return d
    end
end

# TODO: should this be an AbstractDataVector, so it works with PDV's?
function Base.map(f::Function, dv::DataVector)
    n = length(dv)
    res = DataArray(Any, n)
    for i in 1:n
        res[i] = f(dv[i])
    end
    return res
end

function Base.push!{T,R}(pdv::PooledDataVector{T,R}, v::NAtype)
    push!(pdv.refs, zero(R))
    return v
end

function Base.push!{S,R,T}(pdv::PooledDataVector{S,R}, v::T)
    v = convert(S,v)
    push!(pdv.refs, getpoolidx(pdv, v))
    return v
end

Base.pop!(pdv::PooledDataVector) = pdv.pool[pop!(pdv.refs)]

function Base.unshift!{T,R}(pdv::PooledDataVector{T,R}, v::NAtype)
    unshift!(pdv.refs, zero(R))
    return v
end

function Base.unshift!{S,R,T}(pdv::PooledDataVector{S,R}, v::T)
    v = convert(S,v)
    unshift!(pdv.refs, getpoolidx(pdv, v))
    return v
end

Base.shift!(pdv::PooledDataVector) = pdv.pool[shift!(pdv.refs)]

Base.reverse(x::AbstractDataVector) = x[end:-1:1]

# Pad a vector with NA's

function padNA(dv::AbstractDataVector,
               front::Integer,
               back::Integer)
    n = length(dv)
    res = similar(dv, front + n + back)
    for i in 1:n
        res[i + front] = dv[i]
    end
    return res
end
