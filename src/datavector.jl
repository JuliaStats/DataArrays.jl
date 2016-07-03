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
    ccall(:jl_array_grow_beg, Void, (Any, UInt), dv.data, 1)
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

function Base.splice!(dv::DataVector, inds::(@compat Union{Integer, UnitRange{Int}}))
    v = dv[inds]
    deleteat!(dv.data, inds)
    deleteat!(dv.na, inds)
    v
end

function Base.splice!(dv::DataVector, inds::(@compat Union{Integer, UnitRange{Int}}), ins::AbstractVector)
    # We cannot merely use the implementation in Base because this
    # needs to handle NA in the replacement vector
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

    if VERSION >= v"0.5.0-dev+5022"
        if m < d
            delta = d - m
            i = (f - 1 < n - l) ? f : (l - delta + 1)
            Base._deleteat!(a, i, delta)
        elseif m > d
            delta = m - d
            i = (f - 1 < n - l) ? f : (l + 1)
            Base._growat!(a, i, delta)
        end
    else
        if m < d
            delta = d - m
            if f-1 < n-l
                Base._deleteat_beg!(a, f, delta)
            else
                Base._deleteat_end!(a, l-delta+1, delta)
            end
        elseif m > d
            delta = m - d
            if f-1 < n-l
                Base._growat_beg!(a, f, delta)
            else
                Base._growat_end!(a, l+1, delta)
            end
        end
    end

    for k = 1:m
        if !isna(ins, k)
            if isa(ins, DataVector)
                a[f+k-1] = ins.data[k]
            elseif isa(ins, PooledDataVector)
                a[f+k-1] = ins.pool[ins.refs[k]]
            else
                a[f+k-1] = ins[k]
            end
        end
    end

    splice!(dv.na, inds, isna(ins))
    v
end

function Base.deleteat!(dv::DataVector, inds)
    deleteat!(dv.data, inds)
    deleteat!(dv.na, inds)
    dv
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

function Base.splice!(pdv::PooledDataVector, inds::(@compat Union{Integer, UnitRange{Int}}))
    v = pdv[inds]
    deleteat!(pdv.refs, inds)
    v
end

function Base.splice!(pdv::PooledDataVector, inds::(@compat Union{Integer, UnitRange{Int}}), ins::AbstractVector)
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
