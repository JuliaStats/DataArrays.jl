function StatsBase.addcounts!(cm::Dict{U,Int}, x::AbstractDataArray{T}) where {T,U}
    for v in x
        cm[v] = get(cm, v, 0) + 1
    end
    return cm
end

function StatsBase.addcounts!(cm::Dict{U,W}, x::AbstractDataArray{T}, wv::Weights{W}) where {T,U,W}
    n = length(x)
    length(wv) == n || raise_dimerror()
    w = values(wv)
    z = zero(W)

    @inbounds for i in 1:n
        xi = x[i]
        wi = w[i]
        cm[xi] = get(cm, xi, z) + wi
    end
    return cm
end


function StatsBase.countmap(x::AbstractDataArray{T}) where {T}
    addcounts!(Dict{Union{T,Missing}, Int}(), x)
end

function StatsBase.countmap(x::AbstractDataArray{T}, wv::Weights{W}) where {T,W}
    addcounts!(Dict{Union{T,Missing}, W}(), x, wv)
end

"""
    cut(x::AbstractVector, breaks::Vector) -> PooledDataArray
    cut(x::AbstractVector, ngroups::Integer) -> PooledDataArray

Divide the range of `x` into intervals based on the cut points specified in `breaks`,
or into `ngroups` intervals of approximately equal length.

# Examples

```jldoctest
julia> cut([1, 2, 3, 4], [1, 3])
4-element DataArrays.PooledDataArray{String,UInt32,1}:
 "[1,3]"
 "[1,3]"
 "[1,3]"
 "(3,4]"
```
"""
function cut(x::AbstractVector{S}, breaks::Vector{T}) where {S, T}
    if !issorted(breaks)
        sort!(breaks)
    end
    min_x, max_x = minimum(x), maximum(x)
    if breaks[1] > min_x
        unshift!(breaks, min_x)
    end
    if breaks[end] < max_x
        push!(breaks, max_x)
    end
    refs = fill(zero(DEFAULT_POOLED_REF_TYPE), length(x))
    for i in 1:length(x)
        if x[i] == min_x
            refs[i] = 1
        else
            refs[i] = searchsortedfirst(breaks, x[i]) - 1
        end
    end
    n = length(breaks)
    from = map(x -> sprint(showcompact, x), breaks[1:(n - 1)])
    to = map(x -> sprint(showcompact, x), breaks[2:n])
    pool = Vector{String}(n - 1)
    if breaks[1] == min_x
        pool[1] = string("[", from[1], ",", to[1], "]")
    else
        pool[1] = string("(", from[1], ",", to[1], "]")
    end
    for i in 2:(n - 1)
        pool[i] = string("(", from[i], ",", to[i], "]")
    end
    PooledDataArray(RefArray(refs), pool)
end

cut(x::AbstractVector, ngroups::Integer) = cut(x, quantile(x, collect(1 : ngroups - 1) / ngroups))

function Base.repeat(A::DataArray{T,N};
                     inner = ntuple(x->1, ndims(A)),
                     outer = ntuple(x->1, ndims(A))) where {T,N}
    DataArray{T,N}(repeat(A.data; inner=inner, outer=outer),
                   BitArray(repeat(A.na; inner=inner, outer=outer)))
end

function Base.repeat(A::PooledDataArray{T,R,N};
                     inner = ntuple(x->1, ndims(A)),
                     outer = ntuple(x->1, ndims(A))) where {T,R,N}
    PooledDataArray(RefArray{R,N}(repeat(A.refs; inner=inner, outer=outer)),
                    A.pool)
end
