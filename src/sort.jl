# This code is heavily based on the floating point sort code in Base

nas2end!(v::AbstractVector, o::Base.Sort.ForwardOrdering) = nas2right!(v,o)
nas2end!(v::AbstractVector, o::Base.Sort.ReverseOrdering) = nas2left!(v,o)
nas2end!{O<:Base.Order.ForwardOrdering}(v::AbstractVector{Int}, o::Base.Order.Perm{O}) = nas2right!(v,o)
nas2end!{O<:Base.Order.ReverseOrdering}(v::AbstractVector{Int}, o::Base.Order.Perm{O}) = nas2left!(v,o)

myisna(o::Base.Order.Ordering, chunks, i::Int) = Base.unsafe_bitgetindex(chunks, i)

swap(o::Base.Order.DirectOrdering, data, i, j) = setindex!(data, data[i], j)
function swap(o::Base.Order.Perm, data, i, j)
    data[j], data[i] = data[i], data[j]
end

datachunks(o::Base.Order.Perm, v::AbstractVector{Int}) = (v, o.data.na.chunks)
datachunks(o::Base.Order.DirectOrdering, v::DataVector) = (v.data, v.na.chunks)

function nas2left!(v::(@compat Union{AbstractVector{Int}, DataVector}), o::Base.Order.Ordering, lo::Int=1, hi::Int=length(v))
    data, chunks = datachunks(o, v)

    i = lo
    @inbounds while i <= hi && myisna(o, chunks, i)
        i += 1
    end
    j = i + 1
    @inbounds while j <= hi
        if myisna(o, chunks, j)
            swap(o, data, i, j)
            i += 1
        end
        j += 1
    end
    if isa(o, Base.Order.DirectOrdering)
        v.na[lo:i-1] = true
        v.na[i:hi] = false
    end
    return i, hi
end

function nas2right!(v::(@compat Union{AbstractVector{Int}, DataVector}), o::Base.Order.Ordering, lo::Int=1, hi::Int=length(v))
    data, chunks = datachunks(o, v)

    i = hi
    @inbounds while lo <= i && myisna(o, chunks, i)
        i -= 1
    end
    j = i - 1
    @inbounds while lo <= j
        if myisna(o, chunks, j)
            swap(o, data, i, j)
            i -= 1
        end
        j -= 1
    end
    if isa(o, Base.Order.DirectOrdering)
        v.na[lo:i] = false
        v.na[i+1:hi] = true
    end
    return lo, i
end

function dasort!(v::DataVector, a::Base.Sort.Algorithm, o::Base.Order.DirectOrdering)
    lo, hi = nas2end!(v, o)
    sort!(v.data, lo, hi, a, o)
    v
end

function dapermsort!{O<:Base.Order.DirectOrdering,T<:DataVector}(v::AbstractVector{Int}, a::Base.Sort.Algorithm, o::Base.Order.Perm{O,T})
    lo, hi = nas2end!(v, o)
    sort!(v, lo, hi, a, Base.Order.Perm(o.order, o.data.data))
end

Base.sort!(v::DataVector, a::Base.Sort.Algorithm, o::Base.Order.DirectOrdering) = dasort!(v,a,o)
Base.sort!{O<:Base.Order.DirectOrdering,T<:DataVector}(v::Vector{Int}, a::Base.Sort.Algorithm, o::Base.Order.Perm{O,T}) = dapermsort!(v,a,o)
