## Unsafe scalar indexing

# Extract relevant fields of a DataArray to a tuple
# The extracted tuple can be passed to `unsafe_ismissing`,
# `unsafe_getindex_notmissing`, `unsafe_setmissing!`, `unsafe_setnotmissing!`, and
# `unsafe_dasetindex!`. This has a meaningful performance impact within
# very tight loops.
daextract(da::DataArray) = (da.data, da.na.chunks)
daextract(pda::PooledDataArray) = (pda.refs, pda.pool)
daextract(a) = nothing

# Check for missing
unsafe_ismissing(da::DataArray, extr, idx::Real) = Base.unsafe_bitgetindex(extr[2], idx)
unsafe_ismissing(pda::PooledDataArray, extr, idx::Real) = extr[1][idx] == 0
unsafe_ismissing(a, extr, idx::Real) = false
unsafe_getindex_notmissing(da::DataArray, extr, idx::Real) = getindex(extr[1], idx)
unsafe_getindex_notmissing(pda::PooledDataArray, extr, idx::Real) = getindex(extr[2], extr[1][idx])
unsafe_getindex_notmissing(a, extr, idx::Real) = Base.unsafe_getindex(a, idx)

# Set missing or data portion of DataArray

unsafe_bitsettrue!(chunks::Vector{UInt64}, idx::Real) =
    chunks[Base._div64(Int(idx)-1)+1] |= (UInt64(1) << Base._mod64(Int(idx)-1))
unsafe_bitsetfalse!(chunks::Vector{UInt64}, idx::Real) =
    chunks[Base._div64(Int(idx)-1)+1] &= ~(UInt64(1) << Base._mod64(Int(idx)-1))

unsafe_setmissing!(da::DataArray, extr, idx::Real) = unsafe_bitsettrue!(extr[2], idx)
unsafe_setmissing!(da::PooledDataArray, extr, idx::Real) = setindex!(extr[1], 0, idx)
unsafe_setnotmissing!(da::DataArray, extr, idx::Real) = unsafe_bitsetfalse!(extr[2], idx)
unsafe_setnotmissing!(da::PooledDataArray, extr, idx::Real) = nothing

# Fast setting of missing values in DataArrays
# These take the data and chunks (extracted as da.data and
# da.na.chunks), a value, and a linear index. They assume
# a certain initialization pattern:
#
# - For DataArrays, da.na should be falses
# - For PooledDataArrays, pda.refs should be zeros
unsafe_dasetindex!(data::Array, na_chunks::Vector{UInt64}, val::Missing, idx::Real) =
    unsafe_bitsettrue!(na_chunks, idx)
unsafe_dasetindex!(data::Array, na_chunks::Vector{UInt64}, val, idx::Real) =
    setindex!(data, val, idx)
unsafe_dasetindex!(da::DataArray, extr, val::Missing, idx::Real) =
    unsafe_setmissing!(da, extr, idx)
unsafe_dasetindex!(da::PooledDataArray, extr, val::Missing, idx::Real) = nothing
unsafe_dasetindex!(da::DataArray, extr, val, idx::Real) = setindex!(extr[1], val, idx)
unsafe_dasetindex!(pda::PooledDataArray, extr, val, idx::Real) =
    setindex!(extr[1], getpoolidx(pda, val), idx)
unsafe_dasetindex!(a::AbstractArray, extr, val, idx::Real) = setindex!(a, val, idx)

## PooledDataArray helper functions

# Append newpool to pool. Return indices of newpool in pool.
function combine_pools!(pool, newpool)
    seen = Dict{eltype(pool),Int}()
    sizehint!(seen, length(pool)+length(newpool))

    # Create mapping from pool elements to indices
    i = 0
    for elem in pool
        seen[elem] = (i += 1)
    end

    # Find pool elements in existing array, or add them
    poolidx = Vector{Int}(length(newpool))
    for j = 1:length(newpool)
        poolidx[j] = Base.@get!(seen, newpool[j], (push!(pool, newpool[j]); i += 1))
    end
    poolidx
end

## General indexing functions

# Indexing with missing throws an error
function Base.to_index(A::DataArray)
    any(A.na) && throw(MissingException("cannot convert missing value to index"))
    Base.to_index(A.data)
end


if isdefined(Base, :checkindex) && isdefined(Base, :AbstractUnitRange)
    Base.checkindex(::Type{Bool}, ::AbstractUnitRange, ::Missing) =
        throw(MissingException("missing values are not allowed in indices"))
elseif isdefined(Base, :checkindex)
    Base.checkindex(::Type{Bool}, ::UnitRange, ::Missing) =
        throw(MissingException("missing values are not allowed in indices"))
else
    Base.checkbounds(::Type{Bool}, sz::Int, I::AbstractDataVector{Bool}) = length(I) == sz
    function Base.checkbounds{T<:Real}(::Type{Bool}, sz::Int, I::AbstractDataArray{T})
        any(ismissing, I) && throw(MissingException("missing values are not allowed in indices"))
        extr = daextract(I)
        b = true
        for i = 1:length(I)
            @inbounds v = unsafe_getindex_notmissing(I, extr, i)
            b &= Base.checkbounds(Bool, sz, v)
        end
        b
    end
end

import Base: index_shape, index_lengths, setindex_shape_check

if isdefined(Base, :OneTo)
    _index_shape(x...) = Base.to_shape(index_shape(x...))
else
    _index_shape = index_shape
end

# Fallbacks to avoid ambiguity
Base.setindex!(t::AbstractDataArray, x, i::Real) =
    throw(MethodError(setindex!, typeof(t), typeof(x), typeof(i)))
Base.getindex(t::AbstractDataArray, i::Real) =
    throw(MethodError(getindex, typeof(t), typeof(i)))

## getindex: DataArray

Base.IndexStyle(::Type{<:AbstractDataArray}) = Base.IndexLinear()

# Scalar case
function Base.getindex(da::DataArray, I::Real)
    if getindex(da.na, I)
        return missing
    else
        return getindex(da.data, I)
    end
end

@generated function Base._unsafe_getindex!(dest::DataArray, src::DataArray, I::Union{Real, AbstractArray}...)
    N = length(I)
    quote
        $(Expr(:meta, :inline))
        flipbits!(dest.na) # similar initializes with missings
        @nexprs $N d->(J_d = I[d])
        srcextr = daextract(src)
        destextr = daextract(dest)
        srcsz = size(src)
        D = eachindex(dest)
        Ds = start(D)
        @nloops $N j d->J_d begin
            offset_0 = @ncall $N sub2ind srcsz j
            d, Ds = next(D, Ds)
            if unsafe_ismissing(src, srcextr, offset_0)
                unsafe_dasetindex!(dest, destextr, missing, d)
            else
                unsafe_dasetindex!(dest, destextr, unsafe_getindex_notmissing(src, srcextr, offset_0), d)
            end
        end
        dest
    end
end

## getindex: PooledDataArray

# Scalar case
function Base.getindex(pda::PooledDataArray, I::Real)
    if getindex(pda.refs, I) == 0
        return missing
    else
        return pda.pool[getindex(pda.refs, I)]
    end
end

@inline function Base.getindex(pda::PooledDataArray, I::Integer...)
    if getindex(pda.refs, I...) == 0
        return missing
    else
        return pda.pool[getindex(pda.refs, I...)]
    end
end

# Vector case
@inline function Base.getindex(A::PooledDataArray, I::Union{AbstractVector,Colon}...)
    PooledDataArray(RefArray(getindex(A.refs, I...)), copy(A.pool))
end

## setindex!: DataArray

function Base.setindex!(da::DataArray, val::Missing, i::Real)
    da.na[i] = true
    return da
end

function Base.setindex!(da::DataArray, val, ind::Real)
    da.data[ind] = val
    da.na[ind] = false
    return da
end

## setindex!: PooledDataArray

function Base.setindex!(pda::PooledDataArray, val::Missing, ind::Real)
    pda.refs[ind] = 0
    return pda
end

function Base.setindex!(x::PooledDataArray, val, ind::Real)
    x.refs[ind] = getpoolidx(x, val)
    return x
end

## setindex!: both DataArray and PooledDataArray

@generated function Base.setindex!(A::AbstractDataArray, x, J::Union{Real,Colon,AbstractArray}...)
    N = length(J)
    quote
        if !isa(x, AbstractArray) && isa(A, PooledDataArray)
            # Only perform one pool lookup when assigning a scalar value in
            # a PooledDataArray
            setindex!(A.refs, getpoolidx(A, x), J...)
            return A
        end

        Aextr = daextract(A)
        @nexprs $N d->(I_d = Base.to_indices(A, J)[d])
        @ncall $N checkbounds A I
        stride_1 = 1
        @nexprs $N d->(stride_{d+1} = stride_d*size(A,d))
        @nexprs $N d->(offset_d = 1)  # really only need offset_$N = 1
        if !isa(x, AbstractArray)
            @nloops $N i d->I_d d->(@inbounds offset_{d-1} = offset_d + (i_d - 1)*stride_d) begin
                if isa(x, Missing)
                    @inbounds unsafe_setmissing!(A, Aextr, offset_0)
                else
                    @inbounds unsafe_setnotmissing!(A, Aextr, offset_0)
                    @inbounds unsafe_dasetindex!(A, Aextr, x, offset_0)
                end
            end
        else
            X = x
            idxlens = @ncall $N index_lengths I
            @ncall $N setindex_shape_check X (d->idxlens[d])
            k = 1
            if isa(A, PooledDataArray) && isa(X, PooledDataArray)
                # When putting one PDA into another, first unify the pools
                # and then translate the references
                poolmap = combine_pools!(A.pool, X.pool)
                Arefs = A.refs
                Xrefs = X.refs
                @nloops $N i d->I_d d->(@inbounds offset_{d-1} = offset_d + (i_d - 1)*stride_d) begin
                    @inbounds Arefs[offset_0] = Xrefs[k] == 0 ? 0 : poolmap[Xrefs[k]]
                    k += 1
                end
            else
                Xextr = daextract(X)
                @nloops $N i d->I_d d->(@inbounds offset_{d-1} = offset_d + (i_d - 1)*stride_d) begin
                    @inbounds if isa(X, AbstractDataArray) && unsafe_ismissing(X, Xextr, k)
                        unsafe_setmissing!(A, Aextr, offset_0)
                    else
                        unsafe_setnotmissing!(A, Aextr, offset_0)
                        unsafe_dasetindex!(A, Aextr, unsafe_getindex_notmissing(X, Xextr, k), offset_0)
                    end
                    k += 1
                end
            end
        end
        A
    end
end
