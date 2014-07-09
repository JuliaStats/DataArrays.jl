# Indexing with NA throws an error
function Base.to_index(A::DataArray)
    any(A.na) && error("cannot index an array with a DataArray containing NA values")
    Base.to_index(A.data)
end

# Check for NA
unsafe_isna(da::DataArray, idx::Real) = Base.unsafe_bitgetindex(da.na.chunks, idx)
unsafe_isna(pda::PooledDataArray, idx::Real) = pda.refs[idx] == 0
unsafe_getindex_notna(da::DataArray, idx::Real) = getindex(da.data, idx)
unsafe_getindex_notna(pda::PooledDataArray, idx::Real) = getindex(pda.pool, pda.refs[idx])
unsafe_getindex_notna(a, idx::Real) = Base.unsafe_getindex(a, idx)

# Set NA or data portion of DataArray
unsafe_bitsettrue!(chunks::Vector{Uint64}, idx::Real) =
    chunks[Base.@_div64(int(idx)-1)+1] |= (uint64(1) << Base.@_mod64(int(idx)-1))
unsafe_bitsetfalse!(chunks::Vector{Uint64}, idx::Real) =
    chunks[Base.@_div64(int(idx)-1)+1] &= ~(uint64(1) << Base.@_mod64(int(idx)-1))
unsafe_setna!(da::DataArray, idx::Real) = unsafe_bitsettrue!(da.na.chunks, idx)
unsafe_setna!(da::PooledDataArray, idx::Real) = setindex!(da.refs, 0, idx)
unsafe_setnotna!(da::DataArray, idx::Real) = unsafe_bitsetfalse!(da.na.chunks, idx)
unsafe_setnotna!(da::PooledDataArray, idx::Real) = nothing

# Fast setting of NA values in DataArrays
# These take the data and chunks (extracted as da.data and
# da.na.chunks), a value, and a linear index. They assume
# a certain initialization pattern:
#
# - For DataArrays, da.na should be falses
# - For PooledDataArrays, pda.refs should be zeros
unsafe_dasetindex!(data::Array, na_chunks::Vector{Uint64}, val::NAtype, idx::Real) =
    unsafe_bitsettrue!(na_chunks, idx)
unsafe_dasetindex!(data::Array, na_chunks::Vector{Uint64}, val, idx::Real) =
    setindex!(data, val, idx)
unsafe_dasetindex!(da::DataArray, val::NAtype, idx::Real) =
    unsafe_setna!(da, idx)
unsafe_dasetindex!(da::PooledDataArray, val::NAtype, idx::Real) = nothing
unsafe_dasetindex!(da::DataArray, val, idx::Real) = setindex!(da.data, val, idx)
unsafe_dasetindex!(pda::PooledDataArray, val, idx::Real) =
    setindex!(pda.refs, getpoolidx(pda, val), idx)

# Fast implementation of checkbounds for DataArray input
Base.checkbounds(sz::Int, I::AbstractDataVector{Bool}) =
    length(I) == sz || throw(BoundsError())
function Base.checkbounds{T<:Real}(sz::Int, I::AbstractDataArray{T})
    anyna(I) && error("cannot index into an array with a DataArray containing NAs")
    for i = 1:length(I)
        @inbounds v = unsafe_getindex_notna(I, i)
        checkbounds(sz, v)
    end
end

## getindex

#' @description
#'
#' Get a set of elements of a DataArray.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be retrieved.
#' @param ind::Real The index of the element to be retrieved.
#'
#' @returns out::Union(T, NAtype) The value of `da` at the requested
#'          index.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[1]
#'
#' da = @data([NA, 2, 3])
#' da[1]

@nsplat N function getindex(da::DataArray, I::NTuple{N,Real}...)
    if getindex(da.na, I...)
        return NA
    else
        return getindex(da.data, I...)
    end
end

@nsplat N function getindex(pda::PooledDataArray, I::NTuple{N,Real}...)
    if getindex(pda.refs, I...) == 0
        return NA
    else
        return pda.pool[getindex(pda.refs, I...)]
    end
end

getindex(t::AbstractDataArray, i::Real) = error("indexing not defined for ", typeof(t))

@ngenerate N typeof(dest) function _getindex!(dest::AbstractDataArray, src::AbstractDataArray,
                                              I::NTuple{N,Union(Int,AbstractVector)}...)
    Base.checksize(dest, I...)
    stride_1 = 1
    @nexprs N d->(stride_{d+1} = stride_d*size(src,d))
    @nexprs N d->(offset_d = 1)  # only really need offset_$N = 1
    k = 1
    @nloops N i dest d->(@inbounds offset_{d-1} = offset_d + (Base.unsafe_getindex(I_d, i_d)-1)*stride_d) begin
        @inbounds if unsafe_isna(src, offset_0)
            unsafe_dasetindex!(dest, NA, k)
        else
            unsafe_dasetindex!(dest, unsafe_getindex_notna(src, offset_0), k)
        end
        k += 1
    end
    dest
end

function _getindex{T}(A::DataArray{T}, I::(Union(Int,AbstractVector)...))
    shape = Base.index_shape(I...)
    _getindex!(DataArray(Array(T, shape), falses(shape)), A, I...)
end

function _getindex(A::PooledDataArray, I::(Union(Int,AbstractVector)...))
    shape = Base.index_shape(I...)
    _getindex!(similar(A, shape), A, I...)
end

function Base.getindex(A::AbstractDataArray, I::AbstractVector)
    checkbounds(A, I)
    _getindex(A, (Base.to_index(I),))
end

@nsplat N function Base.getindex(A::AbstractDataArray, I::NTuple{N,Union(Real,AbstractVector)}...)
    checkbounds(A, I...)
    _getindex(A, Base.to_index(I...))
end

## setindex

#' @description
#'
#' Set one element of a DataArray to `NA`.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be modified.
#' @param val::NAtype The `NA` value being assigned to an element of `da`.
#' @param ind::Real A real value specifying the index of the element
#'        of `da` being modified.
#'
#' @returns da::DataArray{T, N} The modified DataArray.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[1] = NA
function Base.setindex!(da::DataArray, val::NAtype, i::Real)
    da.na[i] = true
    return da
end

function Base.setindex!(pda::PooledDataArray, val::NAtype, ind::Real)
    pda.refs[ind] = 0
    return pda
end

setindex!(t::AbstractDataArray, x, i::Real) =
    error("setindex! not defined for ",typeof(t))

#' @description
#'
#' Set one element of a DataArray to a new value, `val`.
#'
#' @param da::DataArray{T, N} A DataArray whose element will be modified.
#' @param val::Any The value being assigned to an element of `da`.
#' @param ind::Real A real value specifying the index of the element
#'        of `da` being modified.
#'
#' @returns da::DataArray{T, N} The modified DataArray.
#'
#' @examples
#'
#' da = @data([1, 2, 3])
#' da[1] = 4
function Base.setindex!(da::DataArray, val, ind::Real)
    da.data[ind] = val
    da.na[ind] = false
    return da
end

function Base.setindex!(x::PooledDataArray, val, ind::Real)
    x.refs[ind] = getpoolidx(x, val)
    return x
end

@ngenerate N typeof(A) function Base.setindex!(A::AbstractDataArray, x,
                                               J::NTuple{N,Union(Real,AbstractArray)}...)
    @ncall N checkbounds A J
    @nexprs N d->(I_d = Base.to_index(J_d))
    stride_1 = 1
    @nexprs N d->(stride_{d+1} = stride_d*size(A,d))
    @nexprs N d->(offset_d = 1)  # really only need offset_$N = 1
    if !isa(x, AbstractArray)
        @nloops N i d->(1:length(I_d)) d->(@inbounds offset_{d-1} = offset_d + (Base.unsafe_getindex(I_d, i_d)-1)*stride_d) begin
            if isa(x, NAtype)
                @inbounds unsafe_setna!(A, offset_0)
            else
                @inbounds unsafe_setnotna!(A, offset_0)
                @inbounds unsafe_dasetindex!(A, x, offset_0)
            end
        end
    else
        X = x
        @ncall N Base.setindex_shape_check X I
        k = 1
        @nloops N i d->(1:length(I_d)) d->(@inbounds offset_{d-1} = offset_d + (Base.unsafe_getindex(I_d, i_d)-1)*stride_d) begin
            @inbounds if isa(X, AbstractDataArray) && unsafe_isna(X, k)
                unsafe_setna!(A, offset_0)
            else
                unsafe_setnotna!(A, offset_0)
                unsafe_dasetindex!(A, unsafe_getindex_notna(X, k), offset_0)
            end
            k += 1
        end
    end
    A
end
