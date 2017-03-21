# TODO: Remove some T's from output type signatures

"""
    DataArray{T,N}(d::Array{T,N}, m::AbstractArray{Bool} = falses(size(d)))

Construct a `DataArray`, an `N`-dimensional array with element type `T` that allows missing
values. The resulting array uses the data in `d` with `m` as a bitmask to signify missingness.
That is, for each index `i` in `d`, if `m[i]` is `true`, the array contains `NA` at index `i`,
otherwise it contains `d[i]`.

    DataArray(T::Type, dims...)

Construct a `DataArray` with element type `T` and dimensions specified by `dims`. All elements
default to `NA`.

# Examples

```jldoctest
julia> DataArray([1, 2, 3], [true, false, true])
3-element DataArrays.DataArray{Int64,1}:
  NA
 2
  NA

julia> DataArray(Float64, 3, 3)
3×3 DataArrays.DataArray{Float64,2}:
 NA  NA  NA
 NA  NA  NA
 NA  NA  NA
```
"""
mutable struct DataArray{T, N} <: AbstractDataArray{T, N}
    data::Array{T, N}
    na::BitArray{N}

    function DataArray{T,N}(d::Array{T, N}, m::BitArray{N}) where {T, N}
        # Ensure data values and missingness metadata match
        if size(d) != size(m)
            msg = "Data and missingness arrays must be the same size"
            throw(ArgumentError(msg))
        end
        # additionally check that d does not contain NA entries
        if eltype(d) === Any
            for i in eachindex(d)
                if isassigned(d, i) && isna(d, i)
                    m[i] = true
                end
            end
        elseif eltype(d) <: NAtype
            m = trues(m)
        end
        new(d, m)
    end
end

function DataArray{T, N}(d::Array{T, N},
                         m::BitArray{N} = falses(size(d))) # -> DataArray{T}
    return DataArray{T, N}(d, m)
end

function DataArray(d::Array, m::AbstractArray{Bool}) # -> DataArray{T}
    return DataArray(d, BitArray(m))
end

function DataArray(T::Type, dims::Integer...) # -> DataArray{T}
    return DataArray(Array{T}(dims...), trues(dims...))
end

function DataArray{N}(T::Type, dims::NTuple{N, Int}) # -> DataArray{T}
    return DataArray(Array{T}(dims...), trues(dims...))
end

"""
    DataVector{T}

A 1-dimensional `DataArray` with element type `T`.
"""
const DataVector{T} = DataArray{T, 1}

"""
    DataMatrix{T}

A 2-dimensional `DataArray` with element type `T`.
"""
const DataMatrix{T} = DataArray{T, 2}

Base.copy(d::DataArray) = Base.copy!(similar(d), d) # -> DataArray{T}

function Base.copy!(dest::DataArray, src::DataArray) # -> DataArray{T}
    if isbits(eltype(src)) && isbits(eltype(dest))
        copy!(dest.data, src.data)
    else
        # Elements of src_data are not necessarily initialized, so
        # only copy initialized elements
        dest_data = dest.data
        src_data = src.data
        length(dest_data) >= length(src_data) || throw(BoundsError())
        src_chunks = src.na.chunks
        for i = 1:length(src_data)
            @inbounds if !Base.unsafe_bitgetindex(src_chunks, i)
                dest_data[i] = src_data[i]
            end
        end
    end
    copy!(dest.na, src.na)
    dest
end

function Base.copy!(dest::DataArray, doffs::Integer, src::DataArray) # -> DataArray{T}
    copy!(dest, doffs, src, 1, length(src))
end

# redundant on Julia 0.4
function Base.copy!(dest::DataArray, doffs::Integer, src::DataArray, soffs::Integer) # -> DataArray{T}
    soffs <= length(src) || throw(BoundsError())
    copy!(dest, doffs, src, soffs, length(src)-soffs+1)
end

function Base.copy!(dest::DataArray, doffs::Integer, src::DataArray, soffs::Integer, n::Integer) # -> DataArray{T}
    if n == 0
        return dest
    elseif n < 0
        throw(ArgumentError("tried to copy n=$n elements, but n should be nonnegative"))
    end
    if isbits(eltype(src))
        copy!(dest.data, doffs, src.data, soffs, n)
    else
        # Elements of src_data are not necessarily initialized, so
        # only copy initialized elements
        dest_data = dest.data
        src_data = src.data
        if doffs < 1 || length(dest_data) - doffs + 1 < n ||
           soffs < 1 || length(src_data) - soffs + 1 < n
            throw(BoundsError())
        end
        src_chunks = src.na.chunks
        for i = 0:(n-1)
            di, si = doffs + i, soffs + i

            @inbounds if !Base.unsafe_bitgetindex(src_chunks, si)
                dest_data[di] = src_data[si]
            end
        end
    end
    copy!(dest.na, doffs, src.na, soffs, n)
    dest
end

Base.fill!(A::DataArray, ::NAtype) = (fill!(A.na, true); A)
Base.fill!(A::DataArray, v) = (fill!(A.data, v); fill!(A.na, false); A)

function Base.deepcopy(d::DataArray) # -> DataArray{T}
    return DataArray(deepcopy(d.data), deepcopy(d.na))
end

function Base.resize!{T}(da::DataArray{T,1}, n::Int)
    resize!(da.data, n)
    oldn = length(da.na)
    resize!(da.na, n)
    da.na[oldn+1:n] = true
    da
end

function Base.similar(da::DataArray, T::Type, dims::Dims) #-> DataArray{T}
    return DataArray(Array{T}(dims), trues(dims))
end

Base.size(d::DataArray) = size(d.data) # -> (Int...)
Base.ndims(da::DataArray) = ndims(da.data) # -> Int
Base.length(d::DataArray) = length(d.data) # -> Int
Base.endof(da::DataArray) = endof(da.data) # -> Int

function Base.find(da::DataArray{Bool}) # -> Array{Int}
    data = da.data
    ntrue = 0
    @inbounds @bitenumerate da.na i na begin
        ntrue += !na && data[i]
    end
    res = Vector{Int}(ntrue)
    count = 1
    @inbounds @bitenumerate da.na i na begin
        if !na && data[i]
            res[count] = i
            count += 1
        end
    end
    return res
end

function Base.convert{S, T, N}(::Type{Array{S, N}},
                               x::DataArray{T, N}) # -> Array{S, N}
    if any(isna, x)
        err = "Cannot convert DataArray with NA's to desired type"
        throw(NAException(err))
    else
        return convert(Array{S, N}, x.data)
    end
end

function Base.convert{S, T, N}(::Type{Array{S}}, da::DataArray{T, N})
    return convert(Array{S, N}, da)
end

function Base.convert{T}(::Type{Vector}, dv::DataVector{T})
    return convert(Array{T, 1}, dv)
end

function Base.convert{T}(::Type{Matrix}, dm::DataMatrix{T})
    return convert(Array{T, 2}, dm)
end

function Base.convert{T, N}(::Type{Array}, da::DataArray{T, N})
    return convert(Array{T, N}, da)
end

function Base.convert{S, T, N}(
    ::Type{Array{S, N}},
    da::DataArray{T, N},
    replacement::Any
) # -> Array{S, N}
    replacementS = convert(S, replacement)
    res = Array{S}(size(da))
    for i in 1:length(da)
        if da.na[i]
            res[i] = replacementS
        else
            res[i] = da.data[i]
        end
    end
    return res
end

function Base.convert{T}(::Type{Vector}, dv::DataVector{T}, replacement::Any)
    return convert(Array{T, 1}, dv, replacement)
end

function Base.convert{T}(::Type{Matrix}, dm::DataMatrix{T}, replacement::Any)
    return convert(Array{T, 2}, dm, replacement)
end

function Base.convert{T, N}(::Type{Array}, da::DataArray{T, N}, replacement::Any)
    return convert(Array{T, N}, da, replacement)
end

dropna(dv::DataVector) = dv.data[.!dv.na] # -> Vector
isna(da::DataArray, I::Real) = getindex(da.na, I)

Base.broadcast(::typeof(isna), da::DataArray) = copy(da.na)

Base.any(::typeof(isna), da::DataArray) = any(da.na) # -> Bool
Base.all(::typeof(isna), da::DataArray) = all(da.na) # -> Bool

@nsplat N function isna(da::DataArray, I::NTuple{N,Real}...)
    getindex(da.na, I...)
end

function Base.isfinite(da::DataArray) # -> DataArray{Bool}
    n = length(da)
    res = Array{Bool}(size(da))
    for i in 1:n
        if !da.na[i]
            res[i] = isfinite(da.data[i])
        end
    end
    return DataArray(res, copy(da.na))
end

# Promotion rules

# promote_rule{T, T}(::Type{AbstractDataArray{T}},
#                    ::Type{T}) = promote_rule(T, T)
# promote_rule{S, T}(::Type{AbstractDataArray{S}},
#                    ::Type{T}) = promote_rule(S, T)
# promote_rule{T}(::Type{AbstractDataArray{T}}, ::Type{T}) = T

function Base.convert{S, T, N}(::Type{DataArray{S, N}},
                               a::AbstractArray{T, N}) # -> DataArray{S, N}
    return DataArray(convert(Array{S, N}, a), falses(size(a)))
end
Base.convert{S,T,N}(::Type{DataArray{S}}, x::AbstractArray{T,N}) =
    convert(DataArray{S,N}, x)
Base.convert{T, N}(::Type{DataArray}, x::AbstractArray{T, N}) =
    convert(DataArray{T,N}, x)

function Base.convert{S, T, N}(::Type{DataArray{S, N}},
                               x::DataArray{T, N}) # -> DataArray{S, N}
    v = similar(x.data, S)
    @inbounds for i = 1:length(x)
        if !x.na[i]
            v[i] = convert(S, x.data[i])
        end
    end
    return DataArray(v, x.na)
end

"""
    data(a::AbstractArray) -> DataArray

Convert `a` to a `DataArray`.

# Examples

```jldoctest
julia> data([1, 2, 3])
3-element DataArrays.DataArray{Int64,1}:
 1
 2
 3

julia> data(@data [1, 2, NA])
3-element DataArrays.DataArray{Int64,1}:
 1
 2
  NA
```
"""
data(a::AbstractArray) = convert(DataArray, a)

# TODO: Make sure these handle copying correctly
# TODO: Remove these? They have odd behavior, because they convert to Array's.
# TODO: Rethink multi-item documentation approach
for f in (:(Base.float),)
    @eval begin
        function ($f)(da::DataArray) # -> DataArray
            if any(isna, da)
                err = "Cannot convert DataArray with NA's to desired type"
                throw(NAException(err))
            else
                ($f)(da.data)
            end
        end
    end
end

"""
    finduniques(da::DataArray) -> (Vector, Int)

Get the unique values in `da` as well as the index of the first `NA` value
in `da` if present, or 0 otherwise.
"""
function finduniques{T}(da::DataArray{T}) # -> Vector{T}, Int
    out = Vector{T}(0)
    seen = Set{T}()
    n = length(da)
    firstna = 0
    for i in 1:n
        if isna(da, i)
            if firstna == 0
                firstna = length(out) + 1
            else
                continue
            end
        elseif !in(da.data[i], seen)
            push!(seen, da.data[i])
            push!(out, da.data[i])
        end
    end
    return out, firstna
end

function Base.unique{T}(da::DataArray{T}) # -> DataVector{T}
    unique_values, firstna = finduniques(da)
    n = length(unique_values)
    if firstna > 0
        res = DataArray(Vector{T}(n + 1))
        i = 1
        for val in unique_values
            if i == firstna
                res.na[i] = true
                i += 1
            end
            res.data[i] = val
            i += 1
        end

        if firstna == n + 1
            res.na[n + 1] = true
        end

        return res
    else
        return DataArray(unique_values)
    end
end

"""
    levels(da::DataArray) -> DataVector

Return a vector of the unique values in `da`, excluding any `NA`s.

    levels(a::AbstractArray) -> Vector

Equivalent to `unique(a)`.

# Examples

```jldoctest
julia> levels(@data [1, 2, NA])
2-element DataArrays.DataArray{Int64,1}:
 1
 2
```
"""
function levels(da::DataArray) # -> DataVector{T}
    unique_values, firstna = finduniques(da)
    return DataArray(unique_values)
end

function levels(a::AbstractArray) # -> Vector{T}
    return unique(a)
end
