# TODO: Remove some T's from output type signatures

"""
    DataArray{T,N}(d::Array{T,N}, m::AbstractArray{Bool} = falses(size(d)))

Construct a `DataArray`, an `N`-dimensional array with element type `T` that allows missing
values. The resulting array uses the data in `d` with `m` as a bitmask to signify missingness.
That is, for each index `i` in `d`, if `m[i]` is `true`, the array contains `missing` at index `i`,
otherwise it contains `d[i]`.

    DataArray(T::Type, dims...)

Construct a `DataArray` with element type `T` and dimensions specified by `dims`. All elements
default to `missing`.

# Examples

```jldoctest
julia> DataArray([1, 2, 3], [true, false, true])
3-element DataArrays.DataArray{Int64,1}:
  missing
 2
  missing

julia> DataArray(Float64, 3, 3)
3×3 DataArrays.DataArray{Float64,2}:
 missing  missing  missing
 missing  missing  missing
 missing  missing  missing
```
"""
mutable struct DataArray{T, N} <: AbstractDataArray{T, N}
    data::Array{T, N}
    na::BitArray{N}

    function DataArray{T,N}(d::Array{<:Union{T, Missing}, N}, m::BitArray{N}) where {T, N}
        # Ensure data values and missingness metadata match
        if size(d) != size(m)
            msg = "Data and missingness arrays must be the same size"
            throw(ArgumentError(msg))
        end
        # if input array can contain missing values, we need to mark corresponding entries as missing
        if eltype(d) >: Missing
            # If the original eltype is wider than the target eltype T, conversion may fail
            # in the presence of missings: we need to allocate a copy, leaving entries
            # corresponding to missings undef
            if eltype(d) <: T
                @inbounds for i in eachindex(d)
                    if isassigned(d, i) && ismissing(d, i)
                        m[i] = true
                    end
                end
            else
                d2 = similar(d, T)
                @inbounds for i in eachindex(d)
                    isassigned(d, i) || continue
                    if ismissing(d, i)
                        m[i] = true
                    else
                        d2[i] = d[i]
                    end
                end
                return new(d2, m)
            end
        elseif eltype(d) <: Missing
            m = trues(m)
        end
        new(d, m)
    end
end

DataArray{T}(d::Array{S, N}) where {T, S, N} = DataArray{T, N}(d) # -> DataArray{T}

function DataArray{T, N}(d::Array,
                         m::BitArray{N} = falses(size(d))) where {T, N}  # -> DataArray{T}
    DataArray(convert(Array{Missings.T(T), N}, d), m)
end

function DataArray(d::Array{T, N},
                   m::BitArray{N} = falses(size(d))) where {T, N} # -> DataArray{T}
    return DataArray{Missings.T(T), N}(d, m)
end

function DataArray(d::Array, m::AbstractArray{Bool}) # -> DataArray{T}
    return DataArray(d, BitArray(m))
end

function DataArray(T::Type, dims::Integer...) # -> DataArray{T}
    return DataArray(Array{Missings.T(T)}(undef, dims...), trues(dims...))
end

function DataArray(T::Type, dims::NTuple{N, Int}) where N # -> DataArray{T}
    return DataArray(Array{Missings.T(T)}(dims...), trues(dims...))
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

Base.copy(d::DataArray) = Base.copyto!(similar(d), d) # -> DataArray{T}

function Base.copyto!(dest::DataArray, src::DataArray) # -> DataArray{T}
    if isbits(eltype(src)) && isbits(eltype(dest))
        copyto!(dest.data, src.data)
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
    copyto!(dest.na, src.na)
    dest
end

function Base.copyto!(dest::DataArray, doffs::Integer, src::DataArray) # -> DataArray{T}
    copyto!(dest, doffs, src, 1, length(src))
end

# redundant on Julia 0.4
function Base.copyto!(dest::DataArray, doffs::Integer, src::DataArray, soffs::Integer) # -> DataArray{T}
    soffs <= length(src) || throw(BoundsError())
    copyto!(dest, doffs, src, soffs, length(src)-soffs+1)
end

function Base.copyto!(dest::DataArray, doffs::Integer, src::DataArray, soffs::Integer, n::Integer) # -> DataArray{T}
    if n == 0
        return dest
    elseif n < 0
        throw(ArgumentError("tried to copy n=$n elements, but n should be nonnegative"))
    end
    if isbits(eltype(src))
        copyto!(dest.data, doffs, src.data, soffs, n)
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
    copyto!(dest.na, doffs, src.na, soffs, n)
    dest
end

Base.fill!(A::DataArray, ::Missing) = (fill!(A.na, true); A)
Base.fill!(A::DataArray, v) = (fill!(A.data, v); fill!(A.na, false); A)

function Base.deepcopy(d::DataArray) # -> DataArray{T}
    return DataArray(deepcopy(d.data), deepcopy(d.na))
end

function Base.resize!(da::DataArray{T,1}, n::Int) where T
    resize!(da.data, n)
    oldn = length(da.na)
    resize!(da.na, n)
    da.na[oldn+1:n] = true
    da
end

function Base.similar(da::DataArray, T::Type, dims::Dims) #-> DataArray{T}
    return DataArray(Array{Missings.T(T)}(undef, dims), trues(dims))
end

Base.size(d::DataArray) = size(d.data) # -> (Int...)
Base.ndims(da::DataArray) = ndims(da.data) # -> Int
Base.length(d::DataArray) = length(d.data) # -> Int
Base.lastindex(da::DataArray) = lastindex(da.data) # -> Int

function Base.findall(da::DataArray{Bool}) # -> Array{Int}
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

function Base.convert(::Type{Array{S, N}},
                      x::DataArray{T, N}) where {S, T, N} # -> Array{S, N}
    return S[v for v in x]
end

function Base.convert(::Type{Array{S}}, da::DataArray{T, N}) where {S, T, N}
    return convert(Array{S, N}, da)
end

function Base.convert(::Type{Vector}, dv::DataVector{T}) where T
    return convert(Array{Union{T, Missing}, 1}, dv)
end

function Base.convert(::Type{Matrix}, dm::DataMatrix{T}) where T
    return convert(Array{Union{T, Missing}, 2}, dm)
end

function Base.convert(::Type{Array}, da::DataArray{T, N}) where {T, N}
    return convert(Array{Union{T, Missing}, N}, da)
end

function Base.convert(
    ::Type{Array{S, N}},
    da::DataArray{T, N},
    replacement::Any
) where {S, T, N} # -> Array{S, N}

    replacementS = convert(S, replacement)
    res = Array{S}(undef, size(da))
    for i in 1:length(da)
        if da.na[i]
            res[i] = replacementS
        else
            res[i] = da.data[i]
        end
    end
    return res
end

function Base.convert(::Type{Vector}, dv::DataVector{T}, replacement::Any) where T
    return convert(Array{T, 1}, dv, replacement)
end

function Base.convert(::Type{Matrix}, dm::DataMatrix{T}, replacement::Any) where T
    return convert(Array{T, 2}, dm, replacement)
end

function Base.convert(::Type{Array}, da::DataArray{T, N}, replacement::Any) where {T, N}
    return convert(Array{T, N}, da, replacement)
end


struct EachFailMissing{T<:DataArray}
    da::T
end
Missings.fail(da::DataArray) = EachFailMissing(da)
Base.length(itr::EachFailMissing) = length(itr.da)
function Base.iterate(itr::EachFailMissing, st=1)
    st > length(itr) && return nothing
    itr.da.na[st] && throw(MissingException("missing value encountered in Missings.fail"))
    return (itr.da.data[st], st + 1)
end
Base.eltype(itr::EachFailMissing) = Missings.T(eltype(itr.da))

struct EachDropMissing{T<:DataArray}
    da::T
end
Missings.skipmissing(da::DataArray) = EachDropMissing(da)
function _next_nonna_ind(da::DataArray, ind::Int)
    ind += 1
    @inbounds while ind <= length(da) && da.na[ind]
        ind += 1
    end
    ind
end
Base.length(itr::EachDropMissing) = length(itr.da) - sum(itr.da.na)
function Base.iterate(itr::EachDropMissing, st=_next_nonna_ind(itr.da, 0))
    st > length(itr.da) && return nothing
    return (itr.da.data[st], _next_nonna_ind(itr.da, ind))
end
Base.eltype(itr::EachDropMissing) = Missings.T(eltype(itr.da))

struct EachReplaceMissing{S<:DataArray, T}
    da::S
    replacement::T
end
Missings.replace(da::DataArray, replacement::Any) =
    EachReplaceMissing(da, replacement)
Base.length(itr::EachReplaceMissing) = length(itr.da)
function Base.iterate(itr::EachReplaceMissing, st=1)
    st > length(itr) && return nothing
    item = itr.da.na[st] ? itr.replacement : itr.da.data[st]
    return (item, st + 1)
end
Base.eltype(itr::EachReplaceMissing) = Missings.T(eltype(itr.da))

Base.collect(itr::EachDropMissing{<:DataVector}) = itr.da.data[.!itr.da.na] # -> Vector
Base.collect(itr::EachFailMissing{<:DataVector}) = copy(itr.da.data) # -> Vector

Base.any(::typeof(ismissing), da::DataArray) = any(da.na) # -> Bool
Base.all(::typeof(ismissing), da::DataArray) = all(da.na) # -> Bool

Missings.ismissing(da::DataArray, I::Real, Is::Real...) = getindex(da.na, I, Is...)

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

function Base.convert(::Type{DataArray{S, N}},
                      a::AbstractArray{T, N}) where {S, T, N} # -> DataArray{S, N}
    return DataArray(convert(Array{S, N}, a), falses(size(a)))
end

function Base.convert(::Type{DataArray{S, N}},
                      a::AbstractArray{T, N}) where {S, T>:Missing, N} # -> DataArray{S, N}
    return DataArray(convert(Array{Union{S, Missing}, N}, a), falses(size(a)))
end

function Base.convert(::Type{DataArray{S}}, x::AbstractArray{T, N}) where {S, T, N}
    convert(DataArray{Missings.T(S), N}, x)
end

function Base.convert(::Type{DataArray}, x::AbstractArray{T, N}) where {T, N}
    convert(DataArray{Missings.T(T), N}, x)
end

Base.convert(::Type{DataArray{T, N}}, x::DataArray{T, N}) where {T, N} = x

function Base.convert(::Type{DataArray{S, N}},
                      x::DataArray{T, N}) where {S, T, N} # -> DataArray{S, N}
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

julia> data(@data [1, 2, missing])
3-element DataArrays.DataArray{Int64,1}:
 1
 2
  missing
```
"""
data(a::AbstractArray) = convert(DataArray, a)

# TODO: Make sure these handle copying correctly
# TODO: Remove these? They have odd behavior, because they convert to Array's.
# TODO: Rethink multi-item documentation approach
for f in (:(Base.float),)
    @eval begin
        function ($f)(da::DataArray) # -> DataArray
            if any(ismissing, da)
                err = "Cannot convert DataArray with missings to desired type"
                throw(MissingException(err))
            else
                ($f)(da.data)
            end
        end
    end
end

"""
    finduniques(da::DataArray) -> (Vector, Int)

Get the unique values in `da` as well as the index of the first `missing` value
in `da` if present, or 0 otherwise.
"""
function finduniques(da::DataArray{T}) where T # -> Vector{T}, Int
    out = Vector{T}(undef, 0)
    seen = Set{T}()
    n = length(da)
    firstmissing = 0
    for i in 1:n
        if ismissing(da, i)
            if firstmissing == 0
                firstmissing = length(out) + 1
            else
                continue
            end
        elseif !in(da.data[i], seen)
            push!(seen, da.data[i])
            push!(out, da.data[i])
        end
    end
    return out, firstmissing
end

function Base.unique(da::DataArray{T}) where T # -> DataVector{T}
    unique_values, firstmissing = finduniques(da)
    n = length(unique_values)
    if firstmissing > 0
        res = DataArray(Vector{T}(undef, n + 1))
        i = 1
        for val in unique_values
            if i == firstmissing
                res.na[i] = true
                i += 1
            end
            res.data[i] = val
            i += 1
        end

        if firstmissing == n + 1
            res.na[n + 1] = true
        end

        return res
    else
        return DataArray(unique_values)
    end
end

function Missings.levels(da::DataArray) # -> DataVector{T}
    unique_values, firstmissing = finduniques(da)
    return DataArray(unique_values)
end
